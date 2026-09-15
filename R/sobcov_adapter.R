# Read the resolved API query so fallback uses the exact requested identifiers,
# including historical plan codes, without repeating ADM lookups per year.
adapt_sobcov <- function(data, url) {
  query <- httr::parse_url(url)$query
  split_query <- function(key) {
    value <- query[[key]]
    if (is.null(value) || !nzchar(value)) return(NULL)
    strsplit(value, ",", fixed = TRUE)[[1L]]
  }
  validate_sobcov_options(query$CC)
  data <- filter_sobcov(
    data, crop = split_query("CM"), insurance_plan = split_query("IP"),
    state = split_query("ST"), county = split_query("CT"),
    cov_lvl = split_query("CVL"), delivery_type = split_query("DT"), resolved = TRUE
  )
  ord <- split_query("ORD")
  dimensions <- list(
    CY = "commodity_year", CM = c("commodity_code", "commodity_name"),
    ST = c("state_code", "state_abbrv"), CT = c("county_code", "county_name"),
    DT = c("delivery_type_code", "delivery_type"),
    IP = c("insurance_plan_code", "insurance_plan", "insurance_plan_abbrv"),
    CVL = "cov_level_percent"
  )
  if (any(!ord %in% names(dimensions))) stop("Unsupported SOB COV backup grouping.")
  data$state_abbrv <- data$state_abbreviation
  data$insurance_plan_abbrv <- data$insurance_plan_abbreviation
  # The bulk source has abbreviations, not full plan names. Do not invent names
  # or require an unrelated remote lookup solely to label grouped results.
  data$insurance_plan <- NA_character_
  data$delivery_type_code <- data$delivery_type
  data$delivery_type <- unname(c(RCAT = "Reinsured CAT", RBUP = "Reinsured Buyup",
                                 FCAT = "Federal CAT", FBUP = "Federal Buyup")[data$delivery_type_code])
  data$cov_level_percent <- data$coverage_level_percent
  fields <- c(
    policies_sold = "policies_sold", policies_earning_prem = "policies_earning_prem",
    policies_indemnified = "policies_indemnified", units_earning_prem = "units_earning_prem",
    units_indemnified = "units_indemnified", quantity = "net_reported_quantity",
    companion_endorsed_acres = "endorsed_companion_acres", liabilities = "liability_amount",
    total_prem = "total_premium_amount", subsidy = "subsidy_amount", indemnity = "indemnity_amount",
    efa_prem_discount = "efa_premium_discount", addnl_subsidy = "additional_subsidy_amount",
    state_subsidy = "state_private_subsidy_amount"
  )
  # The API also separates quantity units even for a year-only query, as verified
  # against its retained 2024 workbook. Summing across units would change meaning.
  groups <- unique(c("commodity_year", unlist(dimensions[ord]), "quantity_type"))
  sum_known <- function(x) if (anyNA(x)) NA_real_ else sum(x)
  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(groups)))
  result <- dplyr::summarise(grouped, dplyr::across(dplyr::all_of(unname(fields)), sum_known),
                            .groups = "drop")
  names(result)[match(unname(fields), names(result))] <- names(fields)
  for (name in c("pccp_state_matching_amount", "organic_certified_subsidy_amount",
                  "organic_transitional_subsidy_amount")) result[[name]] <- NA_real_
  ratio <- function(numerator, denominator) {
    # Undefined or unavailable ratios remain missing, not Inf or an invented 0.
    # Retained API workbooks truncate both rates to six decimal places.
    ifelse(is.na(denominator) | denominator == 0, NA_real_,
           trunc(numerator / denominator * 1e6) / 1e6)
  }
  result$earn_prem_rate <- ratio(result$total_prem, result$liabilities)
  result$loss_ratio <- ratio(result$indemnity, result$total_prem)
  dimension_cols <- setdiff(groups, "quantity_type")
  ordered <- c(dimension_cols, names(fields)[1:6], "quantity_type", names(fields)[7:14],
               "pccp_state_matching_amount", "organic_certified_subsidy_amount",
               "organic_transitional_subsidy_amount", "earn_prem_rate", "loss_ratio")
  result[, ordered, drop = FALSE]
}

sob_backup_eligible <- function(error) {
  inherits(error, "rfcip_sob_download_error") &&
    (isTRUE(error$status %in% c(429L, 502L, 503L, 504L)) || sob_transient_error(error$parent))
}

sob_year_with_backup <- function(url, year, timeout, bulk_loader, log = FALSE) {
  if (log) cli::cli_alert_info("SOB year {year}: requesting API data.")
  api <- tryCatch(download_sob_year(url, year, timeout, log), error = function(e) e)
  if (!inherits(api, "error")) {
    if (log) cli::cli_alert_success("SOB year {year}: API data received.")
    return(list(data = api, source = "api"))
  }
  if (!sob_backup_eligible(api)) stop(api)
  bulk <- tryCatch({
    # Reject unsupported source semantics before unnecessary bulk downloads.
    validate_sobcov_options(httr::parse_url(url)$query$CC)
    if (log) cli::cli_alert_info("SOB year {year}: API retries exhausted; trying SOB COV bulk backup.")
    piece <- bulk_loader(year)
    piece$data <- adapt_sobcov(piece$data, url)
    piece
  }, error = function(e) e)
  if (inherits(bulk, "error")) {
    stop(structure(list(
      message = paste0(conditionMessage(api), " SOB COV backup failed: ", conditionMessage(bulk)),
      call = NULL, year = year, url = url, status = api$status, attempts = api$attempts,
      parent = api, bulk_parent = bulk
    ), class = c("rfcip_sob_backup_error", "rfcip_sob_download_error", "error", "condition")))
  }
  warning(structure(list(
    message = paste0("SOB year ", year, ": API retries exhausted; using ",
                     if (bulk$source == "sobcov_cache") "cached " else "", "SOB COV backup. ",
                     "Bulk program coverage/publication may differ; unavailable API fields are NA."),
    call = NULL, year = year, source = bulk$source, parent = api
  ), class = c("rfcip_source_fallback", "warning", "condition")))
  bulk$data <- dplyr::mutate(bulk$data, dplyr::across(dplyr::everything(), as.character))
  bulk
}

normalize_sob_backup_types <- function(data) {
  # readr guesses all-missing columns as logical. Bulk-only API fields must
  # nevertheless have the numeric/character types of the API schema.
  numeric <- c("commodity_year", "cov_level_percent", "policies_sold", "policies_earning_prem",
               "policies_indemnified", "units_earning_prem", "units_indemnified", "quantity",
               "companion_endorsed_acres", "liabilities", "total_prem", "subsidy", "indemnity",
               "efa_prem_discount", "addnl_subsidy", "state_subsidy", "pccp_state_matching_amount",
               "organic_certified_subsidy_amount", "organic_transitional_subsidy_amount",
               "earn_prem_rate", "loss_ratio")
  for (name in intersect(numeric, names(data))) data[[name]] <- as.numeric(data[[name]])
  for (name in intersect(c("commodity_code", "insurance_plan_code"), names(data))) {
    data[[name]] <- as.integer(data[[name]])
  }
  text <- c("state_code", "state_abbrv", "county_code", "county_name", "commodity_name",
            "insurance_plan", "insurance_plan_abbrv", "delivery_type", "delivery_type_code", "quantity_type")
  for (name in intersect(text, names(data))) data[[name]] <- as.character(data[[name]])
  for (name in intersect(c("state_code", "county_code"), names(data))) {
    present <- !is.na(data[[name]])
    width <- if (name == "state_code") "%02d" else "%03d"
    data[[name]][present] <- sprintf(width, as.integer(data[[name]][present]))
  }
  data
}
