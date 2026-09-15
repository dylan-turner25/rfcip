# Published coverage-level files have a fixed positional layout (1989 onward).
sobcov_columns <- function() c(
  "commodity_year", "state_code", "state_abbreviation", "county_code", "county_name",
  "commodity_code", "commodity_name", "insurance_plan_code", "insurance_plan_abbreviation",
  "coverage_category", "delivery_type", "coverage_level_percent", "policies_sold",
  "policies_earning_prem", "policies_indemnified", "units_earning_prem", "units_indemnified",
  "quantity_type", "net_reported_quantity", "endorsed_companion_acres", "liability_amount",
  "total_premium_amount", "subsidy_amount", "state_private_subsidy_amount",
  "additional_subsidy_amount", "efa_premium_discount", "indemnity_amount", "loss_ratio"
)

process_sobcov_zip <- function(path, year) {
  members <- suppressWarnings(utils::unzip(path, list = TRUE))$Name
  if (length(members) != 1L ||
      !grepl("^sobcov_?([0-9]{2}|[0-9]{4})[.]txt$", members, ignore.case = TRUE)) {
    stop("SOB COV archive must contain exactly one recognized data file.")
  }
  # Only a basename matching the above pattern can reach extraction.
  dir <- tempfile("sobcov-extract-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  extracted <- withCallingHandlers(utils::unzip(path, files = members, exdir = dir),
                                   warning = function(w) stop(conditionMessage(w), call. = FALSE))
  if (length(extracted) != 1L || !file.exists(extracted)) stop("Could not extract SOB COV data.")
  bytes <- readBin(extracted, "raw", n = file.info(extracted)$size)
  # RMA uses NUL for unavailable CLIP counts. Replace with whitespace before
  # parsing so delimiters (and missing fields) remain in their original positions.
  bytes[bytes == as.raw(0)] <- charToRaw(" ")
  data <- suppressWarnings(readr::read_delim(
    I(rawToChar(bytes)), delim = "|", col_names = sobcov_columns(), col_types = "c",
    quote = "", trim_ws = TRUE, na = "", progress = FALSE, show_col_types = FALSE
  ))
  if (!nrow(data) || ncol(data) != 28L || nrow(readr::problems(data))) {
    stop("Invalid SOB COV data: expected nonempty records with exactly 28 fields.")
  }
  numeric_cols <- c(1L, 6L, 8L, 12:17, 19:28)
  for (i in numeric_cols) {
    original <- data[[i]]
    value <- suppressWarnings(as.numeric(original))
    if (any(!is.na(original) & (is.na(value) | !is.finite(value)))) {
      stop("Invalid SOB COV numeric field: ", names(data)[i], ".")
    }
    data[[i]] <- value
  }
  for (name in c("commodity_year", "commodity_code", "insurance_plan_code")) {
    if (anyNA(data[[name]]) || any(data[[name]] < 0 | data[[name]] > .Machine$integer.max |
                                  data[[name]] != floor(data[[name]]))) {
      stop("Invalid SOB COV identifier: ", name, ".")
    }
    data[[name]] <- as.integer(data[[name]])
  }
  if (any(data$commodity_year != year)) stop("SOB COV file contains a different commodity year.")
  for (name in c("state_code", "county_code")) {
    width <- if (name == "state_code") 2L else 3L
    if (anyNA(data[[name]]) || any(!grepl(paste0("^[0-9]{1,", width, "}$"), data[[name]]))) {
      stop("Invalid SOB COV FIPS component: ", name, ".")
    }
    data[[name]] <- sprintf(paste0("%0", width, "d"), as.integer(data[[name]]))
  }
  required_text <- c("state_abbreviation", "county_name", "commodity_name",
                     "insurance_plan_abbreviation", "coverage_category", "delivery_type", "quantity_type")
  if (anyNA(data[required_text]) || anyNA(data$coverage_level_percent) ||
      any(data$coverage_level_percent < 0 | data$coverage_level_percent > 1)) {
    stop("Invalid SOB COV dimensions or coverage level.")
  }
  data$fips <- paste0(data$state_code, data$county_code)
  data
}

sobcov_http_download <- function(url, year, path, timeout = sob_timeout(), log = FALSE) {
  rfcip_http_download(url, year, path, timeout, source = "SOB COV",
                      error_class = "rfcip_sobcov_download_error", log = log)
}

locate_sobcov_links <- function(log = FALSE) {
  base <- "https://pubfs-rma.fpac.usda.gov"
  directory <- paste0(base, "/pub/Web_Data_Files/Summary_of_Business/state_county_crop/")
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  sobcov_http_download(paste0(directory, "index.html"), NULL, path, log = log)
  doc <- XML::htmlParse(path)
  on.exit(XML::free(doc), add = TRUE)
  links <- unlist(XML::xpathSApply(doc, "//a/@href"), use.names = FALSE)
  links <- unique(links[grepl("(^|/)sobcov_[0-9]{4}[.]zip$", links, ignore.case = TRUE)])
  urls <- vapply(links, function(link) {
    if (grepl("^https?://", link)) return(link)
    if (startsWith(link, "/")) return(paste0(base, link))
    paste0(directory, sub("^[.]/", "", link))
  }, character(1))
  years <- as.integer(sub(".*sobcov_([0-9]{4})[.]zip$", "\\1", urls, ignore.case = TRUE))
  if (!length(years) || anyDuplicated(years)) stop("SOB COV directory has missing or ambiguous year links.")
  data.frame(year = years, url = unname(urls))
}

new_sobcov_loader <- function(force = FALSE, log = FALSE) {
  links <- NULL
  function(year) {
    if (year < 1989) stop("SOB COV files are available from 1989 onward; cannot retrieve ", year, ".")
    target <- file.path(tools::R_user_dir("rfcip", "cache"), paste0("sobcov_", year, ".zip"))
    cached <- if (file.exists(target)) {
      tryCatch(suppressWarnings(process_sobcov_zip(target, year)), error = function(e) NULL)
    } else NULL
    if (!force && !is.null(cached)) {
      if (log) cli::cli_alert_info("SOB COV year {year}: loading cached bulk data.")
      return(list(data = cached, source = "sobcov_cache"))
    }
    timeout <- sob_timeout()
    path <- tempfile(fileext = ".zip")
    on.exit(unlink(path), add = TRUE)
    fresh <- tryCatch({
      if (is.null(links)) {
        if (log) cli::cli_alert_info("SOB COV year {year}: locating bulk download links.")
        links <<- locate_sobcov_links(log = log)
      }
      url <- links$url[links$year == year]
      if (length(url) != 1L) stop("No SOB COV file found for year ", year, ".")
      if (log) cli::cli_alert_info("SOB COV year {year}: downloading bulk data.")
      sobcov_http_download(url, year, path, timeout, log = log)
      process_sobcov_zip(path, year)
    }, error = function(e) e)
    if (inherits(fresh, "error")) {
      if (is.null(cached)) stop(fresh)
      if (log) cli::cli_alert_info("SOB COV year {year}: refresh failed; loading cached bulk data.")
      warn_sob_cache(paste0("SOB COV year ", year, " refresh failed; using cached data. ",
                            conditionMessage(fresh)), "rfcip_cache_fallback", target, fresh)
      return(list(data = cached, source = "sobcov_cache"))
    }
    if (log) cli::cli_alert_success("SOB COV year {year}: bulk data received.")
    tryCatch({
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      staged <- tempfile("sobcov-staged-", tmpdir = dirname(target), fileext = ".zip")
      on.exit(unlink(staged), add = TRUE)
      if (!file.copy(path, staged)) stop("Could not stage the SOB COV cache.")
      replace_sob_cache_file(staged, target)
    }, error = function(e) {
      warn_sob_cache(paste0("Returning fresh SOB COV year ", year,
                            ", but its cache could not be updated: ", conditionMessage(e)),
                     "rfcip_cache_write_warning", target, e)
    })
    list(data = fresh, source = "sobcov")
  }
}

validate_sobcov_options <- function(comm_cat, group_by = NULL) {
  if (!is.null(group_by)) stop("Explicit SOB COV returns detailed rows; `group_by` must be NULL.")
  if (!identical(comm_cat, "B")) {
    stop("SOB COV supports only comm_cat = 'B' (all published bulk records); S/L classification is unavailable.")
  }
}

sobcov_filter_values <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (!(is.character(x) || is.numeric(x)) || !length(x) || anyNA(x) ||
      any(!nzchar(trimws(as.character(x))))) stop("Invalid `", name, "` filter.")
  trimws(as.character(x))
}

sobcov_code_filter <- function(values, codes, labels, name, allow_absent_codes = FALSE) {
  if (is.null(values)) return(NULL)
  resolved <- lapply(values, function(value) {
    if (grepl("^[0-9]+$", value)) {
      code <- as.numeric(value)
      if (allow_absent_codes || code %in% codes) return(code)
    } else {
      found <- unique(codes[tolower(labels) == tolower(value)])
      if (length(found)) return(found)
    }
    stop("Unknown SOB COV ", name, ": ", value, ".")
  })
  unique(unlist(resolved))
}

filter_sobcov <- function(data, crop = NULL, insurance_plan = NULL, state = NULL,
                          county = NULL, fips = NULL, cov_lvl = NULL, delivery_type = NULL,
                          force = FALSE, resolved = FALSE) {
  values <- lapply(list(crop = crop, insurance_plan = insurance_plan, state = state,
                        county = county, fips = fips, cov_lvl = cov_lvl,
                        delivery_type = delivery_type), function(x) x)
  for (name in names(values)) values[[name]] <- sobcov_filter_values(values[[name]], name)
  keep <- rep(TRUE, nrow(data))
  if (!is.null(crop)) {
    codes <- sobcov_code_filter(values$crop, data$commodity_code, data$commodity_name,
                                "crop", allow_absent_codes = resolved)
    keep <- keep & data$commodity_code %in% codes
  }
  if (!is.null(insurance_plan)) {
    normalize <- function(x) gsub("[^a-z0-9]", "", tolower(x))
    codes <- unlist(lapply(values$insurance_plan, function(value) {
      if (grepl("^[0-9]+$", value)) {
        return(sobcov_code_filter(value, data$insurance_plan_code,
                                  data$insurance_plan_abbreviation, "insurance plan", resolved))
      }
      found <- unique(data$insurance_plan_code[
        normalize(data$insurance_plan_abbreviation) == normalize(value)])
      if (length(found)) return(found)
      get_insurance_plan_codes(year = unique(data$commodity_year), plan = value,
                                force = force)$insurance_plan_code
    }))
    keep <- keep & data$insurance_plan_code %in% codes
  }
  states <- NULL
  if (!is.null(state)) {
    states <- vapply(values$state, function(value) {
      if (grepl("^[0-9]{1,2}$", value)) {
        code <- sprintf("%02d", as.integer(value))
        valid <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13",
                   "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
                   "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",
                   "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48",
                   "49", "50", "51", "53", "54", "55", "56", "60", "66", "69", "72", "78")
        if (!code %in% valid) stop("Invalid state: ", value, ".")
      } else {
        code <- usmap::fips(state = value)
      }
      if (length(code) != 1L || is.na(code) || !nzchar(code)) stop("Invalid state: ", value, ".")
      code
    }, character(1))
    keep <- keep & data$state_code %in% states
  }
  pairs <- NULL
  if (!is.null(county)) {
    if (all(grepl("^[0-9]{4,5}$", values$county))) {
      pairs <- sprintf("%05d", as.integer(values$county))
    } else {
      if (is.null(states)) stop("County names require a state; alternatively supply five-digit FIPS.")
      pairs <- unlist(lapply(values$county, function(value) {
        found <- data$fips[data$state_code %in% states & tolower(data$county_name) == tolower(value)]
        if (!length(found)) stop("Unknown county in the selected state(s): ", value, ".")
        unique(found)
      }))
    }
  }
  if (!is.null(fips)) {
    if (any(!grepl("^[0-9]{4,5}$", values$fips))) stop("`fips` must contain four- or five-digit county FIPS codes.")
    requested <- sprintf("%05d", as.integer(values$fips))
    if (!is.null(pairs) && !setequal(pairs, requested)) stop("Conflicting county and fips filters.")
    pairs <- requested
  }
  if (!is.null(pairs)) {
    if (!is.null(states) && any(!substr(pairs, 1, 2) %in% states)) stop("Conflicting state and county FIPS filters.")
    keep <- keep & data$fips %in% pairs
  }
  if (!is.null(cov_lvl)) {
    coverage <- suppressWarnings(as.numeric(values$cov_lvl))
    if (anyNA(coverage) || any(!is.finite(coverage) | coverage < 0 | coverage > 1)) {
      stop("`cov_lvl` must contain decimal values between zero and one.")
    }
    keep <- keep & data$coverage_level_percent %in% coverage
  }
  if (!is.null(delivery_type)) {
    delivery <- toupper(values$delivery_type)
    if (any(!delivery %in% c("RBUP", "RCAT", "FBUP", "FCAT"))) stop("Invalid delivery_type.")
    keep <- keep & data$delivery_type %in% delivery
  }
  data[keep, , drop = FALSE]
}

get_sobcov_data <- function(year, crop = NULL, insurance_plan = NULL, state = NULL,
                            county = NULL, fips = NULL, cov_lvl = NULL, delivery_type = NULL,
                            comm_cat = "B", group_by = NULL, force = FALSE, log = FALSE) {
  validate_sobcov_options(comm_cat, group_by)
  if (any(year < 1989)) stop("SOB COV files are available from 1989 onward.")
  year <- unique(year)
  loader <- new_sobcov_loader(force, log)
  pieces <- lapply(year, loader)
  data <- filter_sobcov(dplyr::bind_rows(lapply(pieces, `[[`, "data")), crop, insurance_plan,
                        state, county, fips, cov_lvl, delivery_type, force)
  attr(data, "rfcip_sources") <- data.frame(
    year = year, source = vapply(pieces, `[[`, character(1), "source"))
  data
}
