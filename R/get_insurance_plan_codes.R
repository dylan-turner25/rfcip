#' Lookup insurance plan codes for FCIP insurance plans
#'
#' @param year A numeric year or vector of ADM reinsurance years. Defaults to the current year. Years before 2011 use the 2011 lookup with a warning.
#' @param plan can be either a character string indicating an insurance plan (ex: `yp` and `yield protection` are both valid) or a numeric value indicating the insurance plan code (i.e. `1`). Inputting nothing for the `plan` argument will return codes for all insurance plans in the specified year(s).
#' @param force Logical (default FALSE). Forwarded to `get_adm_data()` on every call to attempt fresh asset retrieval. The existing ADM cache controls fallback on failure.
#'
#' @return A tibble with `commodity_year`, `insurance_plan_code`, `insurance_plan`, and `insurance_plan_abbrv`. For compatibility, `commodity_year` contains the ADM reinsurance year used for lookup, including 2011 for pre-2011 requests.
#' @details The complete A00460 asset is cached by year; plan filters are applied
#' after loading, so different filters reuse the same asset. Lookup data comes
#' from the package's GitHub release assets, independently of the SOB application.
#' ADM lookup may still query release metadata when its data file is cached.
#' Names and abbreviations describe the source year and do not establish
#' historical plan participation. The legacy abbreviation `RP-HPE` is accepted
#' as an alias for `RPHPE`. All supplied identifiers must match a plan.
#' Generic ADM cache and fallback behavior is inherited unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#' get_insurance_plan_codes()
#' get_insurance_plan_codes(year = 2023, plan = "yp")
#' get_insurance_plan_codes(year = 2023, plan = 1)
#' get_insurance_plan_codes(year = 2023, plan = "yield protection")
#' get_insurance_plan_codes(year = 2018:2022, plan = c("yp", "rp"))
#' get_insurance_plan_codes(year = 2024, force = TRUE)  # Force fresh download
#' }
#' @importFrom janitor clean_names
#' @importFrom readxl read_excel
#' @import cli
#' @source USDA RMA Actuarial Data Master insurance-plan table A00460, distributed through the package's GitHub release assets.
get_insurance_plan_codes <- function(year = as.numeric(format(Sys.Date(), "%Y")), plan = NULL, force = FALSE) {
  validate_sob_years(year)
  validate_sob_force(force)
  if (!is.null(plan) &&
      (!(is.character(plan) || is.numeric(plan)) || !length(plan) ||
       anyNA(plan) || any(!nzchar(trimws(as.character(plan)))))) {
    stop("`plan` must contain non-missing plan names, abbreviations, or codes.")
  }

  lookup_years <- unique(pmax(year, 2011))
  if (any(year < 2011)) {
    warning(structure(list(
      message = paste0("A00460 is unavailable before 2011; using the 2011 plan lookup for ",
                       paste(unique(year[year < 2011]), collapse = ", "),
                       ". The returned commodity_year identifies the lookup source year."),
      call = NULL
    ), class = c("rfcip_lookup_year_fallback", "warning", "condition")))
  }

  # The ADM cache stores a complete year/dataset asset, independent of plan filters.
  adm <- get_adm_data(year = lookup_years, dataset = "A00460",
                      show_progress = FALSE, force = force)
  required <- c("reinsurance_year", "insurance_plan_code",
                "insurance_plan_name", "insurance_plan_abbreviation")
  if (!is.data.frame(adm) || !all(required %in% names(adm))) {
    stop("A00460 is missing required insurance-plan columns.")
  }
  data <- dplyr::tibble(
    commodity_year = suppressWarnings(as.integer(as.character(adm$reinsurance_year))),
    insurance_plan_code = suppressWarnings(as.integer(as.character(adm$insurance_plan_code))),
    insurance_plan = as.character(adm$insurance_plan_name),
    insurance_plan_abbrv = as.character(adm$insurance_plan_abbreviation)
  )
  if (anyNA(data) || any(!nzchar(trimws(data$insurance_plan))) ||
      any(!nzchar(trimws(data$insurance_plan_abbrv))) ||
      !setequal(unique(data$commodity_year), lookup_years)) {
    stop("A00460 contains invalid or missing insurance-plan lookup data.")
  }
  data <- dplyr::distinct(data)
  if (is.null(plan)) return(data)

  # Accept mixed names/codes/abbreviations, including the legacy RP-HPE spelling.
  normalize_abbr <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  normalize_name <- function(x) {
    gsub("\\bprotection\\b", "prot", tolower(trimws(x)))
  }
  matches <- lapply(as.character(plan), function(value) {
    code <- suppressWarnings(as.numeric(value))
    which((!is.na(code) & data$insurance_plan_code == code) |
          normalize_abbr(data$insurance_plan_abbrv) == normalize_abbr(value) |
          normalize_name(data$insurance_plan) == normalize_name(value))
  })
  missing <- lengths(matches) == 0L
  if (any(missing)) {
    stop("One or more insurance plan codes or names is not valid: ",
         paste(plan[missing], collapse = ", "),
         ". Enter `get_insurance_plan_codes()` to see available plans.")
  }
  data[sort(unique(unlist(matches))), , drop = FALSE]
}
