#' Downloads and imports data from RMA's summary of buisness app
#'
#' @param year a numeric value (either single value or a vector of values) that indicate the crop year (ex: `2024` or `c(2022,2023,2024)`)
#' @param crop can be either a character string indicating a crop (i.e. `corn`) or a numeric value indicating the crop code (i.e. `41`). Inputting a vector with multiple values will return data for multiple crops. To get a data frame containing all the available crops and crop codes use `get_crop_codes()`.
#' @param delivery_type a character string of either "RBUP" for buyup policies or "RCAT" for catastrophic policies. Leaving blank will return data for both types aggregated while inputing a vector with both values (i.e. `c("RBUP","RCAT")`) will return disaggregated data for both types.
#' @param insurance_plan can be either a character string indicating an insurance plan (ex: `yp` and `yield protection` are both valid) or a numeric value indicating the insurance plan code (i.e. `1`). Inputting a vector with multiple values will return data for multiple insurance plans. To get a data frame containing all the available insurance plans and insurance plan codes use `get_insurance_plan_codes()`.
#' @param state can be a character string indicating the state abbreviation or state name. Numeric values corresponding to state FIPS codes can also be supplied.
#' @param county either a character string with a county name or 5-digit FIPS code corresponding to a county. when the county is specified using the name of the county, the state parameter must also be specified.
#' @param fips a numeric value corresponding to a 5-digit FIPS code of a U.S. county.
#' @param cov_lvl a numeric value indicating the coverage level. Valid coverage levels are `c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95)`
#' @param comm_cat a character vector of either "S" for standard, "L" for livestock, or "B" for both (the default).
#' @param dest_file an optional character string specifying a .xlsx file path. When specified, the function will export the data to the supplied file path instead of returning a the data as tibble.
#' @param sob_version a character string indicating the version of the summary of business to use. The default is "sob" which uses the summary of business app. The alternative is "sobtpu" which downloads the summary of business by type, practice, and unit structure.
#' @param group_by an optional character where any of the other parameter names
#' can be entered to further disagregate the data. When left empty, the function
#' returns data the level of dissagregation associated with the specified parameters.
#' For example, `get_sob_data(year = 2023:2024)` will return data for 2023 and 2024
#' dis-aggregated by year only where. The function call
#' `get_sob_data(year = 2023:2024, group_by = c("insurance_plan","cov_lvl"))` will
#'  return the same data, but further dissagregated by insurance plan and coverage level
#' @param force Logical (default FALSE). Each TRUE call attempts fresh retrieval.
#' For SOB exports, a failed download or invalid response falls back to a usable
#' matching disk cache with an `rfcip_cache_fallback` warning.
#' @details SOB results are cached on disk for the exact query. A successful
#' refresh replaces that cache after all requested years have been validated.
#' If saving fresh data fails, the fresh result is returned with an
#' `rfcip_cache_write_warning`; the previous usable cache is preserved.
#' Invalid arguments are errors, not reasons to return cached results.
#' Insurance-plan filters resolve through the ADM A00460 lookup for the
#' requested years (2011 for earlier years) and receive the refresh intent.
#' Errors from the ADM lookup retain their own source and fallback behavior.
#' Crop lookup remains separately memoised and does not gain a guaranteed
#' refresh from this argument. SOBTPU bulk caching retains its existing behavior.
#' Omitting `year` requests the current calendar year. Clearing SOB's disk
#' cache removes its saved results; there is no additional SOB memory cache.
#'
#' Each yearly SOB export makes at most four attempts for HTTP 429, 502, 503,
#' or 504 and recognized transient connection/transfer errors. Retry waits are
#' approximately 1, 2, and 4 seconds with jitter. A server's `Retry-After`
#' delay (seconds or HTTP date) is respected, including on 503. Total retry
#' sleep is limited to 60 seconds per year; a longer required delay stops
#' retrieval rather than retrying early. After failure, the usual forced-call
#' cache fallback applies. Interruptions propagate immediately.
#'
#' Each transfer uses the R `timeout` option, explicitly passed to httr.
#' It must be a finite numeric number of seconds between 0.001 and
#' `.Machine$integer.max / 1000`. This transfer timeout is separate from the
#' retry sleep budget; a multi-year call can take longer than 60 seconds.
#' Other HTTP errors, certificate/local write errors, unrecognized transport
#' errors, and invalid workbooks are not retried. Typed transport errors are
#' recognized with curl 6.0.0 or later; older untyped errors fail immediately.
#' An `rfcip_sob_download_error` retains `year`, `status` (when available),
#' `attempts`, `url`, and the underlying `parent` condition (when available).
#' These retries apply only to SOB exports, not ADM lookup or SOBTPU downloads.
#' @return Returns a tibble
#' @export
#' @importFrom utils download.file
#' @importFrom readxl read_excel
#' @import cli
#' @examples
#' \dontrun{
#' get_sob_data(year = 2023)
#' get_sob_data(year = 2015:2020, crop = "corn")
#' get_sob_data(year = 2022, crop = c(41, 81), group_by = "state")
#' }
#' @source Data is downloaded directly from RMA's summary of business app: \url{https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator}
get_sob_data <- function(year = as.numeric(format(Sys.Date(), "%Y")), 
                         crop = NULL, 
                         delivery_type = NULL,
                         insurance_plan = NULL, 
                         state = NULL, 
                         county = NULL, 
                         fips = NULL, 
                         cov_lvl = NULL, 
                         comm_cat = "B", 
                         dest_file = NULL, 
                         group_by = NULL,
                         sob_version = "sob",
                         force = FALSE) {
  
  validate_sob_years(year)
  validate_sob_force(force)
  sob_version <- match.arg(sob_version, c("sob", "sobtpu"))

  if (sob_version == "sob") {
    cache_params <- list(
      year = year, crop = crop, delivery_type = delivery_type,
      insurance_plan = insurance_plan, state = state, county = county,
      fips = fips, cov_lvl = cov_lvl, comm_cat = comm_cat, group_by = group_by
    )
    cache_key <- generate_cache_key("sob", cache_params, "parquet")
    cache_file <- file.path(tools::R_user_dir("rfcip", which = "cache"), cache_key)
    full_data <- if (!force) load_usable_sob_cache(cache_file) else NULL

    if (!is.null(full_data)) {
      cli::cli_alert_info("Loading data from cache")
    } else {
      # Resolve and validate inputs before the download/fallback handler. Invalid
      # arguments must not be hidden by a usable result cache on a forced call.
      timeout <- sob_timeout()
      plans <- if (!is.null(insurance_plan)) {
        get_insurance_plan_codes(year = unique(year), plan = insurance_plan, force = force)
      } else NULL
      urls <- vapply(year, function(y) {
        codes <- if (!is.null(plans)) {
          unique(plans$insurance_plan_code[plans$commodity_year == max(y, 2011)])
        } else NULL
        if (!is.null(plans) && !length(codes)) {
          stop("No matching insurance plan codes for lookup year ", max(y, 2011), ".")
        }
        get_sob_url(
          year = y, crop = crop, delivery_type = delivery_type,
          insurance_plan = insurance_plan, state = state, county = county,
          fips = fips, cov_lvl = cov_lvl, comm_cat = comm_cat, group_by = group_by,
          .insurance_plan_codes = codes
        )
      }, character(1))

      cli::cli_alert_info("Downloading and caching new data")
      progress <- cli::cli_progress_bar("Downloading summary of business data for specified crop years",
                                        total = length(year))
      fresh <- tryCatch({
        pieces <- lapply(seq_along(year), function(i) {
          cli::cli_progress_update(id = progress)
          download_sob_year(urls[[i]], year[[i]], timeout)
        })
        convert_sob_types(validate_sob_data(dplyr::bind_rows(pieces)))
      }, error = function(e) e)
      cli::cli_progress_done(id = progress)

      if (inherits(fresh, "error")) {
        full_data <- if (force) load_usable_sob_cache(cache_file) else NULL
        if (is.null(full_data)) stop(fresh)
        warn_sob_cache(
          paste0("SOB download failed; using cached data. ", conditionMessage(fresh)),
          "rfcip_cache_fallback", cache_file, fresh
        )
      } else {
        full_data <- fresh
        # A persistence failure must not discard a valid freshly retrieved result.
        tryCatch(save_sob_cache(full_data, cache_file), error = function(e) {
          warn_sob_cache(
            paste0("Returning fresh SOB data, but the cache could not be updated: ",
                   conditionMessage(e)),
            "rfcip_cache_write_warning", cache_file, e
          )
        })
      }
    }
  } else {
    full_data <- convert_sob_types(get_sobtpu_data(
      year = year, crop = crop, insurance_plan = insurance_plan, state = state,
      county = county, fips = fips, cov_lvl = cov_lvl, force = force
    ))
  }

  if (is.null(dest_file)) {
    return(full_data)
  } else {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("writexl package needed for Excel export. Please install it: install.packages('writexl')")
    }
    writexl::write_xlsx(full_data, path = dest_file)
  }
}
