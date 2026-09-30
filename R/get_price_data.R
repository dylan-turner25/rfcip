#' Download RMA projected and harvest prices used for revenue protection insurance plans.
#'
#' @param year a numeric value (either single value or a vector of values) that indicate the crop year (ex: `2024` or `c(2022,2023,2024)`)

#' @param crop can be either a character string indicating a crop (i.e. `corn`) or a numeric value indicating the crop code (i.e. `41`). 
#' Inputting a vector with multiple values will return data for multiple crops. 
#' To look up crop codes, use the `get_crop_codes()` function.
#' @param state either a numeric value representing the state fips code or a character string that can either be the full state name or two-digit state abbreviation.
#' @param force logical (default FALSE). If TRUE, attempts to download fresh data regardless of cache, but falls back to cached data on failure with a warning
#' @return Returns a tibble containing projected and harvest prices.
#' @details Commodity codes are padded to four digits in requests. Empty or
#' malformed cached responses are refreshed automatically. Fresh responses are
#' parsed and validated before replacing the disk cache. Empty responses raise
#' an explicit no-data error. Repeated forced calls attempt fresh downloads;
#' a failed refresh can return a validated prior cache with a warning.
#' @export
#' 
#' @importFrom stringr str_pad
#' @importFrom tidyr pivot_wider
#' @import XML
#' @import cli
#'
#' @examples
#' \dontrun{get_price_data(2024,"corn")}
#' \dontrun{get_price_data(year = 2022:2024, state = "VA")}
#' @source Data is downloaded directly from RMA's price discovery app: \url{https://public-rma.fpac.usda.gov/apps/PriceDiscovery/GetPrices/ManyPrices} 
get_price_data <- function(year = NULL, crop = NULL, state = NULL, force = FALSE) {
  if (!is.null(year)) validate_sob_years(year)
  validate_sob_force(force)
  crop_filter_values(crop)
  cache_key <- generate_cache_key("price", list(year = year, crop = crop, state = state), "xml")
  cache_file <- file.path(tools::R_user_dir("rfcip", "cache"), cache_key)
  cached <- if (file.exists(cache_file)) {
    tryCatch(suppressWarnings(parse_price_data(readLines(cache_file, warn = FALSE))),
             error = function(e) NULL)
  } else NULL
  if (!force && !is.null(cached)) {
    cli::cli_alert_info("Loading data from cache")
    return(cached)
  }
  # Resolve inputs outside the download fallback, so invalid crop/state filters
  # cannot be hidden by a previously cached response.
  url <- get_price_url(year, crop, state, force)
  cli::cli_alert_info("Downloading and caching data")
  fresh <- tryCatch({
    lines <- download_price_xml(url)
    list(lines = lines, data = parse_price_data(lines))
  }, error = function(e) e)
  if (inherits(fresh, "error")) {
    if (is.null(cached)) stop(fresh)
    warn_sob_cache(paste0("Price refresh failed; using cached data. ", conditionMessage(fresh)),
                   "rfcip_cache_fallback", cache_file, fresh)
    return(cached)
  }
  tryCatch(save_price_xml(fresh$lines, cache_file), error = function(e) {
    warn_sob_cache(paste0("Returning fresh prices, but their cache could not be updated: ",
                          conditionMessage(e)), "rfcip_cache_write_warning", cache_file, e)
  })
  fresh$data
}
