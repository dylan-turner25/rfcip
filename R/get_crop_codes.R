#' Lookup crop codes for FCIP commodities
#'
#' @param crop A vector of commodity names (not case sensitive) or numeric crop
#' codes. Character codes and mixed names/codes are also accepted. NULL returns
#' all commodities present in the requested years. Every identifier must match.
#' @param year A numeric crop year or vector of crop years. Defaults to the
#' current year. SOB bulk fallback is available from 1989 onward; earlier years
#' require the SOB application or a previously cached lookup.
#' @param force Logical. If TRUE, refresh from the application, then bulk data
#' on failure. If both fail, use a validated prior lookup with a warning.
#' @return A data frame containing commodity_year, numeric commodity_code, and
#' commodity_name. The compatibility columns commodity_abbreviation and
#' annual_planting_code are NA_character_: SOB does not supply these ADM fields.
#' @details A complete, validated lookup is cached separately for each year,
#' before filtering by crop. On a cache miss, the SOB application is tried first,
#' followed by the state/county/crop coverage-level bulk file. Bulk ZIP caches
#' are shared with get_sob_data(). Application and bulk publication/coverage can
#' differ; fallback emits a warning. An absent crop is never replaced by other
#' crops. Multi-year filters match against the union of the requested years.
#' Lookup failure does not imply that a commodity has no actuarial offer.
#' @export
#' @examples
#' \dontrun{
#' get_crop_codes(year = 2023, crop = "corn")
#' get_crop_codes(year = 2024, crop = 41)
#' get_crop_codes(crop = c("corn", "SOYbEaNs"))
#' get_crop_codes(crop = c(41, 81))
#' get_crop_codes()
#' get_crop_codes(year = 2024, force = TRUE)
#' }
#' @source USDA RMA Summary of Business application and bulk data.
get_crop_codes <- function(year = as.numeric(format(Sys.Date(), "%Y")), crop = NULL, force = FALSE) {
  validate_sob_years(year)
  validate_sob_force(force)
  values <- crop_filter_values(crop)
  year <- unique(year)
  # Do not replace a prior application lookup with stale bulk data on refresh.
  bulk_loader <- new_sobcov_loader(force, fallback_to_cache = !force)
  pieces <- lapply(year, function(y) {
    target <- file.path(tools::R_user_dir("rfcip", "cache"),
                        paste0("crop_codes_year_", y, ".rds"))
    cached <- load_crop_code_cache(target, y)
    if (!force && !is.null(cached)) return(cached$data)
    # Invalid configuration must not silently return stale data.
    timeout <- sob_timeout()
    api <- tryCatch(download_crop_codes_year(y, timeout), error = function(e) e)
    if (!inherits(api, "error")) {
      fresh <- list(data = api, source = "api")
    } else {
      fresh <- tryCatch({
        bulk <- bulk_loader(y)
        list(data = validate_crop_code_data(bulk$data, y), source = bulk$source)
      }, error = function(e) e)
      if (inherits(fresh, "error")) {
        reason <- paste0("Crop-code retrieval failed for ", y, ". Application: ",
                         conditionMessage(api), " Bulk: ", conditionMessage(fresh))
        if (is.null(cached)) stop(reason, call. = FALSE)
        warn_sob_cache(paste0(reason, " Using the cached crop lookup."),
                       "rfcip_cache_fallback", target, fresh)
        return(cached$data)
      }
      warning(structure(list(
        message = paste0("Crop-code application lookup failed for ", y,
                         "; using SOB bulk data. Publication/coverage may differ. ",
                         conditionMessage(api)),
        call = NULL, year = y, source = fresh$source, parent = api
      ), class = c("rfcip_source_fallback", "warning", "condition")))
    }
    tryCatch(save_crop_code_cache(fresh, target), error = function(e) {
      warn_sob_cache(paste0("Returning crop codes for ", y,
                            ", but their cache could not be updated: ", conditionMessage(e)),
                     "rfcip_cache_write_warning", target, e)
    })
    fresh$data
  })
  data <- dplyr::bind_rows(pieces)
  if (is.null(values)) return(data)
  matches <- lapply(values, function(value) {
    if (grepl("^[0-9]+$", value)) {
      which(data$commodity_code == as.numeric(value))
    } else {
      which(tolower(data$commodity_name) == tolower(value))
    }
  })
  missing <- lengths(matches) == 0L
  if (any(missing)) {
    stop("Crop identifiers not found in the SOB lookup for ", paste(year, collapse = ", "),
         ": ", paste(values[missing], collapse = ", "), ".", call. = FALSE)
  }
  data[sort(unique(unlist(matches))), , drop = FALSE]
}
