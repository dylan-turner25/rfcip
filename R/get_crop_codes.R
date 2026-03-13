#' Lookup crop codes for FCIP commodities
#'
#' @param crop A vector of either character strings with commodity names (not case sensitive) or a vector with numeric crop codes. Can be left NULL to return crop codes for all commodities.
#' @param year A single numeric value, or vector of numeric values, indicating what years of the ADM commodity table should be used to get crop names and crop codes. Defaults to the current year.
#' @param force logical (default FALSE). If TRUE, attempts to download fresh data regardless of cache, but falls back to cached data on failure with a warning
#' @return Returns a data frame containing commodity_year, commodity_code, commodity_name, commodity_abbreviation, and annual_planting_code.
#' @export
#'
#' @examples
#' \dontrun{
#' get_crop_codes(year = 2023, crop = "corn")
#' get_crop_codes(year = 2024, crop = 41)
#' get_crop_codes(crop = c("corn", "SOYbEaNs"))
#' get_crop_codes(crop = c(41, 81))
#' get_crop_codes()
#' get_crop_codes(year = 2024, force = TRUE)  # Force fresh download
#' }
#'
#' @source Data is sourced from the USDA RMA Actuarial Data Master commodity table (A00420).
get_crop_codes <- function(year = as.numeric(format(Sys.Date(), "%Y")), crop = NULL, force = FALSE) {

  # Pull commodity data from ADM
  data <- get_adm_data(year = year, dataset = "A00420", show_progress = FALSE, force = force)

  # Select relevant columns
  data <- data[, c("commodity_year", "commodity_code", "commodity_name",
                    "commodity_abbreviation", "annual_planting_code")]

  # Convert commodity_name to character for case-insensitive matching
  data$commodity_name <- as.character(data$commodity_name)
  data$commodity_abbreviation <- as.character(data$commodity_abbreviation)
  data$annual_planting_code <- as.character(data$annual_planting_code)

  # Deduplicate
  data <- unique(data)

  if (!is.null(crop) & is.character(crop)) {
    to_return <- data[which(tolower(data$commodity_name) %in% tolower(crop)), ]
    if (nrow(to_return) == 0) {
      warning("One or more of the entered crop codes or crop names is not valid, returning all crop names and crop codes:")
      return(data)
    } else {
      return(to_return)
    }
  } else if (!is.null(crop) & is.numeric(crop)) {
    to_return <- data[which(as.numeric(data$commodity_code) %in% crop), ]
    if (nrow(to_return) == 0) {
      warning("One or more of the entered crop codes or crop names is not valid, returning all crop names and crop codes:")
      return(data)
    } else {
      return(to_return)
    }
  } else {
    return(data)
  }
}
