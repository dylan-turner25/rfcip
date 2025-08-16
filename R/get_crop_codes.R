#' Lookup crop codes for FCIP commodities
#'
#' @param crop A vector of either character strings with commodity names (not case sensitive) or a vector with numeric crop codes. Can be left NULL to return crop codes for all commodities.
#' @param year A single numeric value, or vector of numeric values, indicating what years of the summary of business should be used to get crop names and crop codes. Defaults to the current year.
#' @param force logical (default FALSE). If TRUE, attempts to download fresh data regardless of cache, but falls back to cached data on failure with a warning
#' @return Returns a tibble containing the relevant commodity year, commodity codes and commodity names.
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
#' @importFrom janitor clean_names
#' @importFrom readxl read_excel
#' @import httr
#' @import cli 
#'
#' @source Data is downloaded directly from RMA's summary of business app: \url{https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator}
get_crop_codes <- function(year = as.numeric(format(Sys.Date(), "%Y")), crop = NULL, force = FALSE) {
  
  # Generate cache key based on parameters
  cache_params <- list(
    year = year,
    crop = crop
  )
  
  cache_key <- generate_cache_key("crop_codes", cache_params, "xlsx")
  dest_dir <- tools::R_user_dir("rfcip", which = "cache")
  cache_file <- file.path(dest_dir, cache_key)
  
  # Check if Excel data is already cached and force=FALSE
  if (file.exists(cache_file) && !force) {
    cli::cli_alert_info("Loading crop codes from cache")
    tf <- cache_file
  } else {
    # Download and cache new Excel data
    cli::cli_alert_info("Downloading and caching crop codes data")
    
    success <- FALSE
    
    tryCatch({
      # url for all commodities with commodity codes
      url <- paste0("https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel?CY=", paste(year, collapse = ","), "&ORD=CY,CM&CC=S&VisibleColumns=CommodityYear,CommodityCode,CommodityName&SortField=&SortDir=")

      # download the file to temporary location
      tf <- tempfile(fileext = ".xlsx")
      httr::GET(url, httr::write_disk(tf))
      
      # Cache the downloaded file
      cached_file <- cache_raw_data(tf, cache_key, "zip")
      
      # Clean up temp file since we now have cached version
      unlink(tf)
      
      # Use cached file for reading
      tf <- cached_file
      
      success <- TRUE
    }, error = function(e) {
      # If download failed and force=TRUE, try to use cached data
      if (force && file.exists(cache_file)) {
        cli::cli_alert_warning("Download failed, using cached crop codes data")
        tf <<- cache_file
        success <<- TRUE
      } else {
        stop("Failed to download crop codes data and no cached data available: ", e$message)
      }
    })
    
    if (!success) {
      stop("Failed to download crop codes data")
    }
  }

  # load the data from the file
  data <- suppressMessages(janitor::clean_names(readxl::read_excel(tf)))

  # check for warnings that appear in the header, if so, need to skip one line
  if (colnames(data)[2] == "x2") {
    data <- suppressMessages(janitor::clean_names(readxl::read_excel(tf, skip = 1)))
  }

  # select necessary columns
  data <- data[, c("commodity_year", "commodity_code", "commodity_name")]


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
