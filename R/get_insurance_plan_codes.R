#' Lookup insurance plan codes for FCIP insurance plans
#'
#' @param year A single numeric value, or vector of numeric values, indicating what years of the summary of business should be used to get crop names and crop codes. Defaults to the current year.
#' @param plan can be either a character string indicating an insurance plan (ex: `yp` and `yield protection` are both valid) or a numeric value indicating the insurance plan code (i.e. `1`). Inputting nothing for the `plan` argument will return codes for all insurance plans in the specified year(s).
#' @param force logical (default FALSE). If TRUE, attempts to download fresh data regardless of cache, but falls back to cached data on failure with a warning
#'
#' @return Returns a tibble containing the relevant commodity year, insurance plan codes, insurance plan name, and insurance plan abbreviation
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
#' @source Data is downloaded directly from RMA's summary of business app: \url{https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator}
get_insurance_plan_codes <- function(year = as.numeric(format(Sys.Date(), "%Y")), plan = NULL, force = FALSE) {
  
  # Generate cache key based on parameters
  cache_params <- list(
    year = year,
    plan = plan
  )
  
  cache_key <- generate_cache_key("insurance_plans", cache_params, "xlsx")
  dest_dir <- tools::R_user_dir("rfcip", which = "cache")
  cache_file <- file.path(dest_dir, cache_key)
  
  # Check if Excel data is already cached and force=FALSE
  if (file.exists(cache_file) && !force) {
    cli::cli_alert_info("Loading insurance plan codes from cache")
    data_path <- cache_file
  } else {
    # Download and cache new Excel data
    cli::cli_alert_info("Downloading and caching insurance plan codes data")
    
    success <- FALSE
    
    tryCatch({
      # url for all insurance plans with codes
      url <- paste0("https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel?CY=", paste(year, collapse = ","), "&ORD=CY,IP&CC=B&VisibleColumns=CommodityYear,InsurancePlanCode,InsurancePlanName,InsurancePlanAbbreviation&SortField=&SortDir=")
      
      # download the file to temporary location
      temp_path <- tempfile(fileext = ".xlsx")
      download.file(url, destfile = temp_path, mode = "wb", quiet = TRUE)
      
      # Cache the downloaded file
      cached_file <- cache_raw_data(temp_path, cache_key, "zip")
      
      # Clean up temp file since we now have cached version
      unlink(temp_path)
      
      # Use cached file for reading
      data_path <- cached_file
      
      success <- TRUE
    }, error = function(e) {
      # If download failed and force=TRUE, try to use cached data
      if (force && file.exists(cache_file)) {
        cli::cli_alert_warning("Download failed, using cached insurance plan codes data")
        data_path <<- cache_file
        success <<- TRUE
      } else {
        stop("Failed to download insurance plan codes data and no cached data available: ", e$message)
      }
    })
    
    if (!success) {
      stop("Failed to download insurance plan codes data")
    }
  }

  # load data
  data <- suppressMessages(janitor::clean_names(readxl::read_excel(data_path)))

  # check for warnings that appear in the header, if so, need to skip one line
  if (colnames(data)[2] == "x2") {
    data <- suppressMessages(janitor::clean_names(readxl::read_excel(data_path, skip = 1)))
  }

  # filter to desired columns
  data <- data[, c("commodity_year", "insurance_plan_code", "insurance_plan", "insurance_plan_abbrv")]
  
  # remove any duplicate entries
  data <- dplyr::distinct(data)
  
  # if no plan is entered, return all plans
  if (is.null(plan)) {
    return(data)
  }

  # check entered plan against insurance plan abbreviation
  to_return <- data[which(tolower(data$insurance_plan_abbrv) %in% tolower(plan)), ]
  if (nrow(to_return) > 0) {
    return(to_return)
  }

  # check entered plan against full insurance plan names
  to_return <- data[which(tolower(data$insurance_plan) %in% tolower(plan)), ]
  if (nrow(to_return) > 0) {
    return(to_return)
  }

  # check entered plan against insurance plan codes
  to_return <- suppressWarnings(data[which(as.numeric(data$insurance_plan_code) %in% as.numeric(plan)), ])
  if (nrow(to_return) > 0) {
    return(to_return)
  }

  # if still no matches, issue warning and return all plan codes
  if (nrow(to_return) == 0) {
    stop("One or more of the entered insurance plan codes or insurance plan names is not valid. Enter `get_insurance_plan_codes()` to see all plans and codes.")
  }
}
