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
#' @param force logical (default FALSE). If TRUE, attempts to download fresh data regardless of cache, but falls back to cached data on failure with a warning
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
  
  # input checking
  stopifnot("`year` must be a numeric value or vector of numeric values." = is.numeric(year))

  # initialize
  full_data <- NULL
  

  # if sob_version is "sob", pull data from the application
  if(sob_version == "sob"){
    
    # Generate cache key based on parameters
    cache_params <- list(
      year = year,
      crop = crop,
      delivery_type = delivery_type,
      insurance_plan = insurance_plan,
      state = state,
      county = county,
      fips = fips,
      cov_lvl = cov_lvl,
      comm_cat = comm_cat,
      group_by = group_by
    )
    
    cache_key <- generate_cache_key("sob", cache_params, "parquet")
    dest_dir <- tools::R_user_dir("rfcip", which = "cache")
    cache_file <- file.path(dest_dir, cache_key)
    
    # Check if data is already cached and force=FALSE
    if (file.exists(cache_file) && !force) {
      cli::cli_alert_info("Loading data from cache")
      full_data <- load_cached_data(cache_file)
    } else {
      # Download and cache new data
      cli::cli_alert_info("Downloading and caching new data")
      
      success <- FALSE
      
      # initialize progress bar
      cli::cli_progress_bar("Downloading summary of business data for specified crop years", total = length(year))
      
      # loop over years to avoid server timeout issues.
      tryCatch({
        for (y in year) {
          
          cli::cli_progress_update()
          
          url <- get_sob_url(
            year = y,
            crop = crop,
            delivery_type = delivery_type,
            insurance_plan = insurance_plan,
            state = state,
            county = county,
            fips = fips,
            cov_lvl = cov_lvl,
            comm_cat = comm_cat,
            group_by = group_by
          )
      
          temp_data <- tempfile(fileext = ".xlsx")
      
          utils::download.file(url, destfile = temp_data, mode = "wb", quiet = T)
      
          data <- suppressMessages(janitor::clean_names(readxl::read_excel(temp_data)))
      
          if (colnames(data)[2] == "x2") {
            data <- suppressMessages(janitor::clean_names(readxl::read_excel(temp_data, skip = 1)))
          }
          
          # remove the temporary file
          unlink(temp_data)
          
          # convert all columns to character values
          data <- dplyr::mutate(data, dplyr::across(dplyr::everything(), as.character))
          
          # bind temp data to full data
          full_data <- dplyr::bind_rows(full_data, data)
        }
        success <- TRUE
      }, error = function(e) {
        if (force && file.exists(cache_file)) {
          cli::cli_alert_warning("Download failed, using cached data")
          full_data <<- load_cached_data(cache_file)
          success <<- TRUE
        } else {
          stop(e)
        }
      })
        
      # close progress bar
      cli::cli_progress_done()
      
      # Cache the processed data only if download was successful
      if (success && !file.exists(cache_file)) {
        cache_processed_data(full_data, cache_key)
      }
    }
    
  }
  
  # is sob_version is "sobtpu", pull data from bulk files
  if(sob_version == "sobtpu"){
    
    # use the helper function get_sobtpu_data
    full_data <- get_sobtpu_data(
      year = year,
      crop = crop,
      insurance_plan = insurance_plan,
      state = state,
      county = county,
      fips = fips,
      cov_lvl = cov_lvl,
      force = force
    )
    
    
  
    
  }
  
  # perform any type checking and converting here
  full_data <- suppressMessages(
    readr::type_convert(
      full_data,
      col_types = readr::cols(
        commodity_code = readr::col_integer(),
        insurance_plan_code = readr::col_integer(),
        cov_level_percent = readr::col_double()
        
      )
    )
  )

  if (is.null(dest_file)) {
    return(full_data)
  } else {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("writexl package needed for Excel export. Please install it: install.packages('writexl')")
    }
    writexl::write_xlsx(full_data, path = dest_file)
  }
}
