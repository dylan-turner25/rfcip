#' Builds a cause of loss data set using data from the specified years
#'
#' @param year a single numeric value or vector of values specifying the commodity years that should be used to construct the cause of loss data set
#' @param force logical (default FALSE). If TRUE, attempts to download fresh data regardless of cache, but falls back to cached data on failure with a warning
#'
#' @return returns a data frame with cause of loss data corresponding to the specified commodity years
#' @export
#'
#' @examples
#' \dontrun{
#' get_col_data(year = 2020)
#' get_col_data(year = 2019:2021)
#' }
#' @importFrom utils unzip
#' @importFrom utils read.delim
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#' @importFrom readr type_convert
#' @import cli
get_col_data <- function(year = c(as.numeric(format(Sys.Date(), "%Y")):as.numeric(format(Sys.Date(), "%Y")) - 4), force = FALSE) {
  # input checking
  stopifnot("`year` must be a vector of numeric values." = is.numeric(year))

  # Check which years are already cached
  cache_check <- check_cached_years("col", year)
  cached_years <- cache_check$cached_years
  missing_years <- cache_check$missing_years
  
  # If force=TRUE, treat all years as missing but keep track of cached data for fallback
  cached_files_for_fallback <- NULL
  if (force) {
    cached_files_for_fallback <- cache_check$cached_files
    missing_years <- year
    cached_years <- numeric(0)
  }
  
  # Load data from cached files
  cached_data_list <- list()
  if (length(cached_years) > 0) {
    cli::cli_alert_info("Loading {length(cached_years)} year{?s} from cache: {paste(cached_years, collapse = ', ')}")
    for (cached_file in cache_check$cached_files) {
      cached_zip <- load_cached_data(cached_file)
      processed_data <- process_col_zip(cached_zip)
      cached_data_list <- append(cached_data_list, list(processed_data))
    }
  }
  
  # Download and process missing years
  missing_data_list <- list()
  if (length(missing_years) > 0) {
    # get the current location of the col files
    cli::cli_alert_info("Locating cause of loss download links on RMA's website.")
    col_urls <- locate_col_links()
    cli::cli_alert_success("Download links located.")

    # initialize progress bar for missing years only
    cli::cli_progress_bar("Downloading cause of loss files for missing years", total = length(missing_years))

    # loop over missing years only
    for (y in missing_years) {
      cli::cli_progress_update()

      # locate url corresponding to the crop year
      url <- col_urls$url[which(col_urls$year == y)]

      if (length(url) == 0) {
        cli::cli_alert_warning("No data found for year {y}")
        next
      }

      # Create cache key for this year
      cache_key <- paste0("col_", y, ".zip")
      
      data <- NULL
      try({
        # set a temporary zip file
        temp_zip <- tempfile(fileext = ".zip")

        # download the zip file
        download_and_verify(url, temp_zip, method = "curl")
        
        # Cache the ZIP file
        cached_zip_path <- cache_raw_data(temp_zip, cache_key, "zip")
        
        # Process the data from cached ZIP
        data <- process_col_zip(cached_zip_path)
        
        # Clean up temp file
        unlink(temp_zip)
      })

      if (!is.null(data)) {
        missing_data_list <- append(missing_data_list, list(data))
      } else if (force && !is.null(cached_files_for_fallback)) {
        # If download failed and force=TRUE, try to load from cache as fallback
        fallback_file <- cached_files_for_fallback[basename(cached_files_for_fallback) == cache_key]
        if (length(fallback_file) > 0) {
          cli::cli_alert_warning("Download failed for year {y}, using cached data")
          cached_zip <- load_cached_data(fallback_file[1])
          fallback_data <- process_col_zip(cached_zip)
          missing_data_list <- append(missing_data_list, list(fallback_data))
        }
      }
    }

    # close progress bar
    cli::cli_progress_done()
  }
  
  # Combine all data
  all_data_list <- c(cached_data_list, missing_data_list)
  
  if (length(all_data_list) == 0) {
    stop("No data available for the specified years")
  }

  # indicate merging of files is taking place
  cli::cli_alert_info("Merging cause of loss files for all specified crop years")

  # Combine all data frames
  col <- all_data_list |>
    dplyr::bind_rows()
  
  # manually type check each column
  col <- suppressWarnings(col |>
    dplyr::mutate(
      dplyr::across(
        c("commodity_year", "state_code", "county_code",
          "commodity_code", "insurance_plan_code", 
          "col_code", "month_of_loss_code", "year_of_loss", 
          "policies_earning_prem", "policies_indemnified", 
          "net_planted_qty", "net_endorsed_acres", "liability", 
          "total_premium", "producer_paid_premium", "subsidy", 
          "state_subsidy", "addnl_subsidy", "efa_prem_discount", 
          "indemnified_quantity", "indem_amount","loss_ratio"),
        as.numeric
      )
    ))
  
  # trim white space on all character columns
  col <- col |>
    dplyr::mutate(
      dplyr::across(
        c("state_abbrv", "county_name", "commodity_name", 
          "insurance_plan_abbrv", "delivery_type", "stage_code", 
          "col_name", "month_of_loss_name"),
        trimws
      )
    )
  

  return(col)
}
