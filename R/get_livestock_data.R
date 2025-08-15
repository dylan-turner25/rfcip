#' Get Livestock Insurance Data from the USDA Risk Management Agency
#' 
#' Downloads and processes USDA Risk Management Agency livestock insurance data for specified years and programs.
#' 
#' @param year A numeric vector of years to retrieve data for. Defaults to the current year and previous four years.
#' @param program A character vector specifying which livestock insurance programs to include. 
#'   Options are "DRP" (Dairy Revenue Protection), "LGM" (Livestock Gross Margin), and "LRP" (Livestock Risk Protection).
#' @param force logical (default FALSE). If TRUE, attempts to download fresh data regardless of cache, but falls back to cached data on failure with a warning
#' @return A data frame containing the specified livestock insurance data for the specified years and program.
#' @export
#' @examples
#' \dontrun{
#' # Get data for the current year only
#' current_year_data <- get_livestock_data(year = as.numeric(format(Sys.Date(), "%Y")))
#' 
#' # Get LRP data for 2020-2022
#' lrp_data <- get_livestock_data(year = 2020:2022, program = "LRP")
#' 
#' # Get all programs for 2021
#' all_programs_2021 <- get_livestock_data(year = 2021, program = c("DRP", "LGM", "LRP"))
#' }
get_livestock_data <- function(year = as.numeric(format(Sys.Date(), "%Y")), 
                               program = "LRP", force = FALSE) {
  # input checking
  stopifnot("`year` must be a vector of numeric values." = is.numeric(year))
  stopifnot("`program` must be one or more of 'DRP', 'LGM', or 'LRP'." = all(program %in% c("DRP", "LGM", "LRP")))
  if (length(program) > 1) {
    stop("Only one program can be specified at a time.")
  }
  
  # Check which years are already cached for this program
  cache_check <- check_cached_years("livestock", year, program)
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
    cli::cli_alert_info("Loading {length(cached_years)} year{?s} from cache for {program}: {paste(cached_years, collapse = ', ')}")
    for (cached_file in cache_check$cached_files) {
      cached_zip <- load_cached_data(cached_file)
      processed_data <- process_livestock_zip(cached_zip, program)
      cached_data_list <- append(cached_data_list, list(processed_data))
    }
  }
  
  # Download and process missing years
  missing_data_list <- list()
  if (length(missing_years) > 0) {
    # get the current location of the livestock files
    cli::cli_alert_info("Locating livestock download links on RMA's website.")
    livestock_urls <- locate_livestock_links()
    
    # Filter to only the requested programs
    livestock_urls <- livestock_urls[livestock_urls$program %in% program, ]
    cli::cli_alert_success("Download links located.")
    
    # initialize progress bar for missing years only
    cli::cli_progress_bar("Downloading livestock files for missing years", total = length(missing_years))
    
    # loop over missing years only
    for (y in missing_years) {
      cli::cli_progress_update()
      
      # locate urls corresponding to the crop year and selected programs
      urls <- livestock_urls$url[which(livestock_urls$year == y & livestock_urls$program %in% program)]
      
      if(length(urls) == 0) {
        cli::cli_alert_warning("No livestock data found for year {y} and program {program}")
        next
      }
      
      # process each URL for the year (should only be one per program/year)
      for(url in urls) {
        # Create cache key for this year/program
        cache_key <- paste0("livestock_", program, "_", y, ".zip")
        
        data <- NULL
        try({
          # set a temporary zip file
          temp_zip <- tempfile(fileext = ".zip")
          
          # download the zip file
          download_and_verify(url, temp_zip, method = "curl")
          
          # Cache the ZIP file
          cached_zip_path <- cache_raw_data(temp_zip, cache_key, "zip")
          
          # Process the data from cached ZIP
          data <- process_livestock_zip(cached_zip_path, program)
          
          # Clean up temp file
          unlink(temp_zip)
        })
        
        if(!is.null(data)) {
          missing_data_list <- append(missing_data_list, list(data))
        } else if (force && !is.null(cached_files_for_fallback)) {
          # If download failed and force=TRUE, try to load from cache as fallback
          fallback_file <- cached_files_for_fallback[basename(cached_files_for_fallback) == cache_key]
          if (length(fallback_file) > 0) {
            cli::cli_alert_warning("Download failed for year {y} and program {program}, using cached data")
            cached_zip <- load_cached_data(fallback_file[1])
            fallback_data <- process_livestock_zip(cached_zip, program)
            missing_data_list <- append(missing_data_list, list(fallback_data))
          }
        }
      }
    }
    
    # close progress bar
    cli::cli_progress_done()
  }
  
  # Combine all data
  all_data_list <- c(cached_data_list, missing_data_list)
  
  if (length(all_data_list) == 0) {
    cli::cli_alert_warning("No livestock data files found for specified years and program")
    return(NULL)
  }
  
  # indicate merging of files is taking place
  cli::cli_alert_info("Merging livestock files for all specified crop years")
  
  # Combine all data frames
  livestock_data <- all_data_list |>
    dplyr::bind_rows()
 
  # apply column names and convert data types
  if(program == "LRP"){
    colnames <- c("reinsurance_year", "commodity_year", "location_state_code", "location_state_abbreviation", 
                  "location_county_code", "location_county_name", "commodity_code", "commodity_name", 
                  "insurance_plan_code", "insurance_plan_name", "type_code", "type_code_name", 
                  "practice_code", "practice_code_name", "sales_effective_date", "endorsement_length", 
                  "coverage_price", "expected_end_value", "coverage_level_percent", "rate", 
                  "cost_per_cwt", "end_date", "endorsements_earning_premium", "endorsements_indemnified", 
                  "net_number_of_head", "total_weight", "subsidy_amount", "total_premium_amount", 
                  "producer_premium_amount", "liability_amount", "indemnity_amount", "insurance_plan_abbreviation")
    
    numeric_cols <- c("reinsurance_year", "commodity_year", "location_state_code", "location_county_code", 
                      "commodity_code", "insurance_plan_code", "endorsement_length", "coverage_price", 
                      "expected_end_value", "coverage_level_percent", "rate", "cost_per_cwt", 
                      "endorsements_earning_premium", "endorsements_indemnified", "net_number_of_head", 
                      "total_weight", "subsidy_amount", "total_premium_amount", "producer_premium_amount", 
                      "liability_amount", "indemnity_amount","type_code","practice_code")
    
    date_cols <- c("sales_effective_date", "end_date")
    
  }
  
  if(program == "DRP"){
    colnames <- c(
      "reinsurance_year",
      "commodity_year",
      "location_state_code",
      "location_state_abbreviation",
      "location_county_code",
      "location_county_name",
      "commodity_code",
      "commodity_name",
      "insurance_plan_code",
      "insurance_plan_name",
      "coverage_type_code",
      "coverage_type_description",
      "type_code",
      "type_code_name",
      "practice_code",
      "practice_code_name",
      "sales_effective_date",
      "coverage_level_percent",
      "protection_factor",
      "class_price_weighting_factor",
      "component_price_weighting_factor",
      "declared_butterfat_test",
      "declared_protein_test",
      "endorsements_earning_premium",
      "endorsements_indemnified",
      "net_declared_covered_milk_production",
      "subsidy_amount",
      "total_premium_amount",
      "producer_premium_amount",
      "liability_amount",
      "indemnity_amount",
      "insurance_plan_abbreviation"
    )
    
    numeric_cols <- c(
      "reinsurance_year", "commodity_year", "location_state_code", "location_county_code",
      "commodity_code", "insurance_plan_code", "type_code", "practice_code",
      "coverage_level_percent", "protection_factor", "class_price_weighting_factor",
      "component_price_weighting_factor", "declared_butterfat_test", "declared_protein_test",
      "endorsements_earning_premium",  "endorsements_indemnified",
      "net_declared_covered_milk_production","subsidy_amount","total_premium_amount",
      "producer_premium_amount","liability_amount","indemnity_amount"
    )
    date_cols <- c("sales_effective_date")
    
  }
  
  if(program == "LGM"){
    colnames <- c(
      "reinsurance_year",
      "commodity_year",
      "location_state_code",
      "location_state_abbreviation",
      "location_county_code",
      "location_county_name",
      "commodity_code",
      "commodity_name",
      "insurance_plan_code",
      "insurance_plan_name",
      "type_code",
      "type_code_name",
      "practice_code",
      "practice_code_name",
      "sales_effective_date",
      "target_marketings_1",
      "target_marketings_2",
      "target_marketings_3",
      "target_marketings_4",
      "target_marketings_5",
      "target_marketings_6",
      "target_marketings_7",
      "target_marketings_8",
      "target_marketings_9",
      "target_marketings_10",
      "target_marketings_11",
      "corn_equivalent_2",
      "corn_equivalent_3",
      "corn_equivalent_4",
      "corn_equivalent_5",
      "corn_equivalent_6",
      "corn_equivalent_7",
      "corn_equivalent_8",
      "corn_equivalent_9",
      "corn_equivalent_10",
      "corn_equivalent_11",
      "soybean_meal_equivalent_2",
      "soybean_meal_equivalent_3",
      "soybean_meal_equivalent_4",
      "soybean_meal_equivalent_5",
      "soybean_meal_equivalent_6",
      "soybean_meal_equivalent_7",
      "soybean_meal_equivalent_8",
      "soybean_meal_equivalent_9",
      "soybean_meal_equivalent_10",
      "soybean_meal_equivalent_11",
      "endorsements_earning_premium",
      "endorsements_indemnified",
      "deductible",
      "live_cattle_target_weight_quantity",
      "feeder_cattle_target_weight_quantity",
      "corn_target_weight_quantity",
      "liability_amount",
      "total_premium_amount",
      "subsidy_amount",
      "producer_premium_amount",
      "indemnity_amount",
      "insurance_plan_abbreviation"
    )
    

    numeric_cols <- c(
      "reinsurance_year", "commodity_year", "location_state_code", "location_county_code",
      "commodity_code", "insurance_plan_code", "type_code", "practice_code",
      "target_marketings_1", "target_marketings_2", "target_marketings_3",
      "target_marketings_4", "target_marketings_5", "target_marketings_6",
      "target_marketings_7", "target_marketings_8", "target_marketings_9",
      "target_marketings_10", "target_marketings_11", 
      "corn_equivalent_2", "corn_equivalent_3", "corn_equivalent_4",
      "corn_equivalent_5", "corn_equivalent_6", "corn_equivalent_7",
      "corn_equivalent_8", "corn_equivalent_9", "corn_equivalent_10",
      "corn_equivalent_11", 
      "soybean_meal_equivalent_2", "soybean_meal_equivalent_3",
      "soybean_meal_equivalent_4", "soybean_meal_equivalent_5",
      "soybean_meal_equivalent_6", "soybean_meal_equivalent_7",
      "soybean_meal_equivalent_8", "soybean_meal_equivalent_9",
      "soybean_meal_equivalent_10",  "soybean_meal_equivalent_11",
      "endorsements_earning_premium",  "endorsements_indemnified",
      "deductible","live_cattle_target_weight_quantity","feeder_cattle_target_weight_quantity",
      "corn_target_weight_quantity","liability_amount","total_premium_amount","subsidy_amount",
      "producer_premium_amount","indemnity_amount"
    )
    
    date_cols <- c("sales_effective_date")
  }
  
  # apply column names
  colnames(livestock_data) <- colnames
  
  # convert data types
  livestock_data <- suppressWarnings(
    livestock_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(numeric_cols),
          as.numeric
        )
      )
  )
  
  # convert date columns to POSIXct
  livestock_data <- suppressWarnings(
    livestock_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(date_cols),
          as.POSIXct
        )
      )
  )
  
  # trimwhite space on all character columns
  livestock_data <- livestock_data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        stringr::str_trim
      )
    )
  
  
  return(livestock_data)
}
