#' Get USDA RMA Insurance Control Elements (ICE) data
#'
#' Retrieves ICE data for specified years and datasets from the USDA RMA ICE repository.
#' The function can return metadata about available files or download and process actual data files.
#' Downloaded files are cached locally to avoid repeated downloads.
#'
#' @param years Numeric vector. The year(s) of ICE data to retrieve. Can be a single year or vector of years. 
#'   When multiple years are provided, data is downloaded for each year and row-bound into a single data frame.
#' @param dataset Character. Either "metadata" to return file information, or keywords to filter 
#'   specific ICE datasets (e.g., "LegalDescription", "InsurancePlan"). Case-insensitive matching.
#'   Must match exactly one file - multiple matches will throw an error since different ICE
#'   datasets have incompatible column structures. Use get_ice_data(dataset = "metadata") 
#'   to explore available files first.
#' @param ice_url Character. Base URL for the ICE repository. Defaults to the public USDA RMA FTP URL.
#' @param show_progress Logical value indicating whether a progress download bar should be displayed. Defaults to TRUE.
#' @param force Logical (default FALSE). If TRUE, attempts to download fresh data regardless of cache, 
#'   but falls back to cached data on failure with a warning.
#'
#' @return If dataset = "metadata", returns a data frame with file information including filename, size, 
#'   date, and descriptions. Otherwise returns a data frame containing the requested ICE data.
#'
#' @details
#' This function serves as the main interface for accessing ICE data. It:
#' \itemize{
#'   \item Can return metadata about available files when dataset = "metadata"
#'   \item Downloads and processes specific ICE datasets when dataset contains filter keywords
#'   \item Caches downloaded files locally to avoid repeated downloads
#'   \item Handles multiple years by combining data across years
#'   \item Uses selective downloading to minimize bandwidth usage
#' }
#'
#' ICE files contain insurance control elements such as legal descriptions, insurance plan details,
#' commodity information, and other regulatory data used in the Federal Crop Insurance Program.
#'
#' @examples
#' \dontrun{
#' # Get metadata for 2024 ICE files
#' ice_meta_2024 <- get_ice_data(years = 2024, dataset = "metadata")
#' 
#' # Get legal description data for 2024
#' legal_data <- get_ice_data(years = 2024, dataset = "LegalDescription")
#' 
#' # Get insurance plan data for multiple years
#' plan_data <- get_ice_data(years = c(2023, 2024), dataset = "InsurancePlan")
#' 
#' # Force fresh download of 2024 data
#' fresh_data <- get_ice_data(years = 2024, dataset = "LegalDescription", force = TRUE)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom cli cli_alert_info cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom utils download.file
#' @export
get_ice_data <- function(
  years = NULL,
  dataset = NULL,
  ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
  show_progress = TRUE,
  force = FALSE
) {
  
  # Input validation
  if (is.null(years)) {
    stop("years parameter is required")
  }
  
  # Handle metadata request
  if (!is.null(dataset) && tolower(dataset) == "metadata") {
    return(get_ice_metadata(years = years, ice_url = ice_url))
  }
  
  # Handle vector of years by looping and row-binding
  if (length(years) > 1) {
    data_list <- list()
    for (i in seq_along(years)) {
      single_year <- years[i]
      single_data <- get_ice_data_single_year(
        year = single_year,
        dataset = dataset,
        ice_url = ice_url,
        show_progress = show_progress,
        force = force
      )
      if (!is.null(single_data) && nrow(single_data) > 0) {
        data_list[[i]] <- single_data
      }
    }
    # Row-bind all data frames
    if (length(data_list) > 0) {
      data <- dplyr::bind_rows(data_list)
      return(data)
    } else {
      return(data.frame())
    }
  }
  
  # Single year processing
  return(get_ice_data_single_year(
    year = years,
    dataset = dataset,
    ice_url = ice_url,
    show_progress = show_progress,
    force = force
  ))
}


#' Get ICE metadata for specified years
#'
#' @param years Numeric vector of years
#' @param ice_url Character URL for ICE repository
#' @return Data frame with file metadata
#' @keywords internal
get_ice_metadata <- function(years, ice_url) {
  metadata_list <- list()
  
  for (year in years) {
    tryCatch({
      year_metadata <- locate_ice_download_links(year = year, ice_url = ice_url)
      if (!is.null(year_metadata) && nrow(year_metadata) > 0) {
        metadata_list[[length(metadata_list) + 1]] <- year_metadata
      }
    }, error = function(e) {
      cli::cli_alert_warning("Could not get metadata for year {year}: {e$message}")
    })
  }
  
  if (length(metadata_list) > 0) {
    return(dplyr::bind_rows(metadata_list))
  } else {
    return(data.frame())
  }
}


#' Get ICE data for a single year
#'
#' @param year Single numeric year
#' @param dataset Character dataset filter
#' @param ice_url Character URL
#' @param show_progress Logical for progress display
#' @param force Logical for cache bypass
#' @return Data frame with ICE data
#' @keywords internal
get_ice_data_single_year <- function(year, dataset, ice_url, show_progress, force) {
  # Generate cache key based on parameters (like SOB data does)
  cache_params <- list(
    year = year,
    dataset = dataset
  )
  
  cache_key <- generate_cache_key("ice", cache_params, "parquet")
  dest_dir <- tools::R_user_dir("rfcip", which = "cache")
  cache_file <- file.path(dest_dir, cache_key)
  
  # Check if data is already cached and force=FALSE
  if (file.exists(cache_file) && !force) {
    if (show_progress) {
      cli::cli_alert_info("Loading ICE data for {year} from cache")
    }
    return(load_cached_data(cache_file))
  }
  
  # Download and process fresh data
  success <- FALSE
  full_data <- data.frame()
  
  tryCatch({
    # Get download links for the year
    download_links <- locate_ice_download_links(year = year, ice_url = ice_url)
    
    if (is.null(download_links) || nrow(download_links) == 0) {
      cli::cli_alert_warning("No ICE data found for year {year}")
      return(data.frame())
    }
    
    # Filter to YTD files only
    download_links <- download_links[grepl("YTD\\.txt$", download_links$filename), ]
    
    if (nrow(download_links) == 0) {
      cli::cli_alert_warning("No YTD ICE files found for year {year}")
      return(data.frame())
    }
    
    # Apply dataset filtering if specified
    if (!is.null(dataset) && dataset != "") {
      pattern <- paste(dataset, collapse = "|")
      download_links <- download_links[grepl(pattern, download_links$filename, ignore.case = TRUE), ]
      
      if (nrow(download_links) == 0) {
        cli::cli_alert_warning("No ICE files matching dataset filter '{dataset}' found for year {year}")
        return(data.frame())
      }
      
      # Error if multiple files match - they have incompatible schemas
      if (nrow(download_links) > 1) {
        matching_files <- download_links$filename
        stop(
          "Multiple ICE files match the pattern '", dataset, "' for year ", year, ":\n",
          paste("  -", matching_files, collapse = "\n"), "\n\n",
          "Different ICE datasets have incompatible column structures and cannot be combined.\n",
          "Please refine your search to match exactly one file:\n",
          "  Use a more specific dataset name (e.g., 'LegalDescription' instead of 'Legal')\n",
          "  Use the exact dataset code (e.g., 'D00001' instead of 'Price')\n",
          "  Use get_ice_data(dataset = 'metadata') to see all available files"
        )
      }
    }
    
    # Process each file without individual caching
    data_list <- list()
    
    if (show_progress) {
      cli::cli_progress_bar("Downloading ICE files for {year}", total = nrow(download_links))
    }
    
    for (i in 1:nrow(download_links)) {
      if (show_progress) {
        cli::cli_progress_update()
      }
      
      link_info <- download_links[i, ]
      file_data <- download_and_process_ice_file(
        download_url = link_info$url,
        filename = link_info$filename
      )
      
      if (!is.null(file_data) && nrow(file_data) > 0) {
        data_list[[length(data_list) + 1]] <- file_data
      }
    }
    
    if (show_progress) {
      cli::cli_progress_done()
    }
    
    # Combine all files
    if (length(data_list) > 0) {
      full_data <- dplyr::bind_rows(data_list)
      success <- TRUE
    }
    
  }, error = function(e) {
    if (force && file.exists(cache_file)) {
      cli::cli_alert_warning("Download failed for year {year}, using cached data")
      full_data <- load_cached_data(cache_file)
      success <- TRUE
    } else {
      cli::cli_alert_warning("Error processing ICE data for year {year}: {e$message}")
      return(data.frame())
    }
  })
  
  # Cache the processed data only if download was successful (like SOB does)
  if (success && !file.exists(cache_file) && nrow(full_data) > 0) {
    cache_processed_data(full_data, cache_key)
  }
  
  return(full_data)
}


#' Download and process a single ICE file (no caching)
#'
#' @param download_url Character URL for file download
#' @param filename Character filename for logging
#' @return Data frame with processed data
#' @keywords internal
download_and_process_ice_file <- function(download_url, filename) {
  # Download and process file (no individual caching)
  tryCatch({
    # Create temporary file for download
    temp_file <- tempfile(fileext = ".txt")
    
    # Download file
    utils::download.file(download_url, destfile = temp_file, mode = "wb", quiet = TRUE)
    
    # Read and clean data
    if (file.exists(temp_file) && file.size(temp_file) > 0) {
      data <- readr::read_delim(temp_file, delim = "|", col_names = TRUE, show_col_types = FALSE)
      data <- clean_ice_data(data)
      
      # Clean up temp file
      unlink(temp_file)
      
      return(data)
    } else {
      cli::cli_alert_warning("Downloaded file is empty or corrupted: {filename}")
      return(data.frame())
    }
    
  }, error = function(e) {
    cli::cli_alert_warning("Failed to download or process {filename}: {e$message}")
    return(data.frame())
  })
}


#' Clean ICE data using standard rfcip patterns
#'
#' @param data Data frame to clean
#' @return Cleaned data frame
#' @keywords internal
clean_ice_data <- function(data) {
  # Apply standard rfcip cleaning
  data <- janitor::clean_names(data)
  
  # Sanitize character data for UTF-8
  char_cols <- sapply(data, is.character)
  for(col in names(data)[char_cols]) {
    data[[col]] <- iconv(data[[col]], from = "", to = "UTF-8", sub = "")
    data[[col]] <- iconv(data[[col]], to = "ASCII//TRANSLIT", sub = "")
  }
  
  # Apply type conversion
  data <- suppressMessages(readr::type_convert(data))
  
  # Parse date columns (same logic as clean_adm_data)
  date_cols <- grep("date", names(data), value = TRUE)
  for(col in date_cols){
    input = as.character(data[[col]]) # convert to character
    input <- as.character(gsub("[^0-9]", "", input)) # remove non-numeric characters
    try({
      converted_dates <- readr::parse_date(input, format = "%Y%m%d", na = c("", "NA"))
      # if converted dates are not all NA
      if(!all(is.na(converted_dates))) {
        data[[col]] <- converted_dates
      }
    })
  }
  
  return(data)
}