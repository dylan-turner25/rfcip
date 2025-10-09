#' Get USDA RMA Actuarial Data Master (ADM) data
#'
#' Retrieves processed ADM data for a specified year and dataset type. The function
#' automatically locates the appropriate data asset and loads it from cache or downloads
#' it from GitHub releases if not available locally.
#'
#' @param year Numeric or vector. The year(s) of ADM data to retrieve. Can be a single year or vector of years. When multiple years are provided, data is downloaded for each year and row-bound into a single data frame. Defaults to NULL.
#' @param dataset Character. The dataset type to retrieve (e.g., "baserate", "premium",
#'   "subsidy"). Defaults to "baserate". Dataset names are case-insensitive and
#'   underscores are automatically handled.
#' @param show_progress Logical value indicating whether a progress download bar should be displayed. Defaults to `True`.
#' @param force logical (default FALSE). If TRUE, attempts to download fresh data regardless of cache, but falls back to cached data on failure with a warning
#' @return A data.frame containing the requested ADM data.
#'
#' @details
#' This function serves as the main interface for accessing processed ADM data. It:
#' \itemize{
#'   \item Locates the appropriate data file based on year and dataset
#'   \item Loads data from local cache or downloads from GitHub releases if needed
#'   \item Handles both legacy RDS and new parquet formats automatically
#'   \item Restores factor levels for parquet files using stored metadata
#'   \item Handles case-insensitive dataset matching
#'   \item Automatically manages data download and caching
#' }
#'
#' The function works with data processed using the \code{\link{download_adm}} workflow,
#' which outputs optimized parquet files with automatic type detection.
#'
#' @examples
#' \dontrun{
#' # Get premium data for 2020
#' price_2020 <- get_adm_data(year = 2020, dataset = "price")
#'
#' # Get subsidy data for 2018
#' combo_2018 <- get_adm_data(year = 2018, dataset = "comborevenuefactor")
#'
#' # Get baserate data for multiple years
#' baserate_multi <- get_adm_data(year = c(2018, 2019, 2020), dataset = "baserate")
#'
#' # Force fresh download of 2020 premium data
#' price_2020_fresh <- get_adm_data(year = 2020, dataset = "price", force = TRUE)
#' }
#'
#' @seealso
#' \code{\link{download_adm}} for processing raw ADM files,
#' \code{\link{list_data_assets}} for listing all available data assets
#'
#' @export
get_adm_data <- function(year = NULL, dataset = "baserate", show_progress = T, force = FALSE){

  # Handle vector of years by looping and row-binding
  if(!is.null(year) && length(year) > 1){
    data_list <- list()

    # First pass: load all data and identify column types
    for(i in seq_along(year)){
      single_year <- year[i]
      file <- locate_data_asset(single_year, dataset)
      data_list[[i]] <- get_cached_data(file, show_progress = show_progress, force = force)
    }

    # Determine which columns should be character vs numeric across all years
    # A column should be character if it's character/factor in ANY year
    all_col_names <- unique(unlist(lapply(data_list, names)))
    char_cols <- character(0)

    for(col_name in all_col_names){
      col_types <- sapply(data_list, function(df){
        if(col_name %in% names(df)){
          class(df[[col_name]])[1]
        } else {
          NA_character_
        }
      })
      # If any year has this as factor or character, treat as character
      if(any(col_types %in% c("factor", "character"), na.rm = TRUE)){
        char_cols <- c(char_cols, col_name)
      }
    }

    # Second pass: standardize column types
    for(i in seq_along(data_list)){
      for(col in char_cols){
        if(col %in% names(data_list[[i]])){
          data_list[[i]][[col]] <- as.character(data_list[[i]][[col]])
        }
      }
    }

    # Row-bind all data frames
    data <- dplyr::bind_rows(data_list)
    return(data)
  }

  # Original logic for single year or NULL
  file  <- locate_data_asset(year, dataset)
  data <- get_cached_data(file, show_progress = show_progress, force = force)
  return(data)
}