#' Livestock ADM dataset code mapping
#'
#' Maps user-friendly dataset names to the codes/patterns used in
#' the FTP directory filenames.
#' @keywords internal
LIVESTOCK_ADM_DATASETS <- list(
  drp_draw         = "A00831",
  drp_milk_yield   = "A00832",
  drp_daily_price  = "A00833",
  drp_actual_price = "A00834",
  drp_fmmo_pricing = "A00835",
  lgm              = "ADMLivestockLgm",
  lrp              = "ADMLivestockLrp"
)


#' Get USDA RMA Livestock Actuarial Data Master (ADM) data
#'
#' Downloads and processes livestock actuarial reference data from the USDA RMA
#' FTP server. This data includes pricing, yield, draw, and gross margin data
#' for Dairy Revenue Protection (DRP), Livestock Risk Protection (LRP), and
#' Livestock Gross Margin (LGM) insurance programs.
#'
#' @param year Numeric vector. The year(s) of livestock ADM data to retrieve
#'   (2014-2026). Defaults to the current year.
#' @param dataset Character. Which dataset to retrieve. One of:
#'   \code{"drp_draw"}, \code{"drp_milk_yield"}, \code{"drp_daily_price"},
#'   \code{"drp_actual_price"}, \code{"drp_fmmo_pricing"}, \code{"lgm"},
#'   \code{"lrp"}. Defaults to \code{"lrp"}.
#' @param date Controls which file version(s) to download for each year:
#'   \itemize{
#'     \item \code{"latest"} (default): Download only the most recent file per year.
#'     \item \code{NULL}: Download ALL files for the year and combine them
#'       (full time series for daily datasets).
#'     \item A date string (e.g., \code{"2024-03-01"}): Download the file with the
#'       closest date on or before the specified date.
#'   }
#' @param force Logical. If \code{TRUE}, re-download even if cached data exists.
#'   Falls back to cached data on failure. Defaults to \code{FALSE}.
#'
#' @return A data.frame containing the requested livestock ADM data, or
#'   \code{NULL} if no data is found.
#'
#' @details
#' The function accesses livestock actuarial data from
#' \url{https://pubfs-rma.fpac.usda.gov/pub/References/adm_livestock/}.
#'
#' Available datasets and their update frequencies:
#' \describe{
#'   \item{drp_draw}{DRP indemnity draw data (quarterly, 2019+)}
#'   \item{drp_milk_yield}{DRP state-level milk yield data (quarterly, 2019+)}
#'   \item{drp_daily_price}{DRP expected commodity prices (daily, 2019+)}
#'   \item{drp_actual_price}{DRP settled/actual prices (quarterly, 2019+)}
#'   \item{drp_fmmo_pricing}{DRP FMMO pricing factors (yearly, 2019+)}
#'   \item{lgm}{Livestock Gross Margin data (daily, 2014+)}
#'   \item{lrp}{Livestock Risk Protection actual ending values (daily, 2014+)}
#' }
#'
#' For daily datasets, each file is a snapshot for a specific date. Using
#' \code{date = NULL} downloads all files and combines them into a full time
#' series, which may be large.
#'
#' @examples
#' \dontrun{
#' # Get latest LRP data for 2025
#' lrp_2025 <- get_livestock_adm_data(year = 2025, dataset = "lrp")
#'
#' # Get DRP FMMO pricing factors for 2020-2024
#' fmmo <- get_livestock_adm_data(year = 2020:2024, dataset = "drp_fmmo_pricing")
#'
#' # Get full daily price time series for 2024
#' daily_prices <- get_livestock_adm_data(year = 2024, dataset = "drp_daily_price", date = NULL)
#'
#' # Get LRP data as of a specific date
#' lrp_aug <- get_livestock_adm_data(year = 2025, dataset = "lrp", date = "2025-08-01")
#' }
#'
#' @export
get_livestock_adm_data <- function(
    year = as.numeric(format(Sys.Date(), "%Y")),
    dataset = "lrp",
    date = "latest",
    force = FALSE
) {

  # --- Input validation ---
  stopifnot("`year` must be a numeric vector." = is.numeric(year))
  stopifnot("`dataset` must be a single character string." = is.character(dataset) && length(dataset) == 1)

  dataset <- tolower(dataset)
  valid_datasets <- names(LIVESTOCK_ADM_DATASETS)
  if (!dataset %in% valid_datasets) {
    stop(
      "`dataset` must be one of: ",
      paste(valid_datasets, collapse = ", "),
      ". Got: '", dataset, "'"
    )
  }

  # Validate date parameter
  target_date <- NULL
  if (!is.null(date)) {
    if (!is.character(date) || length(date) != 1) {
      stop("`date` must be NULL, \"latest\", or a single date string (e.g., \"2024-03-01\").")
    }
    if (date != "latest") {
      target_date <- tryCatch(
        as.Date(date),
        error = function(e) stop("`date` must be NULL, \"latest\", or a valid date string. Got: '", date, "'")
      )
      if (is.na(target_date)) {
        stop("`date` must be NULL, \"latest\", or a valid date string. Got: '", date, "'")
      }
    }
  }

  dataset_code <- LIVESTOCK_ADM_DATASETS[[dataset]]
  date_mode <- if (is.null(date)) "all" else if (date == "latest") "latest" else "specific"

  # --- Scrape FTP listing ---
  cli::cli_alert_info("Locating livestock ADM download links.")
  all_links <- locate_livestock_adm_links(years = year)

  if (nrow(all_links) == 0) {
    cli::cli_alert_warning("No livestock ADM data found on the FTP server for the specified years.")
    return(NULL)
  }

  cli::cli_alert_success("Download links located.")

  # Filter to requested dataset
  links <- all_links[all_links$dataset_code == dataset_code, ]

  if (nrow(links) == 0) {
    cli::cli_alert_warning(
      "No data found for dataset '{dataset}' in year(s) {paste(year, collapse = ', ')}. ",
      "This dataset may not be available for the requested years."
    )
    return(NULL)
  }

  # --- Apply date logic per year ---
  links_to_download <- do.call(rbind, lapply(split(links, links$year), function(year_links) {
    if (date_mode == "latest") {
      # Keep only the most recent file
      year_links[which.max(year_links$file_date), ]
    } else if (date_mode == "specific") {
      # Keep the file with max file_date <= target_date
      eligible <- year_links[year_links$file_date <= target_date, ]
      if (nrow(eligible) == 0) {
        cli::cli_alert_warning(
          "No files found for year {year_links$year[1]} on or before {target_date}."
        )
        return(NULL)
      }
      eligible[which.max(eligible$file_date), ]
    } else {
      # date_mode == "all": keep all files
      year_links
    }
  }))

  if (is.null(links_to_download) || nrow(links_to_download) == 0) {
    cli::cli_alert_warning("No matching files found after applying date filter.")
    return(NULL)
  }

  # Warn about large downloads for daily datasets with date = NULL
  if (date_mode == "all" && dataset %in% c("drp_daily_price", "lgm", "lrp")) {
    cli::cli_alert_warning(
      "Downloading all daily files for {dataset}. This may involve many files and take a while."
    )
  }

  # --- Cache check and download ---
  dest_dir <- tools::R_user_dir("rfcip", which = "cache")
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  data_list <- list()

  if (date_mode == "all") {
    # For "all" mode, check for a combined cache file per year
    for (y in unique(links_to_download$year)) {
      cache_key <- paste0("livestock_adm_", dataset, "_", y, "_all.parquet")
      cache_path <- file.path(dest_dir, cache_key)

      if (file.exists(cache_path) && !force) {
        cli::cli_alert_info("Loading cached combined data for {dataset} {y}.")
        data_list <- append(data_list, list(load_cached_data(cache_path)))
      } else {
        # Download all files for this year, combine, and cache
        year_links <- links_to_download[links_to_download$year == y, ]
        cli::cli_progress_bar(
          paste0("Downloading ", dataset, " files for ", y),
          total = nrow(year_links)
        )

        year_data_list <- list()
        for (i in seq_len(nrow(year_links))) {
          cli::cli_progress_update()
          row <- year_links[i, ]

          data <- tryCatch({
            temp_zip <- tempfile(fileext = ".zip")
            download_and_verify(row$url, temp_zip, method = "curl")
            result <- process_livestock_adm_zip(temp_zip)
            unlink(temp_zip)
            result
          }, error = function(e) {
            cli::cli_alert_warning("Failed to download {row$filename}: {e$message}")
            NULL
          })

          if (!is.null(data)) {
            year_data_list <- append(year_data_list, list(data))
          }
        }
        cli::cli_progress_done()

        if (length(year_data_list) > 0) {
          combined <- dplyr::bind_rows(year_data_list)
          cache_processed_data(combined, cache_key)
          data_list <- append(data_list, list(combined))
        }
      }
    }
  } else {
    # For "latest" or "specific" mode: one file per year
    for (i in seq_len(nrow(links_to_download))) {
      row <- links_to_download[i, ]

      # Build cache key
      date_suffix <- format(row$file_date, "%Y%m%d")
      if (date_mode == "latest") {
        cache_key <- paste0("livestock_adm_", dataset, "_", row$year, ".parquet")
      } else {
        cache_key <- paste0("livestock_adm_", dataset, "_", row$year, "_", date_suffix, ".parquet")
      }
      cache_path <- file.path(dest_dir, cache_key)

      if (file.exists(cache_path) && !force) {
        cli::cli_alert_info("Loading cached data for {dataset} {row$year}.")
        data_list <- append(data_list, list(load_cached_data(cache_path)))
      } else {
        data <- tryCatch({
          if (force && file.exists(cache_path)) {
            cli::cli_alert_info("Forcing fresh download for {dataset} {row$year}.")
          }

          temp_zip <- tempfile(fileext = ".zip")
          download_and_verify(row$url, temp_zip, method = "curl")
          result <- process_livestock_adm_zip(temp_zip)
          unlink(temp_zip)

          # Cache the processed data
          cache_processed_data(result, cache_key)
          result
        }, error = function(e) {
          # On failure with force, try fallback to cache
          if (force && file.exists(cache_path)) {
            cli::cli_alert_warning("Download failed for {dataset} {row$year}, using cached data.")
            load_cached_data(cache_path)
          } else {
            cli::cli_alert_warning("Failed to download {dataset} {row$year}: {e$message}")
            NULL
          }
        })

        if (!is.null(data)) {
          data_list <- append(data_list, list(data))
        }
      }
    }
  }

  # --- Combine and return ---
  if (length(data_list) == 0) {
    cli::cli_alert_warning("No livestock ADM data could be retrieved for the specified parameters.")
    return(NULL)
  }

  if (length(data_list) > 1) {
    cli::cli_alert_info("Merging livestock ADM data.")
  }

  result <- dplyr::bind_rows(data_list)

  # Merge in commodity names if commodity_code exists in the data
  if ("commodity_code" %in% names(result)) {
    crop_codes <- get_crop_codes(year = year)
    crop_codes <- crop_codes[, c("commodity_code", "commodity_name", "commodity_abbreviation")]
    crop_codes <- unique(crop_codes)
    result <- dplyr::left_join(result, crop_codes, by = "commodity_code")
  }

  return(result)
}
