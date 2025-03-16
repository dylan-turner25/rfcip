#' Builds a cause of loss data set using data from the specified years
#'
#' @param year a single numeric value or vector of values specifying the commodity years that should be used to construct the cause of loss data set
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
#' @importFrom purrr  map_dfr
#' @import cli
get_col_data <- function(year = c(as.numeric(format(Sys.Date(), "%Y")):as.numeric(format(Sys.Date(), "%Y")) - 4)) {
  # input checking
  stopifnot("`year` must be a vector of numeric values." = is.numeric(year))

  # get the current location of the col files
  cli::cli_alert_info("Locating cause of loss download links on RMA's website.")
  col_urls <- locate_col_links()
  cli::cli_alert_success("Download links located.")

  # set up a temporary directory
  col_data_files <- tempdir()

  # initialize progress bar
  cli::cli_progress_bar("Downloading cause of loss files for specified crop years", total = length(year))

  # loop over years
  for (y in year) {
    cli::cli_progress_update()


    # locate url corresponding to the crop year
    url <- col_urls$url[which(col_urls$year == y)]

    data <- NULL
    try({
      # set a temporary zip file
      temp_zip <- tempfile(fileext = ".zip")

      # download the zip file
      utils::download.file(url, destfile = temp_zip, mode = "wb", quiet = T)

      # set a temporary txt file
      temp_txt <- tempfile()

      # unzip the file
      utils::unzip(zipfile = temp_zip, exdir = temp_txt)

      # load the text file
      data <- utils::read.delim2(
        file = list.files(temp_txt, full.names = T),
        sep = "|", header = F, skipNul = T
      )
    })


    colnames(data) <- c(
      "commodity_year", "state_code", "state_abbrv",
      "county_code", "county_name", "commodity_code",
      "commodity_name", "insurance_plan_code",
      "insurance_plan_abbrv", "delivery_type",
      "stage_code", "col_code", "col_name",
      "month_of_loss_code", "month_of_loss_name",
      "year_of_loss", "policies_earning_prem",
      "policies_indemnified", "net_planted_qty",
      "net_endorsed_acres", "liability", "total_premium",
      "producer_paid_premium",
      "subsidy", "state_subsidy", "addnl_subsidy",
      "efa_prem_discount", "indemnified_quantity",
      "indem_amount", "loss_ratio"
    )

    # handling type inconsistencies
    data$col_code <- suppressWarnings(as.numeric(data$col_code))

    # save as a rds file
    saveRDS(data, file = paste0(col_data_files, "/col_", y, ".rds"))
  }

  # close progress bar
  cli::cli_progress_done()

  # indicate merging of files is taking place
  cli::cli_alert_info("Merging cause of loss files for all specified crop years")

  # list all files in the temporary directory matching the file naming convention
  files_to_load <- list.files(col_data_files, full.names = T, pattern = "col_\\d+.rds")

  # remove any files that aren't the specified years
  files_to_load <- files_to_load[grepl(paste0(year, collapse = "|"), files_to_load)]

  # load all the `files_to_load` and aggregate them into a single data frame
  col <- files_to_load %>%
    purrr::map_dfr(readRDS) %>%
    dplyr::bind_rows()

  return(col)
}
