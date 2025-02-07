#' Builds a cause of loss dataset using data from the specified years
#'
#' @param years a single numeric value or vector of values specifying the commodity years that should be used to construct the cause of loss data set
#'
#' @return returns a data frame with cause of loss data corresponding to the specified commodity years
#' @export
#'
#' @examples
#' \dontrun{
#' get_col_data(years = 2020)
#' get_col_data(years = 2019:2021)
#' }
#' @importFrom utils unzip
#' @importFrom utils read.delim
#' @importFrom dplyr bind_rows
#' @importFrom purrr  map_dfr
get_col_data <- function(years = c(as.numeric(format(Sys.Date(), "%Y")):as.numeric(format(Sys.Date(), "%Y")) - 4)) {
  # get the current location of the col files
  message("Locating cause of loss download links on RMA's website")
  col_urls <- locate_col_links()

  # define years where col files are still updated weekly (i.e. last 5 years)
  # variable_years <- c(as.numeric(format(Sys.Date(), "%Y")) : (as.numeric(format(Sys.Date(), "%Y")) - 5) )
  # current_month <- format(Sys.Date(), "%m")
  # current_year <- format(Sys.Date(), "%Y")

  # # set up a temporary directory
  col_data_files <- tempdir()

  # loop over years
  for (y in years) {
    message("Downloading data for crop year ", y)
    # if(y %in% variable_years){
    #   url <- paste0("https://www.rma.usda.gov/sites/default/files/",current_year,"-",
    #                 ifelse(nchar(current_month) == 2,
    #                        current_month,
    #                        paste0("0",current_month)) ,
    #                 "/colsom_",y,".zip")
    #
    #
    # } else {
    #   url <- paste0("https://www.rma.usda.gov/sites/default/files/information-tools/colsom_",y,".zip")
    # }

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
      data <- utils::read.delim(
        file = list.files(temp_txt, full.names = T),
        sep = "|", header = F
      )
    })

    # if data is still null, retry the download with the previous month
    # sometimes at the beginning of the month, last months data is still on the site
    # if(is.null(data)){
    #
    #   current_month <- as.numeric(current_month) - 1
    #
    #   url <- paste0("https://www.rma.usda.gov/sites/default/files/",current_year,"-",ifelse(nchar(current_month) == 2,current_month,paste0("0",current_month)),"/colsom_",y,".zip")
    #   # set a temporary zip file
    #   temp_zip <- tempfile(fileext = ".zip")
    #
    #   # download the zip file
    #   utils::download.file(url, destfile = temp_zip,mode = "wb", quiet = T)
    #
    #   # set a temporary txt file
    #   temp_txt <- tempfile()
    #
    #   # unzip the file
    #   unzip(zipfile = temp_zip, exdir = temp_txt)
    #
    #   # load the text file
    #   data <- read.delim(file = list.files(temp_txt,full.names = T),
    #                      sep = "|", header = F)
    # }


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

  message("Merging data from all crop years")

  # list all files in the temporary directory matching the file naming convention
  files_to_load <- list.files(col_data_files, full.names = T, pattern = "col_\\d+.rds")

  # remove any files that aren't the specified years
  files_to_load <- files_to_load[grepl(paste0(years, collapse = "|"), files_to_load)]

  # load all the `files_to_load` and aggregate them into a single data frame
  col <- files_to_load %>%
    purrr::map_dfr(readRDS) %>%
    dplyr::bind_rows()

  return(col)
}
