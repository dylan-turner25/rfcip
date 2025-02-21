#' Lookup crop codes for FCIP commodities
#'
#' @param crop A vector of either character strings with commodity names (not case sensitive) or a vector with numeric crop codes. Can be left NULL to return crop codes for all commodities.
#' @param year A single numeric value, or vector of numeric values, indicating what years of the summary of business should be used to get crop names and crop codes. Defaults to the current year.
#' @return Returns a tibble containing the relevant commodity year, commodity codes and commodity names.
#' @export
#'
#' @examples
#' \dontrun{
#' get_crop_codes(year = 2023, comm = "corn")
#' get_crop_codes(year = 2024, comm = 41)
#' get_crop_codes(crop = c("corn", "SOYbEaNs"))
#' get_crop_codes(crop = c(41, 81))
#' get_crop_codes()
#' }
#'
#' @importFrom janitor clean_names
#' @importFrom readxl read_excel
#' @import httr 
#'
#' @source Data is downloaded directly from RMA's summary of business app: \url{https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator}
get_crop_codes <- function(year = as.numeric(format(Sys.Date(), "%Y")), crop = NULL) {
  # url for all commodities with commodity codes
  url <- paste0("https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel?CY=", paste(year, collapse = ","), "&ORD=CY,CM&CC=S&VisibleColumns=CommodityYear,CommodityCode,CommodityName&SortField=&SortDir=")

  # download the file
  httr::GET(url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

  # load the data from the temporary file
  data <- suppressMessages(janitor::clean_names(readxl::read_excel(tf)))

  # check for warnings that appear in the header, if so, need to skip one line
  if (colnames(data)[2] == "x2") {
    data <- suppressMessages(janitor::clean_names(readxl::read_excel(data_path, skip = 1)))
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
