#' Lookup crop codes for FCIP commodities
#'
#' @param comm A vector of either character strings with commodity names (not case sensitive) or a vector with numeric crop codes. Can be left NULL to return crop codes for all commodities.
#' @param year A single numeric value, or vector of numeric values, indicating what years of the summary of business should be used to get crop names and crop codes. Defaults to the current year.
#' @return Returns a tibble containing the relevant commodity year, commodity codes and commodity names.
#' @export
#'
#' @examples
#' \dontrun{
#' get_crop_codes(year = 2023, comm = "corn")
#' get_crop_codes(year = 2024, comm = 41)
#' get_crop_codes(comm = c("corn","SOYbEaNs"))
#' get_crop_codes(comm = c(41,81))
#' get_crop_codes()
#' }
#'
#' @importFrom janitor clean_names
#' @importFrom utils download.file
#' @importFrom readxl read_excel
#'
#' @source Data is downloaded directly from RMA's summary of business app: \url{https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator}
get_crop_codes <- function(year = as.numeric(format(Sys.Date(), "%Y")),comm = NULL){

  # url for all commodities with commodity codes
  url <- paste0("https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel?CY=",paste(year,collapse = ","),"&ORD=CY,CM&CC=S&VisibleColumns=CommodityYear,CommodityCode,CommodityName&SortField=&SortDir=")

  # set temporary directory
  dir <- tempdir()

  # create a file path for the crop codes file in the temporary directory
  data_path <- paste0(dir,"/crop_codes.xlsx")


  download.file(url, destfile = data_path,mode = "wb")

  # load data
  data <- suppressMessages(janitor::clean_names(readxl::read_excel(data_path)))

  # check for warnings that appear in the header, if so, need to skip one line
  if(colnames(data)[2] == "x2"){
    data <- suppressMessages(janitor::clean_names(readxl::read_excel(data_path, skip = 1)))
  }

  # select neccesary columns
  data <- data[,c("commodity_year","commodity_code","commodity_name")]


  if(!is.null(comm) &  is.character(comm)){
    to_return <- data[which(tolower(data$commodity_name) %in% tolower(comm)),]
    if(nrow(to_return) == 0){
      warning("One or more of the entered crop codes or crop names is not valid, returning all crop names and crop codes:")
      return(data)
    } else {
      return(to_return)
    }
  } else if(!is.null(comm) & is.numeric(comm)){
    to_return <- data[which(as.numeric(data$commodity_code) %in% comm),]
    if(nrow(to_return) == 0){
      warning("One or more of the entered crop codes or crop names is not valid, returning all crop names and crop codes:")
      return(data)
    } else {
      return(to_return)
    }
  } else {
    return(data)
  }
}

