#' Lookup insurance plan codes for FCIP insurance plans
#'
#' @param year A single numeric value, or vector of numeric values, indicating what years of the summary of business should be used to get crop names and crop codes. Defaults to the current year.
#' @param plan can be either a character string indicating an insurance plan (ex: `yp` and `yield protection` are both valid) or a numeric value indicating the insurance plan code (i.e. `1`). Inputting nothing for the `plan` argument will return codes for all insurance plans in the specified year(s).
#'
#' @return Returns a tibble containing the relevant commodity year, insurance plan codes, insurance plan name, and insurance plan abbreviation
#' @export
#'
#' @examples
#' \dontrun{
#' get_insurance_plan_codes()
#' get_insurance_plan_codes(year = 2023, plan = "yp")
#' get_insurance_plan_codes(year = 2023, plan = 1)
#' get_insurance_plan_codes(year = 2023, plan = "yield protection")
#' get_insurance_plan_codes(year = 2018:2022, plan = c("yp", "rp"))
#' }
#' @source Data is downloaded directly from RMA's summary of business app: \url{https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator}
get_insurance_plan_codes <- function(year = as.numeric(format(Sys.Date(), "%Y")), plan = NULL) {
  # url for all commodities with commodity codes
  url <- paste0("https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel?CY=", paste(year, collapse = ","), "&ORD=CY,IP&CC=S&VisibleColumns=CommodityYear,InsurancePlanCode,InsurancePlanName,InsurancePlanAbbreviation&SortField=&SortDir=")

  # set temporary directory
  dir <- tempdir()

  # create a file path for the crop codes file in the temporary directory
  data_path <- paste0(dir, "/plan_codes.xlsx")

  # download the file to the temporary directory
  download.file(url, destfile = data_path, mode = "wb", quiet = TRUE)

  # load data
  data <- suppressMessages(janitor::clean_names(readxl::read_excel(data_path)))

  # check for warnings that appear in the header, if so, need to skip one line
  if (colnames(data)[2] == "x2") {
    data <- suppressMessages(janitor::clean_names(readxl::read_excel(data_path, skip = 1)))
  }

  # filter to desired columns
  data <- data[, c("commodity_year", "insurance_plan_code", "insurance_plan", "insurance_plan_abbrv")]

  # remove temporary file
  unlink(data_path)
  
  # if no plan is entered, return all plans
  if (is.null(plan)) {
    return(data)
  }

  # check entered plan against insurance plan abbreviation
  to_return <- data[which(tolower(data$insurance_plan_abbrv) %in% tolower(plan)), ]
  if (nrow(to_return) > 0) {
    return(to_return)
  }

  # check entered plan against full insurance plan names
  to_return <- data[which(tolower(data$insurance_plan) %in% tolower(plan)), ]
  if (nrow(to_return) > 0) {
    return(to_return)
  }

  # check entered plan against insurance plan codes
  to_return <- suppressWarnings(data[which(as.numeric(data$insurance_plan_code) %in% as.numeric(plan)), ])
  if (nrow(to_return) > 0) {
    return(to_return)
  }

  # if still no matches, issue warning and return all plan codes
  if (nrow(to_return) == 0) {
    stop("One or more of the entered insurance plan codes or insurance plan names is not valid. Enter `get_insurance_plan_codes()` to see all plans and codes.")
  }
}
