#' National Data on RMA's Standard Reinsurance Agreement 
#'
#' A data set containing all of the information published in RMA's annual
#' Standard Reinsurance Agreement reports going back to 1998.
#' 
#' @details
#' The full standard reinsurance agreement can be found at \url{https://www.rma.usda.gov/policy-procedure/reinsurance-agreements}. To 
#' view code used to generate this data set, see `./data-raw/reinsurance_reports/get_reinsurance_reports.R`  
#' 
#'
#' @format  A data frame
#' \describe{
#'   \item{fund_abb}{Abbreviation for the reinsurance fund name }
#'   \item{fund_name}{Name of the reinsurance fund}
#'   \item{reinsurance_year}{Year of the reinsurance report}
#'   \item{report_geography}{Geography of the report (i.e. national)}
#'   \item{value_type}{the type of value being measured}
#'   \item{dollars}{Nominal US Dollars corresponding to what is being measured in the value_type column} 
#'   \item{data_release_month}{Month of the data release}
#'   \item{data_release_year}{Year of the data release}
#'   \item{data_release_day}{Day of the data release}
#'   \item{data_release_date}{Date of the data release}
#'   \item{report_type}{Type of report}
#' }
#' @usage data(nationalSRA)
#' @source \url{https://www.rma.usda.gov/tools-reports/reinsurance-reports}
#' 
"nationalSRA"


#' State level data on RMA's Standard Reinsurance Agreement 
#'
#' A data set containing all of the information published in RMA's annual
#' Standard Reinsurance Agreement reports going back to 1998.
#' 
#' @details
#' The full standard reinsurance agreement can be found at \url{https://www.rma.usda.gov/policy-procedure/reinsurance-agreements}. To 
#' view code used to generate this data set, see `./data-raw/reinsurance_reports/get_reinsurance_reports.R`  
#' 
#'
#' @format  A data frame
#' \describe{
#'   \item{fund_abb}{Abbreviation for the reinsurance fund name }
#'   \item{fund_name}{Name of the reinsurance fund}
#'   \item{reinsurance_year}{Year of the reinsurance report}
#'   \item{report_geography}{Geography of the report (i.e. state)}
#'   \item{value_type}{the type of value being measured}
#'   \item{dollars}{Nominal US Dollars corresponding to what is being measured in the value_type column} 
#'   \item{data_release_month}{Month of the data release}
#'   \item{data_release_year}{Year of the data release}
#'   \item{data_release_day}{Day of the data release}
#'   \item{data_release_date}{Date of the data release}
#'   \item{report_type}{Type of report}
#'   \item{state}{State abbreviation}
#' }
#' @usage data(stateSRA)
#' @source \url{https://www.rma.usda.gov/tools-reports/reinsurance-reports}
#' 
"stateSRA"


#' National Data on RMA's Livestock Price Reinsurance Agreement
#'
#' A data set containing all of the information published in RMA's annual
#' Livestock Price Reinsurance Agreement reports going back to 2014.
#' 
#' @details
#' The full standard reinsurance agreement can be found at \url{https://www.rma.usda.gov/policy-procedure/reinsurance-agreements}. To 
#' view code used to generate this data set, see `./data-raw/reinsurance_reports/get_reinsurance_reports.R`  
#' 
#'
#' @format  A data frame
#' \describe{
#'   \item{reinsurance_year}{Year of the reinsurance report}
#'   \item{report_geography}{Geography of the report (i.e. national)}
#'   \item{value_type}{the type of value being measured}
#'   \item{dollars}{Nominal US Dollars corresponding to what is being measured in the value_type column} 
#'   \item{data_release_month}{Month of the data release}
#'   \item{data_release_year}{Year of the data release}
#'   \item{data_release_day}{Day of the data release}
#'   \item{data_release_date}{Date of the data release}
#'   \item{report_type}{Type of report}
#'   \item{footnote}{Footnote associated with the data if one was found in the downloaded file}
#' }
#' @usage data(nationalSRA)
#' @source \url{https://www.rma.usda.gov/tools-reports/reinsurance-reports}
#' 
"nationalLPRA"