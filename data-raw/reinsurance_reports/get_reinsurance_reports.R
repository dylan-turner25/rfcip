# load required libraries -----------------------------
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)

# define any custom functions -------------------------

#' Download and save reinsurance reports from RMAs website
#'
#' @param year the year of the reinsurance report
#' @param geography a character string specifying the geography of the report. Options are "national" or "state"
#' @param type a character string specifying the type of reinsurance report. Options are "standard" or "livestock"
#'
#' @returns a rds file saved in the data-raw/reinsurance_reports directory
#'
#' @source Data is downloaded directly from RMA's reinsurance reports app: \url{https://public-rma.fpac.usda.gov/apps/ReinsuranceReports/}
download_reinsurance_reports <- function(year,geography,type = "standard") {
    
  # type check geography
  if(tolower(geography) == "national"){
    geography = "NationalFund"
  } else if(tolower(geography) == "state"){
    geography = "StateFund"
  } 
  
  if(geography == "StateFund" & type == "livestock"){
    stop("Livestock reinsurance reports are not available at the state level. Please use 'national' for livestock reports.")
  }
  
  if(type == "standard"){
    tryCatch({ 
    print(year)
    
    # set temporary directory
    file <-tempfile(fileext = ".xls")
    
    # download the file from RMA's website to a temporary file
    download.file(url=paste0("https://public-rma.fpac.usda.gov/apps/ReinsuranceReports/Areas/GetReinsuranceReport?",
                             "reinsuranceYear=",year,"&reportType=",geography,"&reportFormat=EXCEL"), 
                  destfile = file,
                  method = "curl", mode = "wb")
    

    # load the data from the temporary file
    data  <- as.data.frame(xlsx::read.xlsx(file,sheetIndex = 1)) 
    
    # identify which cell in data contains "As of"
    as_of_row <- NULL
    i = 0
    while(length(as_of_row) == 0){
      i = i + 1
      as_of_row <- which(grepl("As of",data[,i]))
      if(i > 20){
        warning("No data date found")
      }
    }
    data_date <- gsub("As of ","",data[as_of_row,i])
    
    
    # locate first content line
    first_content_line <- NULL
    i = 0
    while(length(first_content_line) == 0){
      i = i + 1
      first_content_line <- which(data[,i] == "Fund")
      if(i > 20){
        warning("No content line found")
        break
      }
    }

    # get column names
    colnames(data) <- data[first_content_line,]
    
    # remove the header
    data <- data[(first_content_line + 1):nrow(data),] 
 
    # remove columns that are all NA values
    data <- data[, colSums(is.na(data)) < nrow(data)] 
    
    # clean column names using janitor::clean_names
    data <- janitor::clean_names(data)
    
    # add some additional columns
    data$reinsurance_year <- year
    data$report_geography  <- geography
    data$data_date <- data_date
    
    # pivot longer
    if(geography == "NationalFund"){
      data <- data %>% 
        tidyr::pivot_longer(cols = -c(fund,reinsurance_year,report_geography,data_date), 
                           names_to = "value_type", 
                           values_to = "dollars")
    }
    if(geography == "StateFund"){
      data <- data %>% 
        tidyr::pivot_longer(cols = -c(fund,reinsurance_year,report_geography,data_date,state), 
                            names_to = "value_type", 
                            values_to = "dollars")
    }
    

    # save data as a rds file
    file_path <- paste0("./data-raw/reinsurance_reports/",type,"/",geography,"/", 
                        geography,"_", year, ".rds")
    saveRDS(data, file_path)
    
  }, error=function(e){})
  }
  
  if(type == "livestock"){
    # get livestock reports
  }
}

# cleans the data_date column and extracts the month, year, and day
parse_data_release_date <- function(df, date_col = "data_date") {
  # Ensure the column exists
  if (!date_col %in% names(df)) {
    stop(paste("Column", date_col, "not found in the data frame."))
  }
  
  # Extract month and year
  df$data_release_month <- substr(df[[date_col]], 1, 2)
  df$data_release_year <- substr(df[[date_col]],
                                 nchar(df[[date_col]]) - 3, 
                                 nchar(df[[date_col]]))
  
  # Extract day (or set to "01" if missing)
  df$data_release_day <- ifelse(nchar(df[[date_col]]) > 8,
                                substr(df[[date_col]], 4, 5),
                                "01")
  
  # Create Date object
  df$data_release_date <- as.Date(
    paste(df$data_release_year,
          df$data_release_month,
          df$data_release_day,
          sep = "-"),
    format = "%Y-%m-%d"
  )
  
  return(df)
}

# replaces fund type abbreviations with full names
convert_fund_types <- function(df, abb_col, name_col) {
  # Define the mapping of abbreviations to full names
  fund_type_map <- c(
    AR = "Assigned Risk",
    A = "Assigned Risk",
    CC = "CAT Commercial",
    CD = "CAT Developmental",
    C = "CAT Commercial",
    OA = "Other Assigned Risk",
    OC = "Other Commercial",
    OD = "Other Developmental",
    RC = "Revenue Commercial",
    RD = "Revenue Developmental",
    Total = "Total (All Funds)"
  )
  
  # Replace abbreviations with full names
  df[[name_col]] <- fund_type_map[df[[abb_col]]]
  
  return(df)
}



# download the data -------------------------------------

# download and save national reinsurance reports for standard reinsurance agreement
mapply(FUN = download_reinsurance_reports, 
       year = 1998:as.numeric(format(Sys.Date(),"%Y")), 
       MoreArgs = list(geography = "national", type = "standard"))

# download and save state reinsurance reports for standard reinsurance agreement
mapply(FUN = download_reinsurance_reports, 
       year = 1998:as.numeric(format(Sys.Date(),"%Y")), 
       MoreArgs = list(geography = "state", type = "standard"))



# clean the downloaded national reinsurance reports for the standard reinsurance agreement ---------------
national_reinsurance_reports <- list.files(path = "./data-raw/reinsurance_reports/standard/NationalFund", 
                                           pattern = "*.rds", full.names = TRUE) %>% 
  purrr::map(readRDS) %>%
  dplyr::bind_rows() %>%
  rename(fund_abb = fund)


# clean date using parse_data_release_date function
national_reinsurance_reports <- parse_data_release_date(national_reinsurance_reports) %>%
  select(-data_date)

# add a column with the full fund type name
national_reinsurance_reports <- convert_fund_types(df = national_reinsurance_reports, 
                                                   abb_col = "fund_abb", name_col = "fund_name")

# add a column indicating the type of reinsurance report
national_reinsurance_reports$report_type <- "Standard Resinsurance Agreement"


# export as a data set
nationalSRA = national_reinsurance_reports
usethis::use_data(nationalSRA, overwrite = TRUE)


