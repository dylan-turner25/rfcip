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
#' @param livestock_download_links a list of download links for livestock reinsurance reports. This is only required if type = "livestock"
#'
#' @returns a rds file saved in the data-raw/reinsurance_reports directory
#'
#' @source Data is downloaded directly from RMA's reinsurance reports app: \url{https://public-rma.fpac.usda.gov/apps/ReinsuranceReports/}
download_reinsurance_reports <- function(year,geography,type = "standard",livestock_download_links = NULL){ 
    
  # type check geography
  if(tolower(geography) == "national"){
    geography = "NationalFund"
  } else if(tolower(geography) == "state"){
    geography = "StateFund"
  } 
  
  # check for livestock download links if type is livestock
  if(type == "livestock"){
    if(is.null(livestock_download_links)){
      stop("Livestock download links must be provided for livestock reinsurance reports.")
    }
  }
  
  # check for invalid combinations
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
    
    # geography specific cleaning operations
    if(geography == "StateFund"){
      # make sure dollars is numeric
      data$dollars <- as.numeric(gsub(",","",data$dollars))
      
      # trim ws on state column
      data$state <- trimws(data$state)
    }
    

    # save data as a rds file
    file_path <- paste0("./data-raw/reinsurance_reports/",type,"/",geography,"/", 
                        geography,"_", year, ".rds")
    saveRDS(data, file_path)
    
  }, error=function(e){})
  }
  
  if(type == "livestock"){
    tryCatch({ 
      print(year)
      
      # set temporary directory
      file <-tempfile(fileext = ".xls")
      
      # download the file from RMA's website to a temporary file
      download.file(url=livestock_download_links[[as.character(year)]], 
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
        first_content_line <- which(grepl("Gross",data[,i]))
        if(i > 20){
          warning("No content line found")
          break
        }
      }
      
      # locate footnote line
      footnote_row <- NULL
      i = 0
      while(length(footnote_row) == 0){
        i = i + 1
        footnote_row <- which(grepl("Footnote",data[,i]))
        if(i > 20){
          warning("No footnote found")
        }
      }
      
      # extract footnote
      footnote <- paste0("Footnote: ",data[footnote_row+1,i])
      
      # remove any rows at or after the footnote row
      data <- data[1:(footnote_row-1),]

      
      # get column names
      colnames(data) <- data[first_content_line,]
      
      # remove the header
      data <- data[(first_content_line + 1):nrow(data),] 
      
      # remove columns that are all NA values
      data <- data[, colSums(is.na(data)) < nrow(data)] 
      
      # remove rows that are all NA values
      data <- data[rowSums(is.na(data)) < ncol(data),]
      
      # clean column names using janitor::clean_names
      data <- janitor::clean_names(data)
      
      # add some additional columns
      data$reinsurance_year <- year
      data$report_geography  <- geography
      data$data_date <- data_date
      data$footnote <- footnote
      
      # pivot longer
        data <- data %>% 
          tidyr::pivot_longer(cols = -c(reinsurance_year,report_geography,data_date,footnote), 
                              names_to = "value_type", 
                              values_to = "dollars")
        
   
      # save data as a rds file
      file_path <- paste0("./data-raw/reinsurance_reports/",type,"/", 
                          geography,"_", year, ".rds")
      saveRDS(data, file_path)
      
    }, error=function(e){})
  }
}

# cleans the data_date column and extracts the month, year, and day
parse_data_release_date <- function(df, date_col = "data_date") {
  # Ensure the column exists
  if (!date_col %in% names(df)) {
    stop(paste("Column", date_col, "not found in the data frame."))
  }
  
  # Extract month and year
  df$data_release_month <- gsub("/","",substr(df[[date_col]], 1, 2))
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

download_reinsurance_reports(2020, geography = "state", type = "standard")
# download and save national reinsurance reports for livestock reinsurance agreement

# links for the livestock reinsurance report excel files. Must be periodically updated if new data is available (typically once per year).
# Automatically web scraping the most recent links would be a good feature upgrade later. 
livestock_links <- list("2014" = "https://www.rma.usda.gov/sites/default/files/2024-06/Livestock%20Reinsurance%20Report_2014-xls.xls",
                        "2015" = "https://www.rma.usda.gov/sites/default/files/2024-06/Livestock%20Reinsurance%20Report_2015-xls.xls",
                        "2016" = "https://www.rma.usda.gov/sites/default/files/2024-06/Livestock%20Reinsurance%20Report_2016-xls.xls",
                        "2017" = "https://www.rma.usda.gov/sites/default/files/2024-06/Livestock%20Reinsurance%20Report_2017-xls.xls",
                        "2018" = "https://www.rma.usda.gov/sites/default/files/2024-06/Livestock%20Reinsurance%20Report_2018-xls.xlsx",
                        "2019" = "https://www.rma.usda.gov/sites/default/files/2024-06/Livestock%20Reinsurance%20Report_2019-xls.xls",
                        "2020" = "https://www.rma.usda.gov/sites/default/files/2024-08/Livestock%20Reinsurance%20Report_2020.xls",
                        "2021" = "https://www.rma.usda.gov/sites/default/files/2025-03/Livestock%20Reinsurance%20Report_2021.xls",
                        "2022" = "https://www.rma.usda.gov/sites/default/files/2025-02/Livestock%20Reinsurance%20Report_2022.xls",
                        "2023" = "https://www.rma.usda.gov/sites/default/files/2025-02/Livestock%20Reinsurance%20Report_2023.xls")

# apply the download function to the livestock links
mapply(FUN = download_reinsurance_reports, 
       year = as.numeric(names(livestock_links)), 
       MoreArgs = list(geography = "national", 
                       type = "livestock", 
                       livestock_download_links = livestock_links))


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


# clean the downloaded state reinsurance reports for the standard reinsurance agreement ---------------
state_reinsurance_reports <- list.files(path = "./data-raw/reinsurance_reports/standard/StateFund", 
                                           pattern = "*.rds", full.names = TRUE) %>% 
  purrr::map(readRDS) %>%
  dplyr::bind_rows() %>%
  rename(fund_abb = fund)



# clean date using parse_data_release_date function
state_reinsurance_reports <- parse_data_release_date(state_reinsurance_reports) %>%
  select(-data_date)

# add a column with the full fund type name
state_reinsurance_reports <- convert_fund_types(df = state_reinsurance_reports, 
                                                   abb_col = "fund_abb", name_col = "fund_name")

# add a column indicating the type of reinsurance report
state_reinsurance_reports$report_type <- "Standard Resinsurance Agreement"

# remove in values in the state column containing "Footnote"
state_reinsurance_reports <- state_reinsurance_reports %>% 
  filter(!grepl("Footnote",state),
         !grepl("Footnote",dollars))

# trim ws on state column
state_reinsurance_reports$state <- trimws(state_reinsurance_reports$state)

# replace any state abbreviations containing "All Other" with "All Other States"
state_reinsurance_reports$state <- gsub("All Other","All Other States",state_reinsurance_reports$state)

# remove "*" from the state column
state_reinsurance_reports$state <- gsub("\\*","",state_reinsurance_reports$state)

# remove any rows that have NA in the dollars columns
state_reinsurance_reports <- state_reinsurance_reports %>% 
  filter(!is.na(dollars))

# export as a data set
stateSRA = state_reinsurance_reports
usethis::use_data(stateSRA, overwrite = TRUE)

# clean the downloaded national reinsurance reports for the livestock reinsurance agreement -----------
livestock_reinsurance_reports <- list.files(path = "./data-raw/reinsurance_reports/livestock", 
                                           pattern = "*.rds", full.names = TRUE) %>% 
  purrr::map(readRDS) %>%
  dplyr::bind_rows() 


# clean date using parse_data_release_date function
livestock_reinsurance_reports<- parse_data_release_date(livestock_reinsurance_reports) %>%
  select(-data_date)

# add a column indicating the type of reinsurance report
livestock_reinsurance_reports$report_type <- "Livestock Price Reinsurance Agreement"


# export as a data set
nationalLPRA = livestock_reinsurance_reports
usethis::use_data(nationalLPRA, overwrite = TRUE)



