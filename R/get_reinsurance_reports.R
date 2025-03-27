# get_reinsurance_reports <- function(year = as.numeric(format(Sys.Date(), "%Y")), type = c("National","State")) {
#   
#   # type check type
#   type <- trimws(tolower(type))
#   if(type == "national"){
#     type <- "NationalFund"
#   } else if(type == "state"){
#     type <- "StateFund"
#   } else{
#     stop("Invalid type argument entered. Must be either 'National' or 'State' ")
#   }
#   
#   for( y in year){
#     
#     # construct url to file
#     url = paste0("https://public-rma.fpac.usda.gov/apps/ReinsuranceReports/Areas/GetReinsuranceReport?reinsuranceYear=",
#                   y,"&reportType=",type,"&reportFormat=EXCEL")
#     
#     # download the file
#     httr::GET(url, httr::write_disk(tf <- tempfile(fileext = ".xls")))
#     
#     # load the data from the temporary file
#     data <- suppressMessages(janitor::clean_names(readxl::read_excel(tf, skip = 0)))
#     
#     # calculate where first line of content is 
#     first_content_line <- which(data[,1] == "Fund")
#     
#     # load again skiping to header
#     data <- suppressMessages(janitor::clean_names(readxl::read_excel(tf, skip = first_content_line)))
#     
#     # pivot the data 
#     data <- data %>% tidyr::pivot_longer(cols = -fund, names_to = "value_type", values_to = "dollars")
#     
#     # add a crop year variable
#     data$reinsurance_year = y
#     
#     # replace fund names
#     
#     # if reinsurance data frame doesn't exist, create it
#     if(!exists("reinsurance")){
#       reinsurance <- data
#     } else { # if not add data to existing data frame
#       reinsurance <- dplyr::bind_rows(reinsurance, data)
#     }
#     
#     return(reinsurance)
#     
#   }
# 
#   
# }
