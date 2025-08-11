#' Internal helper to download and verify a ZIP file
#'
#' Attempts to download a ZIP archive from a given URL up to a specified number of times,
#' and verifies that it is a valid ZIP by listing its contents. If the download or verification
#' fails after all attempts, an error is raised.
#'
#' @param url A character string giving the URL of the ZIP file to download.
#' @param destfile A character string giving the path (including filename) where the downloaded
#'   file should be saved locally.
#' @param method Optional character string specifying the download method to pass to
#'   \code{\link[utils]{download.file}} (e.g. "curl", "libcurl", "wget"). If \code{NULL},
#'   R chooses the default.
#' @param attempts Integer number of times to retry the download and verification before
#'   giving up. Defaults to 3.
#'
#' @return Invisibly returns \code{TRUE} if the file was successfully downloaded and verified.
#'   If all attempts fail, the function throws an error.
#'
#' @keywords internal
#' @noRd
download_and_verify <- function(url, destfile, method = NULL, attempts = 3) {
  for (i in seq_len(attempts)) {
    # try a download
    try(utils::download.file(
      url, destfile = destfile,
      mode   = "wb",
      method = method,
      quiet  = TRUE
    ), silent = TRUE)
    
    info <- file.info(destfile)
    # did we at least get a non‐zero file?
    if (!is.na(info$size) && info$size > 0) {
      # try listing the zip contents
      z <- try(utils::unzip(destfile, list = TRUE), silent = TRUE)
      if (inherits(z, "data.frame") && nrow(z) > 0) {
        return(invisible(TRUE))
      }
    }
    
    # otherwise wait a bit and retry
    Sys.sleep(1)
  }
  stop("Failed to download or verify zip from ", url)
}
#' Get summary of business data by type, practice, and unit structure.
#' 
#' This function is a helper function for `get_sob_data()` that gets activated when a user sets `sob_version = "sobtpu"` in `get_sob_data()`. 
#' All of the arguments in `get_sobtpu_data` are inherited from `get_sob_data()`. The function can be used as a standalone function however,
#' but is not exported, meaning it must be called via `rfcip:::get_sobtpu_data()`.
#'
#' @param year a numeric value (either single value or a vector of values) that indicate the crop year (ex: `2024` or `c(2022,2023,2024)`)
#' @param crop can be either a character string indicating a crop (i.e. `corn`) or a numeric value indicating the crop code (i.e. `41`). Inputting a vector with multiple values will return data for multiple crops. To get a data frame containing all the available crops and crop codes use `get_crop_codes()`.
#' @param insurance_plan can be either a character string indicating an insurance plan (ex: `yp` and `yield protection` are both valid) or a numeric value indicating the insurance plan code (i.e. `1`). Inputting a vector with multiple values will return data for multiple insurance plans. To get a data frame containing all the available insurance plans and insurance plan codes use `get_insurance_plan_codes()`.
#' @param state can be a character string indicating the state abbreviation or state name. Numeric values corresponding to state FIPS codes can also be supplied.
#' @param county either a character string with a county name or 5-digit FIPS code corresponding to a county. when the county is specified using the name of the county, the state parameter must also be specified.
#' @param fips a numeric value corresponding to a 5-digit FIPS code of a U.S. county.
#' @param cov_lvl a numeric value indicating the coverage level. Valid coverage levels are `c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95)`
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_dfr
#' @importFrom usmap fips
#' @import cli
#' 
#'
#' @returns A tibble containing the summary of business data by type, practice, and unit structure.
#'
get_sobtpu_data <- function(year = NULL,
                            crop = NULL, 
                            insurance_plan = NULL, 
                            state = NULL, 
                            county = NULL, 
                            fips = NULL, 
                            cov_lvl = NULL) {
  
  
  # clean function arguments
  
  # clean crop entry
  if (!is.null(crop)) {
    crop <- as.numeric(data.frame(get_crop_codes(crop = crop))[, "commodity_code"])
  }
  
  # clean insurance plan entry
  if (!is.null(insurance_plan)) {
    insurance_plan <- as.numeric(data.frame(get_insurance_plan_codes(plan = insurance_plan))[, "insurance_plan_code"])
  }
  
  # clean state county and fips code
  if (is.null(state) & !is.null(county) & !is.null(fips)) {
    stop("county parameter value entered without a state parameter value. Both state and county parameters must be defined to return county level data. Alternatively, the fips parameter can be filled in with a 5-digit FIPS code to return county level data.")
  }
  
  if (!is.null(state) & !is.null(county)) {
    county <- usmap::fips(state = valid_state(state), county = county)
  }
  
  if (!is.null(state)) {
    state <- as.numeric(usmap::fips(state = valid_state(state)))
  }
  
  if (!is.null(fips)) {
    fips <- clean_fips(fips = fips)
    state <- as.numeric(substr(fips, 1, 2))
    county <- as.numeric(substr(fips, 3, 5))
  }
  
  
  # input checking
  stopifnot("`year` must be a vector of numeric values." = is.numeric(year))
  
  # get the current location of the col files
  cli::cli_alert_info("Locating Summary of Business download links on RMA's website.")
  sob_urls <- locate_sobtpu_links()
  cli::cli_alert_success("Download links located.")
  
  # set up a temporary directory
  sobtpu_data_files <- tempdir()
  
  # initialize progress bar
  cli::cli_progress_bar("Downloading Summary of Business files for specified crop years", total = length(year))
  
  # loop over years
  for (y in year) {
    cli::cli_progress_update()
    
    
    # locate url corresponding to the crop year
    url <- sob_urls$url[which(sob_urls$year == y)]
    
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
      
      # remove the temporary files
      unlink(temp_zip)
      unlink(temp_txt, recursive = T)
      
      
    })
    
    
    colnames(data) <- c(
      "commodity_year",
      "state_code",
      "state_name",
      "state_abbreviation",
      "county_code",
      "county_name",
      "commodity_code",
      "commodity_name",
      "insurance_plan_code",
      "insurance_plan_abbreviation",
      "coverage_type_code",
      "coverage_level_percent",
      "delivery_id",
      "type_code",
      "type_name",
      "practice_code",
      "practice_name",
      "unit_structure_code",
      "unit_structure_name",
      "net_reporting_level_amount",
      "reporting_level_type",
      "liability_amount",
      "total_premium_amount",
      "subsidy_amount",
      "indemnity_amount",
      "loss_ratio",
      "endorsed_commodity_reporting_level_amount"
    )
    
    # convert all columns to character values
    data <- dplyr::mutate(data, dplyr::across(dplyr::everything(), as.character))
    
    
    # apply any filters specified by the function arguments
    
    # filter by crop
    if (!is.null(crop)) {
      data <- data[data$commodity_code %in% as.character(crop), ]
    }
    
    # filter by insurance plan
    if (!is.null(insurance_plan)) {
      data <- data[data$insurance_plan_code %in% as.character(insurance_plan), ]
    }
    
    # filter by state
    if (!is.null(state)) {
      data <- data[data$state_code %in% as.character(state), ]
    }
    
    # filter by county
    if (!is.null(county)) {
      data <- data[data$county_code %in% as.character(county), ]
    }
    
  
    # filter by coverage level
    if (!is.null(cov_lvl)) {
      data <- data[as.numeric(data$coverage_level_percent) %in% as.numeric(cov_lvl), ]
    }
    
    # save as a rds file
    saveRDS(data, file = paste0(sobtpu_data_files, "/sobtpu_", y, ".rds"))
    
  }
  
  # close progress bar
  cli::cli_progress_done()
  
  # indicate merging of files is taking place
  cli::cli_alert_info("Merging Summary of Business files for all specified crop years")
  
  # list all files in the temporary directory matching the file naming convention
  files_to_load <- list.files(sobtpu_data_files, full.names = T, pattern = "sobtpu_\\d+.rds")
  
  # remove any files that aren't the specified years
  files_to_load <- files_to_load[grepl(paste0(year, collapse = "|"), files_to_load)]
  
  # load all the `files_to_load` and aggregate them into a single data frame
  sobtpu <- files_to_load %>%
    purrr::map_dfr(readRDS) %>%
    dplyr::bind_rows()
  
  
  return(sobtpu)
  
  
}

#' Generates a data frame of urls and years corresponding to where each cause of loss file is hosted on RMA's website
#' @noRd
#' @keywords internal
#' @importFrom stringr str_match_all
#'
locate_col_links <- function(url = "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/index.html") {

  # read the html
  html <- paste0(readLines(url), collapse = "\n")

  # get all links
  links <- as.character(data.frame(stringr::str_match_all(html, "href=\"(.*?)\""))[, 1])

  # filter to links containing "cause_loss" or "colsom"
  links <- links[grepl("cause-loss|colsom", links)]

  # put links in a data frame
  links <- data.frame(links)
  names(links) <- "url"

  # add a row to indicate the year
  links$year <- NA
  years <- 1989:format(Sys.Date(), "%Y")
  for (y in years) {
    links$year[grepl(y, links$url)] <- y
  }

  # remove na rows
  #links <- links[-which(is.na(links$year)), ]

  # remove url with .doc suffix
  #links <- links[-grepl("\\.doc", links$url), ]

  # remove unneccesary characters from url
  links$url <- gsub("href=\"", "", links$url)
  links$url <- gsub('"', "", links$url)
  links$url <- gsub("\\./", "/", links$url)
  
  # locate rows where actual file download link is on another page
  urls_needed <- links$url[!grepl("zip", links$url)]
  for (u in urls_needed) {
    # read the html
    html <- paste0(readLines(paste0("https://www.rma.usda.gov/", u)), collapse = "\n")

    # get all links
    sublinks <- as.character(data.frame(stringr::str_match_all(html, "href=\"(.*?)\""))[, 1])

    # locate .zip links
    sublinks <- sublinks[grepl("colsom", sublinks)]

    # remove unncessesary characters
    sublinks <- gsub("href=\"", "", sublinks)
    sublinks <- gsub('"', "", sublinks)

    # add sublink to the links data frame
    links$url[which(links$url == u)] <- sublinks
  }

  # add url prefix to links
  links$url <- paste0("https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss", links$url)

  # return data frame of correct urls
  return(links)
}

#' Generates a data frame of urls and years corresponding to where each summary of business by type, practive, 
#' and unit structure file is hosted on RMA's website
#' @noRd
#' @keywords internal
#' @importFrom stringr str_match_all
#'
locate_sobtpu_links <- function(url = "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop/index.html") {
  # read the html
  html <- paste0(readLines(url), collapse = "\n")
  
  # get all links
  links <- as.character(data.frame(stringr::str_match_all(html, "href=\"(.*?)\""))[, 1])
  
  # filter to links containing "sobtpu"
  links <- links[grepl("sobtpu", links)]
  
  # remove any links that have ".pdf" or ".docx" in them
  links <- links[!grepl("\\.pdf|\\.docx", links)]
  
  # put links in a data frame
  links <- data.frame(links)
  names(links) <- "url"
  
  # add a row to indicate the year
  links$year <- NA
  years <- 1989:format(Sys.Date(), "%Y")
  for (y in years) {
    links$year[grepl(y, links$url)] <- y
  }
  
  # remove unneccesary characters from url
  links$url <- gsub("href=\"", "", links$url)
  links$url <- gsub('"', "", links$url)
  links$url <- gsub("\\./", "/", links$url)

  # locate rows where actual file download link is on another page
  urls_needed <- links$url[!grepl("zip", links$url)]
  for (u in urls_needed) {
    # read the html
    html <- paste0(readLines(paste0("https://www.rma.usda.gov", u)), collapse = "\n")
    
    # get all links
    sublinks <- as.character(data.frame(stringr::str_match_all(html, "href=\"(.*?)\""))[, 1])
    
    # locate .zip links
    sublinks <- sublinks[grepl(".zip", sublinks)]
    
    # remove unncessesary characters
    sublinks <- gsub("href=\"", "", sublinks)
    sublinks <- gsub('"', "", sublinks)
    
    # add sublink to the links data frame
    links$url[which(links$url == u)] <- sublinks
  }
  
  # add url prefix to links 
  links$url <- paste0("https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/state_county_crop", links$url)
  
  # return data frame of correct urls
  return(links)
}



#' Generates a data frame of urls and years corresponding to livestock and dairy insurance programs
#' @noRd
#' @keywords internal
#' @importFrom stringr str_match_all
#'
#' @param url URL of the RMA livestock and dairy participation page
#' @return A data frame with columns for url, program, and year
locate_livestock_links <- function(url = "https://www.rma.usda.gov/tools-reports/summary-of-business/livestock-dairy-participation") {
  # read the html
  html <- paste0(readLines(url), collapse = "\n")
  
  # get all links
  links <- as.character(data.frame(stringr::str_match_all(html, "href=\"(.*?)\""))[, 1])
  
  # filter to links containing the relevant patterns for livestock/dairy programs
  links <- links[grepl("drp_|lgm_|lrp_", links)]
  
  # filter out links to documentation (.doc, .pdf, etc.)
  links <- links[grepl("\\.zip", links)]
  
  # put links in a data frame
  links <- data.frame(links)
  names(links) <- "url"
  
  # extract program type (drp, lgm, lrp)
  links$program <- NA
  links$program[grepl("drp_", links$url)] <- "DRP"
  links$program[grepl("lgm_", links$url)] <- "LGM"
  links$program[grepl("lrp_", links$url)] <- "LRP"
  
  # add a row to indicate the year
  links$year <- NA
  years <- 1989:format(Sys.Date(), "%Y")
  for (y in years) {
    links$year[grepl(paste0("_", y, "\\.zip|_", y, "_"), links$url)] <- y
  }
  
  # remove unneccesary characters from url
  links$url <- gsub("href=\"", "", links$url)
  links$url <- gsub('"', "", links$url)
  
  # return data frame of correct urls
  return(links)
}




#' adds & separator to url if necessary
#'
#' @param url a url 
#' @noRd
#' @keywords internal
include_and <- function(url){
  if(!substr(url,nchar(url),nchar(url)) == "?"){
    return(paste0(url,"&"))
  } else{
    return(url)
  }
}

#' Generates a URL corresponding to a query on RMA's summary of business Web App
#' @noRd
#' @keywords internal
#' @importFrom usmap fips
#'
get_sob_url <- function(year = c(2023, 2024), crop = c("corn", "soybeans"), delivery_type = NULL, insurance_plan = NULL, state = NULL, county = NULL, fips = NULL, cov_lvl = NULL, comm_cat = "B", group_by = NULL) {
  # define the prefix and suffix of the URL string (i.e. these are constant)
  prefix <- "https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel?"


  suffix <- "&VisibleColumns=CommodityYear,CommodityName,CommodityCode,LocationStateAbbreviation,LocationStateCode,LocationCountyName,LocationCountyCode,DeliveryTypeCode,DeliveryTypeName,InsurancePlanAbbreviation,InsurancePlanCode,InsurancePlanName,CoverageLevelPercent,PolicySoldCount,PolicyPremiumCount,PolicyIndemnityCount,UnitPremiumCount,UnitIndemnityCount,CommodityReportingLevelAmount,CommodityReportingLevelType,EndorsedReportingLevelAmount,LiabilityAmount,TotalPremiumAmount,SubsidyAmount,IndemnityAmount,EFAPremiumAmount,AdditionalSubsidyAmount,StatePrivateSubsidyAmount,PccpStateMatchingAmount,OrganicCertifiedSubsidyAmount,OrganicTransitionalSubsidyAmount,EarnPremiumRate,loss_ratio&SortField=&SortDir="


  # clean crop entry
  if (!is.null(crop)) {
    crop <- data.frame(get_crop_codes(crop = crop))[, "commodity_code"]
  }

  # clean insurance plan entry
  if (!is.null(insurance_plan)) {
    insurance_plan <- data.frame(get_insurance_plan_codes(plan = insurance_plan))[, "insurance_plan_code"]
  }

  # clean state county and fips code
  if (is.null(state) & !is.null(county) & !is.null(fips)) {
    stop("county parameter value entered without a state parameter value. Both state and county parameters must be defined to return county level data. Alternatively, the fips parameter can be filled in with a 5-digit FIPS code to return county level data.")
  }
  if (!is.null(state) & !is.null(county)) {
    county <- usmap::fips(state = valid_state(state), county = county)
  }
  if (!is.null(state)) {
    state <- usmap::fips(state = valid_state(state))
  }
  if (!is.null(fips)) {
    fips <- clean_fips(fips = fips)
    state <- substr(fips, 1, 2)
    county <- fips
  }

  # initialize ORD parameter and parameter string
  ORD <- c()
  parameter_string <- c()

  # year
  if (!is.null(year)) {
    parameter_string <- paste0(parameter_string, "CY=", paste(year, collapse = ","), "&")
    ORD <- append(ORD, "CY")
  }

  # commodity
  if (!is.null(crop)) {
    parameter_string <- paste0(parameter_string, "CM=", paste(crop, collapse = ","), "&")
    ORD <- append(ORD, "CM")
  }

  # state
  if (!is.null(state)) {
    parameter_string <- paste0(parameter_string, "ST=", paste(state, collapse = ","), "&")
    ORD <- append(ORD, "ST")
  }

  # delivery type
  if (!is.null(delivery_type)) {
    parameter_string <- paste0(parameter_string, "ST=", paste(delivery_type, collapse = ","), "&")
    ORD <- append(ORD, "DT")
  }

  # insurance_plan
  if (!is.null(insurance_plan)) {
    parameter_string <- paste0(parameter_string, "IP=", paste(insurance_plan, collapse = ","), "&")
    ORD <- append(ORD, "IP")
  }

  # county
  if (!is.null(county)) {
    parameter_string <- paste0(parameter_string, "CT=", paste(county, collapse = ","), "&")
    ORD <- append(ORD, "CT")
  }

  # coverage level
  if (!is.null(cov_lvl)) {
    parameter_string <- paste0(parameter_string, "CVL=", paste(cov_lvl, collapse = ","), "&")
    ORD <- append(ORD, "CVL")
  }

  # commodity category
  if (!is.null(comm_cat)) {
    parameter_string <- paste0(parameter_string, "CC=", paste(comm_cat, collapse = ","), "&")
  }


  # paste ORD parameter together
  ORD <- paste0("ORD=", paste(ORD, collapse = ","))

  
  # remove any group_by variables that already have filters specified
  # (data will already be grouped by these)
  for(g in group_by){
    
    # check if the group_by variable is not NULL
    if(!is.null(eval(parse(text = g)))){
      group_by <- group_by[-which(group_by == g)]
    }
    
    # if all group_by variables are removed, set group_by to NULL
    if(length(group_by) == 0){
      group_by = NULL
    }
  }
  
  # add additional grouping parameters if specified 
  if (!is.null(group_by)) {
    
    # if group by includes county, ensure state is also included
    if("county" %in% group_by & ("state" %in% group_by == F)){
      group_by <- c(group_by,"state")
    }
    

    group_by_codes <- c(
      "CY" = "year",
      "CM" = "crop",
      "ST" = "state",
      "DT" = "delivery_type",
      "IP" = "insurance_plan",
      "CT" = "county",
      "CVL" = "cov_lvl"
    )

    ORD <- paste0(ORD, ",", paste0(names(group_by_codes[which(group_by_codes %in% group_by)]), collapse = ","))
  }

  return(paste0(prefix, parameter_string, ORD, suffix))
}

#' A helper function that checks to make sure the specified crop is valid
#' @noRd
#' @keywords internal
#'
valid_crop <- function(comm) {
  crop_codes <- get_crop_codes()
  suppressWarnings({
    if (!(F %in% (as.numeric(comm) %in% as.numeric(crop_codes$commodity_code))) |
      !(F %in% (tolower(comm) %in% tolower(crop_codes$commodity_name)))) {
      valid <- T
    } else {
      valid <- F
    }
    return(valid)
  })
}

#' A helper function that checks to make sure the specified state is valid
#' @noRd
#' @keywords internal
#'
valid_state <- function(state) {
  if (!(F %in% (tolower(state) %in% tolower(datasets::state.name)))) {
    return(state)
  } else if (!(F %in% (tolower(state) %in% tolower(datasets::state.abb)))) {
    return(state)
  } else if (!(F %in% (suppressWarnings(as.numeric(state) %in% as.numeric(usmap::fips(state = datasets::state.name)))))) {
    return(state)
  } else {
    stop("Parameter value for state not valid.")
  }
}


#' A helper function that take a county and state fips code and returns a 5-digit fips code or takes
#' a 4 or 5 digit fips code and ensures it is 5-digits by adding a leading zero.
#' @noRd
#' @keywords internal
#'
clean_fips <- Vectorize(function(fips = NULL, county = NULL, state = NULL) {
  if (is.null(fips)) {
    if (nchar(state) == 1) {
      state_clean <- paste0("0", state)
    }
    if (nchar(state) == 2) {
      state_clean <- as.character(state)
    }
    if (nchar(county) == 1) {
      county_clean <- paste0("00", county)
    }
    if (nchar(county) == 2) {
      county_clean <- paste0("0", county)
    }
    if (nchar(county) == 3) {
      county_clean <- as.character(county)
    }
    fips <- paste0(state_clean, county_clean)
    return(fips)
  }
  if (is.null(fips) == F) {
    if (nchar(fips) == 4) {
      fips_clean <- paste0("0", fips)
    }
    if (nchar(fips) < 4) {
      print("Warning: fips codes should be either 4 or 5 character strings. Returning NA")
      return(NA)
    }
    if (nchar(fips) == 5) {
      fips_clean <- as.character(fips)
    }
    return(fips_clean)
  }
})

# ===== ADM-SPECIFIC HELPER FUNCTIONS =====

# Global variable bindings to avoid R CMD check NOTEs
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "reference_amount_code", "record_category_code", "beta_id",
    "unit_structure_code", "nme", "lookup_rate", "base_rate"
  ))
}

#' @importFrom magrittr %>%

#' @title Locate data asset files by year and dataset
#' @name locate_data_asset
#' @description Internal helper function that searches for available data asset files that match the
#' specified year(s) and dataset. The function performs case-insensitive matching and
#' removes underscores from dataset names for flexible matching.
#'
#' @param year Numeric vector. The year(s) to search for (e.g., 2020 or c(2020, 2021)).
#' @param dataset Character. The dataset name to search for. Underscores are automatically
#'   removed and matching is case-insensitive.
#'
#' @return Character vector of matching file names (in lowercase with underscores removed).
#'
#' @details
#' The function normalizes both the search criteria and available files by:
#' \itemize{
#'   \item Converting to lowercase
#'   \item Removing underscores
#' }
#' This allows flexible matching regardless of case or underscore usage.
#'
#' @seealso \code{\link{list_data_assets}} for listing all available assets
#' @keywords internal
locate_data_asset <- function(year, dataset){

  # convert dataset to lower case and gsub _
  dataset <- tolower(gsub("_", "", dataset))

  # get all files stored
  all_files <- list_data_assets()

  # filter on year
  if(!is.null(year)){
    files <- all_files[grepl(paste0(year, collapse = "|"), all_files)]
  } else {
    files = all_files
  }

  # filter on dataset
  files <- files[grepl(dataset, tolower(gsub("_", "", files)))]

  # if length of files is zero, issue error message and a list of all files
  if(length(files) == 0){
    stop(
      paste0(
        paste0("No files found for year ", year, " and dataset ", dataset, ".\n"),
        " Available datasets are:\n",
        paste0(paste(unique(all_files)), collapse = "\n ")
      )
    )
  }

  return(files)

}


#' Download and read data files from GitHub Releases (supports RDS and parquet)
#'
#' Downloads data files from GitHub releases and loads them into R. Supports both
#' legacy .rds files and new .parquet files with automatic factor level restoration.
#'
#' @param name Character. The basename of the data file (e.g., "foo.rds" or "bar.parquet")
#' @param repo Character. GitHub repository in format "owner/repo"
#' @param tag Character. Which release tag to download from (default: latest)
#' @param show_progress Logical value indicating whether a progress download bar should be displayed. Defaults to `True`.

#' @return A data.frame containing the loaded data with properly restored factor levels
#'
#' @details
#' This function:
#' \itemize{
#'   \item Downloads files from GitHub releases using piggyback
#'   \item Caches files locally to avoid repeated downloads
#'   \item Automatically detects file format (.rds vs .parquet)
#'   \item Restores factor levels for parquet files using stored metadata
#'   \item Maintains backward compatibility with existing RDS files
#' }
#'
#' @importFrom arrow read_parquet
#' @importFrom piggyback pb_download
#' @keywords internal
get_cached_data <- function(name,
                           repo = "dylan-turner25/rmaADM",
                           tag  = NULL,
                           show_progress = T) {
  dest_dir <- tools::R_user_dir("rfcip", which = "cache")
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  dest_file <- file.path(dest_dir, name)

  # Download if not cached
  if (!file.exists(dest_file)) {
    piggyback::pb_download(
      file = name,
      repo = repo,
      tag  = tag,
      dest = dest_dir,
      show_progress = show_progress
    )
  }

  # Read based on file extension
  file_ext <- tools::file_ext(name)

  if (file_ext == "rds") {
    # Legacy RDS files
    data <- readRDS(dest_file)
  } else if (file_ext == "parquet") {
    # New parquet files
    data <- arrow::read_parquet(dest_file)

    # Restore factor levels if metadata exists
    data <- restore_factor_levels(data, name)
  } else {
    stop("Unsupported file format: ", file_ext, ". Supported formats: rds, parquet")
  }

  return(data)
}


#' Restore factor levels for parquet data using metadata
#'
#' @param data data.frame. The data read from parquet file
#' @param filename Character. Original filename to derive metadata key
#' @return data.frame with restored factor levels
#' @keywords internal
restore_factor_levels <- function(data, filename) {
  # Try to load factor metadata from inst/extdata
  metadata_file <- system.file("extdata", "adm_factor_metadata.rds", package = "rfcip")

  # If package metadata doesn't exist, check local inst/extdata folder
  if (!file.exists(metadata_file) || metadata_file == "") {
    metadata_file <- "./inst/extdata/adm_factor_metadata.rds"
  }

  if (!file.exists(metadata_file)) {
    # No metadata available, return data as-is
    return(data)
  }

  # Load metadata
  factor_metadata <- readRDS(metadata_file)

  # Generate metadata key from filename (remove extension and path)
  metadata_key <- tools::file_path_sans_ext(basename(filename))

  # Find matching metadata key (try exact match first, then partial matches)
  matching_keys <- names(factor_metadata)[names(factor_metadata) == metadata_key]
  if (length(matching_keys) == 0) {
    # Try partial matches for dataset names
    matching_keys <- names(factor_metadata)[grepl(metadata_key, names(factor_metadata)) |
                                           grepl(gsub("\\d+", "", metadata_key), names(factor_metadata))]
  }

  if (length(matching_keys) == 0) {
    # No matching metadata, return as-is
    return(data)
  }

  # Use the first matching key
  column_metadata <- factor_metadata[[matching_keys[1]]]

  # Restore factor levels for matching columns
  for (col_name in names(column_metadata)) {
    if (col_name %in% names(data)) {
      factor_levels <- column_metadata[[col_name]]
      data[[col_name]] <- factor(data[[col_name]], levels = factor_levels)
    }
  }

  return(data)
}

#' Clear the package cache of downloaded data files
#'
#' Deletes the entire cache directory used by the **rfcip** package to store
#' downloaded data files. Useful if you need to force re-download of data,
#' or free up disk space.
#'
#' @return Invisibly returns `NULL`. A message is printed indicating which
#'   directory was cleared.
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove all cached data files so they will be re-downloaded on next use
#' clear_rfcip_cache()
#' }
clear_rfcip_cache <- function(){
  dest_dir <- tools::R_user_dir("rfcip", which = "cache")
  if (dir.exists(dest_dir)) {
    unlink(dest_dir, recursive = TRUE, force = TRUE)
  }
  message("Cleared cached files in ", dest_dir)
  invisible(NULL)
}


#' List asset names from the latest GitHub release
#'
#' Retrieves the metadata for the most recent release of the **rmaADM** repository
#' on GitHub and extracts the names of all attached release assets.
#'
#' @return A character vector of file names (assets) in the latest release.
#' @keywords internal
#' @examples
#' \dontrun{
#' files = list_data_assets()
#' }
#' @importFrom gh gh
list_data_assets <- function(){
  # 1. Fetch the release metadata (by tag, or "latest")
  release <- gh::gh(
    "/repos/{owner}/{repo}/releases/latest",
    owner = "dylan-turner25",
    repo  = "rmaADM"
  )

  # 2. Extract the assets list
  assets <- release$assets

  # 3. Pull out the bits you care about
  df <- data.frame(
    name = vapply(assets, `[[`, "", "name"),
    url  = vapply(assets, `[[`, "", "browser_download_url"),
    size = vapply(assets, `[[`, 0,  "size"),
    stringsAsFactors = FALSE
  )

  return(df$name)
}


#' Locate the download link for the actuarial data master
#'
#' @param year the year of the actuarial data master to download
#' @param adm_url the url where the ADM FTP site is
#' @param ice_url the url where the ICE (insurance control elements) FTP site is
#' @param data_source either "adm" or "ice". Defaults to "adm".

#'
#' @returns a list of the data and layout file urls with the time the file was last updated on RMA's server
#'
#' @importFrom stringr str_match_all str_extract
#' @importFrom dplyr mutate filter
#'
#' @examples \dontrun{locate_adm_download_link(year = 2012)}
locate_adm_download_link <- function(year = 2012,
                                 adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/",
                                 ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                                 data_source = "adm"){

  if(data_source == "ice"){
    url <- ice_url
  }

  if(data_source == "adm"){
    url <- adm_url
  }

  # read in the webpage
  html <- suppressWarnings(paste0(readLines(url), collapse = "\n"))

  # locate all the links
  links <- as.character(data.frame(stringr::str_match_all(html,
                                                          "href=\"(.*?)\""))[, 1])

  # get the link with the matching year
  link <- links[grepl(year,links)]
  link <- link[!grepl("test",link)] # for ICE links


  # apply some cleaning opperations
  link <- gsub("href=\"", "", link)
  link <- gsub("\"", "", link)
  link <- gsub("\\./", "", link)
  link <- paste0(url, link)

  # navigate the the cleaned link to get the correct sublink
  html <- suppressWarnings(paste0(readLines(link), collapse = "\n"))

  # Extract the <pre> block where the file info resides
  pre_block <- str_extract(html, "<pre>.*?</pre>")

  # Extract date, time, size, and filename from each line using regex
  matches <- str_match_all(
    pre_block,
    "(\\d{2}/\\d{2}/\\d{4})\\s+(\\d{2}:\\d{2}\\s+[AP]M)\\s+(\\d+)\\s+<a href=\"\\./([^\"]*)\""
  )[[1]]

  # Convert to data frame
  file_info <- data.frame(
    date = matches[, 2],
    time = matches[, 3],
    size_bytes = as.numeric(matches[, 4]),
    filename = matches[, 5],
    stringsAsFactors = FALSE
  )


  # combine date and time into a single POSIXct column
  file_info <- file_info %>%
    mutate(
      datetime = as.POSIXct(paste(date, time), format = "%m/%d/%Y %I:%M %p", tz = "EST")
    )

  # filter the file info to only include the data and layout files
  file_info <- file_info %>%
    filter(grepl("ytd|layout", tolower(filename)))


  # add the base url and year to the file names
  file_info <- file_info %>%
    mutate(filename = paste0(url,year,"/",filename))

  # convert the links to a list and name them
  links <- as.list(file_info$filename)

  # name the link that contains "YTD" as "data"
  names(links)[which(grepl("YTD|ytd",links))] <- "data"

  # name the link that contains "Layout" as "layout"
  names(links)[which(grepl("Layout|layout",links))] <- "layout"

  # add the update date to the links
  links$update_date <- file_info$datetime[grepl("YTD",file_info$filename)]

  # unlist layout and data links
  links$data <- unlist(links$data)
  links$layout <- unlist(links$layout)

  # return the links
  return(links)

}


#' Check file availability and determine download/conversion strategy
#'
#' @param year_dir Character. Directory path for the specific year
#' @param dataset_codes Character vector. Dataset codes to check for
#' @param overwrite Logical. Whether to force overwrite
#'
#' @return List with elements: skip_download (logical), convert_txt_to_rds (logical), rds_to_delete (character vector)
#' @keywords internal
check_file_status <- function(year_dir, dataset_codes, overwrite = FALSE) {
  skip_download <- FALSE
  convert_txt_to_rds <- FALSE
  rds_to_delete <- character(0)

  if (!overwrite && !is.null(dataset_codes)) {
    # Get all RDS and TXT files
    rds_files <- list.files(year_dir, pattern = "\\.rds$", full.names = TRUE)
    txt_files <- list.files(year_dir, pattern = "\\.txt$", full.names = TRUE)

    # Check which codes are represented by RDS and TXT files
    rds_codes_present <- sapply(dataset_codes, function(code) {
      any(grepl(code, rds_files))
    })

    txt_codes_present <- sapply(dataset_codes, function(code) {
      any(grepl(code, txt_files))
    })

    # Check if all dataset codes are represented by either RDS or TXT files
    all_codes_represented <- all(rds_codes_present | txt_codes_present)

    if (all_codes_represented) {
      skip_download <- TRUE

      # Identify RDS files to delete (where both RDS and TXT exist for same code)
      for (i in seq_along(dataset_codes)) {
        code <- dataset_codes[i]
        if (rds_codes_present[i] && txt_codes_present[i]) {
          # Both exist, mark RDS for deletion
          rds_to_delete <- c(rds_to_delete, rds_files[grepl(code, rds_files)])
        }
      }

      # Check if any codes need TXT to RDS conversion
      if (any(txt_codes_present)) {
        convert_txt_to_rds <- TRUE
      }
    }
  }

  return(list(
    skip_download = skip_download,
    convert_txt_to_rds = convert_txt_to_rds,
    rds_to_delete = rds_to_delete
  ))
}


#' Download and process USDA RMA Actuarial Data Master (ADM) files
#'
#' Downloads ADM data files for the specified years from the USDA RMA actuarial data master repository,
#' extracts them, converts each `.txt` file to a memory-efficient `.rds` format using `data.table::fread()`,
#' and processes the data in chunks to minimize RAM usage. Helper files can optionally be retained or filtered
#' based on matching codes. Source archives can also be deleted after extraction.
#'
#' @param years Integer vector. The years of ADM data to download (e.g., `c(2012, 2013)`).
#' @param adm_url Character. Base URL for the ADM repository. Defaults to the public USDA RMA FTP URL.
#' @param dir Character. Local directory where files will be downloaded and processed. Created if it does not exist.
#' @param dataset_codes Character vector. File name patterns to retain, if null, keeps all files.
#' @param keep_source_files Logical. If `FALSE`, removes downloaded `.zip` archives after extraction.
#' @param overwrite Logical. If `TRUE`, re-downloads and re-processes files even if the existing data appears up to date.
#'
#' @return Invisibly returns `NULL`. Processed `.parquet` files are written to disk in the specified directory.
#'
#' @details
#' The function reads each `.txt` file in chunks (default 1 million rows at a time),
#' applies automatic type optimization and factor conversion, then writes the results
#' to compressed `.parquet` files. Files are only re-downloaded if the remote data
#' is newer than the latest local file, unless `overwrite = TRUE`.
#'
#' @note Files are read using `data.table::fread()` with all columns as character to reduce type inference overhead.
#' Chunked processing is used to reduce peak memory usage during conversion.
#'
#' @importFrom data.table fread rbindlist
#' @importFrom cli cli_alert_info cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom utils download.file unzip
#' @export
download_adm <- function(
    years = 2012,
    adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/",
    dir = "./data-raw",
    dataset_codes = c("A01090","A00070"),
    keep_source_files = FALSE,
    overwrite = FALSE
) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  for (year in years) {
    year_dir <- file.path(dir, as.character(year))
    if (!dir.exists(year_dir)) {
      dir.create(year_dir)
    }

    # locate download URLs
    urls <- locate_adm_download_link(year = year, adm_url = adm_url)

    # check file status and determine strategy
    file_status <- check_file_status(year_dir, dataset_codes, overwrite)

    if (file_status$skip_download) {
      # Delete RDS files where both RDS and TXT exist for same code
      if (length(file_status$rds_to_delete) > 0) {
        cli::cli_alert_info("Removing duplicate RDS files for {year}.")
        file.remove(file_status$rds_to_delete)
      }

      if (file_status$convert_txt_to_rds) {
        cli::cli_alert_info("Converting TXT files to RDS for {year}.")
      } else {
        cli::cli_alert_info("Data for {year} is up to date; skipping.")
        next
      }
    }

    if (!file_status$skip_download) {

    # check for existing zip file before downloading
    data_zip <- file.path(year_dir, sprintf("adm_ytd_%s.zip", year))

    if (file.exists(data_zip)) {
      cli::cli_alert_info("Found existing zip file for {year}; using cached download.")
    } else {
      # download data
      utils::download.file(urls$data, data_zip, mode = "wb")
    }

    # unzip data
    utils::unzip(data_zip, exdir = year_dir)
    if (!keep_source_files) file.remove(data_zip)

    # optionally download & unzip layout
    if ("layout" %in% names(urls)) {
      layout_zip <- file.path(year_dir, sprintf("layout_%s.zip", year))

      if (file.exists(layout_zip)) {
        cli::cli_alert_info("Found existing layout zip file for {year}; using cached download.")
      } else {
        # download layout
        utils::download.file(urls$layout, layout_zip, mode = "wb")
      }

      # unzip layout
      utils::unzip(layout_zip, exdir = year_dir)
      if (!keep_source_files) file.remove(layout_zip)
    }
    }

    # list .txt files
    txt_files <- list.files(year_dir, pattern = "\\.txt$", full.names = TRUE)

    # remove large helpers if requested
    if (!is.null(dataset_codes) && length(txt_files)) {
      #sizes <- file.info(txt_files)$size
      to_del <- txt_files[!grepl(paste(dataset_codes, collapse = "|"), txt_files)]

      # Also remove txt files that already have corresponding rds files (unless overwriting)
      if (!overwrite) {
        rds_files <- list.files(year_dir, pattern = "\\.rds$", full.names = TRUE)
        if (length(rds_files) > 0) {
          for (code in dataset_codes) {
            if (any(grepl(code, rds_files))) {
              # RDS file exists for this code, remove corresponding TXT files
              txt_with_rds <- txt_files[grepl(code, txt_files)]
              to_del <- c(to_del, txt_with_rds)
            }
          }
        }
      }

      if (length(to_del)) {
        file.remove(to_del)
      }
      txt_files <- setdiff(txt_files, to_del)
    }

    # convert each .txt to parquet format
    for (f in txt_files) {

      chunk_size <- 1000000
      output_file <- sub("\\.txt$", ".parquet", f)
      temp_dir <- file.path(dirname(output_file), "temp_chunks")
      dir.create(temp_dir, showWarnings = FALSE)

      chunk_count <- 0
      dt_names <- NULL
      rows_read <- 0

      # Process in chunks, saving each to temporary RDS
      repeat {
        dt <- tryCatch({
          data.table::fread(
            input = f,
            sep = "|",
            colClasses = "character",
            showProgress = FALSE,
            skip = rows_read,
            nrows = chunk_size
          )
        }, error = function(e) {
          # If we hit EOF error, return empty data.table to break loop
          data.table::data.table()
        })

        # Break if no rows read (natural EOF or error)
        if (nrow(dt) == 0) break

        rows_read <- rows_read + nrow(dt)

        chunk_count <- chunk_count + 1
        dt <- clean_adm_data(dt)

        if (is.null(dt_names)) {
          dt_names <- names(dt)
        } else {
          colnames(dt) <- dt_names
        }


        # Save chunk to temporary file
        temp_file <- file.path(temp_dir, paste0("chunk_", chunk_count, ".rds"))
        saveRDS(dt, temp_file, compress = "xz")
        rm(dt)
        gc() # Force garbage collection after each chunk
      }

      # Combine chunks using streaming approach (read one at a time)
      if (chunk_count > 0) {
        temp_files <- file.path(temp_dir, paste0("chunk_", seq_len(chunk_count), ".rds"))

        # Initialize with first chunk
        final_dt <- readRDS(temp_files[1])
        file.remove(temp_files[1])

        # Stream remaining chunks
        for (i in 2:length(temp_files)) {
          chunk_dt <- readRDS(temp_files[i])
          final_dt <- rbind(final_dt, chunk_dt)
          rm(chunk_dt)
          file.remove(temp_files[i])
          gc()
        }


        # Save final data as parquet with type optimization
        table_code <- substr(basename(f), 6, 11)
        year <- as.numeric(gsub(".*/(\\d{4})/.*", "\\1", f))
        metadata_key <- paste0(table_code, "_", year)

        compress_adm(final_dt, output_file, metadata_key)
        rm(final_dt)
      }

      # Cleanup
      unlink(temp_dir, recursive = TRUE)
      file.remove(f)
      gc()

    }
  }

  invisible(NULL)
}


#' Test if a column can be converted to numeric without data loss
#'
#' @param x A vector to test for numeric convertibility
#' @return Logical. TRUE if all non-NA values can be converted to numeric without loss
#' @keywords internal
is_numeric_convertible <- function(x, col_name = NULL) {
  if (is.numeric(x)) return(TRUE)
  if (!is.character(x)) return(FALSE)

  # Domain-specific rules for ADM data - keep these as character/factors
  if (!is.null(col_name)) {
    adm_code_patterns <- c(
      "_code$", "_id$", "^state_", "^county_", "^commodity_",
      "^type_code", "^class_code", "^sub_class", "^record_type",
      "^program_type", "^unit_structure", "^insurance_plan_code"
    )

    if (any(grepl(paste(adm_code_patterns, collapse = "|"), col_name, ignore.case = TRUE))) {
      return(FALSE)
    }
  }

  # Remove leading/trailing whitespace
  x_clean <- trimws(x)

  # Additional heuristics to avoid converting codes to numeric
  unique_vals <- unique(x_clean[!is.na(x_clean) & x_clean != ""])

  # If values look like zero-padded codes, keep as character
  if (any(grepl("^0[0-9]+$", unique_vals))) {
    return(FALSE)
  }

  # If high proportion of values are whole numbers that could be codes
  # and cardinality is relatively low, likely categorical
  if (length(unique_vals) / length(x_clean) < 0.1 &&
      all(grepl("^[0-9]+$", unique_vals[1:min(10, length(unique_vals))]))) {
    return(FALSE)
  }

  # Try to convert to numeric
  x_numeric <- suppressWarnings(as.numeric(x_clean))

  # Check if conversion was lossless (ignoring NA values)
  non_na_original <- !is.na(x_clean) & x_clean != ""
  non_na_converted <- !is.na(x_numeric)

  # All non-empty, non-NA values should convert successfully
  all(non_na_original == non_na_converted)
}


#' Optimize data types and save as parquet format
#'
#' Automatically detects numeric columns, converts character columns to factors,
#' and saves as compressed parquet files for optimal storage efficiency while
#' preserving all data.
#'
#' @param df A data.frame to optimize and save
#' @param output_path Character. Path where the parquet file should be saved
#' @param metadata_key Character. Unique key for storing factor metadata
#'
#' @return The optimized data.frame with automatic type conversion
#'
#' @details
#' Performs automatic type optimization:
#' \itemize{
#'   \item Detects columns that can be converted to numeric without data loss
#'   \item Converts character columns to factors for compression
#'   \item Preserves existing numeric, date, and logical columns
#'   \item Saves factor level metadata for reconstruction
#'   \item Writes compressed parquet file
#' }
#'
#' @importFrom arrow write_parquet
#' @importFrom data.table setDT
#' @export
compress_adm <- function(df, output_path, metadata_key) {
  # Convert to data.table for efficient operations
  data.table::setDT(df)

  factor_metadata <- list()

  # Process each column for type optimization
  for (col_name in names(df)) {
    col_data <- df[[col_name]]

    # Skip if already numeric, date, or logical
    if (is.numeric(col_data) || inherits(col_data, "Date") || is.logical(col_data)) {
      next
    }

    # Convert character columns
    if (is.character(col_data)) {
      if (is_numeric_convertible(col_data, col_name)) {
        # Convert to numeric if lossless
        df[, (col_name) := as.numeric(col_data)]
      } else {
        # Use cardinality analysis to decide on factor conversion
        unique_vals <- length(unique(col_data[!is.na(col_data)]))
        total_vals <- length(col_data[!is.na(col_data)])
        cardinality_ratio <- unique_vals / total_vals

        # Convert to factor if cardinality is low (good compression benefit)
        # Keep as character if very high cardinality (factor overhead not worth it)
        if (cardinality_ratio < 0.5 || unique_vals < 1000) {
          factor_col <- as.factor(col_data)
          df[, (col_name) := factor_col]
          factor_metadata[[col_name]] <- levels(factor_col)
        }
        # If high cardinality, keep as character (no conversion)
      }
    }
  }

  # Save factor metadata
  if (length(factor_metadata) > 0) {
    save_factor_metadata(metadata_key, factor_metadata)
  }

  # Final UTF-8 sanitization before writing parquet
  char_cols <- sapply(df, is.character)
  for(col in names(df)[char_cols]) {
    # First try UTF-8 cleaning, then fall back to ASCII if needed
    df[[col]] <- iconv(df[[col]], from = "", to = "UTF-8", sub = "")
    # If still problematic, convert to ASCII
    df[[col]] <- iconv(df[[col]], to = "ASCII//TRANSLIT", sub = "")
  }

  # Write as parquet file with better compression
  arrow::write_parquet(df, output_path, compression = "gzip")

  return(df)
}


#' Save factor metadata to package data folder
#'
#' @param metadata_key Character. Unique key for this dataset
#' @param factor_metadata List. Factor level mappings
#' @keywords internal
save_factor_metadata <- function(metadata_key, factor_metadata) {
  # Create inst/extdata directory if it doesn't exist
  extdata_dir <- "./inst/extdata"
  if (!dir.exists(extdata_dir)) {
    dir.create(extdata_dir, recursive = TRUE)
  }

  metadata_file <- file.path(extdata_dir, "adm_factor_metadata.rds")

  # Load existing metadata or create new
  if (file.exists(metadata_file)) {
    existing_metadata <- readRDS(metadata_file)
  } else {
    existing_metadata <- list()
  }

  # Add/update metadata for this key
  existing_metadata[[metadata_key]] <- factor_metadata

  # Save updated metadata
  saveRDS(existing_metadata, metadata_file, compress = "xz")
}


#' Clean the file name
#'
#' @param file_name the file path of the file name to clean
#' @param file_type_out the type of file (currently only supports rds)
#'
#' @returns a version of the file name that is cleaned (i.e. snake case, .rds suffix, no extraneous information)
#'
#' @examples \dontrun{clean_adm_file_name("./data-raw/2012_A01100_YieldAndTyield_YTD.txt")}
clean_adm_file_name <- function(file_name, file_type_out = "rds"){

  # split the file path into the directory and file name
  parts <- strsplit(file_name, "/")[[1]]

  # file suffix
  suffix <- parts[length(parts)]

  # Sanitize the suffix to ensure valid UTF-8 encoding
  suffix <- iconv(suffix, from = "UTF-8", to = "UTF-8", sub = "")

  # remove any instance of alphabetic character followed by 5 numbers
  #suffix <- gsub("[A-Za-z]\\d{5}", "", suffix)

  # remove the first 4 digits from the suffix if the first 4 digits are numeric digits
  suffix <- gsub("^[0-9]{4}", "", suffix)

  # remove "YTD"
  suffix <- gsub("YTD", "", suffix)

  # remove "_" unless "_" follows a number
  suffix <- gsub("(?<![0-9])_(?![0-9])", "", suffix, perl = TRUE)

  # convert from camel case to snake case
  suffix <- gsub("([a-z])([A-Z])", "\\1_\\2", suffix)
  suffix <- tolower(suffix)

  # split the suffix by the period
  suffix_parts <- strsplit(suffix, "\\.")[[1]]

  # replace the last part with the desired file type
  suffix_parts[length(suffix_parts)] <- file_type_out

  # paste the suffix parts back together
  suffix <- paste(suffix_parts, collapse = ".")

  # paste the directory and file name back together
  file_name <- paste(parts[-length(parts)], collapse = "/")

  # paste the file name and suffix together
  file_name <- paste0(file_name, "/", suffix)

  # return the file name
  return(file_name)

}


#' Apply standardized data cleaning opperations
#'
#' @param df a data frame to clean
#'
#' @returns a cleaned data frame
#' @importFrom janitor clean_names
#' @importFrom readr type_convert
#'
#' @examples \dontrun{clean_adm_data(df)}
clean_adm_data <- function(df){

  # clean column names
  df <- janitor::clean_names(df)

  # Sanitize all character data to ensure valid UTF-8 encoding
  char_cols <- sapply(df, is.character)
  for(col in names(df)[char_cols]) {
    # First try UTF-8 cleaning, then fall back to ASCII if needed
    df[[col]] <- iconv(df[[col]], from = "", to = "UTF-8", sub = "")
    # If still problematic, convert to ASCII
    df[[col]] <- iconv(df[[col]], to = "ASCII//TRANSLIT", sub = "")
  }

  # enforce data types
  df <- suppressMessages(readr::type_convert(df))

  # identify date
  date_cols <- grep("date", names(df), value = TRUE)

  # try to parse dates
  for(col in date_cols){
    input = as.character(df[[col]]) # convert to character
    input <- as.character(gsub("[^0-9]", "", input)) # remove non-numeric characters
    try({
      converted_dates <- readr::parse_date(input, format = "%Y%m%d", na = c("", "NA"))
      # if converted dates are not all NA
      if(!all(is.na(converted_dates))) {
        df[[col]] <- converted_dates
      }
    })
  }


  # Convert key columns to numeric if they exist
  numeric_cols <- intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))
  if(length(numeric_cols) > 0) {
    for(col in numeric_cols) {
      df[[col]] <- as.numeric(as.character(df[[col]]))
    }
  }

  # return the df
  return(df)

}


#' Get File Information from a Directory
#'
#' Scans a specified directory for files with a given suffix and returns a data frame
#' containing their file paths, sizes in bytes, and sizes in megabytes.
#'
#' @param directory A character string specifying the path to the directory to scan.
#'   Defaults to \code{"./data-raw"}.
#' @param file_suffix A character string specifying the file suffix to match.
#'   Defaults to \code{".rds"}.
#'
#' @return A data frame with columns:
#'   \item{file_path}{Full file path}
#'   \item{size_bytes}{File size in bytes}
#'   \item{size_mb}{File size in megabytes}
#'
#' @examples
#' \dontrun{
#' get_adm_file_info()
#' get_adm_file_info(directory = "./my-data", file_suffix = ".csv")
#' }
#'
get_adm_file_info <- function(directory = "./data-raw", file_suffix = ".rds") {
  # Get list of all files recursively
  files <- list.files(path = directory, recursive = TRUE, full.names = TRUE, pattern = file_suffix)

  # Filter only actual files (not directories)
  files <- files[file.info(files)$isdir == FALSE]

  # Get file sizes
  sizes <- file.info(files)$size

  # Create data frame
  df <- data.frame(
    file_path = files,
    size_bytes = sizes,
    size_mb = sizes / (1024 * 1024), # Convert to MB
    stringsAsFactors = FALSE
  )

  return(df)
}
