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
locate_col_links <- function(url = "https://www.rma.usda.gov/tools-reports/summary-of-business/cause-loss") {

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
  links <- links[-which(is.na(links$year)), ]

  # remove url with .doc suffix
  #links <- links[-grepl("\\.doc", links$url), ]

  # remove unneccesary characters from url
  links$url <- gsub("href=\"", "", links$url)
  links$url <- gsub('"', "", links$url)

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
  #links$url <- paste0("https://www.rma.usda.gov", links$url)

  # return data frame of correct urls
  return(links)
}

#' Generates a data frame of urls and years corresponding to where each summary of business by type, practive, 
#' and unit structure file is hosted on RMA's website
#' @noRd
#' @keywords internal
#' @importFrom stringr str_match_all
#'
locate_sobtpu_links <- function(url = "https://www.rma.usda.gov/tools-reports/summary-of-business/state-county-crop-summary-business") {
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
  #links$url <- paste0("https://www.rma.usda.gov", links$url)
  
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
