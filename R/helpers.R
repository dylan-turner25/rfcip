#' Generates a data frame of urls and years corresponding to where each cause of loss file is hosted on RMA's website
#' @noRd
#' @keywords internal
#' @importFrom stringr str_match_all
#'
locate_col_links <- function(url = "https://www.rma.usda.gov/tools-reports/summary-business/cause-loss") {
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
  links <- links[-grepl("\\.doc", links$url), ]

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
  links$url <- paste0("https://www.rma.usda.gov", links$url)

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

  # add additional grouping parameters if specified
  if (!is.null(group_by)) {
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
