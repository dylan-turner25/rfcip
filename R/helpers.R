#' Generates a URL corresponding to a query on RMA's summary of business Web App
#'
#' @param year
#' @param crop
#' @param comm_cat a character vector of either "S" for standard, "L" for livestock, or "B" for both (the default).
#' @param cov_lvl a numeric vector of coverage levels. Values can be in 5 percentage point increments from  50% up to 95%, ex: c(0.5,0.55,0.6,0.7,0.75)


#' @return
#' @noRd
#' @keywords internal
#' @importFrom usmap fips
#'
#' @examples
#'
get_url <- function(year = c(2023,2024), crop = c("corn","soybeans"), delivery_type = NULL, insurance_plan = NULL, state = NULL, county = NULL, fips = NULL, cov_lvl = NULL,comm_cat = "B", group_by = NULL){

  # define the prefix and suffix of the URL string (i.e. these are constant)
  prefix= "https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel?"


  suffix = "&VisibleColumns=CommodityYear,CommodityName,CommodityCode,LocationStateAbbreviation,LocationStateCode,LocationCountyName,LocationCountyCode,DeliveryTypeCode,DeliveryTypeName,InsurancePlanAbbreviation,InsurancePlanCode,InsurancePlanName,CoverageLevelPercent,PolicySoldCount,PolicyPremiumCount,PolicyIndemnityCount,UnitPremiumCount,UnitIndemnityCount,CommodityReportingLevelAmount,CommodityReportingLevelType,EndorsedReportingLevelAmount,LiabilityAmount,TotalPremiumAmount,SubsidyAmount,IndemnityAmount,EFAPremiumAmount,AdditionalSubsidyAmount,StatePrivateSubsidyAmount,PccpStateMatchingAmount,OrganicCertifiedSubsidyAmount,OrganicTransitionalSubsidyAmount,EarnPremiumRate,loss_ratio&SortField=&SortDir="


  # clean crop entry
  if(!is.null(crop)){
    crop = data.frame(get_crop_codes(comm = crop))[,"commodity_code"]
  }

  # clean insurance plan entry
  if(!is.null(insurance_plan)){
    insurance_plan = data.frame(get_insurance_plan_codes(plan = insurance_plan))[,"insurance_plan_code"]
  }

  #clean state county and fips code
  if(is.null(state) & !is.null(county) & !is.null(fips)){
    stop("county parameter value entered without a state parameter value. Both state and county parameters must be defined to return county level data. Alternatively, the fips parameter can be filled in with a 5-digit FIPS code to return county level data.")
  }
  if(!is.null(state) & !is.null(county)){
    county <- usmap::fips(state = valid_state(state), county = county)
  }
  if(!is.null(state)){
    state <- usmap::fips(state = valid_state(state))
  }
  if(!is.null(fips)){
    fips <- clean_fips(fips = fips)
    state <- substr(fips,1,2)
    county <- fips
  }

  # initialize ORD parameter and parameter string
  ORD = c()
  parameter_string <- c()

  # year
  if(!is.null(year)){
    parameter_string <- paste0(parameter_string,"CY=",paste(year,collapse = ","),"&")
    ORD = append(ORD,"CY")
  }

  #commodity
  if(!is.null(crop)){
    parameter_string <- paste0(parameter_string,"CM=",paste(crop,collapse = ","),"&")
    ORD = append(ORD,"CM")
  }

  # state
  if(!is.null(state)){
    parameter_string <- paste0(parameter_string,"ST=",paste(state,collapse = ","),"&")
    ORD = append(ORD,"sT")
  }

  # delivery type
  if(!is.null(delivery_type)){
    parameter_string <- paste0(parameter_string,"ST=",paste(delivery_type,collapse = ","),"&")
    ORD = append(ORD,"DT")
  }

  # insurance_plan
  if(!is.null(insurance_plan)){
    parameter_string <- paste0(parameter_string,"IP=",paste(insurance_plan,collapse = ","),"&")
    ORD = append(ORD,"IP")
  }

  # county
  if(!is.null(county)){
    parameter_string <- paste0(parameter_string,"CT=",paste(county,collapse = ","),"&")
    ORD = append(ORD,"CT")
  }

  #coverage level
  if(!is.null(cov_lvl)){
    parameter_string <- paste0(parameter_string,"CVL=",paste(cov_lvl,collapse = ","),"&")
    ORD = append(ORD,"CVL")
  }

  #commodity category
  if(!is.null(comm_cat)){
    parameter_string <- paste0(parameter_string,"CC=",paste(comm_cat,collapse = ","),"&")
  }


  # paste ORD parameter together
  ORD <- paste0("ORD=",paste(ORD,collapse = ","))

  # add additional grouping parameters if specified
  if(!is.null(group_by)){

    group_by_codes <- c("CY" = "year",
                        "CM" = "crop",
                        "ST" = "state",
                        "DT" = "delivery_type",
                        "IP" = "insurance_plan",
                        "CT" = "county",
                        "CVL" = "cov_lvl")

    ORD <- paste0(ORD,",", paste0(names(group_by_codes[which(group_by_codes %in% group_by)]), collapse = ","))
  }

#   parameter_string <- paste0("CY=",ifelse(is.null(year),"",paste(year,collapse = ",")),           # crop year
#                              "&CM=",ifelse(is.null(crop),"",paste(crop,collapse=",")),            # commodity
#                              "&ST=",ifelse(is.null(state),"",paste(state,collapse = ",")),         # state
#                              "&DT=",ifelse(is.null(delivery_type),"",paste(delivery_type,collapse = ",")),  # delivery type
#                              "&IP=",ifelse(is.null(insurance_plan),"",paste(insurance_plan,collapse = ",")),# insurance plan
#                              "&CT=",ifelse(is.null(county),"",paste(county, collapse = ",")),       # county
#                              "&CVL=",ifelse(is.null(cov_lvl),"",paste(cov_lvl,collapse = ",")),     # coverage level
#                              "CC=",ifelse(is.null(comm_cat),"",toupper(comm_cat)))                    # commodity category
#
#
# #TODO: rearrange if else statements so that if NULL the parameter doesn't show up, also need to add the ORD parameter'
#   parameter_string <- paste0("CY=",ifelse(is.null(year),"",paste(year,collapse = ",")),           # crop year
#                             "&CM=",ifelse(is.null(crop),"",paste(crop,collapse=",")),            # commodity
#                             "&ST=",ifelse(is.null(state),"",paste(state,collapse = ",")),         # state
#                             "&DT=",ifelse(is.null(delivery_type),"",paste(delivery_type,collapse = ",")),  # delivery type
#                             "&IP=",ifelse(is.null(insurance_plan),"",paste(insurance_plan,collapse = ",")),# insurance plan
#                             "&CT=",ifelse(is.null(county),"",paste(county, collapse = ",")),       # county
#                             "&CVL=",ifelse(is.null(cov_lvl),"",paste(cov_lvl,collapse = ",")),     # coverage level
#                             "CC=",ifelse(is.null(comm_cat),"",toupper(comm_cat)))                    # commodity category

return(paste0(prefix,parameter_string,ORD,suffix))
}

valid_crop <- function(comm){

  crop_codes <- get_crop_codes()
  suppressWarnings({
  if( !(F %in% (as.numeric(comm) %in% as.numeric(crop_codes$commodity_code))) |
      !(F %in% (tolower(comm) %in% tolower(crop_codes$commodity_name))) ){
    valid = T
  } else {
    valid = F
  }
  return(valid)
  })
}

valid_state <- function(state){

  if(!(F %in% (tolower(state) %in% tolower(datasets::state.name)))) {
    return(state)
  } else if(!(F %in% (tolower(state) %in% tolower(datasets::state.abb)))){
    return(state)
  } else if(!(F %in% (suppressWarnings(as.numeric(state) %in% as.numeric(usmap::fips(state = datasets::state.name)))))){
    return(state)
  } else{
    stop("Parameter value for state not valid.")
  }
}

clean_fips <- Vectorize(function(fips = NULL,county = NULL,state = NULL){

  if(is.null(fips)){
    if(nchar(state) == 1){
      state_clean <- paste0("0",state)
    }
    if(nchar(state) == 2){
      state_clean <- as.character(state)
    }
    if(nchar(county) == 1){
      county_clean <- paste0("00",county)
    }
    if(nchar(county) == 2){
      county_clean <- paste0("0",county)
    }
    if(nchar(county) == 3){
      county_clean <- as.character(county)
    }
    fips <- paste0(state_clean,county_clean)
    return(fips)
  }
  if(is.null(fips) == F){
    if(nchar(fips) == 4){
      fips_clean <- paste0("0",fips)
    }
    if(nchar(fips) < 4){
      print("Warning: fips codes should be either 4 or 5 character strings. Returning NA")
      return(NA)
    }
    if(nchar(fips) == 5){
      fips_clean <- as.character(fips)
    }
    return(fips_clean)
  }
})


