#' Download RMA projected and harvest prices used for revenue protection insurance plans.
#'
#' @param year a numeric value (either single value or a vector of values) that indicate the crop year (ex: `2024` or `c(2022,2023,2024)`)

#' @param crop can be either a character string indicating a crop (i.e. `corn`) or a numeric value indicating the crop code (i.e. `41`). 
#' Inputting a vector with multiple values will return data for multiple crops. 
#' To look up crop codes, use the `get_crop_codes()` function.
#' @param state either a numeric value representing the state fips code or a character string that can either be the full state name or two-digit state abbreviation.
#' @return Returns a tibble containing data on project and harvest prices.
#' @export
#' 
#' @importFrom stringr str_pad
#' @importFrom tidyr pivot_wider
#' @import XML
#' @import cli
#'
#' @examples
#' \dontrun{get_price_data(2024,"corn")}
#' \dontrun{get_price_data(year = 2022:2024, state = "VA")}
#' @source Data is downloaded directly from RMA's price discovery app: \url{https://public-rma.fpac.usda.gov/apps/PriceDiscovery/GetPrices/ManyPrices} 
get_price_data <- function(year = NULL, crop = NULL, state = NULL){

  cli::cli_alert_info("Downloading data")
  
  # construct url
  url = "https://public-rma.fpac.usda.gov/apps/PriceDiscovery/Services/RevenuePriceDataService.svc/RevenuePrices?"

  # if year was specified, add the years to the url
  if (!is.null(year)) {
    url = paste0(include_and(url),"commodityYears=",paste(year,collapse =","))
  }
  
  # if crop was specified, add commodity codes to the url
  if (!is.null(crop)) {
    crop <- data.frame(get_crop_codes(crop = crop))[, "commodity_code"]
    url = paste0(include_and(url),"commodityCodes=",  paste(crop,collapse = ","))
  }

  # state
  if (!is.null(state)) {
    state = valid_state(state)
    if(is.character(state)){
      state <- usmap::fips(state = state)
    }
    if(nchar(state) == 1){
      state = stringr::str_pad(string = as.character(state),width = 2,pad = "0")
    }
    url = paste0(include_and(url),"stateCodes=",  paste(state,collapse = ","))
  }
  
  # parse the xml data
  xml_doc <- suppressWarnings(XML::xmlParse(readLines(url)))

  # extract the nodes
  nodes <- XML::getNodeSet(xml_doc, "//d:*")

  #create a data frame using the nodes
  df <- XML::xmlToDataFrame(nodes)
  names(df) <- "value"

  # extract the node names and add them as a column
  df$name <- sapply(nodes, XML::xmlName)

  # create a column that will uniquely id each row in the final df
  df$row = stats::ave(rep(1, length(df$name)), df$name, FUN = cumsum)

  # pivot the data frame
  df <- tidyr::pivot_wider(df,id_cols = "row",names_from = "name", values_from = "value")

  # remove unnecessary columns
  df <- df[,!grepl("RSS|Display|row|Composite|Actuarial",colnames(df))]

  # convert data types
  df <- utils::type.convert(df, as.is = T)

  # convert columns representing dates to POSIXct
  df[,grepl("BeginDate|EndDate",colnames(df))] <- lapply(df[,grepl("BeginDate|EndDate",colnames(df))], as.POSIXct)

  # return the data frame
  return(df)

}



