# year = 2024
# crop = c("corn")
# library(XML)
# library(tidyr)

get_price_data <- function(year, crop = NULL){

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

  # parse the xml data
  xml_doc <- suppressWarnings(XML::xmlParse(readLines(url)))

  # extract the nodes
  nodes <- getNodeSet(xml_doc, "//d:*")

  #create a data frame using the nodes
  df <- xmlToDataFrame(nodes)
  names(df) <- "value"

  # extract the node names and add them as a column
  df$name <- sapply(nodes, xmlName)

  # create a column that will uniquely id each row in the final df
  df$row = ave(rep(1, length(df$name)), df$name, FUN = cumsum)

  # pivot the data frame
  df <- tidyr::pivot_wider(df,id_cols = "row",names_from = "name", values_from = "value")

  # remove unnecessary columns
  df <- df[,!grepl("RSS|Display|row|Composite|Actuarial",colnames(df))]

  # convert data types
  df <- type.convert(df, as.is = T)

  # convert columns representing dates to POSIXct
  df[,grepl("BeginDate|EndDate",colnames(df))] <- lapply(df[,grepl("BeginDate|EndDate",colnames(df))], as.POSIXct)

  # return the data frame
  return(df)

}

