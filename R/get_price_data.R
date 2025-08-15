#' Download RMA projected and harvest prices used for revenue protection insurance plans.
#'
#' @param year a numeric value (either single value or a vector of values) that indicate the crop year (ex: `2024` or `c(2022,2023,2024)`)

#' @param crop can be either a character string indicating a crop (i.e. `corn`) or a numeric value indicating the crop code (i.e. `41`). 
#' Inputting a vector with multiple values will return data for multiple crops. 
#' To look up crop codes, use the `get_crop_codes()` function.
#' @param state either a numeric value representing the state fips code or a character string that can either be the full state name or two-digit state abbreviation.
#' @param force logical (default FALSE). If TRUE, attempts to download fresh data regardless of cache, but falls back to cached data on failure with a warning
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
get_price_data <- function(year = NULL, crop = NULL, state = NULL, force = FALSE){

  # Generate cache key based on parameters
  cache_params <- list(
    year = year,
    crop = crop,
    state = state
  )
  
  cache_key <- generate_cache_key("price", cache_params, "xml")
  dest_dir <- tools::R_user_dir("rfcip", which = "cache")
  cache_file <- file.path(dest_dir, cache_key)
  
  # Check if XML data is already cached and force=FALSE
  if (file.exists(cache_file) && !force) {
    cli::cli_alert_info("Loading data from cache")
    xml_lines <- load_cached_data(cache_file)
    xml_doc <- suppressWarnings(XML::xmlParse(xml_lines))
  } else {
    # Download and cache new XML data
    cli::cli_alert_info("Downloading and caching data")
    
    success <- FALSE
    
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
    
    # Try to download new data
    tryCatch({
      # Read XML data
      xml_lines <- readLines(url)
      
      # Cache the XML response
      cache_raw_data(xml_lines, cache_key, "xml")
      
      # Parse the XML data
      xml_doc <- suppressWarnings(XML::xmlParse(xml_lines))
      success <- TRUE
    }, error = function(e) {
      if (force && file.exists(cache_file)) {
        cli::cli_alert_warning("Download failed, using cached data")
        xml_lines <- load_cached_data(cache_file)
        xml_doc <<- suppressWarnings(XML::xmlParse(xml_lines))
        success <<- TRUE
      } else {
        stop(e)
      }
    })
  }

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



