get_price_url <- function(year, crop, state, force = FALSE) {
  url <- "https://public-rma.fpac.usda.gov/apps/PriceDiscovery/Services/RevenuePriceDataService.svc/RevenuePrices?"
  if (!is.null(year)) url <- paste0(url, "commodityYears=", paste(unique(year), collapse = ","))
  if (!is.null(crop)) {
    lookup_year <- if (is.null(year)) as.numeric(format(Sys.Date(), "%Y")) else year
    codes <- get_crop_codes(year = lookup_year, crop = crop, force = force)$commodity_code
    codes <- unique(sprintf("%04d", as.numeric(as.character(codes))))
    url <- paste0(include_and(url), "commodityCodes=", paste(codes, collapse = ","))
  }
  if (!is.null(state)) {
    state <- valid_state(state)
    if (is.character(state)) state <- usmap::fips(state = state)
    codes <- sprintf("%02d", as.integer(state))
    url <- paste0(include_and(url), "stateCodes=", paste(unique(codes), collapse = ","))
  }
  url
}

download_price_xml <- function(url) readLines(url, warn = FALSE)

parse_price_data <- function(lines) {
  if (!length(lines)) stop("No price data returned for the requested filters.", call. = FALSE)
  doc <- XML::xmlParse(lines)
  on.exit(XML::free(doc), add = TRUE)
  nodes <- XML::getNodeSet(doc, "//d:*",
                          namespaces = c(d = "http://schemas.microsoft.com/ado/2007/08/dataservices"))
  if (!length(nodes)) {
    stop("No price data returned for the requested filters (response contains no price records).",
         call. = FALSE)
  }
  df <- XML::xmlToDataFrame(nodes)
  if (ncol(df) != 1L) stop("Invalid price response: expected scalar price fields.")
  names(df) <- "value"
  df$name <- vapply(nodes, XML::xmlName, character(1))
  df$row <- stats::ave(rep(1, nrow(df)), df$name, FUN = cumsum)
  df <- tidyr::pivot_wider(df, id_cols = "row", names_from = "name", values_from = "value")
  df <- df[, !grepl("RSS|Display|row|Composite|Actuarial", names(df))]
  if (!all(c("CommodityYear", "CommodityCode", "CommodityName") %in% names(df))) {
    stop("Invalid price response: missing commodity fields.")
  }
  df <- utils::type.convert(df, as.is = TRUE)
  if (!nrow(df) || anyNA(df[c("CommodityYear", "CommodityCode", "CommodityName")])) {
    stop("Invalid price response: missing commodity identifiers.")
  }
  dates <- grepl("BeginDate|EndDate", names(df))
  df[, dates] <- lapply(df[, dates], as.POSIXct)
  df
}

save_price_xml <- function(lines, target) {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  staged <- tempfile("price-staged-", tmpdir = dirname(target))
  on.exit(unlink(staged), add = TRUE)
  writeLines(lines, staged)
  replace_sob_cache_file(staged, target)
}
