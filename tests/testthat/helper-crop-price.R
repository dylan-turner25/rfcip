local_crop_app <- function(.env = parent.frame()) {
  skip_if_not_installed("writexl")
  state <- new.env(parent = emptyenv())
  state$urls <- character()
  state$status <- 200L
  state$bad <- FALSE
  state$suffix <- ""
  state$codes <- c("0018", "0041", "0078", "0081", "0801")
  state$names <- c("Rice", "Corn", "Sunflowers", "Soybeans", "Feeder Cattle")
  request <- function(url, path, timeout) {
    state$urls <- c(state$urls, url)
    year <- as.numeric(httr::parse_url(url)$query$CY)
    if (state$status != 200L) {
      writeLines("Unavailable", path)
    } else {
      data <- if (state$bad) data.frame(error = "Unavailable") else data.frame(
        CommodityYear = year, CommodityCode = state$codes,
        CommodityName = paste0(state$names, state$suffix))
      writexl::write_xlsx(data, path)
    }
    structure(list(status_code = state$status, headers = list()), class = "response")
  }
  testthat::local_mocked_bindings(
    sob_http_download = function(url, year, path, timeout = sob_timeout(), ...) {
      rfcip_http_download(url, year, path, timeout, request = request,
                         sleep = function(...) NULL, jitter = function() 0)
    },
    get_adm_data = function(...) stop("Crop lookup must not depend on ADM"),
    .package = "rfcip", .env = .env)
  state
}

price_fixture <- function(value = 4.7) {
  paste0('<feed xmlns="http://www.w3.org/2005/Atom" ',
         'xmlns:d="http://schemas.microsoft.com/ado/2007/08/dataservices" ',
         'xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">',
         '<entry><content><m:properties><d:CommodityYear>2025</d:CommodityYear>',
         '<d:CommodityCode>0041</d:CommodityCode><d:CommodityName>Corn</d:CommodityName>',
         '<d:ProjectedPrice>', value, '</d:ProjectedPrice>',
         '<d:ProjectedBeginDate>2025-02-01T00:00:00</d:ProjectedBeginDate>',
         '</m:properties></content></entry></feed>')
}

local_price_download <- function(.env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$urls <- character()
  state$lookup <- list()
  state$xml <- price_fixture()
  state$fail <- FALSE
  testthat::local_mocked_bindings(
    get_crop_codes = function(year, crop, force) {
      state$lookup[[length(state$lookup) + 1L]] <- list(year = year, crop = crop, force = force)
      data.frame(commodity_code = if (length(crop) > 1L) c(41L, 81L, 41L) else 41L)
    },
    download_price_xml = function(url) {
      state$urls <- c(state$urls, url)
      if (state$fail) stop("Connection failed")
      state$xml
    }, .package = "rfcip", .env = .env)
  state
}
