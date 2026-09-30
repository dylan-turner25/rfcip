test_that("price URLs pad and deduplicate crops and forward years and refresh", {
  local_sob_cache()
  state <- local_price_download()
  result <- get_price_data(2025, 41)
  expect_match(state$urls, "commodityYears=2025&commodityCodes=0041", fixed = TRUE)
  expect_equal(result$ProjectedPrice, 4.7)
  expect_s3_class(result$ProjectedBeginDate, "POSIXct")
  get_price_data(c(2024, 2025), c("corn", "soybeans"), state = c(1, 17), force = TRUE)
  expect_match(state$urls[2], "commodityCodes=0041,0081&stateCodes=01,17", fixed = TRUE)
  expect_equal(state$lookup[[2]]$year, c(2024, 2025))
  expect_true(state$lookup[[2]]$force)
  expect_false(memoise::is.memoised(get_price_data))
})

test_that("price disk caches reuse, clear and refresh consistently", {
  local_sob_cache()
  state <- local_price_download()
  first <- get_price_data(2025, 41)
  expect_equal(get_price_data(2025, 41), first)
  expect_length(state$urls, 1)
  state$xml <- price_fixture(5)
  expect_equal(get_price_data(2025, 41, force = TRUE)$ProjectedPrice, 5)
  state$xml <- price_fixture(6)
  expect_equal(get_price_data(2025, 41, force = TRUE)$ProjectedPrice, 6)
  expect_length(state$urls, 3)
  clear_rfcip_cache(function_name = "get_price_data")
  get_price_data(2025, 41)
  expect_length(state$urls, 4)
})

test_that("cached empty feeds recover and fresh empty feeds do not become caches", {
  cache <- local_sob_cache()
  state <- local_price_download()
  path <- file.path(cache, "price_year_2025_crop_41.xml")
  dir.create(cache, recursive = TRUE)
  writeLines('<feed xmlns:d="http://schemas.microsoft.com/ado/2007/08/dataservices"/>', path)
  expect_equal(get_price_data(2025, 41)$ProjectedPrice, 4.7)
  expect_length(state$urls, 1)
  clear_rfcip_cache(function_name = "get_price_data")
  state$xml <- '<feed xmlns:d="http://schemas.microsoft.com/ado/2007/08/dataservices"/>'
  expect_error(get_price_data(2025, 41), "No price data returned")
  expect_false(file.exists(path))
})

test_that("failed or unusable price refreshes preserve and warn about valid cache", {
  cache <- local_sob_cache()
  state <- local_price_download()
  original <- get_price_data(2025, 41)
  path <- file.path(cache, "price_year_2025_crop_41.xml")
  before <- readBin(path, "raw", n = file.info(path)$size)
  for (xml in c('<feed/>', '<feed xmlns:d="http://schemas.microsoft.com/ado/2007/08/dataservices"><d:Error>bad</d:Error></feed>')) {
    state$xml <- xml
    expect_warning(result <- get_price_data(2025, 41, force = TRUE), class = "rfcip_cache_fallback")
    expect_equal(result, original)
    expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  }
  state$fail <- TRUE
  expect_warning(get_price_data(2025, 41, force = TRUE), class = "rfcip_cache_fallback")
  clear_rfcip_cache(function_name = "get_price_data")
  expect_error(get_price_data(2025, 41), "Connection failed")
})

test_that("price cache write failures keep old cache and return fresh data", {
  local_sob_cache()
  state <- local_price_download()
  get_price_data(2025, 41)
  state$xml <- price_fixture(7)
  local_mocked_bindings(replace_sob_cache_file = function(...) stop("rename failed"))
  expect_warning(fresh <- get_price_data(2025, 41, force = TRUE), class = "rfcip_cache_write_warning")
  expect_equal(fresh$ProjectedPrice, 7)
  expect_equal(get_price_data(2025, 41)$ProjectedPrice, 4.7)
})

test_that("a missing final newline is tolerated without suppressing read errors", {
  path <- tempfile(fileext = ".xml")
  on.exit(unlink(path))
  writeChar(price_fixture(), path, eos = NULL)
  expect_warning(xml <- download_price_xml(path), NA)
  expect_equal(parse_price_data(xml)$ProjectedPrice, 4.7)
})
