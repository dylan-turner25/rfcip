test_that("crop lookup caches the whole year and accepts names, codes and livestock", {
  cache <- local_sob_cache()
  app <- local_crop_app()
  corn <- get_crop_codes(2026, "corn")
  expect_equal(corn$commodity_code, 41L)
  expect_type(corn$commodity_code, "integer")
  expect_true(all(is.na(corn$commodity_abbreviation)))
  expect_true(all(is.na(corn$annual_planting_code)))
  expect_equal(get_crop_codes(2026, "0078")$commodity_code, 78L)
  expect_equal(get_crop_codes(2026, c("Rice", "0081"))$commodity_code, c(18L, 81L))
  expect_equal(get_crop_codes(2026, 801)$commodity_name, "Feeder Cattle")
  expect_length(app$urls, 1)
  expect_match(app$urls, "CC=B", fixed = TRUE)
  expect_equal(nrow(get_crop_codes(2026)), 5)
  expect_equal(get_cache_info()$function_type, "get_crop_codes")
  clear_rfcip_cache(function_name = "get_crop_codes")
  get_crop_codes(2026)
  expect_length(app$urls, 2)
  expect_false(memoise::is.memoised(get_crop_codes))
})

test_that("unmatched crops never broaden or partially drop filters", {
  local_sob_cache()
  local_crop_app()
  expect_error(get_crop_codes(2026, "Typo"), "not found.*Typo")
  expect_error(get_crop_codes(2026, c("Corn", "Typo")), "not found.*Typo")
  expect_error(get_crop_codes(2026, c(41, 9999)), "not found.*9999")
  for (crop in list(NA, character(), "", 1.5, -41, Inf)) {
    expect_error(get_crop_codes(2026, crop), "`crop`")
  }
})

test_that("lookup years are exact and old years require no ADM schema", {
  local_sob_cache()
  app <- local_crop_app()
  result <- get_crop_codes(c(2010, 2014, 2014, 2026), "corn")
  expect_equal(result$commodity_year, c(2010L, 2014L, 2026L))
  expect_length(app$urls, 3)
  expect_equal(nrow(get_crop_codes(2014)), 5)
  expect_error(get_crop_codes(NA_real_), "positive")
  expect_error(get_crop_codes(2025, force = NA), "TRUE or FALSE")
})

test_that("bulk fallback shares the SOB ZIP cache and saves a small lookup", {
  cache <- local_sob_cache()
  app <- local_crop_app()
  bulk <- local_sobcov_download()
  app$status <- 503L
  # First populate the actual bulk cache through the existing public SOB path.
  get_sob_data(2024, sob_version = "sobcov")
  expect_equal(bulk$years, 2024)
  expect_warning(result <- get_crop_codes(2024), class = "rfcip_source_fallback")
  expect_equal(result$commodity_code, c(41L, 81L))
  expect_length(app$urls, 4)
  expect_equal(bulk$years, 2024) # reused the ZIP, no second transfer
  expect_true(file.exists(file.path(cache, "crop_codes_year_2024.rds")))
  expect_equal(readRDS(file.path(cache, "crop_codes_year_2024.rds"))$source, "sobcov_cache")
  expect_equal(get_crop_codes(2024, 81)$commodity_name, "Soybeans")
  expect_length(app$urls, 4)
  clear_rfcip_cache(function_name = "get_crop_codes", years = 2024)
  expect_true(file.exists(file.path(cache, "sobcov_2024.zip")))
})

test_that("fresh bulk download is cached and fallback never invents missing crops", {
  cache <- local_sob_cache()
  app <- local_crop_app()
  bulk <- local_sobcov_download()
  app$status <- 503L
  expect_warning(result <- get_crop_codes(2024), class = "rfcip_source_fallback")
  expect_equal(bulk$years, 2024)
  expect_true(file.exists(file.path(cache, "sobcov_2024.zip")))
  expect_error(get_crop_codes(2024, "Feeder Cattle"), "not found.*Feeder Cattle")
  expect_error(get_crop_codes(2024, c("Corn", "Sunflowers")), "not found.*Sunflowers")
  expect_equal(nrow(result), 2)
})

test_that("forced refreshes preserve prior lookups on failure and retry on recovery", {
  cache <- local_sob_cache()
  app <- local_crop_app()
  bulk <- local_sobcov_download()
  original <- get_crop_codes(2024)
  app$suffix <- " updated"
  fresh <- get_crop_codes(2024, force = TRUE)
  expect_equal(get_crop_codes(2024, force = TRUE), fresh)
  expect_length(app$urls, 3)
  expect_false(identical(original, fresh))
  path <- file.path(cache, "crop_codes_year_2024.rds")
  before <- readBin(path, "raw", n = file.info(path)$size)
  # An old bulk ZIP must not replace the more complete prior lookup if refresh fails.
  get_sob_data(2024, sob_version = "sobcov")
  app$status <- bulk$status <- 503L
  expect_warning(fallback <- get_crop_codes(2024, force = TRUE), class = "rfcip_cache_fallback")
  expect_equal(fallback, fresh)
  expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  app$status <- 200L
  app$suffix <- " recovered"
  expect_match(get_crop_codes(2024, force = TRUE)$commodity_name[1], "recovered")
})

test_that("invalid workbooks and caches recover without poisoning usable data", {
  cache <- local_sob_cache()
  app <- local_crop_app()
  bulk <- local_sobcov_download()
  original <- get_crop_codes(2024)
  path <- file.path(cache, "crop_codes_year_2024.rds")
  app$bad <- TRUE
  bulk$bad_content <- TRUE
  expect_warning(result <- get_crop_codes(2024, force = TRUE), class = "rfcip_cache_fallback")
  expect_equal(result, original)
  writeLines("broken cache", path)
  expect_error(get_crop_codes(2024), "Crop-code retrieval failed")
  app$bad <- FALSE
  expect_equal(get_crop_codes(2024), original)
})

test_that("cache replacement failure returns fresh lookup while preserving old cache", {
  local_sob_cache()
  app <- local_crop_app()
  original <- get_crop_codes(2024)
  app$suffix <- " new"
  local_mocked_bindings(replace_sob_cache_file = function(...) stop("rename failed"))
  expect_warning(fresh <- get_crop_codes(2024, force = TRUE), class = "rfcip_cache_write_warning")
  expect_match(fresh$commodity_name[1], "new")
  expect_equal(get_crop_codes(2024), original)
})

test_that("application failure before 1989 reports unsupported bulk coverage", {
  local_sob_cache()
  app <- local_crop_app()
  app$status <- 503L
  expect_error(get_crop_codes(1988), "1989 onward")
})

test_that("crop lookup validation rejects malformed and conflicting identifiers", {
  x <- data.frame(commodity_year = 2024, commodity_code = 41, commodity_name = "Corn")
  expect_error(validate_crop_code_data(x, 2025), "different crop year")
  expect_error(validate_crop_code_data(x[FALSE, ], 2024), "nonempty")
  bad <- rbind(x, transform(x, commodity_name = "Something else"))
  expect_error(validate_crop_code_data(bad, 2024), "conflicting")
  expect_equal(nrow(validate_crop_code_data(rbind(x, x), 2024)), 1)
})

test_that("SOB URL resolution forwards years and refresh and retains all requested crops", {
  local_sob_cache()
  app <- local_crop_app()
  url <- get_sob_url(2014, crop = c("Corn", "Sunflowers"), force = TRUE)
  expect_match(url, "CM=0041,0078&", fixed = TRUE)
  expect_match(app$urls, "CY=2014", fixed = TRUE)
  get_sob_url(2014, crop = "Corn", force = TRUE)
  expect_length(app$urls, 2)
})
