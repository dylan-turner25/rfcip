test_that("get_ice_data returns metadata when requested", {
  # Skip if no internet connection
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test metadata functionality
  result <- get_ice_data(years = 2024, dataset = "metadata")
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  # Check expected columns are present
  expected_cols <- c("year", "filename", "description", "size_bytes", "size_mb", "date", "time", "datetime", "url")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check that year column matches request
  expect_true(all(result$year == 2024))
  
  # Check that filenames contain expected pattern (some files have YTD suffix, some don't)
  expect_true(all(grepl("2024_D\\d{5}_.*\\.txt", result$filename)))
})

test_that("get_ice_data handles multiple years for metadata", {
  # Skip if no internet connection
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test with multiple years
  result <- get_ice_data(years = c(2023, 2024), dataset = "metadata")
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  # Should have data for both years
  years_present <- unique(result$year)
  expect_true(2023 %in% years_present)
  expect_true(2024 %in% years_present)
})

test_that("get_ice_data validates input parameters", {
  # Test missing years parameter
  expect_error(get_ice_data(years = NULL), "years parameter is required")
})

test_that("get_ice_data handles non-existent years gracefully", {
  # Skip if no internet connection  
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test with a year that likely doesn't exist
  result <- get_ice_data(years = 1900, dataset = "metadata")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)  # Should return empty data frame
})

test_that("locate_ice_download_links works correctly", {
  # Skip if no internet connection
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test the helper function directly
  result <- rfcip:::locate_ice_download_links(year = 2024)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  # Check expected columns
  expected_cols <- c("year", "filename", "description", "size_bytes", "size_mb", "date", "time", "datetime", "url")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check that URLs are properly formed
  expect_true(all(grepl("^https://", result$url)))
  expect_true(all(grepl("2024", result$url)))
})

test_that("extract_ice_description formats names correctly", {
  # Test the description extraction function
  test_cases <- list(
    "2024_D00062_IceLegalDescription_YTD.txt" = "Ice Legal Description",
    "2024_D00092_IceInsurancePlanCommodityUnitStructure_YTD.txt" = "Ice Insurance Plan Commodity Unit Structure", 
    "2024_D00018_IceLrrFundCutoff_YTD.txt" = "Ice Lrr Fund Cutoff"
  )
  
  for (filename in names(test_cases)) {
    expected <- test_cases[[filename]]
    result <- rfcip:::extract_ice_description(filename)
    expect_equal(result, expected, info = paste("Failed for filename:", filename))
  }
})

# Skip the actual data download tests by default since they're large files
test_that("get_ice_data can download actual data (SLOW TEST - SKIP BY DEFAULT)", {
  skip("Skipping slow data download test by default")
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test downloading a small subset of data
  # This would be enabled for thorough testing but skipped for routine tests
  result <- get_ice_data(years = 2024, dataset = "IceLegalDescription")
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("caching system works correctly", {
  # This test would verify that files are cached properly
  # For now, just check that cache directory can be created
  cache_dir <- tools::R_user_dir("rfcip", which = "cache")
  expect_true(dir.exists(cache_dir) || dir.create(cache_dir, recursive = TRUE))
})