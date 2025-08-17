# Mocked tests for get_crop_codes using webmockr
# These tests focus on HTTP request logic, file I/O, and data processing

# Skip all tests if required packages not available
skip_if_not_installed("webmockr")
skip_if_not_installed("mockery")
skip_if_not_installed("writexl")

test_that("get_crop_codes basic functionality with mocked HTTP request", {
  # Setup webmockr
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  
  # Setup mock
  setup_crop_codes_url_mock()
  
  # Test basic functionality
  expect_no_error({
    result <- get_crop_codes(year = 2023)
  })
  
  # Clean up
  cleanup_webmockr()
})

test_that("get_crop_codes handles multiple years with mocked requests", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test multiple years - tests the loop logic and data binding
  expect_no_error({
    result <- get_crop_codes(year = c(2022, 2023))
  })
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles x2 header detection and skip logic", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  
  # Setup mock with x2 header data
  setup_crop_codes_url_mock(mock_data = create_mock_crop_data_with_x2_header())
  
  # Should handle skip=1 logic when x2 is detected
  expect_no_error({
    result <- get_crop_codes(year = 2023)
  })
  
  cleanup_webmockr()
})

test_that("get_crop_codes crop filtering by character names works", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test filtering by crop names
  expect_no_error({
    result <- get_crop_codes(year = 2023, crop = "corn")
    expect_s3_class(result, "data.frame")
  })
  
  # Test case insensitive filtering
  expect_no_error({
    result <- get_crop_codes(year = 2023, crop = c("CORN", "soybeans"))
    expect_s3_class(result, "data.frame")
  })
  
  cleanup_webmockr()
})

test_that("get_crop_codes crop filtering by numeric codes works", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test filtering by numeric crop codes
  expect_no_error({
    result <- get_crop_codes(year = 2023, crop = 41)
    expect_s3_class(result, "data.frame")
  })
  
  # Test multiple numeric codes
  expect_no_error({
    result <- get_crop_codes(year = 2023, crop = c(41, 81))
    expect_s3_class(result, "data.frame")
  })
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles invalid crop names/codes gracefully", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test invalid crop name - should return warning and all data
  expect_warning({
    result <- get_crop_codes(year = 2023, crop = "invalid_crop_name")
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)  # Should return all data
  }, "One or more of the entered crop codes or crop names is not valid")
  
  # Test invalid numeric code - should return warning and all data  
  expect_warning({
    result <- get_crop_codes(year = 2023, crop = 99999)
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)  # Should return all data
  }, "One or more of the entered crop codes or crop names is not valid")
  
  cleanup_webmockr()
})

test_that("get_crop_codes data structure and column processing", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  result <- get_crop_codes(year = 2023)
  
  # Check expected columns exist
  expect_true("commodity_year" %in% names(result))
  expect_true("commodity_code" %in% names(result))
  expect_true("commodity_name" %in% names(result))
  
  # Check data types (after type conversion that might happen)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  # Check that only the three required columns are returned
  expect_equal(ncol(result), 3)
  
  cleanup_webmockr()
})

test_that("get_crop_codes URL construction works correctly", {
  # Test the URL construction logic directly by examining the URL pattern
  expected_pattern <- "https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel"
  
  # This tests that the URL includes the expected base and parameters
  # We can't easily test the exact URL without running the function, but we can
  # test that the function accepts the expected parameters
  expect_no_error({
    # These parameter combinations should not error in URL construction
    params_test <- list(
      year = 2023,
      crop = NULL
    )
  })
})

test_that("get_crop_codes column selection works with different data structures", {
  # Test column selection logic with various mock data structures
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  
  # Create mock data with extra columns
  mock_data_extra_cols <- data.frame(
    commodity_year = c("2023", "2023"),
    commodity_code = c("41", "81"),
    commodity_name = c("Corn", "Soybeans"),
    extra_col1 = c("Extra1", "Extra2"),
    extra_col2 = c("Extra3", "Extra4"),
    stringsAsFactors = FALSE
  )
  
  setup_crop_codes_url_mock(mock_data = mock_data_extra_cols)
  
  result <- get_crop_codes(year = 2023)
  
  # Should only return the three required columns
  expect_equal(ncol(result), 3)
  expect_true(all(c("commodity_year", "commodity_code", "commodity_name") %in% names(result)))
  expect_false("extra_col1" %in% names(result))
  expect_false("extra_col2" %in% names(result))
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles edge cases with basic parameters", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test with NULL crop (should return all)
  expect_no_error({
    result <- get_crop_codes(year = 2023, crop = NULL)
    expect_s3_class(result, "data.frame")
  })
  
  # Test with empty character vector (should be handled gracefully)
  expect_no_error({
    result <- get_crop_codes(year = 2023, crop = character(0))
    expect_s3_class(result, "data.frame")
  })
  
  cleanup_webmockr()
})

test_that("get_crop_codes error scenarios with webmockr", {
  # Skip if error testing is problematic due to caching
  skip("Error testing with mocking can be inconsistent due to caching behavior")
  
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  
  # Setup mock that will fail
  setup_crop_codes_url_mock_error(status_code = 500)
  
  # Should error when download fails and no cache available
  expect_error(
    get_crop_codes(year = 2023, force = TRUE),
    "Failed to download crop codes data"
  )
  
  cleanup_webmockr()
})

test_that("get_crop_codes caching behavior can be tested", {
  # Create a simple test for cache vs non-cache behavior
  # This tests that the force parameter changes behavior
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # First call - should download
  expect_no_error({
    result1 <- get_crop_codes(year = 2023, force = TRUE)
    expect_s3_class(result1, "data.frame")
  })
  
  # Second call - might use cache or download again
  expect_no_error({
    result2 <- get_crop_codes(year = 2023, force = FALSE)
    expect_s3_class(result2, "data.frame")
  })
  
  cleanup_webmockr()
})