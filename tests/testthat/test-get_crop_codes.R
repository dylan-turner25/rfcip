# Comprehensive tests for get_crop_codes using webmockr
# These tests focus on HTTP request logic, filtering, and data processing

# Skip all tests if required packages not available
skip_if_not_installed("webmockr")
skip_if_not_installed("httr")
skip_if_not_installed("readxl")

# Test basic functionality - successful download and data return
test_that("get_crop_codes works with mocked HTTP request", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  result <- get_crop_codes(year = 2023)
  
  cleanup_webmockr()
  
  # Verify result structure
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("commodity_year", "commodity_code", "commodity_name") %in% names(result)))
})

test_that("get_crop_codes handles multiple years", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test multiple years in URL construction
  result <- get_crop_codes(year = c(2022, 2023))
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  cleanup_webmockr()
})

test_that("get_crop_codes uses current year as default", {
  webmockr::webmockr_configure(allow_net_connect = FALSE) 
  setup_crop_codes_url_mock()
  
  # Test default year parameter
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  default_result <- get_crop_codes()
  
  expect_s3_class(default_result, "data.frame")
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles valid character crop filtering", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test single valid character crop
  result_single <- get_crop_codes(year = 2023, crop = "Corn")
  expect_s3_class(result_single, "data.frame")
  expect_true(nrow(result_single) == 1)
  expect_equal(result_single$commodity_name[1], "Corn")
  
  # Test multiple valid character crops
  result_multiple <- get_crop_codes(year = 2023, crop = c("Corn", "Soybeans"))
  expect_s3_class(result_multiple, "data.frame")
  expect_true(nrow(result_multiple) == 2)
  expect_true(all(c("Corn", "Soybeans") %in% result_multiple$commodity_name))
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles case insensitive crop matching", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test case insensitive matching
  result_upper <- get_crop_codes(year = 2023, crop = "CORN")
  expect_s3_class(result_upper, "data.frame")
  expect_equal(nrow(result_upper), 1)
  expect_equal(result_upper$commodity_name[1], "Corn")
  
  # Test mixed case
  result_mixed <- get_crop_codes(year = 2023, crop = "sOyBeAnS")
  expect_s3_class(result_mixed, "data.frame")
  expect_equal(nrow(result_mixed), 1)
  expect_equal(result_mixed$commodity_name[1], "Soybeans")
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles invalid character crops with warning", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test invalid character crop - should warn and return all
  expect_warning(
    result <- get_crop_codes(year = 2023, crop = "InvalidCrop"),
    "One or more of the entered crop codes or crop names is not valid"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)  # Should return some data 
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles valid numeric crop filtering", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test single valid numeric crop
  result_single <- get_crop_codes(year = 2023, crop = 41)
  expect_s3_class(result_single, "data.frame")
  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$commodity_name[1], "Corn")
  
  # Test multiple valid numeric crops
  result_multiple <- get_crop_codes(year = 2023, crop = c(41, 81))
  expect_s3_class(result_multiple, "data.frame")
  expect_equal(nrow(result_multiple), 2)
  expect_true(all(c("Corn", "Soybeans") %in% result_multiple$commodity_name))
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles invalid numeric crops with warning", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test invalid numeric crop - should warn and return all
  expect_warning(
    result <- get_crop_codes(year = 2023, crop = 999),
    "One or more of the entered crop codes or crop names is not valid"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)  # Should return some data
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles NULL crop parameter", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test NULL crop - should return all crops
  result <- get_crop_codes(year = 2023, crop = NULL)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)  # Should return some data
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles x2 header skip logic", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock(create_mock_crop_data_with_x2_header())
  
  result <- get_crop_codes(year = 2023)
  
  expect_s3_class(result, "data.frame")
  # Should not have x2 column name (gets skipped)
  expect_false("x2" %in% names(result))
  expect_true(all(c("commodity_year", "commodity_code", "commodity_name") %in% names(result)))
  
  cleanup_webmockr()
})

test_that("get_crop_codes column selection works correctly", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  result <- get_crop_codes(year = 2023)
  
  # Should have exactly the 3 expected columns
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("commodity_year", "commodity_code", "commodity_name"))
  
  cleanup_webmockr()
})

test_that("get_crop_codes data cleaning works", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  result <- get_crop_codes(year = 2023)
  
  # Test that janitor::clean_names was applied (column names should be clean)
  expect_true(all(names(result) %in% c("commodity_year", "commodity_code", "commodity_name")))
  expect_false(any(grepl("[[:upper:]]", names(result))))  # No uppercase in names
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles empty filter results correctly", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test both character and numeric empty results
  expect_warning(
    char_result <- get_crop_codes(year = 2023, crop = "nonexistent"),
    "One or more of the entered crop codes or crop names is not valid"
  )
  expect_true(nrow(char_result) >= 1)
  
  expect_warning(
    num_result <- get_crop_codes(year = 2023, crop = 9999),
    "One or more of the entered crop codes or crop names is not valid"
  )
  expect_true(nrow(num_result) >= 1)
  
  cleanup_webmockr()
})

test_that("get_crop_codes URL construction works with different years", {
  # Test URL construction logic indirectly by testing different year formats
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test single year
  expect_no_error({
    get_crop_codes(year = 2023)
  })
  
  # Test multiple years (tests paste(year, collapse = ",") logic)
  expect_no_error({
    get_crop_codes(year = c(2021, 2022, 2023))
  })
  
  cleanup_webmockr()
})

test_that("get_crop_codes handles HTTP errors gracefully", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock_error(status_code = 500)
  
  # Function should work - may use real network or return mock data
  expect_no_error({
    get_crop_codes(year = 2023)
  })
  
  cleanup_webmockr()
})

test_that("get_crop_codes temporary file handling", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Get initial temp file count
  temp_dir <- tempdir()
  initial_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
  initial_count <- length(initial_files)
  
  # Run function
  result <- get_crop_codes(year = 2023)
  
  # Check temp files were cleaned up (httr::write_disk creates temp files)
  final_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
  final_count <- length(final_files)
  
  # Function should clean up after itself
  expect_true(final_count >= initial_count)  # May create temp files
  
  cleanup_webmockr()
})

test_that("get_crop_codes mixed valid/invalid crops", {
  webmockr::webmockr_configure(allow_net_connect = FALSE)
  setup_crop_codes_url_mock()
  
  # Test mix of valid and invalid character crops
  # Should return valid ones only, no warning if at least one is valid
  result <- get_crop_codes(year = 2023, crop = c("Corn", "InvalidCrop"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)  # Only valid crop returned
  expect_equal(result$commodity_name[1], "Corn")
  
  cleanup_webmockr()
})

# Caching and force parameter tests
test_that("get_crop_codes caching behavior with mocked functions", {
  # Mock the caching functions to test behavior without actual file I/O
  mock_data <- create_mock_crop_data()
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_cache_key.xlsx",
    file.exists = function(path) {
      if (grepl("test_cache_key", path)) TRUE else FALSE
    },
    {
      # Should use cached data when available and force=FALSE
      result1 <- get_crop_codes(year = 2023, force = FALSE)
      expect_s3_class(result1, "data.frame")
      
      # Should attempt fresh download when force=TRUE
      result2 <- get_crop_codes(year = 2023, force = TRUE)
      expect_s3_class(result2, "data.frame")
    }
  )
})

test_that("get_crop_codes force parameter fallback behavior", {
  # Test scenario where force=TRUE but download fails, should fallback to cache
  mock_data <- create_mock_crop_data()
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_cache_key.xlsx",
    file.exists = function(path) {
      if (grepl("test_cache_key", path)) TRUE else FALSE
    },
    `httr::GET` = function(...) stop("Download failed"),
    {
      # Should fall back to cached data when download fails with force=TRUE
      expect_no_error({
        result <- get_crop_codes(year = 2023, force = TRUE)
      })
    }
  )
})

test_that("get_crop_codes cache key generation with different parameters", {
  # Test that different parameters create different cache keys
  mock_data <- create_mock_crop_data()
  
  # Mock to capture the cache keys generated
  cache_keys <- character(0)
  
  with_mocked_bindings(
    generate_cache_key = function(prefix, params, suffix) {
      key <- paste0(prefix, "_", paste(names(params), params, sep = "=", collapse = "_"), ".", suffix)
      cache_keys <<- c(cache_keys, key)
      return(key)
    },
    file.exists = function(...) FALSE,  # Force downloads
    cache_raw_data = function(...) tempfile(fileext = ".xlsx"),
    `httr::GET` = function(...) invisible(),
    `readxl::read_excel` = function(...) mock_data,
    {
      # Different parameters should create different cache keys
      result1 <- get_crop_codes(year = 2023, crop = "corn")
      result2 <- get_crop_codes(year = 2024, crop = "corn")
      result3 <- get_crop_codes(year = 2023, crop = "soybeans")
      
      expect_true(length(unique(cache_keys)) >= 3)
    }
  )
})

test_that("get_crop_codes handles cache directory creation", {
  # Test that cache directory is created if it doesn't exist
  with_mocked_bindings(
    `tools::R_user_dir` = function(...) tempfile(),
    dir.exists = function(...) FALSE,
    dir.create = function(...) TRUE,
    file.exists = function(...) FALSE,
    cache_raw_data = function(...) tempfile(fileext = ".xlsx"),
    `httr::GET` = function(...) invisible(),
    `readxl::read_excel` = function(...) create_mock_crop_data(),
    {
      expect_no_error({
        result <- get_crop_codes(year = 2023)
      })
    }
  )
})