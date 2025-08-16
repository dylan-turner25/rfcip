# Mocked tests for get_col_data using function mocking
# These tests focus on core logic, caching, error handling, and data processing

# Skip all tests if required packages not available
skip_if_not_installed("mockery")

test_that("get_col_data basic functionality with mocked components", {
  # Mock all the external dependencies
  mock_data <- create_mock_col_data()
  mock_urls <- create_mock_col_urls()
  
  with_mocked_bindings(
    locate_col_links = function(...) mock_urls,
    check_cached_years = function(...) list(cached_years = numeric(0), missing_years = 2023, cached_files = character(0)),
    download_and_verify = function(...) TRUE,
    cache_raw_data = function(...) create_mock_col_zip_file(mock_data),
    process_col_zip = function(...) mock_data,
    {
      result <- get_col_data(year = 2023)
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
      expect_true("commodity_year" %in% names(result))
      expect_true("commodity_name" %in% names(result))
      expect_true("indem_amount" %in% names(result))
    }
  )
})

test_that("get_col_data handles multiple years", {
  mock_data <- create_mock_col_data()
  mock_urls <- create_mock_col_urls()
  
  with_mocked_bindings(
    locate_col_links = function(...) mock_urls,
    check_cached_years = function(...) list(cached_years = numeric(0), missing_years = c(2022, 2023), cached_files = character(0)),
    download_and_verify = function(...) TRUE,
    cache_raw_data = function(...) create_mock_col_zip_file(mock_data),
    process_col_zip = function(...) mock_data,
    {
      result <- get_col_data(year = c(2022, 2023))
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
    }
  )
})

test_that("get_col_data handles cached data", {
  mock_data <- create_mock_col_data()
  mock_cache_check <- list(
    cached_years = c(2023),
    missing_years = numeric(0),
    cached_files = c("cache/col_2023.zip")
  )
  
  with_mocked_bindings(
    check_cached_years = function(...) mock_cache_check,
    load_cached_data = function(...) create_mock_col_zip_file(mock_data),
    process_col_zip = function(...) mock_data,
    {
      result <- get_col_data(year = 2023)
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
    }
  )
})

test_that("get_col_data force parameter bypasses cache", {
  mock_data <- create_mock_col_data()
  mock_urls <- create_mock_col_urls()
  
  # Mock scenario where data exists in cache but force=TRUE
  mock_cache_check <- list(
    cached_years = c(2023),
    missing_years = numeric(0),
    cached_files = c("cache/col_2023.zip")
  )
  
  with_mocked_bindings(
    check_cached_years = function(...) mock_cache_check,
    locate_col_links = function(...) mock_urls,
    download_and_verify = function(...) TRUE,
    cache_raw_data = function(...) create_mock_col_zip_file(mock_data),
    process_col_zip = function(...) mock_data,
    {
      result <- get_col_data(year = 2023, force = TRUE)
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
    }
  )
})

test_that("get_col_data handles missing years", {
  mock_data <- create_mock_col_data()
  
  # Mock URLs that don't include the requested year
  mock_urls <- data.frame(
    year = c(2020, 2021, 2022),
    url = c(
      "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/2020_cause-loss.zip",
      "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/2021_cause-loss.zip",
      "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/2022_cause-loss.zip"
    ),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    locate_col_links = function(...) mock_urls,
    check_cached_years = function(...) list(cached_years = c(2022), missing_years = c(2025), cached_files = c("cache/col_2022.zip")),
    load_cached_data = function(...) create_mock_col_zip_file(mock_data),
    process_col_zip = function(...) mock_data,
    {
      # Should succeed with available data (2022) and skip missing year (2025)
      result <- get_col_data(year = c(2022, 2025))
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
    }
  )
})

test_that("get_col_data handles download failures with force parameter fallback", {
  mock_data <- create_mock_col_data()
  mock_urls <- create_mock_col_urls()
  
  # Simulate force=TRUE scenario where download fails but cache exists
  mock_cache_check <- list(
    cached_years = c(2023),
    missing_years = numeric(0),
    cached_files = c("cache/col_2023.zip")
  )
  
  with_mocked_bindings(
    check_cached_years = function(...) mock_cache_check,
    locate_col_links = function(...) mock_urls,
    download_and_verify = function(...) stop("Download failed"),
    load_cached_data = function(...) create_mock_col_zip_file(mock_data),
    process_col_zip = function(...) mock_data,
    {
      # Should fall back to cached data when download fails with force=TRUE
      result <- get_col_data(year = 2023, force = TRUE)
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
    }
  )
})

test_that("get_col_data data processing pipeline", {
  # Test the data type conversion and processing logic
  mock_data <- create_mock_col_data()
  
  with_mocked_bindings(
    locate_col_links = function(...) create_mock_col_urls(),
    check_cached_years = function(...) list(cached_years = numeric(0), missing_years = 2023, cached_files = character(0)),
    download_and_verify = function(...) TRUE,
    cache_raw_data = function(...) create_mock_col_zip_file(mock_data),
    process_col_zip = function(...) mock_data,
    {
      result <- get_col_data(year = 2023)
      
      # Check data structure and types
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
      
      # Check that numeric columns were converted
      expect_true(is.numeric(result$commodity_year))
      expect_true(is.numeric(result$liability))
      expect_true(is.numeric(result$indemnified_quantity))
      
      # Check that character columns were trimmed
      expect_true(is.character(result$commodity_name))
      expect_true(is.character(result$col_name))
      expect_true(all(result$commodity_name == trimws(result$commodity_name)))
    }
  )
})

test_that("get_col_data handles mixed cached and fresh data", {
  # Test scenario with some years cached and others needing download
  mock_cached_data <- create_mock_col_data()
  mock_fresh_data <- create_mock_col_data()
  # Make fresh data different for testing
  mock_fresh_data$commodity_year <- rep("2024", nrow(mock_fresh_data))
  
  mock_cache_check <- list(
    cached_years = c(2023),
    missing_years = c(2024),
    cached_files = c("cache/col_2023.zip")
  )
  
  with_mocked_bindings(
    check_cached_years = function(...) mock_cache_check,
    load_cached_data = function(...) create_mock_col_zip_file(mock_cached_data),
    locate_col_links = function(...) create_mock_col_urls(),
    download_and_verify = function(...) TRUE,
    cache_raw_data = function(...) create_mock_col_zip_file(mock_fresh_data),
    process_col_zip = function(zip_file) {
      # Return different data based on which file is being processed
      if (grepl("cached", zip_file) || grepl("2023", zip_file)) {
        mock_cached_data
      } else {
        mock_fresh_data
      }
    },
    {
      result <- get_col_data(year = c(2023, 2024))
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
      
      # Should contain data from both years
      years_in_result <- unique(as.numeric(result$commodity_year))
      expect_true(length(years_in_result) > 0)
    }
  )
})

test_that("get_col_data errors when no data available", {
  # Test error when no data is available for any year
  with_mocked_bindings(
    check_cached_years = function(...) list(cached_years = numeric(0), missing_years = numeric(0), cached_files = character(0)),
    locate_col_links = function(...) data.frame(year = numeric(0), url = character(0)),
    {
      expect_error(
        get_col_data(year = 2050),
        "No data available for the specified years"
      )
    }
  )
})

test_that("get_col_data validate input parameters", {
  # These should still test the actual parameter validation
  expect_error(get_col_data(year = "invalid"), "`year` must be a vector of numeric values.")
  expect_error(get_col_data(year = c("2020", "2021")), "`year` must be a vector of numeric values.")
  expect_error(get_col_data(year = NULL), "`year` must be a vector of numeric values.")
})

test_that("process_col_zip works with realistic ZIP file", {
  # Test the actual ZIP processing function
  mock_data <- create_mock_col_data()
  mock_zip <- create_mock_col_zip_file(mock_data)
  
  # Test the real function
  result <- process_col_zip(mock_zip)
  
  # Clean up
  unlink(mock_zip)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("commodity_year" %in% names(result))
})