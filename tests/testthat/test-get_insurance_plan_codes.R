# Tests for get_insurance_plan_codes function
# These tests cover basic functionality, caching behavior, and parameter validation

skip_if_not_installed("mockery")

test_that("get_insurance_plan_codes basic functionality", {
  # Mock data
  mock_data <- data.frame(
    commodity_year = c("2023", "2023", "2023"),
    insurance_plan_code = c("1", "2", "3"),
    insurance_plan = c("Yield Protection", "Revenue Protection", "Revenue Protection with Harvest Price Exclusion"),
    insurance_plan_abbrv = c("YP", "RP", "RP-HPE"),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_insurance_key.xlsx",
    file.exists = function(...) FALSE,  # Force download
    cache_raw_data = function(...) tempfile(fileext = ".xlsx"),
    download.file = function(...) invisible(),
    `readxl::read_excel` = function(...) mock_data,
    {
      result <- get_insurance_plan_codes(year = 2023)
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
      expect_true(all(c("commodity_year", "insurance_plan_code", "insurance_plan", "insurance_plan_abbrv") %in% names(result)))
    }
  )
})

test_that("get_insurance_plan_codes handles plan filtering by abbreviation", {
  mock_data <- data.frame(
    commodity_year = c("2023", "2023", "2023"),
    insurance_plan_code = c("1", "2", "3"),
    insurance_plan = c("Yield Protection", "Revenue Protection", "Revenue Protection with Harvest Price Exclusion"),
    insurance_plan_abbrv = c("YP", "RP", "RP-HPE"),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_insurance_key.xlsx",
    file.exists = function(...) FALSE,
    cache_raw_data = function(...) tempfile(fileext = ".xlsx"),
    download.file = function(...) invisible(),
    `readxl::read_excel` = function(...) mock_data,
    {
      # Test filtering by abbreviation
      result <- get_insurance_plan_codes(year = 2023, plan = "YP")
      
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
      expect_equal(result$insurance_plan_abbrv[1], "YP")
    }
  )
})

test_that("get_insurance_plan_codes handles plan filtering by full name", {
  mock_data <- data.frame(
    commodity_year = c("2023", "2023", "2023"),
    insurance_plan_code = c("1", "2", "3"),
    insurance_plan = c("Yield Protection", "Revenue Protection", "Revenue Protection with Harvest Price Exclusion"),
    insurance_plan_abbrv = c("YP", "RP", "RP-HPE"),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_insurance_key.xlsx",
    file.exists = function(...) FALSE,
    cache_raw_data = function(...) tempfile(fileext = ".xlsx"),
    download.file = function(...) invisible(),
    `readxl::read_excel` = function(...) mock_data,
    {
      # Test filtering by full plan name
      result <- get_insurance_plan_codes(year = 2023, plan = "yield protection")
      
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
      expect_equal(result$insurance_plan[1], "Yield Protection")
    }
  )
})

test_that("get_insurance_plan_codes handles plan filtering by numeric code", {
  mock_data <- data.frame(
    commodity_year = c("2023", "2023", "2023"),
    insurance_plan_code = c("1", "2", "3"),
    insurance_plan = c("Yield Protection", "Revenue Protection", "Revenue Protection with Harvest Price Exclusion"),
    insurance_plan_abbrv = c("YP", "RP", "RP-HPE"),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_insurance_key.xlsx",
    file.exists = function(...) FALSE,
    cache_raw_data = function(...) tempfile(fileext = ".xlsx"),
    download.file = function(...) invisible(),
    `readxl::read_excel` = function(...) mock_data,
    {
      # Test filtering by numeric plan code
      result <- get_insurance_plan_codes(year = 2023, plan = 1)
      
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
      expect_equal(as.numeric(result$insurance_plan_code[1]), 1)
    }
  )
})

test_that("get_insurance_plan_codes handles invalid plan with error", {
  mock_data <- data.frame(
    commodity_year = c("2023", "2023", "2023"),
    insurance_plan_code = c("1", "2", "3"),
    insurance_plan = c("Yield Protection", "Revenue Protection", "Revenue Protection with Harvest Price Exclusion"),
    insurance_plan_abbrv = c("YP", "RP", "RP-HPE"),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_insurance_key.xlsx",
    file.exists = function(...) FALSE,
    cache_raw_data = function(...) tempfile(fileext = ".xlsx"),
    download.file = function(...) invisible(),
    `readxl::read_excel` = function(...) mock_data,
    {
      # Test invalid plan should error
      expect_error(
        get_insurance_plan_codes(year = 2023, plan = "invalid_plan"),
        "One or more of the entered insurance plan codes or insurance plan names is not valid"
      )
    }
  )
})

# Caching and force parameter tests
test_that("get_insurance_plan_codes caching behavior", {
  mock_data <- data.frame(
    commodity_year = c("2023", "2023"),
    insurance_plan_code = c("1", "2"),
    insurance_plan = c("Yield Protection", "Revenue Protection"),
    insurance_plan_abbrv = c("YP", "RP"),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_cache_key.xlsx",
    file.exists = function(path) {
      if (grepl("test_cache_key", path)) TRUE else FALSE
    },
    `readxl::read_excel` = function(...) mock_data,
    {
      # Should use cached data when available and force=FALSE
      result1 <- get_insurance_plan_codes(year = 2023, force = FALSE)
      expect_s3_class(result1, "data.frame")
      
      # Should attempt fresh download when force=TRUE
      result2 <- get_insurance_plan_codes(year = 2023, force = TRUE)
      expect_s3_class(result2, "data.frame")
    }
  )
})

test_that("get_insurance_plan_codes force parameter fallback behavior", {
  mock_data <- data.frame(
    commodity_year = c("2023"),
    insurance_plan_code = c("1"),
    insurance_plan = c("Yield Protection"),
    insurance_plan_abbrv = c("YP"),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_cache_key.xlsx",
    file.exists = function(path) {
      if (grepl("test_cache_key", path)) TRUE else FALSE
    },
    download.file = function(...) stop("Download failed"),
    `readxl::read_excel` = function(...) mock_data,
    {
      # Should fall back to cached data when download fails with force=TRUE
      expect_no_error({
        result <- get_insurance_plan_codes(year = 2023, force = TRUE)
      })
    }
  )
})

test_that("get_insurance_plan_codes cache key generation", {
  mock_data <- data.frame(
    commodity_year = c("2023"),
    insurance_plan_code = c("1"),
    insurance_plan = c("Yield Protection"),
    insurance_plan_abbrv = c("YP"),
    stringsAsFactors = FALSE
  )
  
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
    download.file = function(...) invisible(),
    `readxl::read_excel` = function(...) mock_data,
    {
      # Different parameters should create different cache keys
      result1 <- get_insurance_plan_codes(year = 2023, plan = "YP")
      result2 <- get_insurance_plan_codes(year = 2024, plan = "YP")
      result3 <- get_insurance_plan_codes(year = 2023, plan = "RP")
      
      expect_true(length(unique(cache_keys)) >= 3)
    }
  )
})

test_that("get_insurance_plan_codes handles multiple years", {
  mock_data <- data.frame(
    commodity_year = c("2022", "2023"),
    insurance_plan_code = c("1", "1"),
    insurance_plan = c("Yield Protection", "Yield Protection"),
    insurance_plan_abbrv = c("YP", "YP"),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_multi_year_key.xlsx",
    file.exists = function(...) FALSE,
    cache_raw_data = function(...) tempfile(fileext = ".xlsx"),
    download.file = function(...) invisible(),
    `readxl::read_excel` = function(...) mock_data,
    {
      result <- get_insurance_plan_codes(year = c(2022, 2023))
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
    }
  )
})

test_that("get_insurance_plan_codes handles x2 header skip logic", {
  # Mock data with x2 header that should be skipped
  mock_data_with_header <- data.frame(
    x1 = c("Header", "2023"),
    x2 = c("Row", "1"),
    x3 = c("To", "Yield Protection"),
    x4 = c("Skip", "YP"),
    stringsAsFactors = FALSE
  )
  
  mock_data_clean <- data.frame(
    commodity_year = c("2023"),
    insurance_plan_code = c("1"),
    insurance_plan = c("Yield Protection"),
    insurance_plan_abbrv = c("YP"),
    stringsAsFactors = FALSE
  )
  
  call_count <- 0
  
  with_mocked_bindings(
    generate_cache_key = function(...) "test_header_key.xlsx",
    file.exists = function(...) FALSE,
    cache_raw_data = function(...) tempfile(fileext = ".xlsx"),
    download.file = function(...) invisible(),
    readxl::read_excel = function(path, skip = 0) {
      call_count <<- call_count + 1
      if (call_count == 1 && skip == 0) {
        return(mock_data_with_header)
      } else {
        return(mock_data_clean)
      }
    },
    {
      result <- get_insurance_plan_codes(year = 2023)
      
      expect_s3_class(result, "data.frame")
      # Should have been called twice - once with skip=0, once with skip=1
      expect_equal(call_count, 2)
    }
  )
})