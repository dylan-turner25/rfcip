# Tests for get_adm_data function
# These tests use mocking to avoid external dependencies

# Skip all tests if required packages not available
skip_if_not_installed("mockery")

test_that("get_adm_data handles basic functionality", {
  # Mock the helper functions
  mock_data <- data.frame(
    year = c(2020, 2020, 2020),
    dataset = c("baserate", "baserate", "baserate"),
    value = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    locate_data_asset = function(...) "mock_file.parquet",
    get_cached_data = function(...) mock_data,
    .package = "rfcip",
    {
      result <- get_adm_data(year = 2020, dataset = "baserate")
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
      expect_equal(result, mock_data)
    }
  )
})

test_that("get_adm_data handles multiple years", {
  # Mock data for different years
  mock_data_2020 <- data.frame(
    year = c(2020, 2020),
    value = c(100, 200),
    stringsAsFactors = FALSE
  )
  
  mock_data_2021 <- data.frame(
    year = c(2021, 2021),
    value = c(300, 400),
    stringsAsFactors = FALSE
  )
  
  # Track which year is being requested
  mock_cached_data <- function(file, ...) {
    if (grepl("2020", file)) {
      return(mock_data_2020)
    } else if (grepl("2021", file)) {
      return(mock_data_2021)
    }
    return(data.frame())
  }
  
  # Mock locate_data_asset to return different files for different years
  mock_locate_asset <- function(year, dataset) {
    paste0("mock_", year, "_", dataset, ".parquet")
  }
  
  with_mocked_bindings(
    locate_data_asset = mock_locate_asset,
    get_cached_data = mock_cached_data,
    {
      result <- get_adm_data(year = c(2020, 2021), dataset = "baserate")
      
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
      expect_equal(nrow(result), 4)  # Should have data from both years
      expect_true(2020 %in% result$year)
      expect_true(2021 %in% result$year)
    }
  )
})

test_that("get_adm_data handles single year correctly", {
  mock_data <- data.frame(
    year = c(2020),
    dataset = c("premium"),
    value = c(500),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    locate_data_asset = function(...) "mock_premium_2020.parquet",
    get_cached_data = function(...) mock_data,
    {
      result <- get_adm_data(year = 2020, dataset = "premium")
      
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
      expect_equal(result$year, 2020)
      expect_equal(result$dataset, "premium")
    }
  )
})

test_that("get_adm_data handles NULL year parameter", {
  mock_data <- data.frame(
    data_type = c("default"),
    value = c(123),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    locate_data_asset = function(...) "mock_default.parquet",
    get_cached_data = function(...) mock_data,
    {
      result <- get_adm_data(year = NULL, dataset = "baserate")
      
      expect_s3_class(result, "data.frame")
      expect_equal(result, mock_data)
    }
  )
})

test_that("get_adm_data handles different dataset types", {
  datasets <- c("baserate", "premium", "subsidy", "comborevenuefactor")
  
  for (dataset in datasets) {
    mock_data <- data.frame(
      dataset_type = dataset,
      value = runif(5),
      stringsAsFactors = FALSE
    )
    
    with_mocked_bindings(
      locate_data_asset = function(...) paste0("mock_", dataset, ".parquet"),
      get_cached_data = function(...) mock_data,
      {
        result <- get_adm_data(year = 2020, dataset = dataset)
        
        expect_s3_class(result, "data.frame")
        expect_equal(result$dataset_type, rep(dataset, 5))
      }
    )
  }
})

test_that("get_adm_data handles show_progress parameter", {
  mock_data <- data.frame(
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  
  # Test with show_progress = TRUE
  with_mocked_bindings(
    locate_data_asset = function(...) "mock_file.parquet",
    get_cached_data = function(file, show_progress = TRUE) {
      expect_true(show_progress)
      return(mock_data)
    },
    {
      result <- get_adm_data(year = 2020, dataset = "baserate", show_progress = TRUE)
      expect_s3_class(result, "data.frame")
    }
  )
  
  # Test with show_progress = FALSE
  with_mocked_bindings(
    locate_data_asset = function(...) "mock_file.parquet",
    get_cached_data = function(file, show_progress = FALSE) {
      expect_false(show_progress)
      return(mock_data)
    },
    {
      result <- get_adm_data(year = 2020, dataset = "baserate", show_progress = FALSE)
      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("get_adm_data handles errors from helper functions", {
  # Test error propagation from locate_data_asset
  with_mocked_bindings(
    locate_data_asset = function(...) stop("File not found"),
    {
      expect_error(
        get_adm_data(year = 2020, dataset = "invalid"),
        "File not found"
      )
    }
  )
  
  # Test error propagation from get_cached_data
  with_mocked_bindings(
    locate_data_asset = function(...) "mock_file.parquet",
    get_cached_data = function(...) stop("Cache error"),
    {
      expect_error(
        get_adm_data(year = 2020, dataset = "baserate"),
        "Cache error"
      )
    }
  )
})

test_that("get_adm_data handles empty data from helper functions", {
  # Test handling of empty data frame
  empty_data <- data.frame()
  
  with_mocked_bindings(
    locate_data_asset = function(...) "empty_file.parquet",
    get_cached_data = function(...) empty_data,
    {
      result <- get_adm_data(year = 2020, dataset = "baserate")
      
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 0)
    }
  )
})

test_that("get_adm_data parameter defaults work correctly", {
  mock_data <- data.frame(
    default_test = c(1, 2),
    stringsAsFactors = FALSE
  )
  
  with_mocked_bindings(
    locate_data_asset = function(year, dataset) {
      # Check that default parameters are passed correctly
      expect_null(year)
      expect_equal(dataset, "baserate")
      return("mock_default.parquet")
    },
    get_cached_data = function(file, show_progress = TRUE) {
      expect_true(show_progress)
      return(mock_data)
    },
    {
      # Test calling with all defaults
      result <- get_adm_data()
      expect_s3_class(result, "data.frame")
      expect_equal(result, mock_data)
    }
  )
})

test_that("get_adm_data handles mixed success and failure scenarios", {
  # Mock scenario where some years succeed and others fail
  counter <- 0
  
  mock_data_success <- data.frame(
    year = c(2020),
    status = c("success"),
    stringsAsFactors = FALSE
  )
  
  mock_cached_data <- function(...) {
    counter <<- counter + 1
    if (counter == 1) {
      return(mock_data_success)  # First call succeeds
    } else {
      stop("Failed to load second year")  # Second call fails
    }
  }
  
  with_mocked_bindings(
    locate_data_asset = function(year, dataset) paste0("mock_", year, ".parquet"),
    get_cached_data = mock_cached_data,
    {
      # Should fail when second year fails to load
      expect_error(
        get_adm_data(year = c(2020, 2021), dataset = "baserate"),
        "Failed to load second year"
      )
    }
  )
})

test_that("get_adm_data handles large number of years", {
  # Test with many years to ensure the loop works correctly
  years <- 2015:2020
  
  mock_data_template <- data.frame(
    value = c(100),
    stringsAsFactors = FALSE
  )
  
  mock_cached_data <- function(file, ...) {
    # Extract year from filename for realistic simulation
    year <- gsub(".*_(\\d{4})_.*", "\\1", file)
    data <- mock_data_template
    data$year <- as.numeric(year)
    return(data)
  }
  
  with_mocked_bindings(
    locate_data_asset = function(year, dataset) paste0("mock_", year, "_", dataset, ".parquet"),
    get_cached_data = mock_cached_data,
    {
      result <- get_adm_data(year = years, dataset = "baserate")
      
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), length(years))
      expect_true(all(years %in% result$year))
    }
  )
})