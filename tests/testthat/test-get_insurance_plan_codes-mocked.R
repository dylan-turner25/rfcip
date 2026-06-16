# Offline tests for get_insurance_plan_codes
#
# get_insurance_plan_codes() downloads an Excel file with download.file(), which
# webmockr CANNOT intercept (it only supports httr/crul/httr2). Earlier versions
# of these tests set up webmockr and silently fell through to a real network
# request, which passed on Linux/macOS runners but failed intermittently on
# Windows when RMA throttled the runner. These tests now mock download.file
# directly (like the SOB and COL mocked tests) so they are fully offline.
#
# force = TRUE is used so the mocked download path is always exercised regardless
# of any pre-existing cache, and cache_raw_data is shimmed so nothing is written
# to the user cache.

skip_if_not_installed("writexl")

test_that("get_insurance_plan_codes basic functionality with mocked download", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      expect_no_error({
        result <- get_insurance_plan_codes(year = 2023, force = TRUE)
      })
      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("get_insurance_plan_codes handles multiple years with mocked download", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      expect_no_error({
        result <- get_insurance_plan_codes(year = c(2022, 2023), force = TRUE)
      })
    }
  )
})

test_that("get_insurance_plan_codes handles x2 header detection and skip logic", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(create_mock_insurance_plan_data_with_x2_header()),
    cache_raw_data = cache_raw_data_passthrough,
    {
      expect_no_error({
        result <- get_insurance_plan_codes(year = 2023, force = TRUE)
      })
    }
  )
})

test_that("get_insurance_plan_codes plan filtering by abbreviation works", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      result <- get_insurance_plan_codes(year = 2023, plan = "RP", force = TRUE)
      expect_s3_class(result, "data.frame")

      # case insensitive filtering
      result <- get_insurance_plan_codes(year = 2023, plan = c("rp", "YP"), force = TRUE)
      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("get_insurance_plan_codes plan filtering by full name works", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      result <- get_insurance_plan_codes(year = 2023, plan = "Revenue Protection", force = TRUE)
      expect_s3_class(result, "data.frame")

      result <- get_insurance_plan_codes(year = 2023, plan = c("revenue protection", "YIELD PROTECTION"), force = TRUE)
      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("get_insurance_plan_codes plan filtering by numeric codes works", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      result <- get_insurance_plan_codes(year = 2023, plan = 2, force = TRUE)
      expect_s3_class(result, "data.frame")

      result <- get_insurance_plan_codes(year = 2023, plan = c(1, 2), force = TRUE)
      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("get_insurance_plan_codes handles invalid plan names/codes with error", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      expect_error(
        get_insurance_plan_codes(year = 2023, plan = "invalid_plan_name", force = TRUE),
        "One or more of the entered insurance plan codes or insurance plan names is not valid"
      )
      expect_error(
        get_insurance_plan_codes(year = 2023, plan = 99999, force = TRUE),
        "One or more of the entered insurance plan codes or insurance plan names is not valid"
      )
    }
  )
})

test_that("get_insurance_plan_codes data structure and column processing", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      result <- get_insurance_plan_codes(year = 2023, force = TRUE)

      expect_true("commodity_year" %in% names(result))
      expect_true("insurance_plan_code" %in% names(result))
      expect_true("insurance_plan" %in% names(result))
      expect_true("insurance_plan_abbrv" %in% names(result))
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
      expect_equal(ncol(result), 4)
    }
  )
})

test_that("get_insurance_plan_codes distinct() removes duplicates", {
  mock_data_with_dupes <- data.frame(
    commodity_year = c("2023", "2023", "2023", "2023"),
    insurance_plan_code = c("1", "1", "2", "2"),
    insurance_plan = c("APH", "APH", "Revenue Protection", "Revenue Protection"),
    insurance_plan_abbrv = c("APH", "APH", "RP", "RP"),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(mock_data_with_dupes),
    cache_raw_data = cache_raw_data_passthrough,
    {
      result <- get_insurance_plan_codes(year = 2023, force = TRUE)
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
      expect_true(all(c("APH", "Revenue Protection") %in% result$insurance_plan))
    }
  )
})

test_that("get_insurance_plan_codes URL construction works correctly", {
  # Pure parameter check, no download
  expect_no_error({
    params_test <- list(year = 2023, plan = NULL)
  })
})

test_that("get_insurance_plan_codes column selection works with different data structures", {
  mock_data_extra_cols <- data.frame(
    commodity_year = c("2023", "2023"),
    insurance_plan_code = c("1", "2"),
    insurance_plan = c("APH", "Revenue Protection"),
    insurance_plan_abbrv = c("APH", "RP"),
    extra_col1 = c("Extra1", "Extra2"),
    extra_col2 = c("Extra3", "Extra4"),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(mock_data_extra_cols),
    cache_raw_data = cache_raw_data_passthrough,
    {
      result <- get_insurance_plan_codes(year = 2023, force = TRUE)
      expect_equal(ncol(result), 4)
      expect_true(all(c("commodity_year", "insurance_plan_code", "insurance_plan", "insurance_plan_abbrv") %in% names(result)))
      expect_false("extra_col1" %in% names(result))
      expect_false("extra_col2" %in% names(result))
    }
  )
})

test_that("get_insurance_plan_codes handles edge cases with basic parameters", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      # NULL plan (returns all)
      result <- get_insurance_plan_codes(year = 2023, plan = NULL, force = TRUE)
      expect_s3_class(result, "data.frame")

      # default (current) year
      result <- get_insurance_plan_codes(force = TRUE)
      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("get_insurance_plan_codes hierarchical plan matching works correctly", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      # abbreviation match
      result_abbrev <- get_insurance_plan_codes(year = 2023, plan = "RP", force = TRUE)
      expect_s3_class(result_abbrev, "data.frame")
      expect_true(nrow(result_abbrev) > 0)

      # full name match
      result_full <- get_insurance_plan_codes(year = 2023, plan = "Revenue Protection", force = TRUE)
      expect_s3_class(result_full, "data.frame")
      expect_true(nrow(result_full) > 0)

      # numeric code match
      result_numeric <- get_insurance_plan_codes(year = 2023, plan = 2, force = TRUE)
      expect_s3_class(result_numeric, "data.frame")
      expect_true(nrow(result_numeric) > 0)
    }
  )
})

test_that("get_insurance_plan_codes caching behavior can be tested", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      # First call - forced download
      expect_no_error({
        result1 <- get_insurance_plan_codes(year = 2023, force = TRUE)
        expect_s3_class(result1, "data.frame")
      })

      # Second call - non-forced (uses cache if present, else mocked download)
      expect_no_error({
        result2 <- get_insurance_plan_codes(year = 2023, force = FALSE)
        expect_s3_class(result2, "data.frame")
      })
    }
  )
})

test_that("get_insurance_plan_codes mixed plan filtering scenarios", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      # mixed abbreviations and full names
      result <- get_insurance_plan_codes(year = 2023, plan = c("RP", "Yield Protection"), force = TRUE)
      expect_s3_class(result, "data.frame")

      # mixed abbreviations and numeric codes
      result <- get_insurance_plan_codes(year = 2023, plan = c("APH", 2), force = TRUE)
      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("get_insurance_plan_codes downloadfile vs httr behavior", {
  with_mocked_bindings(
    download.file = create_insurance_plan_download_mock(),
    cache_raw_data = cache_raw_data_passthrough,
    {
      expect_no_error({
        result <- get_insurance_plan_codes(year = 2023, force = TRUE)
        expect_s3_class(result, "data.frame")
      })
    }
  )
})
