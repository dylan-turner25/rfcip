# Mocked tests for get_crop_codes
# These tests mock get_adm_data() to avoid network calls

# Helper: create mock ADM commodity data
create_mock_commodity_adm <- function() {
  data.frame(
    record_type_code = rep("A00420", 5),
    record_category_code = rep(1, 5),
    reinsurance_year = rep(2024, 5),
    commodity_year = rep(2024, 5),
    commodity_code = c(41, 81, 91, 28, 801),
    commodity_name = c("Corn", "Soybeans", "Sunflowers", "Almonds", "Feeder Cattle"),
    commodity_abbreviation = c("CORN", "SOYBEANS", "SUNFLWRS", "ALMONDS", "FDR CATTLE"),
    annual_planting_code = c("A", "A", "A", "P", "N"),
    last_released_date = rep(as.Date(NA), 5),
    released_date = rep(as.Date("2023-06-14"), 5),
    deleted_date = rep(as.Date(NA), 5),
    stringsAsFactors = FALSE
  )
}

test_that("get_crop_codes basic functionality with mocked get_adm_data", {
  with_mocked_bindings(
    get_adm_data = function(...) create_mock_commodity_adm(),
    {
      result <- get_crop_codes(year = 2024)
      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)
    }
  )
})

test_that("get_crop_codes returns expected columns", {
  with_mocked_bindings(
    get_adm_data = function(...) create_mock_commodity_adm(),
    {
      result <- get_crop_codes(year = 2024)
      expect_true(all(c("commodity_year", "commodity_code", "commodity_name",
                         "commodity_abbreviation", "annual_planting_code") %in% names(result)))
      expect_equal(ncol(result), 5)
    }
  )
})

test_that("get_crop_codes crop filtering by character names works", {
  with_mocked_bindings(
    get_adm_data = function(...) create_mock_commodity_adm(),
    {
      result <- get_crop_codes(year = 2024, crop = "Corn")
      expect_equal(nrow(result), 1)
      expect_equal(result$commodity_code[1], 41)
    }
  )
})

test_that("get_crop_codes crop filtering is case-insensitive", {
  with_mocked_bindings(
    get_adm_data = function(...) create_mock_commodity_adm(),
    {
      result <- get_crop_codes(year = 2024, crop = c("CORN", "soybeans"))
      expect_equal(nrow(result), 2)
    }
  )
})

test_that("get_crop_codes crop filtering by numeric codes works", {
  with_mocked_bindings(
    get_adm_data = function(...) create_mock_commodity_adm(),
    {
      result <- get_crop_codes(year = 2024, crop = 41)
      expect_equal(nrow(result), 1)
      expect_equal(as.character(result$commodity_name[1]), "Corn")
    }
  )
})

test_that("get_crop_codes handles multiple numeric codes", {
  with_mocked_bindings(
    get_adm_data = function(...) create_mock_commodity_adm(),
    {
      result <- get_crop_codes(year = 2024, crop = c(41, 81))
      expect_equal(nrow(result), 2)
    }
  )
})

test_that("get_crop_codes includes livestock commodities", {
  with_mocked_bindings(
    get_adm_data = function(...) create_mock_commodity_adm(),
    {
      result <- get_crop_codes(year = 2024, crop = 801)
      expect_equal(nrow(result), 1)
      expect_equal(as.character(result$commodity_name[1]), "Feeder Cattle")
    }
  )
})

test_that("get_crop_codes handles invalid crop names gracefully", {
  with_mocked_bindings(
    get_adm_data = function(...) create_mock_commodity_adm(),
    {
      expect_warning(
        result <- get_crop_codes(year = 2024, crop = "invalid_crop_name"),
        "One or more of the entered crop codes or crop names is not valid"
      )
      expect_true(nrow(result) > 0)
    }
  )
})

test_that("get_crop_codes handles invalid numeric codes gracefully", {
  with_mocked_bindings(
    get_adm_data = function(...) create_mock_commodity_adm(),
    {
      expect_warning(
        result <- get_crop_codes(year = 2024, crop = 99999),
        "One or more of the entered crop codes or crop names is not valid"
      )
      expect_true(nrow(result) > 0)
    }
  )
})

test_that("get_crop_codes with NULL crop returns all data", {
  with_mocked_bindings(
    get_adm_data = function(...) create_mock_commodity_adm(),
    {
      result <- get_crop_codes(year = 2024, crop = NULL)
      expect_equal(nrow(result), 5)
    }
  )
})

test_that("get_crop_codes passes force parameter to get_adm_data", {
  captured_args <- NULL
  with_mocked_bindings(
    get_adm_data = function(...) {
      captured_args <<- list(...)
      create_mock_commodity_adm()
    },
    {
      get_crop_codes(year = 2024, force = TRUE)
      expect_equal(captured_args$force, TRUE)
    }
  )
})

test_that("get_crop_codes default parameters are correct", {
  formals_list <- formals(get_crop_codes)

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  expect_equal(eval(formals_list$year), current_year)
  expect_null(formals_list$crop)
  expect_equal(formals_list$force, FALSE)
})
