test_that("get_livestock_adm_data input validation works", {
  # Test invalid year input
  expect_error(get_livestock_adm_data(year = "2020"), "must be a numeric vector")

  # Test invalid dataset input
  expect_error(get_livestock_adm_data(dataset = "INVALID"), "must be one of")

  # Test multiple datasets
  expect_error(get_livestock_adm_data(dataset = c("lrp", "lgm")), "must be a single character string")

  # Test invalid date
  expect_error(get_livestock_adm_data(date = "not-a-date"), 'must be "all", "latest", or a valid date string')

  # Test date vector
  expect_error(get_livestock_adm_data(date = c("2024-01-01", "2024-02-01")), 'must be "all", "latest", or a single date string')

  # Test NULL date gives helpful error
  expect_error(get_livestock_adm_data(date = NULL), 'cannot be NULL.*Use "all"')
})

test_that("get_livestock_adm_data handles default parameters correctly", {
  formals_list <- formals(get_livestock_adm_data)

  # Test default year is current year
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  default_year <- eval(formals_list$year)
  expect_equal(default_year, current_year)

  # Test default dataset is "lrp"
  expect_equal(formals_list$dataset, "lrp")

  # Test default date is "all"
  expect_equal(formals_list$date, "all")

  # Test default force is FALSE
  expect_equal(formals_list$force, FALSE)
})

test_that("LIVESTOCK_ADM_DATASETS contains all 7 datasets", {
  expect_length(LIVESTOCK_ADM_DATASETS, 7)
  expect_true(all(
    c("drp_draw", "drp_milk_yield", "drp_daily_price",
      "drp_actual_price", "drp_fmmo_pricing", "lgm", "lrp")
    %in% names(LIVESTOCK_ADM_DATASETS)
  ))
})

test_that("LIVESTOCK_ADM_DATASETS codes are correct", {
  expect_equal(LIVESTOCK_ADM_DATASETS$drp_draw, "A00831")
  expect_equal(LIVESTOCK_ADM_DATASETS$drp_milk_yield, "A00832")
  expect_equal(LIVESTOCK_ADM_DATASETS$drp_daily_price, "A00833")
  expect_equal(LIVESTOCK_ADM_DATASETS$drp_actual_price, "A00834")
  expect_equal(LIVESTOCK_ADM_DATASETS$drp_fmmo_pricing, "A00835")
  expect_equal(LIVESTOCK_ADM_DATASETS$lgm, "ADMLivestockLgm")
  expect_equal(LIVESTOCK_ADM_DATASETS$lrp, "ADMLivestockLrp")
})

test_that("get_livestock_adm_data dataset matching is case-insensitive", {
  # Should not error on uppercase
  expect_error(get_livestock_adm_data(year = 9999, dataset = "LRP"), NA)
  expect_error(get_livestock_adm_data(year = 9999, dataset = "DRP_DRAW"), NA)
})

test_that("locate_livestock_adm_links returns valid structure", {
  skip_on_cran()
  skip_if_offline()

  links <- locate_livestock_adm_links(years = 2025)
  expect_s3_class(links, "data.frame")
  expect_true(nrow(links) > 0)
  expect_true(all(c("year", "dataset_code", "url", "file_date", "filename", "size_bytes")
                  %in% names(links)))

  # Should have multiple dataset codes
  expect_true(length(unique(links$dataset_code)) >= 2)

  # All years should be 2025
  expect_true(all(links$year == 2025))

  # URLs should be well-formed
  expect_true(all(grepl("^https://", links$url)))

  # file_date should be Date class
  expect_s3_class(links$file_date, "Date")
})

test_that("locate_livestock_adm_links handles early years (2014)", {
  skip_on_cran()
  skip_if_offline()

  links <- locate_livestock_adm_links(years = 2014)
  expect_s3_class(links, "data.frame")
  expect_true(nrow(links) > 0)

  # 2014 should only have LGM and LRP, not DRP
  expect_true(all(links$dataset_code %in% c("ADMLivestockLgm", "ADMLivestockLrp")))
})

test_that("locate_livestock_adm_links handles invalid year gracefully", {
  skip_on_cran()
  skip_if_offline()

  # A year that doesn't exist should return empty data frame
  links <- locate_livestock_adm_links(years = 1990)
  expect_s3_class(links, "data.frame")
  expect_equal(nrow(links), 0)
})
