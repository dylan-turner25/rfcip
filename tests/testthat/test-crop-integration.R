test_that("public SOB calls resolve the requested year and keep crop filters", {
  local_sob_cache()
  crop_years <- list()
  local_mocked_bindings(get_crop_codes = function(year, crop, force) {
    crop_years[[length(crop_years) + 1L]] <<- list(year = year, crop = crop, force = force)
    data.frame(commodity_code = c(41L, 78L))
  })
  sob <- local_sob_download()
  get_sob_data(2014, crop = c("Corn", "Sunflowers"), force = TRUE)
  expect_equal(crop_years[[1]], list(year = 2014, crop = c("Corn", "Sunflowers"), force = TRUE))
  expect_match(sob$urls[1], "CM=0041,0078&", fixed = TRUE)
})

test_that("SOBTPU passes requested years and refresh to crop resolution", {
  local_sob_cache()
  seen <- NULL
  local_mocked_bindings(
    get_crop_codes = function(year, crop, force) {
      seen <<- list(year = year, crop = crop, force = force)
      stop("resolution boundary")
    })
  expect_error(get_sobtpu_data(year = c(2014, 2015), crop = "Sunflowers", force = TRUE),
               "resolution boundary")
  expect_equal(seen, list(year = c(2014, 2015), crop = "Sunflowers", force = TRUE))
})

test_that("legacy livestock data joins labels without requiring annual planting codes", {
  cache <- local_sob_cache()
  dir.create(cache, recursive = TRUE)
  write_parquet_compat(data.frame(commodity_code = c(801L, 802L), coverage_price = c(1, 2)),
                       file.path(cache, "livestock_adm_lrp_2014_all.parquet"))
  local_mocked_bindings(
    locate_livestock_adm_links = function(years) data.frame(
      year = 2014, dataset_code = "ADMLivestockLrp", file_date = as.Date("2014-01-02"),
      filename = "mock.zip", url = "https://example.invalid/mock.zip"),
    get_crop_codes = function(...) stop("Livestock labels must not depend on SOB"),
    get_adm_data = function(year, dataset, force, ...) {
      expect_equal(year, 2014)
      expect_equal(dataset, "A00420")
      # Older ADM schemas can have factor codes and no annual_planting_code.
      data.frame(commodity_code = factor(c("801", "802")),
                 commodity_name = c("Feeder Cattle", "Fed Cattle"),
                 commodity_abbreviation = c("FDR CATTLE", "FED CATTLE"))
    })
  result <- suppressWarnings(get_livestock_adm_data(2014, "lrp"))
  expect_equal(result$commodity_name, c("Feeder Cattle", "Fed Cattle"))
  expect_equal(result$coverage_price, c(1, 2))
  local_mocked_bindings(get_adm_data = function(...) stop("ADM unavailable"))
  expect_warning(result <- suppressMessages(get_livestock_adm_data(2014, "lrp", date = "all")),
                 "Downloading all daily files|Livestock commodity labels")
  expect_true(all(is.na(result$commodity_name)))
  expect_equal(result$coverage_price, c(1, 2))
})

test_that("optional livestock label failures do not multiply or discard observations", {
  local_mocked_bindings(get_adm_data = function(...) data.frame(
    commodity_code = c(801, 801), commodity_name = c("A", "B"),
    commodity_abbreviation = c("A", "B")))
  expect_warning(labels <- livestock_commodity_labels(2014), "conflicting")
  expect_equal(nrow(labels), 0)
})
