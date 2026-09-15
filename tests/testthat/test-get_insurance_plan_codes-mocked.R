# A00460 lookup tests: no requests to the SOB application.

test_that("plan lookup maps ADM fields and accepts mixed identifiers and aliases", {
  local_mocked_bindings(get_adm_data = function(year, ...) mock_plan_adm(year))
  data <- get_insurance_plan_codes(2024)
  expect_s3_class(data, "tbl_df")
  expect_named(data, c("commodity_year", "insurance_plan_code", "insurance_plan", "insurance_plan_abbrv"))
  expect_type(data$insurance_plan_code, "integer")
  expect_equal(data$commodity_year, rep(2024L, 3))
  expect_equal(get_insurance_plan_codes(2024, "rp")$insurance_plan_code, 2L)
  expect_equal(get_insurance_plan_codes(2024, "Revenue Protection")$insurance_plan_code, 2L)
  expect_equal(get_insurance_plan_codes(2024, 2)$insurance_plan_code, 2L)
  expect_equal(get_insurance_plan_codes(2024, "02")$insurance_plan_code, 2L)
  expect_equal(get_insurance_plan_codes(2024, "RP-HPE")$insurance_plan_code, 3L)
  expect_equal(get_insurance_plan_codes(2024, "Revenue Protection with Harvest Price Exclusion")$insurance_plan_code, 3L)
  expect_equal(get_insurance_plan_codes(2024, c("RP", "Yield Protection", "3"))$insurance_plan_code, 1:3)
  expect_error(get_insurance_plan_codes(2024, c("RP", "bad")), "not valid: bad")
})

test_that("factor-backed ADM identifiers and names retain their actual values", {
  local_mocked_bindings(get_adm_data = function(year, ...) {
    x <- mock_plan_adm(year)
    x[] <- lapply(x, factor)
    x$insurance_plan_code <- factor(c("1", "2", "3"), levels = c("3", "1", "2"))
    x
  })
  expect_equal(get_insurance_plan_codes(2024, "RP")$insurance_plan_code, 2L)
  expect_equal(get_insurance_plan_codes(2024)$commodity_year, rep(2024L, 3))
})

test_that("plan years and force reach ADM on every direct call", {
  calls <- list()
  local_mocked_bindings(get_adm_data = function(year, dataset, show_progress, force) {
    calls[[length(calls) + 1L]] <<- list(year = year, dataset = dataset, force = force)
    expect_false(show_progress)
    mock_plan_adm(year)
  })
  get_insurance_plan_codes(2023:2024, force = TRUE)
  get_insurance_plan_codes(2023:2024, force = TRUE)
  expect_equal(calls, rep(list(list(year = 2023:2024, dataset = "A00460", force = TRUE)), 2))
  expect_warning(data <- get_insurance_plan_codes(c(1990, 2000, 2011, 2024)), class = "rfcip_lookup_year_fallback")
  expect_equal(calls[[3]]$year, c(2011, 2024))
  expect_setequal(data$commodity_year, c(2011, 2024))
})

test_that("invalid inputs fail before retrieval and missing contemporary assets do not substitute", {
  requests <- 0L
  local_mocked_bindings(get_adm_data = function(year, ...) {
    requests <<- requests + 1L
    stop("ADM asset unavailable for year ", year)
  })
  expect_error(get_insurance_plan_codes(NA_real_), "whole years")
  expect_error(get_insurance_plan_codes(2024, NA), "plan")
  expect_error(get_insurance_plan_codes(2024, character()), "plan")
  expect_error(get_insurance_plan_codes(2024, force = NA), "force")
  expect_equal(requests, 0)
  expect_error(get_insurance_plan_codes(2025), "ADM asset unavailable for year 2025")
  expect_equal(requests, 1)
})

test_that("invalid or incomplete ADM schemas cannot become successful lookup results", {
  local_mocked_bindings(get_adm_data = function(...) data.frame(error = "unavailable"))
  expect_error(get_insurance_plan_codes(2024), "missing required")
  with_mocked_bindings(get_adm_data = function(...) mock_plan_adm(2023), {
    expect_error(get_insurance_plan_codes(2024), "invalid or missing")
  })
})

test_that("different plan filters reuse a single real ADM cache asset", {
  cache <- local_sob_cache()
  asset <- "2024_A00460_InsurancePlan_YTD.parquet"
  requests <- 0L
  local_mocked_bindings(list_data_assets = function() asset)
  local_mocked_bindings(pb_download = function(file, repo, tag, dest, show_progress) {
    requests <<- requests + 1L
    write_parquet_compat(mock_plan_adm(2024), file.path(dest, file))
  }, .package = "piggyback")
  expect_equal(get_insurance_plan_codes(2024, "YP")$insurance_plan_code, 1L)
  expect_equal(get_insurance_plan_codes(2024, "RP")$insurance_plan_code, 2L)
  expect_equal(requests, 1)
  expect_equal(list.files(cache), asset)
  get_insurance_plan_codes(2024, force = TRUE)
  get_insurance_plan_codes(2024, force = TRUE)
  expect_equal(requests, 3)
  clear_rfcip_cache(function_name = "get_insurance_plan_codes", years = 2024)
  get_insurance_plan_codes(2024)
  expect_equal(requests, 4)
})

test_that("ADM lookup recovery is not hidden by memoised fallback", {
  local_sob_cache()
  asset <- "2024_A00460_InsurancePlan_YTD.parquet"
  requests <- 0L
  fail <- FALSE
  local_mocked_bindings(list_data_assets = function() asset)
  local_mocked_bindings(pb_download = function(file, repo, tag, dest, show_progress) {
    requests <<- requests + 1L
    if (fail) stop("GitHub unavailable")
    write_parquet_compat(mock_plan_adm(2024), file.path(dest, file))
  }, .package = "piggyback")
  first <- get_insurance_plan_codes(2024)
  fail <- TRUE
  # The underlying ADM cache retains its existing fallback notification semantics.
  expect_equal(get_insurance_plan_codes(2024, force = TRUE), first)
  fail <- FALSE
  get_insurance_plan_codes(2024, force = TRUE)
  expect_equal(requests, 3)
})

test_that("plan-cache clearing matches only A00460 and legacy lookup files", {
  cache <- local_sob_cache()
  dir.create(cache, recursive = TRUE)
  files <- c("2011_A00460_InsurancePlan_YTD.parquet", "2024_A00460_InsurancePlan_YTD.parquet",
             "insurance_plans_year_1990_plan_RP.xlsx", "insurance_plans_year_2024_plan_RP.xlsx",
             "2024_A00420_Commodity_YTD.parquet", "2024_A004600_other.parquet",
             "sob_year_2024.parquet")
  file.create(file.path(cache, files))
  clear_rfcip_cache("get_insurance_plan_codes", years = 1990)
  expect_setequal(list.files(cache), files[-c(1, 3)])
  clear_rfcip_cache("get_insurance_plan_codes")
  expect_setequal(list.files(cache), files[5:7])
})

test_that("SOBTPU plan filters work without contacting the application", {
  skip_if_not_installed("writexl")
  local_sob_cache()
  calls <- list()
  downloads <- 0L
  source_dir <- withr::local_tempdir()
  # A minimal valid 27-column SOBTPU file, with two different plans.
  row <- c(2024, 19, "Iowa", "IA", 1, "Adair", 41, "Corn", 1, "YP",
           "A", 0.8, "RBUP", 16, "Grain", 3, "Non-Irrigated", "EU", "Enterprise",
           100, "Acres", 1000, 100, 50, 10, 0.1, 0)
  second <- row
  second[9:10] <- c("2", "RP")
  writeLines(c(paste(row, collapse = "|"), paste(second, collapse = "|")),
             file.path(source_dir, "SOBSCCTPU24.TXT"))
  archive <- file.path(source_dir, "fixture.zip")
  withr::with_dir(source_dir, utils::zip(archive, "SOBSCCTPU24.TXT", flags = "-q"))
  local_mocked_bindings(
    get_adm_data = function(year, dataset, force, ...) {
      calls[[length(calls) + 1L]] <<- list(year = year, dataset = dataset, force = force)
      mock_plan_adm(year)
    },
    get_crop_codes = function(...) data.frame(commodity_code = 41L),
    locate_sobtpu_links = function(...) data.frame(year = 2024, url = "https://bulk.example/sobtpu.zip"),
    sob_http_request = function(...) stop("SOB application must not be contacted")
  )
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    expect_equal(url, "https://bulk.example/sobtpu.zip")
    downloads <<- downloads + 1L
    file.copy(archive, destfile)
    invisible(0L)
  }, .package = "utils")
  yp <- get_sob_data(2024, crop = "corn", insurance_plan = "YP", sob_version = "sobtpu")
  rp <- get_sob_data(2024, crop = "corn", insurance_plan = 2, sob_version = "sobtpu")
  expect_equal(yp$insurance_plan_code, 1L)
  expect_equal(rp$insurance_plan_code, 2L)
  expect_equal(downloads, 1)
  get_sob_data(2024, insurance_plan = 2, sob_version = "sobtpu", force = TRUE)
  expect_equal(downloads, 2)
  expect_equal(calls[[3]], list(year = 2024, dataset = "A00460", force = TRUE))
})
