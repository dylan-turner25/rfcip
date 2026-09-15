# Exercise the public function loaded through .onLoad, with real isolated caches.

test_that("SOB ordinary calls reuse disk and clearing causes another download", {
  cache <- local_sob_cache()
  state <- local_sob_download()
  first <- get_sob_data(2024)
  expect_equal(length(state$urls), 1)
  expect_length(list.files(cache, pattern = "parquet$"), 1)
  expect_equal(get_sob_data(2024), first)
  expect_equal(length(state$urls), 1)
  clear_rfcip_cache(function_name = "get_sob_data")
  get_sob_data(2024)
  expect_equal(length(state$urls), 2)
  clear_rfcip_cache()
  get_sob_data(2024)
  expect_equal(length(state$urls), 3)
  expect_false(any(file.exists(state$paths)))
})

test_that("every forced call downloads and fresh data replaces the disk cache", {
  cache <- local_sob_cache()
  state <- local_sob_download()
  get_sob_data(2024)
  state$marker <- 5
  fresh <- get_sob_data(2024, force = TRUE)
  get_sob_data(2024, force = TRUE)
  expect_equal(length(state$urls), 3)
  expect_true(all(fresh$total_prem == 5))
  saved <- read_parquet_compat(list.files(cache, pattern = "parquet$", full.names = TRUE))
  expect_true(all(saved$total_prem == 5))
  expect_equal(get_sob_data(2024), fresh)
  expect_equal(length(state$urls), 3)
})

test_that("forced failure returns a detectable fallback and recovery is retried", {
  cache <- local_sob_cache()
  state <- local_sob_download()
  original <- get_sob_data(2024)
  file <- list.files(cache, pattern = "parquet$", full.names = TRUE)
  before <- readBin(file, "raw", n = file.info(file)$size)
  state$fail <- TRUE
  expect_warning(fallback <- get_sob_data(2024, force = TRUE), class = "rfcip_cache_fallback")
  expect_equal(fallback, original)
  expect_identical(readBin(file, "raw", n = file.info(file)$size), before)
  state$fail <- FALSE
  state$marker <- 9
  expect_true(all(get_sob_data(2024, force = TRUE)$total_prem == 9))
  expect_equal(length(state$urls), 6)
  expect_equal(state$sleeps, c(1, 2, 4))
  expect_false(any(file.exists(state$paths)))
})

test_that("malformed workbooks cannot replace a usable SOB cache", {
  local_sob_cache()
  state <- local_sob_download()
  original <- get_sob_data(2024)
  state$bad_content <- TRUE
  expect_warning(result <- get_sob_data(2024, force = TRUE), class = "rfcip_cache_fallback")
  expect_equal(result, original)
  expect_equal(get_sob_data(2024), original)
  clear_rfcip_cache()
  expect_error(get_sob_data(2024), class = "rfcip_sob_download_error")
  expect_false(any(file.exists(state$paths)))
})

test_that("cache write failure returns fresh data and retains the old file", {
  cache <- local_sob_cache()
  state <- local_sob_download()
  get_sob_data(2024)
  state$marker <- 8
  local_mocked_bindings(write_parquet_compat = function(x, sink, ...) {
    writeLines("partial parquet", sink)
    stop("disk write failed")
  })
  expect_warning(fresh <- get_sob_data(2024, force = TRUE), class = "rfcip_cache_write_warning")
  expect_true(all(fresh$total_prem == 8))
  expect_true(all(get_sob_data(2024)$total_prem == 1))
  expect_length(list.files(cache), 1)
})

test_that("replacement failure preserves a valid SOB cache", {
  cache <- local_sob_cache()
  state <- local_sob_download()
  get_sob_data(2024)
  state$marker <- 7
  local_mocked_bindings(replace_sob_cache_file = function(...) stop("rename failed"))
  expect_warning(fresh <- get_sob_data(2024, force = TRUE), class = "rfcip_cache_write_warning")
  expect_true(all(fresh$total_prem == 7))
  expect_true(all(get_sob_data(2024)$total_prem == 1))
  expect_length(list.files(cache), 1)
})

test_that("corrupt caches are unavailable, never successful fallback", {
  cache <- local_sob_cache()
  state <- local_sob_download()
  get_sob_data(2024)
  path <- list.files(cache, full.names = TRUE)[1]
  writeLines("not parquet", path)
  state$marker <- 3
  expect_true(all(get_sob_data(2024)$total_prem == 3))
  expect_equal(length(state$urls), 2)
  writeLines("corrupt again", path)
  state$fail <- TRUE
  expect_error(get_sob_data(2024, force = TRUE), class = "rfcip_sob_download_error")
  expect_error(get_sob_data(2024), class = "rfcip_sob_download_error")
})

test_that("a multi-year request only caches after all years succeed", {
  cache <- local_sob_cache()
  state <- local_sob_download()
  state$fail_year <- 2024
  expect_error(get_sob_data(2023:2024), "year 2024")
  expect_length(list.files(cache), 0)
  state$fail_year <- NULL
  original <- get_sob_data(2023:2024)
  expect_setequal(original$commodity_year, 2023:2024)
  state$marker <- 6
  state$fail_year <- 2024
  expect_warning(fallback <- get_sob_data(2023:2024, force = TRUE), class = "rfcip_cache_fallback")
  expect_equal(fallback, original)
  expect_equal(get_sob_data(2023:2024), original)
  expect_false(any(file.exists(state$paths)))
})

test_that("different filters keep separate report caches and invalid inputs error", {
  local_sob_cache()
  state <- local_sob_download()
  get_sob_data(2024)
  get_sob_data(2024, state = "IA")
  expect_equal(length(state$urls), 2)
  expect_match(state$urls[2], "ST=19")
  expect_error(get_sob_data(2024, group_by = "bad", force = TRUE), "Invalid group_by")
  expect_error(get_sob_data(2024, sob_version = "invalid"), "arg")
  expect_error(get_sob_data(numeric()), "whole years")
  expect_error(get_sob_data(c(2024, NA)), "whole years")
  expect_error(get_sob_data(2024.5), "whole years")
  expect_error(get_sob_data(2024, force = NA), "force")
  expect_equal(length(state$urls), 2)
})

test_that("cached SOB results still create each requested Excel export", {
  local_sob_cache()
  state <- local_sob_download()
  path <- withr::local_tempfile(fileext = ".xlsx")
  get_sob_data(2024, dest_file = path)
  expect_true(file.exists(path))
  unlink(path)
  get_sob_data(2024, dest_file = path)
  expect_true(file.exists(path))
  expect_equal(nrow(readxl::read_excel(path)), 3)
  expect_equal(length(state$urls), 1)
})

test_that("plan lookup resolves once using requested years and refresh intent", {
  local_sob_cache()
  state <- local_sob_download()
  lookups <- list()
  local_mocked_bindings(get_adm_data = function(year, dataset, show_progress, force) {
    lookups[[length(lookups) + 1L]] <<- list(year = year, dataset = dataset, force = force)
    mock_plan_adm(year)
  })
  get_sob_data(2023:2024, insurance_plan = "RP", force = TRUE)
  expect_length(lookups, 1)
  expect_equal(lookups[[1]], list(year = 2023:2024, dataset = "A00460", force = TRUE))
  expect_length(state$urls, 2)
  expect_true(all(grepl("IP=2&", state$urls)))
  get_sob_data(2023:2024, insurance_plan = "RP")
  expect_length(lookups, 1)
  expect_length(state$urls, 2)
  expect_error(get_sob_data(2023:2024, insurance_plan = c("RP", "bad"), force = TRUE), "not valid")
  expect_length(state$urls, 2)
})

test_that("pre-2011 plan lookup never changes the requested report year", {
  local_sob_cache()
  state <- local_sob_download()
  calls <- list()
  local_mocked_bindings(get_adm_data = function(year, ...) {
    calls[[length(calls) + 1L]] <<- year
    mock_plan_adm(year)
  })
  expect_warning(get_sob_data(c(1990, 1991), insurance_plan = 2), class = "rfcip_lookup_year_fallback")
  expect_equal(calls, list(2011))
  expect_match(state$urls[1], "CY=1990")
  expect_match(state$urls[2], "CY=1991")
})

test_that("cache replacement rolls back on platforms without overwrite rename", {
  dir <- withr::local_tempdir()
  old <- file.path(dir, "cache")
  fresh <- file.path(dir, "staged")
  writeLines("old", old)
  writeLines("new", fresh)
  n <- 0L
  rename <- function(from, to) {
    n <<- n + 1L
    if (n == 2L) return(FALSE)
    file.rename(from, to)
  }
  expect_error(replace_sob_cache_file(fresh, old, windows = TRUE, rename = rename), "Could not replace")
  expect_equal(readLines(old), "old")
  expect_equal(readLines(fresh), "new")
  n <- 0L
  throwing_rename <- function(from, to) {
    n <<- n + 1L
    if (n == 2L) stop("commit failed")
    file.rename(from, to)
  }
  expect_error(replace_sob_cache_file(fresh, old, windows = TRUE, rename = throwing_rename),
               "Could not replace")
  expect_equal(readLines(old), "old")
  replace_sob_cache_file(fresh, old, windows = TRUE)
  expect_equal(readLines(old), "new")
  expect_length(list.files(dir), 1)
})
