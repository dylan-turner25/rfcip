test_that("SOB COV positional schema preserves types, units and missing markers", {
  path <- withr::local_tempfile(fileext = ".zip")
  rows <- sobcov_fixture_rows(1989)
  rows[1, c(4, 11, 12, 13, 19, 21, 27)] <- c("000", "FBUP", ".0000", "~", "", "4000000000", "-20")
  write_sobcov_fixture(path, rows, member = "SOBCOV89.TXT", nul = TRUE)
  data <- process_sobcov_zip(path, 1989)
  expect_named(data, c(sobcov_columns(), "fips"))
  expect_equal(nrow(data), 4)
  expect_equal(data$commodity_code, c(41L, 41L, 41L, 81L))
  expect_equal(data$fips, c("01000", "01003", "02003", "01005"))
  expect_equal(data$commodity_name[1], "Corn")
  expect_equal(data$delivery_type[1], "FBUP")
  expect_equal(data$coverage_level_percent[1], 0)
  expect_true(is.na(data$policies_sold[1]))
  expect_true(is.na(data$net_reported_quantity[1]))
  expect_equal(data$liability_amount[1], 4e9)
  expect_equal(data$indemnity_amount[1], -20)
  expect_equal(data$quantity_type[2], "Tons")
  expect_error(process_sobcov_zip(path, 1990), "different commodity year")
})

test_that("invalid archive content and layouts cannot be accepted", {
  for (kind in c("html", "columns", "numeric", "member")) {
    path <- withr::local_tempfile(fileext = ".zip")
    rows <- sobcov_fixture_rows()
    if (kind == "html") writeLines("<html>error</html>", path)
    if (kind == "columns") write_sobcov_fixture(path, rows[, -28])
    if (kind == "numeric") {
      rows[1, 22] <- "wrong"
      write_sobcov_fixture(path, rows)
    }
    if (kind == "member") write_sobcov_fixture(path, rows, member = "unexpected.txt")
    expect_error(suppressWarnings(process_sobcov_zip(path, 2008)))
  }
})

test_that("explicit bulk access avoids API and shares annual caches across filters", {
  cache <- local_sob_cache()
  state <- local_sobcov_download()
  local_mocked_bindings(sob_http_request = function(...) stop("API must not be used"),
                         get_adm_data = function(...) stop("ADM must not be used"))
  data <- get_sob_data(2008:2009, sob_version = "sobcov")
  expect_equal(nrow(data), 8)
  expect_equal(state$years, 2008:2009)
  expect_equal(state$discoveries, 1)
  expect_equal(attr(data, "rfcip_sources")$source, c("sobcov", "sobcov"))
  corn <- get_sob_data(2008:2009, crop = "corn", insurance_plan = "rp", sob_version = "sobcov")
  expect_equal(nrow(corn), 6)
  expect_equal(state$years, 2008:2009)
  expect_equal(state$discoveries, 1)
  expect_equal(attr(corn, "rfcip_sources")$source, c("sobcov_cache", "sobcov_cache"))
  expect_setequal(list.files(cache), c("sobcov_2008.zip", "sobcov_2009.zip"))
  expect_false(any(file.exists(state$paths)))
})

test_that("filters use complete FIPS pairs and retain typed empty intersections", {
  local_sob_cache()
  local_sobcov_download()
  call <- function(...) get_sob_data(2008, sob_version = "sobcov", ...)
  expect_equal(nrow(call(state = "AL", county = "Baldwin")), 2)
  expect_setequal(call(fips = c("01005", "02003"))$fips, c("01005", "02003"))
  expect_equal(nrow(call(county = 1003)), 2)
  expect_equal(nrow(call(crop = "0041", cov_lvl = .85, delivery_type = "RBUP")), 1)
  expect_equal(nrow(call(insurance_plan = 1)), 1)
  empty <- call(state = "AK", crop = "Soybeans")
  expect_equal(nrow(empty), 0)
  expect_type(empty$total_premium_amount, "double")
  expect_error(call(crop = c("corn", "typo")), "Unknown")
  expect_error(call(crop = 999), "Unknown")
  expect_error(call(county = "Baldwin"), "require a state")
  expect_error(call(state = "AL", fips = "02003"), "Conflicting")
  expect_error(call(county = "01003", fips = "01005"), "Conflicting")
  expect_error(call(fips = 123), "FIPS")
  expect_error(call(delivery_type = "bad"), "delivery_type")
  expect_error(call(cov_lvl = "bad"), "cov_lvl")
  expect_error(call(state = 99), "Invalid state")
  expect_error(call(group_by = "state"), "group_by")
  expect_error(call(comm_cat = "S"), "comm_cat")
  expect_error(get_sob_data(1988, sob_version = "sobcov"), "1989")
})

test_that("full plan names use ADM but codes and abbreviations do not", {
  local_sob_cache()
  local_sobcov_download()
  calls <- list()
  local_mocked_bindings(get_insurance_plan_codes = function(year, plan, force) {
    calls[[length(calls) + 1L]] <<- list(year, plan, force)
    data.frame(insurance_plan_code = 2L)
  })
  data <- get_sob_data(2008:2009, sob_version = "sobcov", insurance_plan = "revenue protection")
  expect_equal(nrow(data), 6)
  expect_equal(calls, list(list(2008:2009, "revenue protection", FALSE)))
})

test_that("annual refresh commits valid data and recovers after failed refreshes", {
  cache <- local_sob_cache()
  state <- local_sobcov_download()
  call <- function(force = FALSE) get_sob_data(2008, sob_version = "sobcov", force = force)
  original <- call()
  path <- file.path(cache, "sobcov_2008.zip")
  before <- readBin(path, "raw", file.info(path)$size)
  state$status <- 503L
  expect_warning(fallback <- suppressMessages(call(TRUE)), class = "rfcip_cache_fallback")
  expect_equal(fallback$total_premium_amount, original$total_premium_amount)
  expect_identical(readBin(path, "raw", file.info(path)$size), before)
  expect_equal(state$sleeps, c(1, 2, 4))
  state$status <- 200L
  state$marker <- "900"
  expect_equal(call(TRUE)$total_premium_amount[1], 900)
  expect_equal(call()$total_premium_amount[1], 900)
  call(TRUE)
  expect_length(state$years, 7)
  state$bad_content <- TRUE
  expect_warning(call(TRUE), class = "rfcip_cache_fallback")
  expect_equal(call()$total_premium_amount[1], 900)
  expect_false(any(file.exists(state$paths)))
})

test_that("bulk cache write failures preserve old data while returning fresh data", {
  cache <- local_sob_cache()
  state <- local_sobcov_download()
  get_sob_data(2008, sob_version = "sobcov")
  state$marker <- "900"
  local_mocked_bindings(replace_sob_cache_file = function(...) stop("cannot rename"))
  expect_warning(fresh <- get_sob_data(2008, sob_version = "sobcov", force = TRUE),
                  class = "rfcip_cache_write_warning")
  expect_equal(fresh$total_premium_amount[1], 900)
  expect_equal(get_sob_data(2008, sob_version = "sobcov")$total_premium_amount[1], 100)
  expect_length(list.files(cache), 1)
})

test_that("missing years and corrupt caches cannot yield incomplete results", {
  cache <- local_sob_cache()
  state <- local_sobcov_download()
  state$available <- 2008
  expect_error(get_sob_data(2008:2009, sob_version = "sobcov"), "2009")
  expect_true(file.exists(file.path(cache, "sobcov_2008.zip")))
  state$available <- 2008:2009
  expect_equal(nrow(get_sob_data(2008:2009, sob_version = "sobcov")), 8)
  expect_equal(state$years, c(2008, 2009))
  writeLines("bad zip", file.path(cache, "sobcov_2008.zip"))
  state$status <- 404L
  expect_error(get_sob_data(2008, sob_version = "sobcov"), class = "rfcip_sobcov_download_error")
})

test_that("bulk adapter groups like the API without mixing quantities", {
  path <- withr::local_tempfile(fileext = ".zip")
  write_sobcov_fixture(path)
  data <- process_sobcov_zip(path, 2008)
  url <- get_sob_url(2008, crop = NULL)
  result <- adapt_sobcov(data, url)
  acres <- result[result$quantity_type == "Acres", ]
  expect_equal(nrow(result), 2)
  expect_equal(acres$policies_sold, 60)
  expect_equal(acres$total_prem, 800)
  expect_equal(acres$loss_ratio, 150 / 800)
  expect_equal(acres$earn_prem_rate, trunc(800 / 3000 * 1e6) / 1e6)
  expect_true(all(is.na(result$organic_certified_subsidy_amount)))
  county <- adapt_sobcov(data, get_sob_url(2008, crop = NULL, group_by = "fips"))
  expect_equal(nrow(county), 4)
  expect_true(all(c("state_code", "county_code") %in% names(county)))
  plan <- adapt_sobcov(data, get_sob_url(2008, crop = NULL, group_by = c("insurance_plan", "cov_lvl")))
  expect_true(all(c("insurance_plan_code", "insurance_plan_abbrv", "cov_level_percent") %in% names(plan)))
  data$policies_sold[1] <- NA_real_
  data$total_premium_amount <- 0
  unknown <- adapt_sobcov(data, url)
  expect_true(is.na(unknown$policies_sold[unknown$quantity_type == "Acres"]))
  expect_true(all(is.na(unknown$loss_ratio)))
})

test_that("a failed API year falls back without changing the next year's API-first order", {
  cache <- local_sob_cache()
  api <- local_sob_download()
  bulk <- local_sobcov_download()
  api$statuses <- c(200, rep(503, 4), 200)
  expect_warning(data <- suppressMessages(get_sob_data(2008:2010)), class = "rfcip_source_fallback")
  expect_equal(as.numeric(sub(".*CY=([0-9]+).*", "\\1", api$urls)), c(2008, rep(2009, 4), 2010))
  expect_equal(bulk$years, 2009)
  expect_equal(attr(data, "rfcip_sources"), data.frame(year = 2008:2010, source = c("api", "sobcov", "api")))
  expect_equal(data$total_prem[data$commodity_year == 2008], rep(1, 3))
  expect_equal(sort(data$total_prem[data$commodity_year == 2009]), c(200, 800))
  expect_type(data$total_prem, "double")
  expect_length(list.files(cache, pattern = "^sob_"), 0)
  expect_false(any(file.exists(c(api$paths, bulk$paths))))
  # New calls try the API again even though the failed year's bulk data is cached.
  get_sob_data(2008:2010)
  expect_length(api$urls, 9)
  expect_length(bulk$years, 1)
  expect_length(list.files(cache, pattern = "^sob_"), 1)
})

test_that("consecutive failed years reset API retries and share discovery", {
  local_sob_cache()
  api <- local_sob_download()
  bulk <- local_sobcov_download()
  api$fail <- TRUE
  data <- suppressWarnings(suppressMessages(get_sob_data(2008:2009)))
  expect_length(api$urls, 8)
  expect_equal(api$sleeps, rep(c(1, 2, 4), 2))
  expect_equal(bulk$years, 2008:2009)
  expect_equal(bulk$discoveries, 1)
  expect_setequal(data$commodity_year, 2008:2009)
  expect_type(data$organic_certified_subsidy_amount, "double")
  suppressWarnings(suppressMessages(get_sob_data(2008:2009)))
  expect_length(api$urls, 16)
  expect_length(bulk$years, 2)
  suppressWarnings(suppressMessages(get_sob_data(2008:2009, force = TRUE)))
  expect_length(api$urls, 24)
  expect_length(bulk$years, 4)
})

test_that("both-source failure preserves final API-cache fallback and diagnostics", {
  local_sob_cache()
  api <- local_sob_download()
  bulk <- local_sobcov_download()
  original <- get_sob_data(2008)
  api$fail <- TRUE
  bulk$status <- 503L
  expect_warning(result <- suppressMessages(get_sob_data(2008, force = TRUE)), class = "rfcip_cache_fallback")
  expect_equal(result, original)
  clear_rfcip_cache()
  error <- tryCatch(suppressMessages(get_sob_data(2008)), error = identity)
  expect_s3_class(error, "rfcip_sob_backup_error")
  expect_equal(error$parent$status, 503)
  expect_equal(error$bulk_parent$status, 503)
  expect_equal(error$year, 2008)
})

test_that("terminal API errors and unsupported backup filters cannot silently switch sources", {
  local_sob_cache()
  api <- local_sob_download()
  bulk <- local_sobcov_download()
  for (status in c(400, 401, 403, 404, 500)) {
    api$statuses <- status
    expect_error(get_sob_data(2008), class = "rfcip_sob_download_error")
  }
  expect_length(bulk$years, 0)
  api$fail <- TRUE
  expect_error(suppressMessages(get_sob_data(2008, comm_cat = "L")), "comm_cat")
  expect_length(bulk$years, 0)
})

test_that("cache clearing, labels and repeated Excel exports include SOB COV", {
  cache <- local_sob_cache()
  bulk <- local_sobcov_download()
  get_sob_data(2008:2009, sob_version = "sobcov")
  expect_equal(get_cache_info()$function_type, rep("get_sob_data (SOB COV)", 2))
  path <- withr::local_tempfile(fileext = ".xlsx")
  get_sob_data(2008, sob_version = "sobcov", dest_file = path)
  unlink(path)
  get_sob_data(2008, sob_version = "sobcov", dest_file = path)
  expect_equal(nrow(readxl::read_excel(path)), 4)
  expect_length(bulk$years, 2)
  clear_rfcip_cache(function_name = "get_sob_data", years = 2008)
  expect_equal(list.files(cache), "sobcov_2009.zip")
})

test_that("link discovery distinguishes SOB COV links and historical years", {
  local_mocked_bindings(sobcov_http_download = function(url, year, path, ...) {
    writeLines(c('<a href="./sobcov_2008.zip">2008</a>',
                 '<a href="/pub/files/SOBCOV_1989.ZIP">1989</a>',
                 '<a href="sobtpu_2008.zip">other</a>', '<a href="layout.pdf">layout</a>'), path)
  })
  links <- locate_sobcov_links()
  expect_setequal(links$year, c(1989, 2008))
  expect_true(all(startsWith(links$url, "https://pubfs-rma.fpac.usda.gov/")))
})

test_that("a server wait beyond the budget triggers backup without retrying early", {
  local_sob_cache()
  api <- local_sob_download()
  bulk <- local_sobcov_download()
  requests <- 0L
  local_mocked_bindings(sob_http_request = function(url, path, timeout) {
    requests <<- requests + 1L
    writeLines("503", path)
    structure(list(status_code = 503L, headers = list(`retry-after` = "61")), class = "response")
  })
  expect_warning(data <- get_sob_data(2008), class = "rfcip_source_fallback")
  expect_equal(requests, 1L)
  expect_length(api$sleeps, 0)
  expect_equal(bulk$years, 2008)
  expect_equal(unique(data$commodity_year), 2008)
})

test_that("interrupting bulk backup propagates and preserves prior API caches", {
  local_sob_cache()
  api <- local_sob_download()
  local_sobcov_download()
  original <- get_sob_data(2008)
  api$fail <- TRUE
  paths <- character()
  interrupted <- structure(list(message = "interrupted", call = NULL), class = c("interrupt", "condition"))
  local_mocked_bindings(sobcov_http_download = function(url, year, path, ...) {
    paths <<- c(paths, path)
    writeLines("partial", path)
    stop(interrupted)
  })
  result <- tryCatch(suppressMessages(get_sob_data(2008, force = TRUE)), interrupt = identity)
  expect_identical(result, interrupted)
  expect_false(any(file.exists(paths)))
  expect_equal(get_sob_data(2008), original)
})

test_that("unsafe or multiple archive members are rejected before extraction", {
  n <- 0L
  local_mocked_bindings(unzip = function(zipfile, list = FALSE, ...) {
    n <<- n + 1L
    if (!list) stop("Extraction must not happen")
    data.frame(Name = c("../sobcov08.txt", "sobcov08.txt"))
  }, .package = "utils")
  expect_error(process_sobcov_zip("unsafe.zip", 2008), "exactly one")
  expect_equal(n, 1L)
})
