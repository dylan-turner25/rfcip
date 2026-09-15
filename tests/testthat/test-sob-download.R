local_retry_probe <- function(responses, headers = list(), .env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$attempts <- 0L
  state$sleeps <- state$timeouts <- numeric()
  state$existing <- logical()
  state$clock <- as.POSIXct("2026-09-15 12:00:00", tz = "GMT")
  state$path <- withr::local_tempfile(fileext = ".xlsx", .local_envir = .env)
  state$request <- function(url, path, timeout) {
    state$attempts <- state$attempts + 1L
    state$timeouts <- c(state$timeouts, timeout)
    state$existing <- c(state$existing, file.exists(path))
    writeLines("response body", path)
    response <- responses[[min(state$attempts, length(responses))]]
    if (inherits(response, "condition")) stop(response)
    structure(list(status_code = response, headers = headers), class = "response")
  }
  state$sleep <- function(seconds) {
    state$sleeps <- c(state$sleeps, seconds)
    state$clock <- state$clock + seconds
  }
  state$run <- function(...) sob_http_download(
    "https://example.invalid/export?CY=2024", 2024, state$path,
    request = state$request, sleep = state$sleep, now = function() state$clock,
    jitter = function() 0.125, ...
  )
  state
}

test_that("transient HTTP responses retry with bounded exponential waits", {
  for (statuses in list(c(503, 200), c(503, 503, 200), c(429, 502, 504, 200))) {
    state <- local_retry_probe(as.list(statuses))
    result <- suppressMessages(state$run())
    expect_equal(result, list(status = 200, attempts = length(statuses)))
    expect_equal(state$sleeps, 2^(seq_len(length(statuses) - 1) - 1) + 0.125)
    expect_false(any(state$existing))
    expect_equal(readLines(state$path), "response body")
  }
})

test_that("four failed attempts expose diagnostics and remove the response body", {
  state <- local_retry_probe(list(503))
  error <- tryCatch(suppressMessages(state$run()), error = identity)
  expect_s3_class(error, "rfcip_sob_download_error")
  expect_equal(error$status, 503)
  expect_equal(error$year, 2024)
  expect_equal(error$attempts, 4L)
  expect_match(error$url, "CY=2024", fixed = TRUE)
  expect_match(conditionMessage(error), "year 2024.*HTTP 503.*4 attempts")
  expect_false(grepl("https://", conditionMessage(error), fixed = TRUE))
  expect_equal(state$sleeps, c(1.125, 2.125, 4.125))
  expect_false(file.exists(state$path))
})

test_that("other HTTP statuses stop immediately without parsing an error body", {
  for (status in c(204, 206, 301, 400, 401, 403, 404, 500)) {
    state <- local_retry_probe(list(status, 200))
    error <- tryCatch(state$run(), error = identity)
    expect_equal(error$status, status)
    expect_equal(error$attempts, 1L)
    expect_length(state$sleeps, 0)
    expect_false(file.exists(state$path))
  }
})

test_that("Retry-After supports seconds and HTTP dates for 503 and 429", {
  for (status in c(503, 429)) {
    for (header in c("10", "Tue, 15 Sep 2026 12:00:10 GMT")) {
      state <- local_retry_probe(list(status, 200), list(`Retry-After` = header))
      suppressMessages(state$run())
      expect_equal(state$sleeps, 10)
    }
  }
  state <- local_retry_probe(list(503, 503, 200),
                             list(`retry-after` = "Tue, 15 Sep 2026 12:00:10 GMT"))
  suppressMessages(state$run())
  expect_equal(state$sleeps, c(10, 2.125))
})

test_that("absent, invalid, zero, and past Retry-After use normal backoff", {
  for (header in list(NULL, NA_character_, "", "tomorrow", "-5", "1.5", "0",
                     "Mon, 14 Sep 2026 12:00:00 GMT")) {
    state <- local_retry_probe(list(503, 200), list(`retry-after` = header))
    suppressMessages(state$run())
    expect_equal(state$sleeps, 1.125)
  }
})

test_that("required waits are never shortened to fit the cumulative budget", {
  for (header in c("61", "9999999999999999999999999999999999999999",
                   "Tue, 15 Sep 2026 12:01:01 GMT")) {
    state <- local_retry_probe(list(503), list(`retry-after` = header))
    expect_error(state$run(), "wait budget")
    expect_equal(state$attempts, 1L)
    expect_length(state$sleeps, 0)
    expect_false(file.exists(state$path))
  }
  state <- local_retry_probe(list(503), list(`retry-after` = "30"))
  expect_error(suppressMessages(state$run()), "wait budget")
  expect_equal(state$sleeps, c(30, 30))
  expect_equal(state$attempts, 3L)
  state <- local_retry_probe(list(503, 200), list(`retry-after` = "60"))
  expect_equal(suppressMessages(state$run())$attempts, 2L)
  expect_equal(sum(state$sleeps), 60)
})

test_that("only explicitly transient curl errors are retried", {
  for (suffix in c("couldnt_resolve_proxy", "couldnt_resolve_host", "couldnt_connect",
                   "operation_timedout", "partial_file", "got_nothing", "send_error",
                   "recv_error", "http2", "http2_stream")) {
    error <- structure(list(message = "transfer failed", call = NULL),
                       class = c(paste0("curl_error_", suffix), "curl_error", "error", "condition"))
    state <- local_retry_probe(list(error, 200))
    expect_equal(suppressMessages(state$run())$attempts, 2L)
    expect_equal(state$sleeps, 1.125)
  }
  for (suffix in c("peer_failed_verification", "ssl_certproblem", "ssl_cacert_badfile",
                   "ssl_connect_error", "write_error", "url_malformat",
                   "unsupported_protocol", "aborted_by_callback", "unknown")) {
    error <- structure(list(message = "request failed", call = NULL),
                       class = c(paste0("curl_error_", suffix), "curl_error", "error", "condition"))
    state <- local_retry_probe(list(error, 200))
    result <- tryCatch(state$run(), error = identity)
    expect_identical(result$parent, error)
    expect_equal(result$attempts, 1L)
    expect_null(result$status)
    expect_length(state$sleeps, 0)
    expect_false(file.exists(state$path))
  }
  state <- local_retry_probe(list(simpleError("HTTP status was 503"), 200))
  expect_error(state$run(), class = "rfcip_sob_download_error")
  expect_equal(state$attempts, 1L)
})

test_that("interruptions during transfers and waits propagate with cleanup", {
  interrupted <- structure(list(message = "user interrupt", call = NULL),
                            class = c("interrupt", "condition"))
  state <- local_retry_probe(list(interrupted))
  result <- tryCatch(state$run(), interrupt = identity)
  expect_identical(result, interrupted)
  expect_equal(state$attempts, 1L)
  expect_length(state$sleeps, 0)
  expect_false(file.exists(state$path))
  state <- local_retry_probe(list(503))
  state$sleep <- function(seconds) stop(interrupted)
  result <- tryCatch(suppressMessages(state$run()), interrupt = identity)
  expect_identical(result, interrupted)
  expect_equal(state$attempts, 1L)
  expect_false(file.exists(state$path))
})

test_that("the R timeout is validated and passed to every httr transfer", {
  withr::local_options(timeout = 17)
  state <- local_retry_probe(list(503, 200))
  suppressMessages(state$run())
  expect_equal(state$timeouts, c(17, 17))
  for (bad in list(NULL, numeric(), c(1, 2), "60", NA_real_, Inf, -1, 0, 0.0001,
                   .Machine$integer.max)) {
    expect_error(sob_timeout(bad), "timeout.*finite number")
  }
  expect_equal(sob_timeout(0.001), 0.001)
  captured <- NULL
  local_mocked_bindings(GET = function(url, ...) {
    captured <<- list(url = url, configs = list(...))
  }, .package = "httr")
  sob_http_request("https://example.invalid/export", state$path, 17)
  expect_equal(captured$configs[[1]]$options$timeout_ms, 17000)
  expect_equal(captured$configs[[2]]$output$path, state$path)
})

test_that("invalid timeout settings cannot turn a forced call into cached fallback", {
  local_sob_cache()
  state <- local_sob_download()
  get_sob_data(2024)
  withr::local_options(timeout = Inf)
  expect_error(get_sob_data(2024, force = TRUE), "timeout.*finite number")
  expect_length(state$urls, 1)
  # A cached read needs no transfer timeout.
  expect_equal(nrow(get_sob_data(2024)), 3L)
})

test_that("successful years are retained while only the failing year retries", {
  cache <- local_sob_cache()
  state <- local_sob_download()
  state$statuses <- c(200, 503, 503, 200)
  result <- suppressMessages(get_sob_data(2023:2024))
  expect_equal(as.numeric(sub(".*[?&]CY=([0-9]+).*", "\\1", state$urls)),
               c(2023, 2024, 2024, 2024))
  expect_setequal(result$commodity_year, 2023:2024)
  expect_length(list.files(cache, pattern = "parquet$"), 1)
  expect_false(any(file.exists(state$paths)))
  expect_equal(get_sob_data(2023:2024), result)
  expect_length(state$urls, 4)
})

test_that("a 200 with invalid content is never retried or cached", {
  cache <- local_sob_cache()
  state <- local_sob_download()
  original <- get_sob_data(2024)
  path <- list.files(cache, pattern = "parquet$", full.names = TRUE)
  before <- readBin(path, "raw", file.info(path)$size)
  local_mocked_bindings(sob_http_request = function(url, path, timeout) {
    state$paths <- c(state$paths, path)
    writeLines("<html>Service Unavailable</html>", path)
    structure(list(status_code = 200L, headers = list()), class = "response")
  })
  expect_warning(fallback <- get_sob_data(2024, force = TRUE), class = "rfcip_cache_fallback")
  expect_equal(fallback, original)
  expect_identical(readBin(path, "raw", file.info(path)$size), before)
  expect_length(state$paths, 2)
  expect_length(state$sleeps, 0)
  clear_rfcip_cache()
  error <- tryCatch(get_sob_data(2024), error = identity)
  expect_s3_class(error, "rfcip_sob_download_error")
  expect_equal(error$status, 200L)
  expect_equal(error$attempts, 1L)
  expect_false(any(file.exists(state$paths)))
})

test_that("interruptions in a forced public call never return cached fallback", {
  local_sob_cache()
  state <- local_sob_download()
  original <- get_sob_data(2024)
  interrupted <- structure(list(message = "user interrupt", call = NULL),
                            class = c("interrupt", "condition"))
  local_mocked_bindings(sob_http_request = function(url, path, timeout) {
    state$paths <- c(state$paths, path)
    writeLines("partial workbook", path)
    stop(interrupted)
  })
  result <- tryCatch(get_sob_data(2024, force = TRUE), interrupt = identity)
  expect_identical(result, interrupted)
  expect_equal(get_sob_data(2024), original)
  expect_false(any(file.exists(state$paths)))
})
