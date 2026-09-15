local_sob_cache <- function(.env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .env)
  withr::local_envvar(c(R_USER_CACHE_DIR = root), .local_envir = .env)
  # Fixtures use plain columns; skip unrelated, expensive global factor metadata.
  # The actual parquet serializer, cache I/O, and ADM retrieval path remain in use.
  testthat::local_mocked_bindings(
    get_parquet_backend = function() "nanoparquet",
    restore_factor_levels = function(data, filename) data,
    .package = "rfcip", .env = .env
  )
  tools::R_user_dir("rfcip", "cache")
}

mock_plan_adm <- function(year = 2024) {
  dplyr::bind_rows(lapply(year, function(y) data.frame(
    reinsurance_year = y,
    insurance_plan_code = c(1L, 2L, 3L),
    insurance_plan_name = c("Yield Protection", "Revenue Protection",
                            "Revenue Prot with Harvest Price Exclusion"),
    insurance_plan_abbreviation = c("YP", "RP", "RPHPE")
  )))
}

local_sob_download <- function(.env = parent.frame()) {
  skip_if_not_installed("writexl")
  state <- new.env(parent = emptyenv())
  state$urls <- state$paths <- character()
  state$marker <- 1
  state$fail <- FALSE
  state$bad_content <- FALSE
  state$fail_year <- NULL
  state$statuses <- integer()
  state$sleeps <- numeric()
  download <- function(url, path, timeout) {
    destfile <- path
    state$urls <- c(state$urls, url)
    state$paths <- c(state$paths, destfile)
    year <- as.numeric(sub(".*[?&]CY=([0-9]+).*", "\\1", url))
    status <- if (state$fail || identical(year, state$fail_year)) 503L else 200L
    if (length(state$statuses)) {
      status <- state$statuses[1L]
      state$statuses <- state$statuses[-1L]
    }
    if (status != 200L) {
      writeLines("partial response", destfile)
    } else if (state$bad_content) {
      writexl::write_xlsx(data.frame(error = "Service Unavailable"), destfile)
    } else {
      data <- create_mock_sob_data()
      data$commodity_year <- year
      data$total_prem <- state$marker
      writexl::write_xlsx(data, destfile)
    }
    structure(list(status_code = status, headers = list()), class = "response")
  }
  http_download <- sob_http_download
  testthat::local_mocked_bindings(
    sob_http_request = download,
    sob_http_download = function(url, year, path, timeout) {
      http_download(url, year, path, timeout, sleep = function(seconds) {
        state$sleeps <- c(state$sleeps, seconds)
      }, jitter = function() 0)
    },
                                  .package = "rfcip", .env = .env)
  state
}
