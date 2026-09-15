sobcov_fixture_rows <- function(year = 2008) {
  row <- c(as.character(year), "01", "AL", "003", "Baldwin   ", "0041", "Corn  ",
           "02", "RP   ", "A", "RBUP", ".7500", "10", "8", "2", "12", "3", "Acres",
           "100", "5", "1000", "100", "60", "0", "0", "0", "50", ".50")
  rows <- matrix(rep(row, 4), nrow = 4, byrow = TRUE)
  rows[2, 18] <- "Tons"
  rows[2, 22] <- "200"
  rows[3, c(2, 3, 5, 12, 13, 22)] <- c("02", "AK", "Other County", ".8500", "20", "300")
  rows[4, c(4, 5, 6, 7, 8, 9, 13, 22)] <- c("005", "Barbour", "0081", "Soybeans", "01", "YP", "30", "400")
  rows
}

write_sobcov_fixture <- function(path, rows = sobcov_fixture_rows(), member = "sobcov08.txt",
                                 nul = FALSE) {
  dir <- tempfile("sobcov-fixture-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  bytes <- charToRaw(paste0(paste(apply(rows, 1, paste, collapse = "|"), collapse = "\n"), "\n"))
  if (nul) bytes[bytes == charToRaw("~")] <- as.raw(0)
  writeBin(bytes, file.path(dir, member))
  withr::with_dir(dir, utils::zip(path, member, flags = "-q"))
  invisible(path)
}

local_sobcov_download <- function(.env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$years <- numeric()
  state$paths <- character()
  state$sleeps <- numeric()
  state$discoveries <- 0L
  state$status <- 200L
  state$bad_content <- FALSE
  state$marker <- "100"
  state$available <- 1989:2026
  request <- function(url, path, timeout) {
    year <- as.numeric(sub(".*sobcov_([0-9]{4})[.]zip", "\\1", url))
    state$years <- c(state$years, year)
    state$paths <- c(state$paths, path)
    if (state$status != 200L || state$bad_content) {
      writeLines("<html>Service Unavailable</html>", path)
    } else {
      rows <- sobcov_fixture_rows(year)
      rows[1, 22] <- state$marker
      write_sobcov_fixture(path, rows)
    }
    structure(list(status_code = state$status, headers = list()), class = "response")
  }
  testthat::local_mocked_bindings(
    locate_sobcov_links = function(...) {
      state$discoveries <- state$discoveries + 1L
      data.frame(year = state$available,
                  url = paste0("https://bulk.example/sobcov_", state$available, ".zip"))
    },
    sobcov_http_download = function(url, year, path, timeout = sob_timeout(), log = FALSE) {
      rfcip_http_download(url, year, path, timeout, request = request,
                          sleep = function(seconds) state$sleeps <- c(state$sleeps, seconds),
                          jitter = function() 0, source = "SOB COV",
                          error_class = "rfcip_sobcov_download_error", log = log)
    }, .package = "rfcip", .env = .env
  )
  state
}
