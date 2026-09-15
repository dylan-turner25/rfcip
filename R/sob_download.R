# Shared transport policy for SOB application and SOB COV, not ADM or SOBTPU.
sob_timeout <- function(seconds = getOption("timeout", 60)) {
  if (!is.numeric(seconds) || length(seconds) != 1L || is.na(seconds) ||
      !is.finite(seconds) || seconds < 0.001 || seconds > .Machine$integer.max / 1000) {
    stop("The R `timeout` option must be a finite number of seconds between 0.001 and ",
         .Machine$integer.max / 1000, ".", call. = FALSE)
  }
  seconds
}

sob_http_request <- function(url, path, timeout) {
  httr::GET(url, httr::timeout(timeout), httr::write_disk(path, overwrite = TRUE))
}

sob_transient_error <- function(error) {
  # Unknown/untyped errors deliberately fail immediately. In particular, do not
  # infer retryability from a message mentioning a connection or HTTP status.
  inherits(error, paste0("curl_error_", c(
    "couldnt_resolve_proxy", "couldnt_resolve_host", "couldnt_connect",
    "operation_timedout", "partial_file", "got_nothing", "send_error",
    "recv_error", "http2", "http2_stream"
  )))
}

sob_retry_after <- function(headers, now) {
  index <- match("retry-after", tolower(names(headers)))
  if (is.na(index)) return(0)
  value <- headers[[index]]
  if (is.null(value) || length(value) != 1L || is.na(value)) return(0)
  value <- trimws(as.character(value))
  if (grepl("^[0-9]+$", value)) return(as.numeric(value))
  date <- suppressWarnings(httr::parse_http_date(value))
  if (length(date) != 1L || is.na(date)) return(0)
  max(0, as.numeric(difftime(date, now(), units = "secs")))
}

sob_download_error <- function(year, url, attempts, status = NULL,
                               reason, parent = NULL, source = "SOB export",
                               error_class = "rfcip_sob_download_error") {
  structure(list(
    message = paste0(source, " failed", if (!is.null(year)) paste0(" for year ", year),
                     if (!is.null(status)) paste0(" (HTTP ", status, ")"),
                     " after ", attempts, " attempt", if (attempts != 1L) "s",
                     ": ", reason),
    call = NULL, year = year, url = url, attempts = attempts,
    status = status, parent = parent
  ), class = c(error_class, "error", "condition"))
}

sob_http_download <- function(url, year, path, timeout = sob_timeout(),
                              request = sob_http_request, sleep = Sys.sleep,
                              now = Sys.time, jitter = function() stats::runif(1, 0, 0.25),
                              log = FALSE) {
  rfcip_http_download(url, year, path, timeout, request, sleep, now, jitter, log = log)
}

rfcip_http_download <- function(url, year, path, timeout = sob_timeout(),
                                request = sob_http_request, sleep = Sys.sleep,
                                now = Sys.time, jitter = function() stats::runif(1, 0, 0.25),
                                source = "SOB export", error_class = "rfcip_sob_download_error",
                                log = FALSE) {
  timeout <- sob_timeout(timeout)
  fail <- function(attempts, status = NULL, reason, parent = NULL) {
    stop(sob_download_error(year, url, attempts, status, reason, parent, source, error_class))
  }
  completed <- FALSE
  on.exit(if (!completed) unlink(path), add = TRUE)
  slept <- 0
  for (attempt in seq_len(4L)) {
    # A failed transfer may have left an error page or a partial workbook.
    if (file.exists(path) && unlink(path) != 0L) {
      fail(attempt - 1L, reason = "Could not remove the temporary response file.")
    }
    response <- tryCatch(request(url, path, timeout), error = function(e) e)
    status <- NULL
    if (inherits(response, "error")) {
      retry <- sob_transient_error(response)
      reason <- if (retry) "Transient connection or transfer failure." else
        "Request failed; see the parent condition for details."
      parent <- response
      required_wait <- 0
    } else {
      status <- httr::status_code(response)
      if (status == 200L) {
        completed <- TRUE
        return(list(status = status, attempts = attempt))
      }
      retry <- status %in% c(429L, 502L, 503L, 504L)
      reason <- "The server did not return a successful export."
      parent <- NULL
      required_wait <- if (retry) sob_retry_after(httr::headers(response), now) else 0
    }
    if (!retry || attempt == 4L) {
      fail(attempt, status, reason, parent)
    }
    delay <- max(2^(attempt - 1L) + jitter(), required_wait)
    if (delay > 60 - slept) {
      fail(attempt, status, "The required retry delay exceeds the remaining 60-second wait budget.",
           parent)
    }
    if (log) cli::cli_alert_info(paste0(
      if (source == "SOB export") paste0("SOB year ", year) else
        paste0(source, if (!is.null(year)) paste0(" year ", year)),
      ": ", if (source == "SOB export") "API ",
      if (is.null(status)) "transfer failed" else paste0("HTTP ", status),
      " (attempt ", attempt, "/4); retrying in ", format(round(delay, 2), trim = TRUE), " seconds."
    ))
    sleep(delay)
    slept <- slept + delay
  }
}
