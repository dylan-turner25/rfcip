# Internal SOB validation and persistence helpers.
validate_sob_years <- function(year) {
  stopifnot("`year` must be a numeric value or vector of numeric values." = is.numeric(year))
  if (!length(year) || anyNA(year) || any(!is.finite(year)) ||
      any(year <= 0 | year != floor(year))) {
    stop("`year` must contain positive, finite whole years.")
  }
  invisible(NULL)
}

validate_sob_force <- function(force) {
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {
    stop("`force` must be TRUE or FALSE.")
  }
}

convert_sob_types <- function(data) {
  data <- dplyr::as_tibble(data)
  if (!any(vapply(data, is.character, logical(1)))) return(data)
  suppressMessages(readr::type_convert(data, col_types = readr::cols(
    commodity_code = readr::col_integer(),
    insurance_plan_code = readr::col_integer(),
    cov_level_percent = readr::col_double()
  )))
}

validate_sob_data <- function(data) {
  if (!is.data.frame(data) ||
      !all(c("commodity_year", "policies_sold") %in% names(data))) {
    stop("Invalid SOB data: missing commodity_year or policies_sold.")
  }
  years <- suppressWarnings(as.numeric(as.character(data$commodity_year)))
  counts <- suppressWarnings(as.numeric(as.character(data$policies_sold)))
  if (anyNA(years) || any(!is.finite(years)) || any(years != floor(years)) ||
      any(!is.na(data$policies_sold) & is.na(counts))) {
    stop("Invalid SOB data: unrecognized year or policy count.")
  }
  data
}

load_usable_sob_cache <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(suppressWarnings(convert_sob_types(validate_sob_data(load_cached_data(path)))),
           error = function(e) NULL)
}

download_sob_year <- function(url, year, timeout = sob_timeout(), log = FALSE) {
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  transfer <- sob_http_download(url, year, path, timeout, log = log)
  tryCatch({
    data <- suppressMessages(janitor::clean_names(readxl::read_excel(path)))
    if (ncol(data) >= 2L && names(data)[2L] == "x2") {
      data <- suppressMessages(janitor::clean_names(readxl::read_excel(path, skip = 1)))
    }
    validate_sob_data(data)
    if (any(as.numeric(as.character(data$commodity_year)) != year)) {
      stop("Workbook contains a different commodity year than requested.")
    }
    dplyr::mutate(data, dplyr::across(dplyr::everything(), as.character))
  }, error = function(e) {
    stop(sob_download_error(year, url, transfer$attempts, transfer$status,
                           paste0("Invalid workbook: ", conditionMessage(e)), e))
  })
}

warn_sob_cache <- function(message, class, cache_file, parent) {
  warning(structure(list(message = message, call = NULL,
                         cache_file = cache_file, parent = parent),
                    class = c(class, "warning", "condition")))
}

# POSIX rename replaces an existing file atomically. Windows requires moving the
# old file aside; restore it if the staged replacement cannot be committed.
replace_sob_cache_file <- function(staged, target, windows = .Platform$OS.type == "windows",
                                   rename = file.rename) {
  backup <- NULL
  if (windows && file.exists(target)) {
    backup <- tempfile("sob-backup-", tmpdir = dirname(target))
    if (!rename(target, backup)) stop("Could not preserve the previous SOB cache.")
  }
  committed <- tryCatch(rename(staged, target), error = function(e) FALSE)
  if (!isTRUE(committed)) {
    restored <- is.null(backup) ||
      isTRUE(tryCatch(rename(backup, target), error = function(e) FALSE))
    if (!restored) {
      stop("Could not restore the previous SOB cache; it is preserved at ", backup)
    }
    stop("Could not replace the SOB cache file.")
  }
  if (!is.null(backup)) unlink(backup)
  invisible(target)
}

save_sob_cache <- function(data, target) {
  dir <- dirname(target)
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE)) {
    stop("Could not create the SOB cache directory.")
  }
  staged <- tempfile("sob-staged-", tmpdir = dir, fileext = ".parquet")
  on.exit(unlink(staged), add = TRUE)
  write_parquet_compat(data, staged)
  if (!file.exists(staged) || file.info(staged)$size == 0) {
    stop("Writing the SOB cache produced no data.")
  }
  replace_sob_cache_file(staged, target)
}
