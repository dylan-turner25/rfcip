# Reuse SOB transport primitives without calling get_sob_data(), which itself
# needs crop resolution.
crop_filter_values <- function(crop) {
  if (is.null(crop)) return(NULL)
  if (!(is.character(crop) || is.numeric(crop)) || !length(crop) || anyNA(crop) ||
      any(!nzchar(trimws(as.character(crop)))) ||
      (is.numeric(crop) && any(!is.finite(crop) | crop < 1 | crop > 9999 | crop != floor(crop)))) {
    stop("`crop` must contain non-missing commodity names or whole crop codes from 1 to 9999.")
  }
  trimws(as.character(crop))
}

validate_crop_code_data <- function(data, year) {
  required <- c("commodity_year", "commodity_code", "commodity_name")
  if (!is.data.frame(data) || !all(required %in% names(data)) || !nrow(data)) {
    stop("Crop lookup must contain nonempty year, code, and name columns.")
  }
  data <- as.data.frame(data[, required, drop = FALSE])
  for (name in required[1:2]) {
    data[[name]] <- suppressWarnings(as.numeric(as.character(data[[name]])))
  }
  data$commodity_name <- trimws(as.character(data$commodity_name))
  if (anyNA(data) || any(data$commodity_year != year) ||
      any(!is.finite(data$commodity_code) | data$commodity_code < 1 |
          data$commodity_code > 9999 | data$commodity_code != floor(data$commodity_code)) ||
      any(!nzchar(data$commodity_name))) {
    stop("Crop lookup contains invalid identifiers, names, or a different crop year.")
  }
  data <- unique(data)
  if (anyDuplicated(data$commodity_code)) stop("Crop lookup contains conflicting names for one code.")
  data$commodity_year <- as.integer(data$commodity_year)
  data$commodity_code <- as.integer(data$commodity_code)
  data$commodity_abbreviation <- NA_character_
  data$annual_planting_code <- NA_character_
  data <- data[order(data$commodity_code), , drop = FALSE]
  rownames(data) <- NULL
  data
}

download_crop_codes_year <- function(year, timeout = sob_timeout()) {
  url <- paste0("https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel?CY=",
                year, "&ORD=CY,CM&CC=B&VisibleColumns=CommodityYear,CommodityCode,CommodityName&SortField=&SortDir=")
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  sob_http_download(url, year, path, timeout)
  data <- suppressMessages(janitor::clean_names(readxl::read_excel(path)))
  if (ncol(data) >= 2L && names(data)[2L] == "x2") {
    data <- suppressMessages(janitor::clean_names(readxl::read_excel(path, skip = 1)))
  }
  validate_crop_code_data(data, year)
}

load_crop_code_cache <- function(path, year) {
  if (!file.exists(path)) return(NULL)
  tryCatch(suppressWarnings({
    entry <- readRDS(path)
    if (!is.list(entry) || !identical(entry$version, 1L) ||
        length(entry$source) != 1L || !entry$source %in% c("api", "sobcov", "sobcov_cache")) {
      stop("Invalid crop lookup cache.")
    }
    entry$data <- validate_crop_code_data(entry$data, year)
    entry
  }), error = function(e) NULL)
}

save_crop_code_cache <- function(entry, target) {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  staged <- tempfile("crop-codes-staged-", tmpdir = dirname(target))
  on.exit(unlink(staged), add = TRUE)
  entry$version <- 1L
  entry$retrieved_at <- Sys.time()
  saveRDS(entry, staged)
  replace_sob_cache_file(staged, target)
}

# Livestock enrichment requires ADM abbreviations, which SOB does not publish.
# Missing optional metadata must never discard downloaded observations.
livestock_commodity_labels <- function(year, force = FALSE) {
  empty <- data.frame(commodity_code = integer(), commodity_name = character(),
                      commodity_abbreviation = character())
  tryCatch({
    data <- get_adm_data(year = year, dataset = "A00420", show_progress = FALSE, force = force)
    fields <- names(empty)
    if (!all(fields %in% names(data))) stop("A00420 is missing commodity label columns.")
    data <- as.data.frame(data[, fields, drop = FALSE])
    data$commodity_code <- suppressWarnings(as.integer(as.character(data$commodity_code)))
    data$commodity_name <- as.character(data$commodity_name)
    data$commodity_abbreviation <- as.character(data$commodity_abbreviation)
    data <- unique(data)
    if (anyNA(data$commodity_code) || anyDuplicated(data$commodity_code)) {
      stop("A00420 has invalid or conflicting commodity labels.")
    }
    data
  }, error = function(e) {
    warning("Livestock commodity labels could not be retrieved; returning NA labels. ",
            conditionMessage(e), call. = FALSE)
    empty
  })
}
