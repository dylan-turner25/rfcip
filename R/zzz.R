# Suppress R CMD check notes (from rmaADM)
.datatable.aware <- TRUE

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".SD", ":="
  ))
}

.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 3600)

  # memoise functions from original rfcip
  get_crop_codes <<- memoise::memoise(get_crop_codes)
  get_sob_data <<- memoise::memoise(get_sob_data)
  get_insurance_plan_codes <<- memoise::memoise(get_insurance_plan_codes)
  get_col_data <<- memoise::memoise(get_col_data)
  get_price_data <<- memoise::memoise(get_price_data)
  get_livestock_data <<- memoise::memoise(get_livestock_data)

  # memoise functions from merged rmaADM (with renamed function)
  locate_adm_download_link <<- memoise::memoise(locate_adm_download_link)

}

.onAttach <- function(libname, pkgname) {
  # Detect and report parquet backend
  backend <- get_parquet_backend()

  if (backend == "arrow") {
    packageStartupMessage("rfcip: Using arrow for parquet files")
  } else {
    packageStartupMessage("rfcip: Using nanoparquet for parquet files")
    packageStartupMessage("Tip: Install arrow for lower peak memory usage on large files: install.packages('arrow')")
  }
}
