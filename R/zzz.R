.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 3600)

  # memoise functions
  get_crop_codes <<- memoise::memoise(get_crop_codes)
  get_sob_data <<- memoise::memoise(get_sob_data)
  get_insurance_plan_codes <<- memoise::memoise(get_insurance_plan_codes)
  get_col_data <<- memoise::memoise(get_col_data)
  get_price_data <<- memoise::memoise(get_price_data)
  
}
