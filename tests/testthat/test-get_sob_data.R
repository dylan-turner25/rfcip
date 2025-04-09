test_that("get_sob_data filters work", {
  obj <- get_sob_data(year = 2023, crop = "corn", insurance_plan = 2, state = "IL", group_by = "cov_lvl")
  expect_equal(unique(obj$commodity_name),"Corn")
  expect_equal(unique(obj$commodity_year),2023)
  expect_equal(unique(obj$insurance_plan_abbrv),"RP") 
  expect_equal(unique(obj$state_abbrv),"IL")
  expect_equal(length(unique(obj$cov_level_percent)) > 5,TRUE)
})


test_that("get_sob_data filters work when sob_version is set to sobtpu",{
  obj <- get_sob_data(year = 2012:2014,crop = "soybeans",fips = 18005 , insurance_plan = 1, cov_lvl = 0.5 ,sob_version = "sobtpu")
  expect_equal(unique(obj$commodity_name),"Soybeans")
  expect_equal(min(obj$commodity_year),2012)
  expect_equal(max(obj$commodity_year),2014)
  expect_equal(unique(obj$state_name),"Indiana")
  expect_equal(unique(obj$county_code),5)
  expect_equal(unique(obj$insurance_plan_abbreviation),"YP")
})