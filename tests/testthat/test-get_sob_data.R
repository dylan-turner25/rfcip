test_that("get_sob_data filters work", {
  obj <- get_sob_data(year = 2023, crop = "corn", insurance_plan = 2, state = "IL", group_by = "cov_lvl")
  expect_equal(unique(obj$commodity_name),"Corn")
  expect_equal(unique(obj$commodity_year),2023)
  expect_equal(unique(obj$insurance_plan_abbrv),"RP")
  expect_equal(unique(obj$state_abbrv),"IL")
  expect_equal(length(unique(obj$cov_level_percent)) > 5,TRUE)
})
