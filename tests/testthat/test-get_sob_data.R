test_that("get_sob_data validates year parameter", {
  expect_error(get_sob_data(year = "invalid"), "`year` must be a numeric value or vector of numeric values.")
  expect_error(get_sob_data(year = c("2020", "2021")), "`year` must be a numeric value or vector of numeric values.")
  expect_error(get_sob_data(year = NULL), "`year` must be a numeric value or vector of numeric values.")
})