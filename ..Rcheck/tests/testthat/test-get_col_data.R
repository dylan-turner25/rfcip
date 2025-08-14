test_that("get_col_data validates year parameter", {
  expect_error(get_col_data(year = "invalid"), "`year` must be a vector of numeric values.")
  expect_error(get_col_data(year = c("2020", "2021")), "`year` must be a vector of numeric values.")
  expect_error(get_col_data(year = NULL), "`year` must be a vector of numeric values.")
})