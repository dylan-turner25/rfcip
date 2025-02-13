test_that("get_col_data year filter works", {
  obj <- get_col_data(year = 2020)
  expect_equal(unique(obj$commodity_year),2020)
})
