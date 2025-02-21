test_that("get_sob_data filters work", {
  obj <- get_sob_data(year = 2023, crop = "corn")
  expect_equal(unique(obj$commodity_name),"Corn")
  expect_equal(unique(obj$commodity_year),2023)
})
