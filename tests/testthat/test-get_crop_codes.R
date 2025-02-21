test_that("get_crop_codes filters work", {
  obj <- get_crop_codes(year = 2020,crop = c(41,81))
  expect_equal(as.numeric(unique(obj$commodity_year)),2020)
  expect_equal(nrow(obj),2)
  expect_equal(length(unique(obj$commodity_name)),2)
})
