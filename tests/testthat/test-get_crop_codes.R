test_that("get_crop_codes filters work", {
  obj <- get_crop_codes(year = 2020,crop = c(41,81))
  expect_equal(as.numeric(unique(obj$commodity_year)),2020)
  expect_equal(nrow(obj),2)
  expect_equal(length(unique(obj$commodity_name)),2)
})

test_that("get_crop_codes returns all data when invalid crop is entered", {
  
  obj <- get_crop_codes(year = 2020,crop = 99999999)
  expect_equal(nrow(obj) > 1, TRUE)

  obj <- get_crop_codes(year = 2020,crop = "not a crop")
  expect_equal(nrow(obj) > 1, TRUE)
})

