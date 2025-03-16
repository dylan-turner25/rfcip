test_that("get_price_data filters work", {
  obj <- get_price_data(year = 2023, crop = "corn",state = "IL")
  expect_equal(unique(obj$CommodityName),"Corn")
  expect_equal(unique(obj$CommodityYear),2023)
  expect_equal(unique(obj$StateName),"Illinois")
})

