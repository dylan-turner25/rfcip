
test_that("valid_crop returns expected output", {
  expect_equal(valid_crop("corn"), T)
  expect_equal(valid_crop("not a crop"), F)
  expect_equal(valid_crop(41), T)
  expect_equal(valid_crop(123456789), F)
})

test_that("clean fips returns expected output", {
  expect_equal(clean_fips(fips = 1001), "01001")
  expect_equal(clean_fips(county = 1, state = 1), "01001")
})

test_that("valid_state returns expected output",{
  expect_equal(valid_state(51), 51)
  expect_equal(valid_state("KS"), "KS")
  expect_error(valid_state("XX"),"Parameter value for state not valid.")
})

test_that("get_sob_url also returns state when group_by county is selected", {
  expect_equal(grepl("ST=17",get_sob_url(year = 2023, 
                                         crop = "corn",
                                         insurance_plan = 1,
                                         state = "IL", 
                                         group_by = "county")), TRUE)
})


test_that("locate_livestock_links returns expected structure", {
  result <- locate_livestock_links()
  expect_s3_class(result, "data.frame")
  expect_true(all(c("url", "program", "year") %in% names(result)))
  
  expect_true("DRP" %in% result$program)
  expect_true("LGM" %in% result$program)
  expect_true("LRP" %in% result$program)
  
})