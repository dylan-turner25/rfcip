
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

