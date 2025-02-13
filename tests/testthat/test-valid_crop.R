
test_that("valid crop returns expected output", {
  expect_equal(valid_crop("corn"), T)
  expect_equal(valid_crop("not a crop"), F)
  expect_equal(valid_crop(41), T)
  expect_equal(valid_crop(123456789), F)
})
