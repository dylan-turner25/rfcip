test_that("get_price_data parameter processing works", {
  # Test state parameter validation (should not error)
  expect_silent(valid_state("IL"))
  expect_silent(valid_state("Illinois"))
  expect_silent(valid_state(17))
  
  # Test state parameter errors
  expect_error(valid_state("XX"), "Parameter value for state not valid.")
  expect_error(valid_state("InvalidState"), "Parameter value for state not valid.")
})