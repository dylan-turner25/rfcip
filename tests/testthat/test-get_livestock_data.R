test_that("get_livestock_data input validation works", {
  # Test invalid year input
  expect_error(get_livestock_data(year = "2020"), "must be a vector of numeric values")
  
  # Test invalid program input 
  expect_error(get_livestock_data(program = "INVALID"), "must be one or more of")
  
  # Test multiple programs input
  expect_error(get_livestock_data(program = c("LRP", "DRP")), "Only one program can be specified")
})




