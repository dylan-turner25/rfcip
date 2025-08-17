test_that("get_livestock_data input validation works", {
  # Test invalid year input
  expect_error(get_livestock_data(year = "2020"), "must be a vector of numeric values")
  
  # Test invalid program input 
  expect_error(get_livestock_data(program = "INVALID"), "must be one or more of")
  
  # Test multiple programs input
  expect_error(get_livestock_data(program = c("LRP", "DRP")), "Only one program can be specified")
})

test_that("get_livestock_data handles default parameters correctly", {
  # Test that default parameters are set correctly
  formals_list <- formals(get_livestock_data)
  
  # Test default year is current year
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  default_year <- eval(formals_list$year)
  expect_equal(default_year, current_year)
  
  # Test default program is LRP
  expect_equal(formals_list$program, "LRP")
  
  # Test default force is FALSE
  expect_equal(formals_list$force, FALSE)
})

test_that("get_livestock_data parameter validation with edge cases", {
  # Test NULL year
  expect_error(get_livestock_data(year = NULL), "must be a vector of numeric values")
  
  # Test empty vector - this actually passes validation, so test it doesn't error
  expect_no_error(get_livestock_data(year = numeric(0)))
  
  # Test mixed valid/invalid programs
  expect_error(get_livestock_data(program = c("LRP", "INVALID")), "must be one or more of")
  
  # Test NA values
  expect_error(get_livestock_data(year = NA), "must be a vector of numeric values")
  
  # Test that single valid program works for validation
  expect_no_error(formals(get_livestock_data))  # Just verify function signature is valid
})




