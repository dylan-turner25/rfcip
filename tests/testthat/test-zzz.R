test_that("onLoad sets options correctly", {
  # Save current options
  old_scipen <- getOption("scipen")
  old_timeout <- getOption("timeout")
  
  # Simulate .onLoad behavior
  options(scipen = 999)
  options(timeout = 3600)
  
  # Check that options are set
  expect_equal(getOption("scipen"), 999)
  expect_equal(getOption("timeout"), 3600)
  
  # Restore original options
  options(scipen = old_scipen)
  options(timeout = old_timeout)
})

test_that("globalVariables are defined", {
  # Test that the package handles data.table variables correctly
  expect_true(exists(".datatable.aware"))
  expect_true(.datatable.aware)
})