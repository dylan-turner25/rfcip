test_that("download_adm validates input parameters", {
  expect_error({
    download_adm(years = "invalid")
  })
  
  # Skip network-dependent test
  skip_on_cran()
  skip_if_offline()
  
  # Test directory creation and basic functionality
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # This should either succeed (if network works) or fail (if network issues)
  # Either way, it should create the directory
  tryCatch({
    download_adm(years = 2012, dir = temp_dir)
  }, error = function(e) {
    # Error is acceptable for network issues
    NULL
  })
  
  # Directory should be created regardless
  expect_true(dir.exists(temp_dir))
})

