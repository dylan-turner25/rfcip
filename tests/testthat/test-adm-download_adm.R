test_that("download_adm validates input parameters", {
  expect_error({
    download_adm(years = "invalid")
  })
  
  # Skip network-dependent test
  skip_on_cran()
  skip_if_offline()
  
  # Test only basic directory creation without actual download
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # This should create directory but fail on network call (which is expected)
  expect_error({
    download_adm(years = 2012, dir = temp_dir)
  })
  
  # Directory should still be created
  expect_true(dir.exists(temp_dir))
})

