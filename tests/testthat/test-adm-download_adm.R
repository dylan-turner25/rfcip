test_that("download_adm validates input parameters", {
  expect_error({
    download_adm(years = "invalid")
  })
})

