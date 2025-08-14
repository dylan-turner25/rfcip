test_that("filter_earliest_trends works correctly", {
  # Create sample data
  sample_data <- data.table::data.table(
    FCIP_INSURANCE_POOL = c("A", "A", "B", "B", "C"),
    yield_year = c(2020, 2021, 2020, 2022, 2023),
    commodity_year = c(2023, 2023, 2023, 2023, 2023),
    yield_amount = c(100, 110, 90, 95, 120)
  )
  
  result <- filter_earliest_trends(sample_data)
  
  # Should keep only the earliest trends (minimum age) for each pool
  expect_true("age" %in% names(result))
  expect_true("age_min" %in% names(result))
  
  # Pool A should keep 2020 (age = -3), Pool B should keep 2020 (age = -3), Pool C should keep 2023 (age = 0)
  expect_equal(nrow(result), 3)
  expect_true(all(result$age == result$age_min))
})

test_that("reconcile_yield_history processes data correctly", {
  # Create sample data with different yield years
  sample_data <- data.table::data.table(
    FCIP_INSURANCE_POOL = c("A", "B", "A", "B"),
    yield_year = c(2020, 2020, 2021, 2021),
    commodity_year = c(2023, 2023, 2024, 2024),
    yield_amount = c(100, 90, 110, 95),
    trended_yield_amount = c(105, 95, 115, 100),
    detrended_yield_amount = c(98, 88, 108, 93)
  )
  
  result <- reconcile_yield_history(sample_data)
  
  # Should have one row per pool per unique yield year
  expect_true(nrow(result) >= 2)
  expect_true(all(c("FCIP_INSURANCE_POOL", "commodity_year", "yield_amount", 
                   "trended_yield_amount", "detrended_yield_amount") %in% names(result)))
  
  # commodity_year should be set to the yield_year being processed
  expect_true(all(result$commodity_year %in% c(2020, 2021)))
})