
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

test_that("get_sob_url also returns state when group_by county is selected", {
  # Mock the two lookups so this URL-construction test does not hit RMA's servers
  # (get_crop_codes uses httr; get_insurance_plan_codes uses download.file).
  with_mocked_bindings(
    get_crop_codes = function(...) data.frame(commodity_code = "0041", stringsAsFactors = FALSE),
    get_insurance_plan_codes = function(...) data.frame(insurance_plan_code = "1", stringsAsFactors = FALSE),
    {
      expect_equal(grepl("ST=17", get_sob_url(year = 2023,
                                              crop = "corn",
                                              insurance_plan = 1,
                                              state = "IL",
                                              group_by = "county")), TRUE)
    }
  )
})

test_that("get_sob_url does not duplicate ORD codes when a filter and group_by overlap", {
  # a state filter adds ST to ORD; group_by county must not add it again
  # (RMA's server returns a 500 on ORD=CY,ST,ST,CT)
  url <- get_sob_url(year = 2023, crop = NULL, state = "IL", group_by = "county")
  expect_true(grepl("ORD=CY,ST,CT&", url, fixed = TRUE))

  url <- get_sob_url(year = 2023, crop = NULL, state = "IL", group_by = c("state", "county"))
  expect_true(grepl("ORD=CY,ST,CT&", url, fixed = TRUE))
})

test_that("get_sob_url treats group_by fips as state plus county", {
  url <- get_sob_url(year = 2023, crop = NULL, group_by = "fips")
  expect_true(grepl("ORD=CY,ST,CT&", url, fixed = TRUE))
})

test_that("get_sob_url rejects invalid group_by values", {
  # an unknown value used to produce a trailing comma in ORD and a server 500
  expect_error(
    get_sob_url(year = 2023, crop = NULL, group_by = "not_a_column"),
    "Invalid group_by value"
  )
})

test_that("get_sob_url puts delivery_type in the DT parameter, not ST", {
  url <- get_sob_url(year = 2023, crop = NULL, delivery_type = "RBUP")
  expect_true(grepl("DT=RBUP&", url, fixed = TRUE))
  expect_false(grepl("ST=RBUP", url, fixed = TRUE))
})

test_that("include_and works correctly", {
  expect_equal(include_and("http://example.com?"), "http://example.com?")
  expect_equal(include_and("http://example.com"), "http://example.com&")
  expect_equal(include_and("http://example.com/path"), "http://example.com/path&")
})

test_that("is_numeric_convertible correctly identifies convertible values", {
  # Should be convertible
  expect_true(is_numeric_convertible(c("1", "2", "3")))
  expect_true(is_numeric_convertible(c("1.5", "2.0", "3.14")))
  expect_true(is_numeric_convertible(c(1, 2, 3)))
  
  # Should not be convertible - text values
  expect_false(is_numeric_convertible(c("apple", "banana", "cherry")))
  expect_false(is_numeric_convertible(c("1", "2", "text")))
  
  # Should not be convertible - zero-padded codes
  expect_false(is_numeric_convertible(c("01", "02", "03")))
  expect_false(is_numeric_convertible(c("001", "002", "003")))
  
  # Should not be convertible - domain-specific codes
  expect_false(is_numeric_convertible(c("1", "2", "3"), "commodity_code"))
  expect_false(is_numeric_convertible(c("10", "20", "30"), "state_code"))
  expect_false(is_numeric_convertible(c("1", "2"), "insurance_plan_code"))
})


