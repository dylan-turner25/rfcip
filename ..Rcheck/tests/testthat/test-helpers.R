
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
  expect_equal(grepl("ST=17",get_sob_url(year = 2023, 
                                         crop = "corn",
                                         insurance_plan = 1,
                                         state = "IL", 
                                         group_by = "county")), TRUE)
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


