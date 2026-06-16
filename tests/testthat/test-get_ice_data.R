test_that("get_ice_data returns metadata when requested", {
  # Skip if no internet connection
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test metadata functionality
  result <- skip_if_rma_unreachable(get_ice_data(years = 2024, dataset = "metadata"))

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Check expected columns are present
  expected_cols <- c("year", "filename", "description", "size_bytes", "size_mb", "date", "time", "datetime", "url")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check that year column matches request
  expect_true(all(result$year == 2024))
  
  # Check that filenames contain expected pattern (some files have YTD suffix, some don't)
  expect_true(all(grepl("2024_D\\d{5}_.*\\.txt", result$filename)))
})

test_that("get_ice_data handles multiple years for metadata", {
  # Skip if no internet connection
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test with multiple years
  result <- skip_if_rma_unreachable(get_ice_data(years = c(2023, 2024), dataset = "metadata"))

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Should have data for both years
  years_present <- unique(result$year)
  expect_true(2023 %in% years_present)
  expect_true(2024 %in% years_present)
})

test_that("get_ice_data validates input parameters", {
  # Test missing years parameter
  expect_error(get_ice_data(years = NULL), "years parameter is required")
})

test_that("get_ice_data handles non-existent years gracefully", {
  # Skip if no internet connection  
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test with a year that likely doesn't exist
  result <- get_ice_data(years = 1900, dataset = "metadata")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)  # Should return empty data frame
})

test_that("locate_ice_download_links works correctly", {
  # Skip if no internet connection
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test the helper function directly
  result <- skip_if_rma_unreachable(rfcip:::locate_ice_download_links(year = 2024))

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Check expected columns
  expected_cols <- c("year", "filename", "description", "size_bytes", "size_mb", "date", "time", "datetime", "url")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check that URLs are properly formed
  expect_true(all(grepl("^https://", result$url)))
  expect_true(all(grepl("2024", result$url)))
})

test_that("extract_ice_description formats names correctly", {
  # Test the description extraction function
  test_cases <- list(
    "2024_D00062_IceLegalDescription_YTD.txt" = "Ice Legal Description",
    "2024_D00092_IceInsurancePlanCommodityUnitStructure_YTD.txt" = "Ice Insurance Plan Commodity Unit Structure", 
    "2024_D00018_IceLrrFundCutoff_YTD.txt" = "Ice Lrr Fund Cutoff"
  )
  
  for (filename in names(test_cases)) {
    expected <- test_cases[[filename]]
    result <- rfcip:::extract_ice_description(filename)
    expect_equal(result, expected, info = paste("Failed for filename:", filename))
  }
})

# Skip the actual data download tests by default since they're large files
test_that("get_ice_data can download actual data (SLOW TEST - SKIP BY DEFAULT)", {
  skip("Skipping slow data download test by default")
  skip_if_not(curl::has_internet(), "No internet connection")
  
  # Test downloading a small subset of data
  # This would be enabled for thorough testing but skipped for routine tests
  result <- get_ice_data(years = 2024, dataset = "IceLegalDescription")
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("caching system works correctly", {
  # This test would verify that files are cached properly
  # For now, just check that cache directory can be created
  cache_dir <- tools::R_user_dir("rfcip", which = "cache")
  expect_true(dir.exists(cache_dir) || dir.create(cache_dir, recursive = TRUE))
})

# ---------------------------------------------------------------------------
# AIP (Approved Insurance Providers) panel
# ---------------------------------------------------------------------------

test_that("clean_aip_data drops AA/Test/deleted rows and keeps the panel columns", {
  # Simulate what clean_ice_data() produces: read_delim coerces the literal
  # string "NA" to a missing value, so the NAU Country row has aip_code = NA
  raw <- data.frame(
    reinsurance_year = rep(2025, 5),
    record_type_code = rep("D00100", 5),
    aip_code = c("AA", "CM", NA, "RH", "ZZ"),
    aip_name = c(
      "Test Insurance Provider",
      "Country Mutual Insurance Company",
      "NAU Country Insurance Company",
      "Ace American Insurance Company",
      "Exited Provider Company"
    ),
    released_date = as.Date("2026-04-15"),
    last_released_date = as.Date("2026-02-24"),
    deleted_date = c(NA, NA, NA, NA, as.Date("2026-06-01")),
    stringsAsFactors = FALSE
  )

  cleaned <- rfcip:::clean_aip_data(raw)

  expect_equal(names(cleaned), c("reinsurance_year", "aip_code", "aip_name"))
  # AA placeholder, Test name, and deleted ZZ row all removed
  expect_setequal(cleaned$aip_code, c("CM", "NA", "RH"))
  # The valid "NA" code is restored from the coerced missing value
  expect_true("NA" %in% cleaned$aip_code)
  expect_equal(cleaned$aip_name[cleaned$aip_code == "NA"], "NAU Country Insurance Company")
})

test_that("load_aip_pre2011 reads, filters, and handles missing/blank files", {
  # Custom curated CSV with a comment line and the literal "NA" code
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(
    "# curated test roster",
    "reinsurance_year,aip_code,aip_name",
    "2008,NA,NAU Country Insurance Company",
    "2008,RH,Ace Property and Casualty Ins. Co.",
    "2009,RH,Ace Property and Casualty Ins. Co."
  ), tmp)

  res_2008 <- rfcip:::load_aip_pre2011(2008, csv_path = tmp)
  expect_equal(names(res_2008), c("reinsurance_year", "aip_code", "aip_name"))
  expect_equal(nrow(res_2008), 2)
  expect_true("NA" %in% res_2008$aip_code)  # literal code preserved (na = "")

  # Year filtering
  res_2009 <- rfcip:::load_aip_pre2011(2009, csv_path = tmp)
  expect_equal(nrow(res_2009), 1)
  expect_equal(res_2009$aip_code, "RH")

  # Missing file -> empty frame with correct columns
  res_missing <- rfcip:::load_aip_pre2011(2008, csv_path = tempfile(fileext = ".csv"))
  expect_equal(nrow(res_missing), 0)
  expect_equal(names(res_missing), c("reinsurance_year", "aip_code", "aip_name"))

  # Shipped blank template -> empty
  res_blank <- rfcip:::load_aip_pre2011(2008)
  expect_equal(nrow(res_blank), 0)
})

test_that("get_aip_panel merges curated rows with PASS precedence and dedups", {
  fake_pass <- data.frame(
    reinsurance_year = c(2011, 2011),
    aip_code = c("RH", "WN"),
    aip_name = c("PASS Rain & Hail", "PASS ARMtech"),
    stringsAsFactors = FALSE
  )
  fake_curated <- data.frame(
    reinsurance_year = c(2008, 2008, 2011),
    aip_code = c("RH", "ST", "RH"),  # 2011/RH overlaps PASS -> PASS wins
    aip_name = c("Curated RH 2008", "Curated Stonington 2008", "Curated RH 2011"),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    get_ice_data_single_year = function(year, ...) fake_pass[fake_pass$reinsurance_year == year, ],
    load_aip_pre2011 = function(years, ...) fake_curated[fake_curated$reinsurance_year %in% years, ],
    {
      result <- rfcip:::get_aip_panel(
        years = c(2008, 2011),
        dataset = "IceAip",
        ice_url = "x",
        show_progress = FALSE,
        force = FALSE
      )
    }
  )

  # 2008 from curated (RH, ST) + 2011 from PASS (RH, WN); curated 2011/RH dropped
  expect_equal(nrow(result), 4)
  expect_setequal(paste(result$reinsurance_year, result$aip_code),
                  c("2008 RH", "2008 ST", "2011 RH", "2011 WN"))
  # PASS precedence for the overlapping 2011/RH pair
  expect_equal(result$aip_name[result$reinsurance_year == 2011 & result$aip_code == "RH"],
               "PASS Rain & Hail")
  # Stable ordering by year then code
  expect_equal(result$reinsurance_year, c(2008, 2008, 2011, 2011))
})

test_that("get_ice_data(dataset = 'IceAip') returns the cleaned 12-provider 2025 panel", {
  fixture <- create_mock_aip_file(2025)
  on.exit(unlink(fixture), add = TRUE)

  # Mock utils::download.file to deliver the fixture, then mock the rfcip-side
  # link locator and cache writer so the test stays offline and side-effect free
  with_mocked_bindings(
    download.file = create_aip_download_mock(fixture),
    .package = "utils",
    {
      with_mocked_bindings(
        locate_ice_download_links = function(year, ice_url) create_mock_aip_links(year),
        cache_processed_data = function(...) invisible(NULL),
        {
          result <- get_ice_data(years = 2025, dataset = "IceAip",
                                 show_progress = FALSE, force = TRUE)
        }
      )
    }
  )

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("reinsurance_year", "aip_code", "aip_name"))
  expect_equal(nrow(result), 12)
  expect_setequal(
    result$aip_code,
    c("CM", "CP", "EF", "FA", "FH", "GA", "HU", "NA", "PL", "PS", "RH", "WN")
  )
  # NA code (NAU Country) survives the read coercion gotcha
  expect_equal(result$aip_name[result$aip_code == "NA"], "NAU Country Insurance Company")
})

# Live end-to-end check, kept out of the default fast suite like the slow ICE test
test_that("get_ice_data(dataset = 'IceAip') 2025 has 12 providers (LIVE - SKIP BY DEFAULT)", {
  skip("Skipping live AIP download test by default")
  skip_if_not(curl::has_internet(), "No internet connection")

  result <- get_ice_data(years = 2025, dataset = "IceAip", force = TRUE)
  expect_equal(nrow(result), 12)
  expect_setequal(
    result$aip_code,
    c("CM", "CP", "EF", "FA", "FH", "GA", "HU", "NA", "PL", "PS", "RH", "WN")
  )
})