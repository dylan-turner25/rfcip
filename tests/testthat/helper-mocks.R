# Helper functions for creating mock data and setting up webmockr

# Create realistic mock SOB data that matches the expected structure
create_mock_sob_data <- function() {
  data.frame(
    commodity_year = c(2023, 2023, 2023),
    commodity_code = c("41", "81", "91"), 
    commodity_name = c("Corn", "Soybeans", "Sunflowers"),
    state_code = c("17", "17", "17"),
    state_abbrv = c("IL", "IL", "IL"),
    policies_sold = c(1000, 800, 200),
    policies_earning_prem = c(950, 750, 180),
    policies_indemnified = c(100, 75, 20),
    units_earning_prem = c(1200, 900, 220),
    units_indemnified = c(120, 90, 25),
    quantity = c(150000, 120000, 30000),
    quantity_type = c("Acres", "Acres", "Acres"),
    liabilities = c(85000000, 70000000, 15000000),
    total_prem = c(3500000, 2800000, 650000),
    subsidy = c(2100000, 1680000, 390000),
    indemnity = c(2500000, 2000000, 500000),
    loss_ratio = c(0.71, 0.71, 0.77),
    insurance_plan_code = c("2", "2", "2"),
    insurance_plan_abbrv = c("RP", "RP", "RP"),
    stringsAsFactors = FALSE
  )
}

# Create mock data with x2 header (tests skip=1 logic)
create_mock_sob_data_with_x2_header <- function() {
  # Create data with x2 in the second column name to trigger skip=1 logic
  data.frame(
    x1 = c("Header", "2023", "2023"),
    x2 = c("Row", "41", "81"), 
    x3 = c("To", "Corn", "Soybeans"),
    x4 = c("Skip", "17", "17"),
    x5 = c("Extra", "1000", "800"),
    stringsAsFactors = FALSE
  )
}

# Create a temporary Excel file with mock data
create_mock_excel_file <- function(data = create_mock_sob_data()) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    skip("writexl not available for creating mock Excel files")
  }
  
  temp_file <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data, temp_file)
  return(temp_file)
}

# Setup webmockr for SOB URL patterns
setup_sob_url_mock <- function(mock_data = create_mock_sob_data(), status_code = 200) {
  if (!requireNamespace("webmockr", quietly = TRUE)) {
    skip("webmockr not available")
  }
  
  # Create temporary Excel file with mock data
  mock_file <- create_mock_excel_file(mock_data)
  mock_file_content <- readBin(mock_file, "raw", file.info(mock_file)$size)
  
  # Clean up temp file
  unlink(mock_file)
  
  # Mock the SOB app URL pattern
  webmockr::stub_request("get", "https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator") %>%
    webmockr::wi_th(
      query = list(.pattern = TRUE)  # Match any query parameters
    ) %>%
    webmockr::to_return(
      status = status_code,
      body = mock_file_content,
      headers = list(
        "content-type" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        "content-length" = as.character(length(mock_file_content))
      )
    )
}

# Setup webmockr for error scenarios
setup_sob_url_mock_error <- function(status_code = 500, error_message = "Server Error") {
  if (!requireNamespace("webmockr", quietly = TRUE)) {
    skip("webmockr not available")
  }
  
  webmockr::stub_request("get", "https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator") %>%
    webmockr::wi_th(query = list(.pattern = TRUE)) %>%
    webmockr::to_return(
      status = status_code,
      body = error_message
    )
}

# Clean up all webmockr stubs
cleanup_webmockr <- function() {
  if (requireNamespace("webmockr", quietly = TRUE)) {
    webmockr::stub_registry_clear()
  }
}

# Create realistic mock crop codes data
create_mock_crop_data <- function() {
  data.frame(
    commodity_year = c("2023", "2023", "2023", "2023", "2023"),
    commodity_code = c("41", "81", "91", "28", "21"),
    commodity_name = c("Corn", "Soybeans", "Sunflowers", "Almonds", "Cotton"),
    stringsAsFactors = FALSE
  )
}

# Create mock crop data with x2 header (tests skip=1 logic)
create_mock_crop_data_with_x2_header <- function() {
  data.frame(
    x1 = c("Header", "2023", "2023", "2023"),
    x2 = c("Row", "41", "81", "91"), 
    x3 = c("To", "Corn", "Soybeans", "Sunflowers"),
    stringsAsFactors = FALSE
  )
}

# Setup webmockr for crop codes URL patterns
setup_crop_codes_url_mock <- function(mock_data = create_mock_crop_data(), status_code = 200) {
  if (!requireNamespace("webmockr", quietly = TRUE)) {
    skip("webmockr not available")
  }
  
  # Create temporary Excel file with mock data
  mock_file <- create_mock_excel_file(mock_data)
  mock_file_content <- readBin(mock_file, "raw", file.info(mock_file)$size)
  
  # Clean up temp file
  unlink(mock_file)
  
  # Mock the crop codes URL pattern using same approach as SOB
  stub <- webmockr::stub_request("get", "https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel")
  stub <- webmockr::wi_th(stub, query = list(.pattern = TRUE))
  webmockr::to_return(stub,
    status = status_code,
    body = mock_file_content,
    headers = list(
      "content-type" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      "content-length" = as.character(length(mock_file_content))
    )
  )
}

# Setup webmockr for crop codes error scenarios
setup_crop_codes_url_mock_error <- function(status_code = 500, error_message = "Server Error") {
  if (!requireNamespace("webmockr", quietly = TRUE)) {
    skip("webmockr not available")
  }
  
  stub <- webmockr::stub_request("get", "https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator/ExportToExcel")
  stub <- webmockr::wi_th(stub, query = list(.pattern = TRUE))
  webmockr::to_return(stub,
    status = status_code,
    body = error_message
  )
}