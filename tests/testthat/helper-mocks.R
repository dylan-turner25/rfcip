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

# Create realistic mock cause of loss data
create_mock_col_data <- function() {
  data.frame(
    commodity_year = c("2023", "2023", "2023"),
    state_code = c("17", "17", "17"),
    state_abbrv = c("IL", "IL", "IL"),
    county_code = c("001", "003", "005"),
    county_name = c("Adams", "Alexander", "Bond"),
    commodity_code = c("41", "81", "91"),
    commodity_name = c("Corn", "Soybeans", "Sunflowers"),
    insurance_plan_code = c("2", "2", "2"),
    insurance_plan_abbrv = c("RP", "RP", "RP"),
    delivery_type = c("A", "A", "A"),
    stage_code = c("", "", ""),
    col_code = c("11", "12", "16"),
    col_name = c("Drought", "Excess Moisture/Precip/Rain", "Wind/Windstorm"),
    month_of_loss_code = c("7", "6", "8"),
    month_of_loss_name = c("July", "June", "August"),
    year_of_loss = c("2023", "2023", "2023"),
    policies_earning_prem = c("100", "85", "45"),
    policies_indemnified = c("25", "20", "12"),
    net_planted_qty = c("50000", "40000", "25000"),
    net_endorsed_acres = c("50000", "40000", "25000"),
    liability = c("15000000", "12000000", "7500000"),
    total_premium = c("750000", "600000", "375000"),
    producer_paid_premium = c("225000", "180000", "112500"),
    subsidy = c("525000", "420000", "262500"),
    state_subsidy = c("0", "0", "0"),
    addnl_subsidy = c("0", "0", "0"),
    efa_prem_discount = c("0", "0", "0"),
    indemnified_quantity = c("12500", "10000", "6250"),
    indem_amount = c("3750000", "3000000", "1875000"),
    loss_ratio = c("5.00", "5.00", "5.00"),
    stringsAsFactors = FALSE
  )
}

# Create mock URLs data for locate_col_links
create_mock_col_urls <- function() {
  data.frame(
    year = c(2020, 2021, 2022, 2023),
    url = c(
      "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/2020_cause-loss.zip",
      "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/2021_cause-loss.zip", 
      "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/2022_cause-loss.zip",
      "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/2023_cause-loss.zip"
    ),
    stringsAsFactors = FALSE
  )
}

# Create a temporary ZIP file with mock COL data
create_mock_col_zip_file <- function(data = create_mock_col_data()) {
  # Create temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # Write data to pipe-delimited text file (COL format)
  temp_txt <- file.path(temp_dir, "cause_of_loss_data.txt")
  write.table(data, temp_txt, sep = "|", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Create ZIP file
  temp_zip <- tempfile(fileext = ".zip")
  utils::zip(temp_zip, temp_txt, flags = "-j")
  
  # Clean up temp directory
  unlink(temp_dir, recursive = TRUE)
  
  return(temp_zip)
}

# Setup webmockr for COL locate_col_links HTML response
setup_col_links_mock <- function(mock_urls = create_mock_col_urls(), status_code = 200) {
  if (!requireNamespace("webmockr", quietly = TRUE)) {
    skip("webmockr not available")
  }
  
  # Create mock HTML response with download links
  html_content <- paste0(
    '<html><body>',
    paste0('<a href="', mock_urls$url, '">', basename(mock_urls$url), '</a>', collapse = '\n'),
    '</body></html>\n'
  )
  
  # Mock the index.html page
  webmockr::stub_request("get", "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/index.html") %>%
    webmockr::to_return(
      status = status_code,
      body = html_content,
      headers = list("content-type" = "text/html")
    )
}

# Setup webmockr for COL ZIP file downloads
setup_col_download_mock <- function(mock_data = create_mock_col_data(), status_code = 200) {
  if (!requireNamespace("webmockr", quietly = TRUE)) {
    skip("webmockr not available")
  }
  
  # Create mock ZIP file
  mock_zip <- create_mock_col_zip_file(mock_data)
  mock_zip_content <- readBin(mock_zip, "raw", file.info(mock_zip)$size)
  
  # Clean up temp ZIP file
  unlink(mock_zip)
  
  # Mock specific URL patterns for COL ZIP downloads
  # Pattern 1: cause-loss files
  webmockr::stub_request("get", "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/") %>%
    webmockr::wi_th(
      query = list(.pattern = TRUE)
    ) %>%
    webmockr::to_return(
      status = status_code,
      body = mock_zip_content,
      headers = list(
        "content-type" = "application/zip",
        "content-length" = as.character(length(mock_zip_content))
      )
    )
  
  # Also mock individual file downloads with full URLs
  for (year in c(2020, 2021, 2022, 2023, 2024)) {
    for (pattern in c("cause-loss", "colsom")) {
      url <- paste0("https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/", 
                    year, "_", pattern, ".zip")
      webmockr::stub_request("get", url) %>%
        webmockr::to_return(
          status = status_code,
          body = mock_zip_content,
          headers = list(
            "content-type" = "application/zip",
            "content-length" = as.character(length(mock_zip_content))
          )
        )
    }
  }
}

# Setup webmockr for COL error scenarios  
setup_col_links_mock_error <- function(status_code = 500, error_message = "Server Error") {
  if (!requireNamespace("webmockr", quietly = TRUE)) {
    skip("webmockr not available")
  }
  
  webmockr::stub_request("get", "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/index.html") %>%
    webmockr::to_return(
      status = status_code,
      body = error_message
    )
}

# Setup webmockr for COL download error scenarios
setup_col_download_mock_error <- function(status_code = 500, error_message = "Download Error") {
  if (!requireNamespace("webmockr", quietly = TRUE)) {
    skip("webmockr not available")
  }
  
  # Mock base URL pattern
  webmockr::stub_request("get", "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/") %>%
    webmockr::wi_th(query = list(.pattern = TRUE)) %>%
    webmockr::to_return(
      status = status_code,
      body = error_message
    )
  
  # Also mock individual file downloads with errors
  for (year in c(2020, 2021, 2022, 2023, 2024)) {
    for (pattern in c("cause-loss", "colsom")) {
      url <- paste0("https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/", 
                    year, "_", pattern, ".zip")
      webmockr::stub_request("get", url) %>%
        webmockr::to_return(
          status = status_code,
          body = error_message
        )
    }
  }
}

# Create realistic mock insurance plan codes data
create_mock_insurance_plan_data <- function() {
  data.frame(
    commodity_year = c("2023", "2023", "2023", "2023", "2023"),
    insurance_plan_code = c("1", "2", "3", "4", "14"),
    insurance_plan = c("APH", "Revenue Protection", "Yield Protection", "Revenue Protection with Harvest Price Exclusion", "Enhanced Coverage Option"),
    insurance_plan_abbrv = c("APH", "RP", "YP", "RP-HPE", "ECO"),
    stringsAsFactors = FALSE
  )
}

# Create mock insurance plan data with x2 header (tests skip=1 logic)
create_mock_insurance_plan_data_with_x2_header <- function() {
  data.frame(
    x1 = c("Header", "2023", "2023", "2023"),
    x2 = c("Row", "1", "2", "3"), 
    x3 = c("To", "APH", "Revenue Protection", "Yield Protection"),
    x4 = c("Skip", "APH", "RP", "YP"),
    stringsAsFactors = FALSE
  )
}

# Setup webmockr for insurance plan codes URL patterns
setup_insurance_plan_codes_url_mock <- function(mock_data = create_mock_insurance_plan_data(), status_code = 200) {
  if (!requireNamespace("webmockr", quietly = TRUE)) {
    skip("webmockr not available")
  }
  
  # Create temporary Excel file with mock data
  mock_file <- create_mock_excel_file(mock_data)
  mock_file_content <- readBin(mock_file, "raw", file.info(mock_file)$size)
  
  # Clean up temp file
  unlink(mock_file)
  
  # Mock the insurance plan codes URL pattern using same approach as crop codes
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

# Setup webmockr for insurance plan codes error scenarios
setup_insurance_plan_codes_url_mock_error <- function(status_code = 500, error_message = "Server Error") {
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