pkgname <- "rfcip"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rfcip')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("FCIP_FORCE_NUMERIC_KEYS")
### * FCIP_FORCE_NUMERIC_KEYS

flush(stderr()); flush(stdout())

### Name: FCIP_FORCE_NUMERIC_KEYS
### Title: Column names to coerce to numeric
### Aliases: FCIP_FORCE_NUMERIC_KEYS
### Keywords: datasets

### ** Examples

## Not run: 
##D # View default keys
##D FCIP_FORCE_NUMERIC_KEYS
## End(Not run)




cleanEx()
nameEx("FCIP_INSURANCE_ELECTION")
### * FCIP_INSURANCE_ELECTION

flush(stderr()); flush(stdout())

### Name: FCIP_INSURANCE_ELECTION
### Title: Insurance election identifier fields
### Aliases: FCIP_INSURANCE_ELECTION
### Keywords: datasets

### ** Examples

## Not run: 
##D # Default election fields
##D FCIP_INSURANCE_ELECTION
## End(Not run)



cleanEx()
nameEx("FCIP_INSURANCE_ELECTION_RCODED")
### * FCIP_INSURANCE_ELECTION_RCODED

flush(stderr()); flush(stdout())

### Name: FCIP_INSURANCE_ELECTION_RCODED
### Title: Insurance election identifier fields (recoded)
### Aliases: FCIP_INSURANCE_ELECTION_RCODED
### Keywords: datasets

### ** Examples

## Not run: 
##D # View the default recoded election fields
##D FCIP_INSURANCE_ELECTION_RCODED
##D 
## End(Not run)



cleanEx()
nameEx("FCIP_INSURANCE_POOL")
### * FCIP_INSURANCE_POOL

flush(stderr()); flush(stdout())

### Name: FCIP_INSURANCE_POOL
### Title: Insurance pool identifier fields
### Aliases: FCIP_INSURANCE_POOL
### Keywords: datasets

### ** Examples

## Not run: 
##D # Default insurance pool fields
##D FCIP_INSURANCE_POOL
## End(Not run)



cleanEx()
nameEx("clean_adm_data")
### * clean_adm_data

flush(stderr()); flush(stdout())

### Name: clean_adm_data
### Title: Apply standardized data cleaning opperations
### Aliases: clean_adm_data

### ** Examples

## Not run: clean_adm_data(df)



cleanEx()
nameEx("clean_adm_file_name")
### * clean_adm_file_name

flush(stderr()); flush(stdout())

### Name: clean_adm_file_name
### Title: Clean the file name
### Aliases: clean_adm_file_name

### ** Examples

## Not run: clean_adm_file_name("./data-raw/2012_A01100_YieldAndTyield_YTD.txt")



cleanEx()
nameEx("clear_rfcip_cache")
### * clear_rfcip_cache

flush(stderr()); flush(stdout())

### Name: clear_rfcip_cache
### Title: Clear the package cache of downloaded data files
### Aliases: clear_rfcip_cache

### ** Examples

## Not run: 
##D # Remove all cached data files so they will be re-downloaded on next use
##D clear_rfcip_cache()
## End(Not run)



cleanEx()
nameEx("get_adm_data")
### * get_adm_data

flush(stderr()); flush(stdout())

### Name: get_adm_data
### Title: Get USDA RMA Actuarial Data Master (ADM) data
### Aliases: get_adm_data

### ** Examples

## Not run: 
##D # Get premium data for 2020
##D price_2020 <- get_adm_data(year = 2020, dataset = "price")
##D 
##D # Get subsidy data for 2018
##D combo_2018 <- get_adm_data(year = 2018, dataset = "comborevenuefactor")
##D 
##D # Get baserate data for multiple years
##D baserate_multi <- get_adm_data(year = c(2018, 2019, 2020), dataset = "baserate")
## End(Not run)




cleanEx()
nameEx("get_adm_file_info")
### * get_adm_file_info

flush(stderr()); flush(stdout())

### Name: get_adm_file_info
### Title: Get File Information from a Directory
### Aliases: get_adm_file_info

### ** Examples

## Not run: 
##D get_adm_file_info()
##D get_adm_file_info(directory = "./my-data", file_suffix = ".csv")
## End(Not run)




cleanEx()
nameEx("get_col_data")
### * get_col_data

flush(stderr()); flush(stdout())

### Name: get_col_data
### Title: Builds a cause of loss data set using data from the specified
###   years
### Aliases: get_col_data

### ** Examples

## Not run: 
##D get_col_data(year = 2020)
##D get_col_data(year = 2019:2021)
## End(Not run)



cleanEx()
nameEx("get_crop_codes")
### * get_crop_codes

flush(stderr()); flush(stdout())

### Name: get_crop_codes
### Title: Lookup crop codes for FCIP commodities
### Aliases: get_crop_codes

### ** Examples

## Not run: 
##D get_crop_codes(year = 2023, comm = "corn")
##D get_crop_codes(year = 2024, comm = 41)
##D get_crop_codes(crop = c("corn", "SOYbEaNs"))
##D get_crop_codes(crop = c(41, 81))
##D get_crop_codes()
## End(Not run)




cleanEx()
nameEx("get_insurance_plan_codes")
### * get_insurance_plan_codes

flush(stderr()); flush(stdout())

### Name: get_insurance_plan_codes
### Title: Lookup insurance plan codes for FCIP insurance plans
### Aliases: get_insurance_plan_codes

### ** Examples

## Not run: 
##D get_insurance_plan_codes()
##D get_insurance_plan_codes(year = 2023, plan = "yp")
##D get_insurance_plan_codes(year = 2023, plan = 1)
##D get_insurance_plan_codes(year = 2023, plan = "yield protection")
##D get_insurance_plan_codes(year = 2018:2022, plan = c("yp", "rp"))
## End(Not run)



cleanEx()
nameEx("get_livestock_data")
### * get_livestock_data

flush(stderr()); flush(stdout())

### Name: get_livestock_data
### Title: Get Livestock Insurance Data from the USDA Risk Management
###   Agency
### Aliases: get_livestock_data

### ** Examples

## Not run: 
##D # Get data for the current year only
##D current_year_data <- get_livestock_data(year = as.numeric(format(Sys.Date(), "%Y")))
##D 
##D # Get LRP data for 2020-2022
##D lrp_data <- get_livestock_data(year = 2020:2022, program = "LRP")
##D 
##D # Get all programs for 2021
##D all_programs_2021 <- get_livestock_data(year = 2021, program = c("DRP", "LGM", "LRP"))
## End(Not run)



cleanEx()
nameEx("get_price_data")
### * get_price_data

flush(stderr()); flush(stdout())

### Name: get_price_data
### Title: Download RMA projected and harvest prices used for revenue
###   protection insurance plans.
### Aliases: get_price_data

### ** Examples

## Not run: get_price_data(2024,"corn")
## Not run: get_price_data(year = 2022:2024, state = "VA")



cleanEx()
nameEx("get_sob_data")
### * get_sob_data

flush(stderr()); flush(stdout())

### Name: get_sob_data
### Title: Downloads and imports data from RMA's summary of buisness app
### Aliases: get_sob_data

### ** Examples

## Not run: 
##D get_sob_data(year = 2023)
##D get_sob_data(year = 2015:2020, crop = "corn")
##D get_sob_data(year = 2022, crop = c(41, 81), group_by = "state")
## End(Not run)



cleanEx()
nameEx("list_data_assets")
### * list_data_assets

flush(stderr()); flush(stdout())

### Name: list_data_assets
### Title: List asset names from the latest GitHub release
### Aliases: list_data_assets
### Keywords: internal

### ** Examples

## Not run: 
##D files = list_data_assets()
## End(Not run)



cleanEx()
nameEx("locate_adm_download_link")
### * locate_adm_download_link

flush(stderr()); flush(stdout())

### Name: locate_adm_download_link
### Title: Locate the download link for the actuarial data master
### Aliases: locate_adm_download_link

### ** Examples

## Not run: locate_adm_download_link(year = 2012)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
