rfcip (R FCIP)
================

- [Introduction](#introduction)
- [Installation](#installation)
- [Supported Data Sources](#supported-data-sources)
  - [Summary of Business](#summary-of-business)
    - [Summary of Business by Type, Practice, and Unit
      Structure](#summary-of-business-by-type-practice-and-unit-structure)
  - [Cause of Loss Files](#cause-of-loss-files)
  - [Price](#price)
  - [Reinsurance Agreements](#reinsurance-agreements)
    - [Standard Reinsurance Agreement](#standard-reinsurance-agreement)
    - [Livestock Price Reinsurance
      Agreement](#livestock-price-reinsurance-agreement)
  - [Livestock and Dairy Participation
    Reports](#livestock-and-dairy-participation-reports)

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R-CMD-check](https://github.com/dylan-turner25/rfcip/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dylan-turner25/rfcip/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![codecov](https://codecov.io/gh/dylan-turner25/rfcip/graph/badge.svg?token=6sJUenko4X)](https://codecov.io/gh/dylan-turner25/rfcip)

# Introduction

`rfcip` provides a set of tools to allow users to access publicly
available data related to the Federal Crop Insurance Program. The
package provides a set of functions to easily navigate and access data
that is publicly available, but otherwise scattered across different
urls, files, and data portals. Although no official API exists for
Federal Crop Insurance Data, much of the data can be located with a
structured url meaning the `rfcip` package effectively functions like a
defacto API wrapper.

**Disclaimer:** This product uses data provided by the USDA, but is not
endorsed by or affiliated with USDA or the Federal Government.

# Installation

`rfcip` can be installed directly from github using
`remotes::install_github("https://github.com/dylan-turner25/rfcip")`

# Supported Data Sources

## Summary of Business

The [Summary of
Business](https://www.rma.usda.gov/tools-reports/summary-of-business)
files produced by the USDA Risk Management Agency contains crop
insurance participation measures and outcomes by state, county, crop,
and insurance policy choices. Insured acres, collected premiums,
disbursed subsides, liabilities, number of policies sold, number of
indemnified policies, and loss ratios are all available from the summary
of business.

Accessing data from the summary of business can be done using the
`get_sob_data`. With no arguments specified, the `get_sob_data` function
will default to downloading data from [RMA’s summary of business report
generator](https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator)
for the current year, at the highest level aggregation.

``` r
library(rfcip)
get_sob_data()
#> # A tibble: 10 × 21
#>    commodity_year policies_sold policies_earning_prem policies_indemnified
#>             <dbl>         <dbl>                 <dbl>                <dbl>
#>  1           2025       2526176                270896                59739
#>  2           2025          2513                  2051                  424
#>  3           2025            60                    27                    5
#>  4           2025           804                   359                  159
#>  5           2025            82                    75                    0
#>  6           2025          3917                  3261                  346
#>  7           2025            16                    11                    0
#>  8           2025         55482                 22663                  860
#>  9           2025          7701                  6956                 5079
#> 10           2025          2023                  2023                 1004
#> # ℹ 17 more variables: units_earning_prem <dbl>, units_indemnified <dbl>,
#> #   quantity <dbl>, quantity_type <chr>, companion_endorsed_acres <dbl>,
#> #   liabilities <dbl>, total_prem <dbl>, subsidy <dbl>, indemnity <dbl>,
#> #   efa_prem_discount <dbl>, addnl_subsidy <dbl>, state_subsidy <dbl>,
#> #   pccp_state_matching_amount <dbl>, organic_certified_subsidy_amount <dbl>,
#> #   organic_transitional_subsidy_amount <dbl>, earn_prem_rate <dbl>,
#> #   loss_ratio <dbl>
```

Most of the arguments for the `get_sob_data` function filter the
returned data. For example, specifying the `year = 2022` and
`crop = "corn"` will return data for corn in crop year 2022. For a
description of all the arguments that can be supplied to `get_sob_data`
see the help file for the function using `help(get_sob_data)`

``` r
get_sob_data(year = 2022, crop = "corn")
#> # A tibble: 1 × 23
#>   commodity_year commodity_code commodity_name policies_sold
#>            <dbl>          <int> <chr>                  <dbl>
#> 1           2022             41 Corn                  590773
#> # ℹ 19 more variables: policies_earning_prem <dbl>, policies_indemnified <dbl>,
#> #   units_earning_prem <dbl>, units_indemnified <dbl>, quantity <dbl>,
#> #   quantity_type <chr>, companion_endorsed_acres <dbl>, liabilities <dbl>,
#> #   total_prem <dbl>, subsidy <dbl>, indemnity <dbl>, efa_prem_discount <dbl>,
#> #   addnl_subsidy <dbl>, state_subsidy <dbl>, pccp_state_matching_amount <dbl>,
#> #   organic_certified_subsidy_amount <dbl>,
#> #   organic_transitional_subsidy_amount <dbl>, earn_prem_rate <dbl>, …
```

In the above example, the data set was filtered to a single crop,
“corn”. This potentially raises the question of what other values can be
passed to the arguments that control the filters. For arguments with
limited options, the options are explained in the functions help file
(again, accessed via `help(get_sob_data)`). For some arguments that have
many options, there are dedicated functions for pulling up values than
can be passed to these arguments.

The `get_crop_codes` function will return the commodity_code and
commodity_name for a supplied year. Note that the `commodity_code` and
`commodity_name` can be used interchangeably as values for the `crop`
argument in `get_sob_data`. If there is a crop that is suspected to be
an option, the `comm` argument can also be specified which will return
just that specified crop if it exists.

``` r
# get all crop codes and crop names for 2024
get_crop_codes(2024)
#> # A tibble: 129 × 3
#>    commodity_year commodity_code commodity_name        
#>    <chr>          <chr>          <chr>                 
#>  1 2024           0107           Alfalfa Seed          
#>  2 2024           0211           All Other Citrus Trees
#>  3 2024           0028           Almonds               
#>  4 2024           0332           Annual Forage         
#>  5 2024           1191           Apiculture            
#>  6 2024           0184           Apple Trees           
#>  7 2024           0054           Apples                
#>  8 2024           0212           Avocado Trees         
#>  9 2024           0019           Avocados              
#> 10 2024           0255           Banana                
#> # ℹ 119 more rows
```

``` r

# double check that "corn" is a valid crop
get_crop_codes(2024, crop = "corn")
#> # A tibble: 1 × 3
#>   commodity_year commodity_code commodity_name
#>   <chr>          <chr>          <chr>         
#> 1 2024           0041           Corn

# below are other ways to call the same data
get_crop_codes(2024, crop = "CORN")
#> # A tibble: 1 × 3
#>   commodity_year commodity_code commodity_name
#>   <chr>          <chr>          <chr>         
#> 1 2024           0041           Corn
get_crop_codes(2024, crop = 41)
#> # A tibble: 1 × 3
#>   commodity_year commodity_code commodity_name
#>   <chr>          <chr>          <chr>         
#> 1 2024           0041           Corn
```

The `get_insurance_plan_codes` function works analogously to the
`get_crop_codes` function and helps identiy valid names of insurance
plans.

``` r
# return all insurance plans avaliable in 2024
get_insurance_plan_codes(year = 2024)
#> # A tibble: 35 × 4
#>    commodity_year insurance_plan_code insurance_plan        insurance_plan_abbrv
#>    <chr>          <chr>               <chr>                 <chr>               
#>  1 2024           90                  APH                   APH                 
#>  2 2024           91                  APH Price Component   APHPC               
#>  3 2024           43                  Aquaculture Dollar    AQDOL               
#>  4 2024           47                  Actual Revenue Histo… ARH                 
#>  5 2024           05                  Area Revenue Protect… ARP                 
#>  6 2024           06                  Area Revenue Protect… ARP - HPE           
#>  7 2024           04                  Area Yield Protection AYP                 
#>  8 2024           50                  Dollar Amount Of Ins… DO                  
#>  9 2024           83                  Dairy Revenue Protec… DRP                 
#> 10 2024           88                  Enhanced Cov Opt - R… ECO-RP              
#> # ℹ 25 more rows

# return the insurance plan code for the revenue projection plan
get_insurance_plan_codes(year = 2024, plan = "revenue protection")
#> # A tibble: 1 × 4
#>   commodity_year insurance_plan_code insurance_plan     insurance_plan_abbrv
#>   <chr>          <chr>               <chr>              <chr>               
#> 1 2024           02                  Revenue Protection RP

# below are other ways to call the same data
get_insurance_plan_codes(year = 2024, plan = 2)
#> # A tibble: 1 × 4
#>   commodity_year insurance_plan_code insurance_plan     insurance_plan_abbrv
#>   <chr>          <chr>               <chr>              <chr>               
#> 1 2024           02                  Revenue Protection RP
get_insurance_plan_codes(year = 2024, plan = "RP")
#> # A tibble: 1 × 4
#>   commodity_year insurance_plan_code insurance_plan     insurance_plan_abbrv
#>   <chr>          <chr>               <chr>              <chr>               
#> 1 2024           02                  Revenue Protection RP
get_insurance_plan_codes(year = 2024, plan = "reVeNue PrOtEcTiOn")
#> # A tibble: 1 × 4
#>   commodity_year insurance_plan_code insurance_plan     insurance_plan_abbrv
#>   <chr>          <chr>               <chr>              <chr>               
#> 1 2024           02                  Revenue Protection RP
```

As was previously stated above, most arguments for the `get_sob_data`
function are for filtering the returned data. One exception is the
`group_by` argument which does not filter the data being returned, but
instead alters the level of aggregation. Taking the above example that
returns data for corn in 2022 and setting `group_by = "county"` will
return the same underlying as above, but decomposed by county.

``` r
get_sob_data(year = 2022, crop = "corn", group_by = "county")
#> # A tibble: 2,405 × 27
#>    commodity_year commodity_code commodity_name        state_code state_abbrv
#>             <dbl>          <int> <chr>                 <chr>      <chr>      
#>  1           2022           9999 All Other Commodities 01         AL         
#>  2           2022           9999 All Other Commodities 01         AL         
#>  3           2022           9999 All Other Commodities 01         AL         
#>  4           2022           9999 All Other Commodities 01         AL         
#>  5           2022           9999 All Other Commodities 01         AL         
#>  6           2022           9999 All Other Commodities 01         AL         
#>  7           2022           9999 All Other Commodities 01         AL         
#>  8           2022           9999 All Other Commodities 01         AL         
#>  9           2022           9999 All Other Commodities 01         AL         
#> 10           2022           9999 All Other Commodities 01         AL         
#> # ℹ 2,395 more rows
#> # ℹ 22 more variables: county_code <chr>, county_name <chr>,
#> #   policies_sold <dbl>, policies_earning_prem <dbl>,
#> #   policies_indemnified <dbl>, units_earning_prem <dbl>,
#> #   units_indemnified <dbl>, quantity <dbl>, quantity_type <chr>,
#> #   companion_endorsed_acres <dbl>, liabilities <dbl>, total_prem <dbl>,
#> #   subsidy <dbl>, indemnity <dbl>, efa_prem_discount <dbl>, …
```

We can confirm `get_sob_data(year = 2022, crop = "corn")` and
`get_sob_data(year = 2022, crop = "corn", group_by = "county")` return
the same underlying data by summing up one of individual columns in the
county level data.

``` r
national_data <- get_sob_data(year = 2022, crop = "corn")
print(paste("Liabilities from national data: ",sum(national_data$liabilities)))
#> [1] "Liabilities from national data:  67671651665"

county_data <- get_sob_data(year = 2022, crop = "corn", group_by = "county")
print(paste("Liabilities from county data:   ",sum(county_data$liabilities)))
#> [1] "Liabilities from county data:    67671651665"
```

A unique property of the summary of business data set is that its
continuously updated (one per week) as new information is reported to
USDA by [approved insurance
providers](https://cropinsuranceinamerica.org/who-are-approved-insurance-providers-aips/).
This means that analysis using the summary of business can quickly
become outdated. One advantage of the the `rfcip` package is that it
allows the raw data source to be directly integrated into the analysis.
For example, the chart below plots indemnities for each crop year from
2015 up to the current year. The plot will automatically update with the
latest data every time the plot is regenerated. Note that functions in
`rfcip` are [memoised](https://en.wikipedia.org/wiki/Memoization) for
the duration of the R session. This means that calling the same function
with the same arguments will return a previously cached data set. In
other words, in the below example, the data would not update if the code
was run multiple times in the same R session, regaurdless of if the
underlying data source changed.

``` r
library(rfcip)
library(dplyr)
library(ggplot2)

get_sob_data(year = 2015:as.numeric(format(Sys.Date(), "%Y"))) %>%
select(commodity_year, indemnity) %>%
group_by(commodity_year) %>%
summarize(indemnity = sum(indemnity)) %>%
mutate(indemnity = indemnity/1000000000) %>%
  ggplot(., aes(y = indemnity, x = commodity_year)) +
  geom_bar(stat = "identity", fill = "firebrick4") +
  xlab("") + ylab("USD (Billions)") +
  ggtitle(paste0("FCIP Indemnities by Year: 2015 - ",format(Sys.Date(), "%Y"))) +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()
#> Downloading summary of business data for specified crop years ■■■■■■
#> …Downloading summary of business data for specified crop years ■■■■■■■■■
#> …Downloading summary of business data for specified crop years ■■■■■■■■■■■■
#> …Downloading summary of business data for specified crop years ■■■■■■■■■■■■■■■
#> …Downloading summary of business data for specified crop years
#> ■■■■■■■■■■■■■■■■■…Downloading summary of business data for specified crop years
#> ■■■■■■■■■■■■■■■■■…Downloading summary of business data for specified crop years
#> ■■■■■■■■■■■■■■■■■…Downloading summary of business data for specified crop years
#> ■■■■■■■■■■■■■■■■■…Downloading summary of business data for specified crop years
#> ■■■■■■■■■■■■■■■■■…
```

<img src="man/figures/sob-method-chaining-1.png" width="100%" />

### Summary of Business by Type, Practice, and Unit Structure

The default behavior of the `get_sob_data()`function is to pull data
from [RMA’s summary of business report
generator](https://public-rma.fpac.usda.gov/apps/SummaryOfBusiness/ReportGenerator).
Although this allows for server side filtering of data before it gets to
your local machine it does not allow access to the most granular version
of the data know as the [Summary of Business by Type, Practice, and Unit
Structure](https://www.rma.usda.gov/tools-reports/summary-of-business/state-county-crop-summary-business).
By setting the optional parameter `sob_version = "sobtpu"` the behavior
of the `get_sob_data()` function will change to pull data from the
[Summary of Business by Type, Practice, and Unit
Structure](https://www.rma.usda.gov/tools-reports/summary-of-business/state-county-crop-summary-business).
Note that when `sob_version = "sobtpu"`, the arguments `delivery_type`,
`comm_cat`, and `group_by` are not applicable and will be ignored.
Otherwise, the function behaves the same as when `sob_version = "sob"`
(the default).

``` r
# get the summary of business by type, practice, and unit structure
data <- get_sob_data(year = 2022, crop = "corn", sob_version = "sobtpu")
#> ℹ Locating Summary of Business download links on RMA's website.
#> ✔ Download links located.
#> ℹ Merging Summary of Business files for all specified crop years

head(data)
#>   commodity_year state_code state_name state_abbreviation county_code
#> 1           2022          1    Alabama                 AL           1
#> 2           2022          1    Alabama                 AL           1
#> 3           2022          1    Alabama                 AL           1
#> 4           2022          1    Alabama                 AL           1
#> 5           2022          1    Alabama                 AL           1
#> 6           2022          1    Alabama                 AL           1
#>   county_name commodity_code commodity_name insurance_plan_code
#> 1     Autauga             41           Corn                   1
#> 2     Autauga             41           Corn                   2
#> 3     Autauga             41           Corn                   2
#> 4     Autauga             41           Corn                   2
#> 5     Autauga             41           Corn                   2
#> 6     Autauga             41           Corn                   2
#>   insurance_plan_abbreviation coverage_type_code coverage_level_percent
#> 1                          YP                  C                   0.50
#> 2                          RP                  A                   0.60
#> 3                          RP                  A                   0.65
#> 4                          RP                  A                   0.70
#> 5                          RP                  A                   0.70
#> 6                          RP                  A                   0.70
#>   delivery_id type_code         type_name practice_code         practice_name
#> 1           R         0 No Type Specified             0 No Practice Specified
#> 2           R        16             Grain             3         Non-Irrigated
#> 3           R         0 No Type Specified             0 No Practice Specified
#> 4           R         0 No Type Specified             0 No Practice Specified
#> 5           R        16             Grain             2             Irrigated
#> 6           R        16             Grain             3         Non-Irrigated
#>   unit_structure_code unit_structure_name net_reporting_level_amount
#> 1                  OU       Optional Unit                          0
#> 2                  BU          Basic Unit                         76
#> 3                  OU       Optional Unit                          0
#> 4                  OU       Optional Unit                          0
#> 5                  OU       Optional Unit                        355
#> 6                  OU       Optional Unit                         51
#>   reporting_level_type liability_amount total_premium_amount subsidy_amount
#> 1                Acres                0                    0              0
#> 2                Acres            33327                 3520           2253
#> 3                Acres                0                    0              0
#> 4                Acres                0                    0              0
#> 5                Acres           319499                22646          13361
#> 6                Acres            21822                 4094           2416
#>   indemnity_amount loss_ratio endorsed_commodity_reporting_level_amount
#> 1                0       0.00                                         0
#> 2                0       0.00                                         0
#> 3                0       0.00                                         0
#> 4                0       0.00                                         0
#> 5            81213       3.59                                         0
#> 6            11644       2.84                                         0
```

Once consideration when using `sob_version = "sobtpu"` is that the data
is only accessible via bulk download by year. This means there will be
little to no performance advantage from filtering data via the function
arguments since the full data set must be downloaded before the filters
are applied. However, the function does apply the filters to each
year-specific file as they are read in to minimize memory usage to the
extent possible.

## Cause of Loss Files

Although RMA’s [Summary of
Business](https://www.rma.usda.gov/tools-reports/summary-of-business)
files do report indemnities, they don’t report the cause of loss
associated with the indemnities which is often relevant. To obtain
indemnities with the associated cause of loss that generated those
indemnities, accessing the [cause of loss
files](https://www.rma.usda.gov/tools-reports/summary-business/cause-loss)
is necessary. Unlike data contained in the [summary of
Business](https://www.rma.usda.gov/tools-reports/summary-of-business),
the [cause of loss
files](https://www.rma.usda.gov/tools-reports/summary-business/cause-loss)
can only be accessed by bulk downloading all data for a particular year.
Because of this, the cause of loss data cannot be filtered prior to
loading it into local memory meaning there is no advantage to offering
any within-function filtering options. To download [cause of loss
files](https://www.rma.usda.gov/tools-reports/summary-business/cause-loss)
for a year or series of years, use the `get_col_data` function. The
function will automatically download all the relevant cause of loss
files and merge them into a single data frame that will be returned.

``` r
col_data <- get_col_data(year = 2020:2022)
#> ℹ Locating cause of loss download links on RMA's website.
#> ✔ Download links located.
#> Downloading cause of loss files for specified crop years ■■■■■■■■■■■■■■■■■■■■■ …                                                                                 ℹ Merging cause of loss files for all specified crop years
head(col_data)
#>   commodity_year state_code state_abbrv county_code county_name commodity_code
#> 1           2020          1          AL           1     Autauga             21
#> 2           2020          1          AL           1     Autauga             21
#> 3           2020          1          AL           1     Autauga             21
#> 4           2020          1          AL           1     Autauga             41
#> 5           2020          1          AL           1     Autauga             41
#> 6           2020          1          AL           1     Autauga             41
#>   commodity_name insurance_plan_code insurance_plan_abbrv delivery_type
#> 1         Cotton                   2                   RP             A
#> 2         Cotton                   2                   RP             A
#> 3         Cotton                   2                   RP             A
#> 4           Corn                   2                   RP             A
#> 5           Corn                   2                   RP             A
#> 6           Corn                   2                   RP             A
#>   stage_code col_code                           col_name month_of_loss_code
#> 1          H       31 Excess Moisture/Precipitation/Rain                  9
#> 2          H       92      Hurricane/Tropical Depression                 10
#> 3          H       92      Hurricane/Tropical Depression                  9
#> 4          H        1                   Decline in Price                  9
#> 5          H       11                            Drought                  7
#> 6          R       93                           Wildlife                  5
#>   month_of_loss_name year_of_loss policies_earning_prem policies_indemnified
#> 1                SEP         2020                     1                    1
#> 2                OCT         2020                     3                    3
#> 3                SEP         2020                     3                    3
#> 4                SEP         2020                     1                    1
#> 5                JUL         2020                     1                    1
#> 6                MAY         2020                     1                    1
#>   net_planted_qty net_endorsed_acres  liability total_premium
#> 1          38.925                  0  11519.000      1389.000
#> 2         992.425                  0 495420.000     38585.500
#> 3         392.600                  0 220061.000     17815.500
#> 4          12.900                  0   2543.665       437.095
#> 5           8.100                  0   1597.185       274.455
#> 6          30.000                  0   5915.500      1016.500
#>   producer_paid_premium  subsidy state_subsidy addnl_subsidy efa_prem_discount
#> 1               570.000   819.00             0             0                 0
#> 2              8457.500 30128.00             0             0                 0
#> 3              4157.500 13658.00             0             0                 0
#> 4               196.725   240.37             0             0                 0
#> 5               123.525   150.93             0             0                 0
#> 6               457.500   559.00             0             0                 0
#>   indemnified_quantity indem_amount loss_ratio
#> 1               38.925      1938.00       1.40
#> 2              992.425     95015.00       2.46
#> 3              392.600     18962.00       1.06
#> 4               25.800      1602.18       3.67
#> 5               16.200      1006.02       3.67
#> 6               14.000       439.00       0.43
```

## Price

Insurance guarantees and indemnities for revenue protection plans are
base, in part, on projected commodity prices and harvest commodity
prices. These are available via RMA’s [price discovery
application](https://public-rma.fpac.usda.gov/apps/PriceDiscovery/), but
can also be obtained using the `get_price_data` function which supports
arguments for `year`, `crop`, and `state`. For example, prices for corn
in Illinois from 2020-2024 can be obtained with the following code.

``` r
price_data <- get_price_data(year = 2020:2024,
                             crop = "corn",
                             state = "IL")
#> ℹ Downloading data

head(price_data) 
#> # A tibble: 6 × 38
#>   CommodityYear CommodityCode CommodityName TypeCode TypeName       PracticeCode
#>           <int>         <int> <chr>            <int> <chr>                 <int>
#> 1          2020            41 Corn                16 High Amylose              2
#> 2          2020            41 Corn                16 High Amylose              3
#> 3          2020            41 Corn                17 All (Non-High…            2
#> 4          2020            41 Corn                17 All (Non-High…            3
#> 5          2021            41 Corn                16 High Amylose              2
#> 6          2021            41 Corn                16 High Amylose              3
#> # ℹ 32 more variables: PracticeName <chr>, StateCode <int>, StateName <chr>,
#> #   PriceMultiplicativeFactor <dbl>, PriceAdditiveFactor <dbl>,
#> #   ProjectedPriceExchangeCode <chr>, ProjectedPriceMarketSymbolCode <chr>,
#> #   ProjectedPricePreviousMarketSymbolCode <chr>,
#> #   ProjectedPriceCurrencyMarketSymbolCode <lgl>,
#> #   ProjectedPriceBeginDate <dttm>, ProjectedPriceEndDate <dttm>,
#> #   ProjectedPriceDateRange <chr>, ProjectedPrice <dbl>, …
```

## Reinsurance Agreements

### Standard Reinsurance Agreement

Data related to the Standard Reinsurance Agreement (including retained
liabilities, premiums, indemnities, and net underwriting gains and
losses) is included as a static internal data set. To load the national
level data set, use `data(nationalSRA)`. This data set is based on data
from RMA’s [reinsurance
reports](https://www.rma.usda.gov/tools-reports/reinsurance-reports).

``` r
# load the national SRA data set
data(nationalSRA)

head(nationalSRA)
#> # A tibble: 6 × 11
#>   fund_abb reinsurance_year report_geography value_type            dollars
#>   <chr>               <dbl> <chr>            <chr>                   <dbl>
#> 1 AR                   1998 NationalFund     gross_liability    3286742595
#> 2 AR                   1998 NationalFund     gross_premium       313517141
#> 3 AR                   1998 NationalFund     gross_indemnity     463493075
#> 4 AR                   1998 NationalFund     retained_liability  597494229
#> 5 AR                   1998 NationalFund     retained_premium     58503706
#> 6 AR                   1998 NationalFund     retained_indemnity   51840753
#> # ℹ 6 more variables: data_release_month <dbl>, data_release_year <dbl>,
#> #   data_release_day <dbl>, data_release_date <date>, fund_name <chr>,
#> #   report_type <chr>

# pull up the data sets documentation file.
?nationalSRA
#> ℹ Rendering development documentation for "nationalSRA"
```

Similarly, a state level version of the SRA data set is also available.
To load the state level data set, use `data(stateSRA)`.

``` r
# load the state SRA data set
data(stateSRA)

head(stateSRA)
#> # A tibble: 6 × 12
#>   state fund_abb reinsurance_year report_geography value_type          dollars
#>   <chr> <chr>               <int> <chr>            <chr>                 <dbl>
#> 1 AL    AR                   1998 StateFund        gross_liability    63545502
#> 2 AL    AR                   1998 StateFund        gross_premium       7153137
#> 3 AL    AR                   1998 StateFund        gross_indemnity    16444842
#> 4 AL    AR                   1998 StateFund        retained_liability 12709100
#> 5 AL    AR                   1998 StateFund        retained_premium    1430627
#> 6 AL    AR                   1998 StateFund        retained_indemnity  1507266
#> # ℹ 6 more variables: data_release_month <dbl>, data_release_year <dbl>,
#> #   data_release_day <dbl>, data_release_date <date>, fund_name <chr>,
#> #   report_type <chr>

# pull up the data sets documentation file.
?stateSRA
#> ℹ Rendering development documentation for "stateSRA"
```

### Livestock Price Reinsurance Agreement

Data related to the Livestock Price Reinsurance Agreement (including
retained liabilities, premiums, indemnities, and net underwriting gains
and losses) is included as a static internal data set. To load the
national level data set, use `data(nationalLPRA)`. This data set is
based on data from [reinsurance
reports](https://www.rma.usda.gov/tools-reports/reinsurance-reports).

``` r
# load the national SRA data set
data(nationalLPRA)

head(nationalLPRA)
#> # A tibble: 6 × 10
#>   reinsurance_year report_geography footnote                  value_type dollars
#>              <dbl> <chr>            <chr>                     <chr>        <dbl>
#> 1             2014 NationalFund     "Footnote: Amounts shown… gross_lia…  1.04e9
#> 2             2014 NationalFund     "Footnote: Amounts shown… gross_pre…  2.28e7
#> 3             2014 NationalFund     "Footnote: Amounts shown… gross_ind…  1.05e7
#> 4             2014 NationalFund     "Footnote: Amounts shown… retained_…  4.03e8
#> 5             2014 NationalFund     "Footnote: Amounts shown… retained_…  8.64e6
#> 6             2014 NationalFund     "Footnote: Amounts shown… retained_…  3.69e6
#> # ℹ 5 more variables: data_release_month <dbl>, data_release_year <dbl>,
#> #   data_release_day <dbl>, data_release_date <date>, report_type <chr>

# pull up the data sets documentation file.
?nationalLPRA
#> ℹ Rendering development documentation for "nationalLPRA"
```

Please note that `rfcip` is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/#:~:text=rOpenSci%20is%20committed%20to%20providing,understand%E2%80%9D%20or%20%E2%80%9CWhy%E2%80%9D.).
By contributing to the package you agree to abide by its terms.

## Livestock and Dairy Participation Reports

Some data on participation in livestock insurance plans can be retrieved
using the `get_sob_data` function, however, more detailed information is
available from the [Livestock and Dairy
Participation](https://www.rma.usda.gov/tools-reports/summary-of-business/livestock-dairy-participation)
reports. The `get_ldp_data` function can be used to download the most
recent versions of the data contained there.

``` r

# get data on the livestock risk protection plan from 2020 to 2022
lrp_data <- get_livestock_data(year = 2020:2022, program = "LRP")
#> ℹ Locating livestock download links on RMA's website.
#> ✔ Download links located.
#> ℹ Merging livestock files for all specified crop years

# get data on the  dairy revenue protection plan from 2020 to 2022
drp_data <- get_livestock_data(year = 2020:2022, program = "DRP")
#> ℹ Locating livestock download links on RMA's website.
#> ✔ Download links located.
#> ℹ Merging livestock files for all specified crop years

# get data on the livestock gross margin plan from 2020 to 2022
lgm_data <- get_livestock_data(year = 2020:2022, program = "LGM")
#> ℹ Locating livestock download links on RMA's website.
#> ✔ Download links located.
#> ℹ Merging livestock files for all specified crop years
```
