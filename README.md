rfcip (R FCIP)
================

- [Coverage Report](#coverage-report)
- [Introduction](#introduction)
- [Installation](#installation)
- [Getting Started](#getting-started)
- [Why use the `rcfip` package?](#why-use-the-rcfip-package)

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R-CMD-check](https://github.com/dylan-turner25/rfcip/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dylan-turner25/rfcip/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![codecov](https://codecov.io/gh/dylan-turner25/rfcip/graph/badge.svg?token=F6ZBMNEHYT)](https://codecov.io/gh/dylan-turner25/rfcip)

# Coverage Report

The `rfcip` integration with CodeCov is not currently working. Below are
package coverage metrics as a substitute.

    #> Running specific tests for package 'rfcip'
    #>   Running 'testthat.R'
    #> rfcip Coverage: 29.34%
    #> R/get_price_data.R: 0.00%
    #> R/zzz.R: 0.00%
    #> R/get_livestock_data.R: 7.39%
    #> R/helpers.R: 21.03%
    #> R/build_county_yield_history.R: 35.42%
    #> R/get_sob_data.R: 52.27%
    #> R/get_crop_codes.R: 87.50%
    #> R/get_insurance_plan_codes.R: 87.76%
    #> R/get_col_data.R: 90.91%
    #> R/get_adm_data.R: 100.00%

# Introduction

`rfcip` provides a set of tools to allow users to access publicly
available data related to the Federal Crop Insurance Program, including
both Summary of Business data and Actuarial Data Master (ADM) datasets.
The package provides a set of functions to easily navigate and access
data that is publicly available, but otherwise scattered across
different urls, files, and data portals. Although no official API exists
for Federal Crop Insurance Data, much of the data can be located with a
structured url meaning the `rfcip` package effectively functions like a
defacto API wrapper.

**Disclaimer:** This product uses data provided by the USDA, but is not
endorsed by or affiliated with USDA or the Federal Government.

# Installation

`rfcip` can be installed directly from github using
`remotes::install_github("https://github.com/dylan-turner25/rfcip")`

# Getting Started

The `rfcip` package does not require any special setup or configuration.
Once downloaded, data can be immediately accessed using any of the
available functions. For example, the following pulls up RMA’s summary
of business data for corn in 2022. For a comprehensive introduction of
the `rfcip` package, please see the [getting
started](vignettes/rfcip-introduction.md) vignette.

``` r

# get summary of business data for corn in 2022
library(rfcip)
get_sob_data(year = 2022, crop = "corn")
#> ℹ Loading data from cache
#> # A tibble: 1 × 23
#>   commodity_year commodity_code commodity_name policies_sold
#>            <dbl>          <int> <chr>                  <dbl>
#> 1           2022             41 Corn                  590774
#> # ℹ 19 more variables: policies_earning_prem <dbl>, policies_indemnified <dbl>,
#> #   units_earning_prem <dbl>, units_indemnified <dbl>, quantity <dbl>,
#> #   quantity_type <chr>, companion_endorsed_acres <dbl>, liabilities <dbl>,
#> #   total_prem <dbl>, subsidy <dbl>, indemnity <dbl>, efa_prem_discount <dbl>,
#> #   addnl_subsidy <dbl>, state_subsidy <dbl>, pccp_state_matching_amount <dbl>,
#> #   organic_certified_subsidy_amount <dbl>,
#> #   organic_transitional_subsidy_amount <dbl>, earn_prem_rate <dbl>, …
```

# Why use the `rcfip` package?

Although the data that the `rfcip` package provides access to is all
publicly available, it is often scattered across various files,
interactive portals, or ftp sites and often comes in different file
formats or arrives in files that are too large to work with in local
memory. The `rfcip` package provides a set of functions that make it
easy to access the data without having to think about these
complexities. The package also makes it easy to update data for existing
analysis since the provided functions can be easily integrated into a
reproducible data pipeline for analysis or visualizations. This is
particularly pertinent to the FCIP’s primary data source, the Summary of
Business, which is updated weekly. In short, the `rfcip` is a major time
saver for anyone working with data related to the FCIP and has the added
benefit of improving data accuracy and reproducibility.
