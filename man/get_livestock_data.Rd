% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_livestock_data.R
\name{get_livestock_data}
\alias{get_livestock_data}
\title{Get Livestock Insurance Data from the USDA Risk Management Agency}
\usage{
get_livestock_data(
  year = as.numeric(format(Sys.Date(), "\%Y")),
  program = "LRP"
)
}
\arguments{
\item{year}{A numeric vector of years to retrieve data for. Defaults to the current year and previous four years.}

\item{program}{A character vector specifying which livestock insurance programs to include.
Options are "DRP" (Dairy Revenue Protection), "LGM" (Livestock Gross Margin), and "LRP" (Livestock Risk Protection).}
}
\value{
A data frame containing the specified livestock insurance data for the specified years and program.
}
\description{
Downloads and processes USDA Risk Management Agency livestock insurance data for specified years and programs.
}
\examples{
\dontrun{
# Get data for the current year only
current_year_data <- get_livestock_data(year = as.numeric(format(Sys.Date(), "\%Y")))

# Get LRP data for 2020-2022
lrp_data <- get_livestock_data(year = 2020:2022, program = "LRP")

# Get all programs for 2021
all_programs_2021 <- get_livestock_data(year = 2021, program = c("DRP", "LGM", "LRP"))
}
}
