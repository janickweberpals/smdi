% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smdi_vis.R
\name{smdi_vis}
\alias{smdi_vis}
\title{Quick ggplot2 barchart visualization of partially observed/missing variables}
\usage{
smdi_vis(data = NULL, covar = NULL, strata = NULL)
}
\arguments{
\item{data}{dataframe or tibble object with partially observed/missing variables. Assumes a a one-row-per-patient format}

\item{covar}{character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation}

\item{strata}{character name of variable/column by which results should be stratified}
}
\value{
returns ggplot2 graph displaying selected or automatically identified variables by percent missing
}
\description{
This function takes a dataframe and outputs a nicely formatted
ggplot2 vertical barchart plot that visualizes the proportion
missing for a given variable (vector) or all existent missing
variables. Results can also be stratified by another variable
in which case the proportion missing refers to the amount of
patients in the respective stratum.

Important: Function assumes the data is in a one-row-per-patient format.
}
\examples{
library(smdi)

smdi_vis(data = smdi_data)

}
