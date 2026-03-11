# Utility helper to give a light summary of partially observed covariates

This function takes a dataframe and automatically returns the amount and
proportion of missing for partially observed covariates assuming a
one-row-per-patient dataframe. This is an important utility function for
other functions in this package. Results can also be stratified by
another variable in which case the proportion missing refers to the
amount of patients in the respective stratum.

## Usage

``` r
smdi_summarize(data = NULL, covar = NULL, strata = NULL)
```

## Arguments

- data:

  dataframe or tibble object with partially observed/missing variables.
  Assumes a a one-row-per-patient format.

- covar:

  character covariate or covariate vector with partially observed
  variable/column name(s) to investigate. If NULL, the function
  automatically includes all columns with at least one missing
  observation.

- strata:

  character name of variable/column by which results should be
  stratified

## Value

returns count and proportion of missing values. If strata is specified,
the returned proportion refers to the amount of patients in the
respective stratum.

## Examples

``` r
library(smdi)

smdi_vis(data = smdi_data)

```
