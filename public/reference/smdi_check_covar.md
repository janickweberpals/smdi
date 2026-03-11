# This is a utility function to help check input data and covariates provided

This is a utility function to help check input data and covariates
provided

## Usage

``` r
smdi_check_covar(data = NULL, covar = NULL)
```

## Arguments

- data:

  dataframe or tibble object with partially observed/missing variables

- covar:

  character covariate or covariate vector with partially observed
  variable/column name(s) to investigate. If NULL, the function
  automatically looks for and includes all columns with at least one
  missing observation

## Value

returns the covariate vector for subsequent tasks or warnings/errors
