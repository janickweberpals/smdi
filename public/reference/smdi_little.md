# Computes Little's test

Little’s chi-squared test takes into account possible patterns of
missingness across all variables in the dataset. Rejection of the null
hypothesis of this test would provide sufficient evidence to indicate
that the data are (globally) not MCAR. Please note that compared to
[smdi_hotelling](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_hotelling.md),
this function tests for MCAR globally across all missing covariates.

\#' \#' Important: don't include variables like ID variables, ZIP codes,
dates, etc.

## Usage

``` r
smdi_little(data = NULL)
```

## Arguments

- data:

  dataframe or tibble object with partially observed/missing variables

## Value

returns a little object with statistics on little's test globally.

## Details

CAVE: Hotelling's and Little's show high susceptibility with large
sample sizes and it is recommended to always interpret the results along
with the other diagnostics.

## References

Little RJA. A Test of Missing Completely at Random for Multivariate Data
with Missing Values. J Am Stat Assoc. 1988;83(404):1198-1202.

## See also

[`mcar_test`](https://naniar.njtierney.com/reference/mcar_test.html)

## Examples

``` r
library(smdi)
library(dplyr)

smdi_data %>%
 smdi_little()
#> $statistic
#> [1] 811.5801
#> 
#> $df
#> [1] 93
#> 
#> $p.value
#> [1] 0
#> 
#> $missing.patterns
#> [1] 8
#> 
#> attr(,"class")
#> [1] "little"
#> attr(,"row.names")
#> [1] 1
```
