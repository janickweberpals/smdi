# Computes hotelling's multivariate t-test

Hotelling's multivariate t-test, which examines variable differences
conditional on having an observed covariate value or not. As the power
of statistical hypothesis tests can be influenced by sample size, the
combined investigation along with
[`smdi_asmd`](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_asmd.md)
is highly recommended.

Important: don't include variables like ID variables, ZIP codes, dates,
etc.

## Usage

``` r
smdi_hotelling(data = NULL, covar = NULL, n_cores = 1)
```

## Arguments

- data:

  dataframe or tibble object with partially observed/missing variables

- covar:

  character covariate or covariate vector with partially observed
  variable/column name(s) to investigate. If NULL, the function
  automatically includes all columns with at least one missing
  observation and all remaining covariates will be used as predictors

- n_cores:

  integer, if \>1, computations will be parallelized across amount of
  cores specified in n_cores (only UNIX systems)

## Value

returns a hotelling object with statistics on hotellings test by
covariate. That is, for each covar, the following outputs are provided:

- stats: hotelling test statistics (for more information see
  [`hotelling.test`](https://rdrr.io/pkg/Hotelling/man/hotelling.test.html))

- pval: p-value of hotelling test

## Details

CAVE: Hotelling's and Little's show high susceptibility with large
sample sizes and it is recommended to always interpret the results along
with the other diagnostics.

## References

Hotelling H. The Generalization of Student’s Ratio. Ann Math Stat.
1931;2(3):360-378.

## See also

[`hotelling.test`](https://rdrr.io/pkg/Hotelling/man/hotelling.test.html)

## Examples

``` r
library(smdi)

smdi_hotelling(data = smdi_data)
#>   covariate hotteling_p
#> 1  ecog_cat       0.783
#> 2  egfr_cat       <.001
#> 3  pdl1_num       <.001
```
