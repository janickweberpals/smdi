# Computes three group missing data summary diagnostics

This function bundles and calls all three group diagnostics and returns
the most important summary metrics. For more information and details,
please refer to the individual functions.

Important: don't include variables like ID variables, ZIP codes, dates,
etc.

## Usage

``` r
smdi_diagnose(
  data = NULL,
  covar = NULL,
  median = TRUE,
  includeNA = FALSE,
  train_test_ratio = c(0.7, 0.3),
  set_seed = 42,
  ntree = 1000,
  n_cores = 1,
  model = c("logistic", "linear", "cox"),
  form_lhs = NULL,
  exponentiated = FALSE
)
```

## Arguments

- data:

  dataframe or tibble object with partially observed/missing variables

- covar:

  character covariate or covariate vector with partially observed
  variable/column name(s) to investigate. If NULL, the function
  automatically includes all columns with at least one missing
  observation and all remaining covariates will be used as predictors

- median:

  logical if the median (= TRUE; recommended default) or mean of all
  absolute standardized mean differences (asmd) should be computed
  (smdi_asmd())

- includeNA:

  logical, should missingness of other partially observed covariates be
  explicitly modeled for computation of absolute standardized mean
  differences (default is FALSE)

- train_test_ratio:

  numeric vector to indicate the test/train split ratio for random
  forest missingness prediction model, e.g. c(.7, .3) is the default

- set_seed:

  seed for reproducibility of random forest missingness prediction
  model, defaults to 42

- ntree:

  integer, number of trees for random forest missingness prediction
  model (defaults to 1000 trees)

- n_cores:

  integer, if \>1, computations will be parallelized across amount of
  cores specified in n_cores (only UNIX systems)

- model:

  character describing which outcome model to fit to assess the
  association between covar missingness indicator and outcome. Currently
  supported are models of type logistic, linear and cox (see
  smdi_outcome)

- form_lhs:

  string specifying the left-hand side of the outcome formula (see
  smdi_outcome)

- exponentiated:

  logical, should results of outcome regression to assess association
  between missingness and outcome be exponentiated (default is FALSE)

## Value

smdi object including a summary table of all three smdi group
diagnostics:

**Group 1 diagnostic:**

- `asmd_mean` or `asmd_median`: average/median absolute standardized
  mean difference (and min, max) of patient characteristics between
  those without (1) and with (0) observed covariate

- hotteling_p: p-value of hotelling test. Rejecting the H0 means that
  Hotelling's test detects a significant difference in the distribution
  between patients without (1) and with (0) the observed covariate

**Group 2 diagnostic:**

- `rf_auc`: The area under the receiver operating curve (AUC) as a
  measure of the ability to predict the missingness of the partially
  observed covariate

**Group 3 diagnostic:**

- `estimate_univariate`: univariate association between missingness
  indicator of covar and outcome

- `estimate_adjusted`: association between missingness indicator of
  covar and outcome conditional on other fully observed covariates and
  missing indicator variables of other partially observed covariates

## Details

Wrapper for individual diagnostics function.

## References

TBD

## See also

[`smdi_asmd`](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_asmd.md)
[`smdi_hotelling`](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_hotelling.md)
[`smdi_little`](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_little.md)
[`smdi_rf`](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_rf.md)
[`smdi_outcome`](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_outcome.md)

## Examples

``` r
library(smdi)

smdi_diagnose(
 data = smdi_data,
 covar = "egfr_cat",
 model = "cox",
 form_lhs = "Surv(eventtime, status)"
 )
#> smdi summary table:
#> # A tibble: 1 × 6
#>   covariate asmd_median_min_max  hotteling_p rf_auc estimate_univariate      
#>   <chr>     <chr>                <chr>       <chr>  <glue>                   
#> 1 egfr_cat  0.243 (0.010, 0.485) <.001       0.629  0.06 (95% CI -0.03, 0.15)
#> # ℹ 1 more variable: estimate_adjusted <glue>
#> 
#> p_little: <.001
```
