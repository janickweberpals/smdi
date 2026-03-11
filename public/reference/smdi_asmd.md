# Computes mean/median absolute standardized mean differences between observed and missing observations

This function takes a dataframe with covariates which are partially
observed/missing and returns the median/average absolute standardized
mean difference (asmd) and more details for every specified covariate in
covar (if NULL all covariates with at least one NA are considered).

Important: don't include variables like ID variables, ZIP codes, dates,
etc.

## Usage

``` r
smdi_asmd(
  data = NULL,
  covar = NULL,
  median = TRUE,
  includeNA = FALSE,
  n_cores = 1
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

- includeNA:

  logical, should missingness of other partially observed covariates be
  explicitly modeled (default is FALSE)

- n_cores:

  integer, if \>1, computations will be parallelized across amount of
  cores specified in n_cores (only UNIX systems)

## Value

returns an asmd object with average/median absolute standardized mean
differences. That is, for each covar, the following outputs are
provided:

- asmd_covar: name of covariate investigated

- asmd_table1: detailed "table 1" illustrating distributions and
  differences of patient characteristics between those without (1) and
  with (0) observed covariate

- asmd_plot: plot of absolute standardized mean differences (asmd)
  between patients without (1) and with (0) observed covariate (sorted
  by asmd)

- asmd_aggregate: average/median absolute standardized mean difference
  (and min, max) of patient characteristics between those without (1)
  and with (0) observed covariate

## Details

The asmd may be one indicator as to how much patient characteristics
differ between patients with and without an observed value for a
partially observed covariate. If the median/average asmd is above a
certain threshold this may indicate imbalance in patient covariate
distributions which may be indicative of the partially observed
covariate following a missing at random (MAR) mechanims, i.e. the
missingness is explainable by other observed covariates. Similarly, no
imbalance between observed covariates may be indicative that missingness
cannot be explained with observed covariates and the underlying
missingness mechanism may be completely at random (MCAR) or not at
random (e.g. missingness is only associated with unobserved factors or
through the partially observed covariate itself).

A clear cut-off is hard to determine and analogues to propensity scores,
some researchers have proposed that a standardized difference of 0.1 (10
per cent) denotes meaningful imbalance in the baseline covariate.

The asmd is computed for every covariate one-by-one and not jointly. If
there is multivariate missingness, i.e. more than just one missing
covariate exists, you can decide what should happen with the other
partially observed 'predictor' covariates using the includeNA parameter.
That is, if includeNA is set to FALSE (default), only the asmd between
observed cases will be computed, and if includeNA is set to TRUE,
missingness is modeled as an explicit category (categorical covariates
only).

If any other behavior is desired, data transformations for example with
the
[`smdi_na_indicator`](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_na_indicator.md)
function, may make sense before calling the function.

The dataframe should generally consist of the exposure variable, the
outcome variable(s), the partially observed covariates and all other
fully observed covariates which are deemed important for the final
modeling and (optionally) which could be considered as auxiliary
variables. If no partially observed covariates are provided, the
function automatically looks for all variables/columns with NA (powered
by the
[`smdi_summarize`](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_summarize.md)
function)

## References

Austin PC. Balance diagnostics for comparing the distribution of
baseline covariates between treatment groups in propensity-score matched
samples. Stat Med. 2009 Nov 10;28(25):3083-107.

Normand SLT, Landrum MB, Guadagnoli E, Ayanian JZ, Ryan TJ, Cleary PD,
McNeil BJ. Validating recommendations for coronary angiography following
an acute myocardial infarction in the elderly: a matched analysis using
propensity scores. Journal of Clinical Epidemiology. 2001;54:387–398.

## See also

[`CreateTableOne`](https://rdrr.io/pkg/tableone/man/CreateTableOne.html)

## Examples

``` r
library(smdi)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# S3 print method
asmd <- smdi_asmd(data = smdi_data)
asmd
#> # A tibble: 3 × 4
#>   covariate asmd_median asmd_min asmd_max
#> * <chr>     <chr>       <chr>    <chr>   
#> 1 ecog_cat  0.029       0.003    0.071   
#> 2 egfr_cat  0.243       0.010    0.485   
#> 3 pdl1_num  0.062       0.019    0.338   

# let's look at the first variable
# we can check the complete covariate distribution
asmd$pdl1_num$asmd_table1
#>                        Stratified by pdl1_num_NA
#>                         0               1               p        test SMD     
#>   n                     " 1983"         "  517"         ""       ""   ""      
#>   exposure (mean (SD))  " 0.43 (0.50)"  " 0.27 (0.45)"  "<0.001" ""   " 0.338"
#>   age_num (mean (SD))   "60.60 (14.04)" "62.07 (14.47)" " 0.036" ""   " 0.103"
#>   female_cat = 1 (%)    "  717 (36.2) " "  205 (39.7) " " 0.157" ""   " 0.072"
#>   smoking_cat = 1 (%)   "  990 (49.9) " "  263 (50.9) " " 0.739" ""   " 0.019"
#>   physical_cat = 1 (%)  "  707 (35.7) " "  175 (33.8) " " 0.476" ""   " 0.038"
#>   alk_cat = 1 (%)       "   44 ( 2.2) " "   25 ( 4.8) " " 0.002" ""   " 0.142"
#>   histology_cat = 1 (%) "  411 (20.7) " "   97 (18.8) " " 0.354" ""   " 0.049"
#>   ses_cat (%)           " "             "  "            " 0.925" ""   " 0.020"
#>      1_low              "  413 (20.8) " "  111 (21.5) " ""       ""   ""      
#>      2_middle           "  772 (38.9) " "  197 (38.1) " ""       ""   ""      
#>      3_high             "  798 (40.2) " "  209 (40.4) " ""       ""   ""      
#>   copd_cat = 1 (%)      " 1057 (53.3) " "  281 (54.4) " " 0.707" ""   " 0.021"
#>   eventtime (mean (SD)) " 2.20 (1.82)"  " 1.99 (1.81)"  " 0.019" ""   " 0.117"
#>   status (mean (SD))    " 0.80 (0.40)"  " 0.83 (0.38)"  " 0.217" ""   " 0.062"
#>   ecog_cat = 1 (%)      "  779 (61.1) " "  193 (59.0) " " 0.523" ""   " 0.043"
#>   egfr_cat = 1 (%)      "  252 (20.3) " "   58 (23.8) " " 0.258" ""   " 0.084"
```
