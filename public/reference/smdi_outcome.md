# Computes association between missingness and outcome

This function fits outcome models with a covariate missingness
indicator(s) of the covariates specified with *covar*. The estimates are
computed by univariate and adjusted models on all other prognostic
covariates in the dataset. Based on the underlying missingness
mechanism, the estimate for the covariate missingness indicator may
indicate a meaningful difference in the outcome between patients with vs
w/o the observed confounder conditional on other covariates that could
explain that difference.

## Usage

``` r
smdi_outcome(
  data = NULL,
  covar = NULL,
  model = c("glm", "linear", "cox"),
  glm_family = NULL,
  form_lhs = NULL,
  exponentiated = FALSE,
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

- model:

  character describing which outcome model to fit to assess the
  association between covar missingness indicator and outcome. Currently
  supported are models of type glm, linear and cox

- glm_family:

  glm family object to specify a certain family of generalized linear
  models (e.g., binomial, gaussian, Gamma, poisson, etc.). For all
  options see ?stats::family

- form_lhs:

  string specifying the left-hand side of the outcome formula (see
  details)

- exponentiated:

  logical, should results be exponentiated (default is FALSE)

- n_cores:

  integer, if \>1, computations will be parallelized across amount of
  cores specified in n_cores (only UNIX systems)

## Value

returns a tibble with univariate and adjusted estimates for each
partially observed covar:

- estimate_univariate: univariate association between missingness
  indicator of covar and outcome

- estimate_adjusted: association between missingness indicator of covar
  and outcome conditional on other fully observed covariates and missing
  indicator variables of other partially observed covariates

## Details

The function automatically fits a univariate and adjusted outcome model.
The currently supported models are glm (glm), linear (lm) and cox
(survival). For adjusted models, the function uses all available
covariates found in the dataset specified with the data parameter. If
covariates should not be include in the outcome model, these covariates
should be dropped beforehand (as with all other functions in the smdi
package).

The left-hand side of the formula (form_lhs) needs to specify the
outcome in one of the following ways:

- glm (binary): character of column name with binary outcome, e.g.
  "MACE"

- lm (continuous): character of column name with binary outcome, e.g.
  "WEIGHT_LOSS"

- cox (time-to-event): LHS specifying time-to-event outcome, e.g.
  "Surv(TIME, STATUS)"

## References

...

## See also

[`glm`](https://rdrr.io/r/stats/glm.html)
[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html)

## Examples

``` r
library(smdi)

smdi_outcome(
 data = smdi_data,
 model = "cox",
 form_lhs = "Surv(eventtime, status)"
)
#> # A tibble: 3 × 3
#>   covariate estimate_univariate        estimate_adjusted         
#>   <chr>     <glue>                     <glue>                    
#> 1 ecog_cat  -0.06 (95% CI -0.16, 0.03) -0.06 (95% CI -0.16, 0.03)
#> 2 egfr_cat  0.06 (95% CI -0.03, 0.15)  -0.01 (95% CI -0.10, 0.09)
#> 3 pdl1_num  0.12 (95% CI 0.01, 0.23)   0.11 (95% CI -0.00, 0.22) 

```
