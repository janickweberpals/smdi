# Computes random forest-based AUC

The function trains and fits a random forest model to assess the ability
to predict missingness for the specified covariate(s). If missing
indicator can be predicted as a function of observed covariates, MAR may
be a likely scenario and would imply that imputation may be feasible.

Important: don't include variables like ID variables, ZIP codes, dates,
etc.

## Usage

``` r
smdi_rf(
  data = NULL,
  covar = NULL,
  train_test_ratio = c(0.7, 0.3),
  tune = FALSE,
  set_seed = 42,
  ntree = 1000,
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

- train_test_ratio:

  numeric vector to indicate the test/train split ratio, e.g. c(.7, .3)
  which is the default

- tune:

  logical,if TRUE, a 5-fold cross validation is performed combined with
  a random search for the optimal number of optimal number of variables
  randomly sampled as candidates at each split (mtry). FALSE is the
  default due to potentially extensive computation times.

- set_seed:

  seed for reproducibility, defaults to 42

- ntree:

  integer, number of trees (defaults to 1000 trees)

- n_cores:

  integer, if \>1, computations will be parallelized across amount of
  cores specified in n_cores (only UNIX systems)

## Value

returns an rf object which comes as a list that contains the ROC AUC
value and corresponding variable importance in training dataset (latter
as ggplot object). That is, for each covar, the following outputs are
provided:

- rf_table: The area under the receiver operating curve (AUC) as a
  measure of the ability to predict the missingness of the partially
  observed covariate

- rf_plot: ggplot object illustrating the variable importance for the
  prediction made expressed by the mean decrease in accuracy per
  predictor. That is how much would the accuracy of the prediction (# of
  correct predictions/Total \# of predictions made) decrease, had we
  left out this specific predictor.

- OOB: estimated OOB error for each investigated partially observed
  confounder (indicates the performance of the random forest model for
  data points that are not used in training a tree.)

## Details

The random forest utilizes the
[randomForest](https://rdrr.io/pkg/randomForest/man/randomForest.html)
engine.

CAVE: If the missingness indicator variables of other partially observed
covariates (indicated by suffix \_NA) have an extremely high variable
importance (combined with an unusually high AUC), this might be an
indicator of a monotone missing data pattern. In this case it is
advisable to exclude other partially observed covariates and run
missingness diagnostics separately.

## References

Sondhi A, Weberpals J, Yerram P, Jiang C, Taylor M, Samant M, Cherng S.
A systematic approach towards missing lab data in electronic health
records: A case study in non-small cell lung cancer and multiple
myeloma. CPT Pharmacometrics Syst Pharmacol. 2023 Jun 15. \<doi:
10.1002/psp4.12998.\> Epub ahead of print. PMID: 37322818.

## See also

[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)

## Examples

``` r
library(smdi)

smdi_rf(data = smdi_data, covar = "ecog_cat")
#> # A tibble: 1 × 2
#>   covariate rf_auc
#> * <chr>     <chr> 
#> 1 ecog_cat  0.510 
```
