# Get started with smdi

``` r
library(smdi)
library(gt)
#> Warning: package 'gt' was built under R version 4.5.2
```

## `smdi_diagnose()` - the flagship function

The `smdi` main function is
[`smdi_diagnose()`](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_diagnose.md)
which calls all three group diagnostics, all of which are also
accessible individually.

[`smdi_diagnose()`](https://janickweberpals.gitlab-pages.partners.org/smdi/index/reference/smdi_diagnose.md)
builds on theoretical concepts developed and validated in a
comprehensive simulation study based on the
[workstream:](https://www.sentinelinitiative.org/methods-data-tools/methods/approaches-handling-partially-observed-confounder-data-electronic-health)

**Approaches to Handling Partially Observed Confounder Data From
Electronic Health Records (EHR) In Non-randomized Studies of Medication
Outcomes**.

A most minimal example could look like this (if you want to accept all
of the default parameters).

``` r
smdi_diagnose(
  data = smdi_data,
  covar = NULL, # NULL includes all covariates with at least one NA
  model = "cox",
  form_lhs = "Surv(eventtime, status)"
  ) %>% 
  smdi_style_gt()
```

[TABLE]
