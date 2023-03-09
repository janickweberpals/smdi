# smdi <img src="./man/figures/smdi_hexagon.png" align="right" width="180"/>

**`S`tructural `M`issing `D`ata `I`nvestigations**

[![](https://cranlogs.r-pkg.org/badges/smdi)](https://cran.rstudio.com/web/packages/smdi/index.html)

This package aims to be a helpful addition to routine healthcare database analytics with a focus on structural missing data investigations.

>Please keep in mind this package is still in beta

## Installation

You can install the latest version of `smdi` with:

```r
remotes::install_github("https://gitlab-scm.partners.org/janickweberpals/smdi")
```

## Package website

Check out the `smdi` pkgdown website:

<https://janickweberpals.gitlab-pages.partners.org/smdi>

## About

The core of this package aims to conveniently implement routine missing covariate data diagnostics in epidemiological studies.

The theoretical backbone of this package is based on a plasmode simulation performed by the Sentinel Innovation Center [workgroup]((https://www.sentinelinitiative.org/methods-data-tools/methods/approaches-handling-partially-observed-confounder-data-electronic-health)) on 

*Approaches to Handling Partially Observed Confounder Data From Electronic Health Records (EHR) In Non-randomized Studies of Medication Outcomes*.

In brief, the simulation found that the combination of three group diagnostics may give researchers some idea about the potential underlying missing data mechanisms of covariates in the dataset at hand. Aiming to differentiate between missingness mechanisms, these group diagnostics check the following: 

* **Group diagnostic 1:** Comparison of distributions between patients with or without an observed value of the partially observed covariate

* **Group diagnostic 2:** Assessing the ability to predict missingness based on observed covariates

* **Group diagnostic 3:** Estimating if the missingness of a covariate is associated with the outcome (differential missingness)

![Table. Overview three group diagnostics](./vignettes/smdi_diagnose_table.png)

## Vignettes

To learn how to use the `smdi` R package, refer to the package vignettes:

```r
utils::browseVignettes("smdi")
```

## More References
