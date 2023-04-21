# smdi <img src="./man/figures/smdi_hexagon.png" align="right" width="180"/>

**`S`tructural `M`issing `D`ata `I`nvestigations**

This package aims to be a helpful addition to routine healthcare database analytics with a focus on structural missing data investigations.

::: {style="color:red"}
Please note that this version is currently in beta testing phase and no major decisions should be based on this package. If you encounter any unforeseen errors or have any suggestions, comments or recommendations, please feel free to reach out to [jweberpals\@bwh.harvard.edu](mailto:jweberpals@bwh.harvard.edu) or [open an issue](https://gitlab.partners.org/janickweberpals/smdi/-/issues).
:::

## Installation

You can install the latest release version of `smdi` using the `devtools` package via:

``` r
devtools::install_git("https://gitlab-scm.partners.org/janickweberpals/smdi.git")
```

To install the development version, please use the `dev` branch:

``` r
devtools::install_git("https://gitlab-scm.partners.org/janickweberpals/smdi.git", ref = "dev")
```

Once beta testing has completed, we plan to submit the package to CRAN. Stay tuned!

## About

The core of this package aims to conveniently implement routine missing covariate data diagnostics in epidemiological studies.

The theoretical backbone of this package is based on a plasmode simulation performed by the Sentinel Innovation Center [workgroup](https://www.sentinelinitiative.org/methods-data-tools/methods/approaches-handling-partially-observed-confounder-data-electronic-health) on

*Approaches to Handling Partially Observed Confounder Data From Electronic Health Records (EHR) In Non-randomized Studies of Medication Outcomes*.

In brief, the simulation found that the combination of three group diagnostics may give researchers some idea about the potential underlying missing data mechanisms of covariates in the dataset at hand. Aiming to differentiate between missingness mechanisms, these group diagnostics check the following:

-   **Group diagnostic 1:** Comparison of distributions between patients with or without an observed value of the partially observed covariate

-   **Group diagnostic 2:** Assessing the ability to predict missingness based on observed covariates

-   **Group diagnostic 3:** Estimating if the missingness of a covariate is associated with the outcome (differential missingness)

![Table. Overview three group diagnostics](./vignettes/smdi_diagnose_table.png)

If you want to learn more, check out the presentation at the 2023 Innovation Day

<iframe width="750" height="421.875" src="https://www.youtube.com/embed/Dml_AP0ohTY?start=7376" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

## Package website

Check out the `smdi` pkgdown website:

[janickweberpals.gitlab-pages.partners.org/smdi](https://janickweberpals.gitlab-pages.partners.org/smdi)

## References

Lee KJ, Tilling KM, Cornish RP, Little RJA, Bell ML, Goetghebeur E, Hogan JW, Carpenter JR; STRATOS initiative. Framework for the treatment and reporting of missing data in observational studies: The Treatment And Reporting of Missing data in Observational Studies framework. J Clin Epidemiol. 2021 Jun;134:79-88. doi: 10.1016/j.jclinepi.2021.01.008. Epub 2021 Feb 2. PMID: 33539930; PMCID: PMC8168830.

Weberpals J, Sondhi A, Jiang C, Yerram P, Taylor MD, Samant M, Cherng ST. A Systematic Approach Towards Missing Lab Data in Electronic Health Records: A Case Study in Non-Small Cell Lung Cancer and Multiple Myeloma. 37th International Conference on Pharmacoepidemiology & Therapeutic Risk Management 2021. Pharmacoepidemiol Drug Saf 2021; 30:36-36.

Carpenter JR, Smuk M. Missing data: A statistical framework for practice. Biom J. 2021 Jun;63(5):915-947. doi: 10.1002/bimj.202000196. Epub 2021 Feb 24. PMID: 33624862.
