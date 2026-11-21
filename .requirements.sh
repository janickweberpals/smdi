#!/bin/bash

# Note: 'parallel' ships with base R and does not need to be installed

R -e "install.packages('pak')"

# Development and documentation
R -e "pak::pak(c(
  'devtools',
  'usethis',
  'roxygen2@7.3.2',
  'knitr',
  'rmarkdown',
  'here'
))"

# Testing
R -e "pak::pak(c(
  'testthat',
  'vdiffr',
  'covr',
  'diffviewer',
  'diffdf'
))"

# Data wrangling and utilities
R -e "pak::pak(c(
  'tidyverse',
  'magrittr',
  'glue',
  'lifecycle',
  'fastDummies'
))"

# Missing data
R -e "pak::pak(c(
  'mice',
  'naniar',
  'Hotelling'
))"

# Modeling and diagnostics
R -e "pak::pak(c(
  'caret',
  'randomForest',
  'pROC',
  'smd'
))"

# Survival analysis
R -e "pak::pak(c(
  'survival',
  'survminer',
  'simsurv'
))"

# Tables and reporting
R -e "pak::pak(c(
  'broom',
  'broom.helpers',
  'tableone',
  'gt',
  'gtsummary',
  'cardx',
  'gridExtra',
  'reactable',
  'reactR'
))"
