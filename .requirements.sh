#!/bin/bash

# Note: 'parallel' ships with base R and does not need to be installed

R -e "install.packages('pak')"

# Development and documentation
R -e "pak::pak('devtools')"
R -e "pak::pak('usethis')"
R -e "pak::pak('roxygen2@7.3.2')"
R -e "pak::pak('knitr')"
R -e "pak::pak('rmarkdown')"
R -e "pak::pak('here')"

# Testing
R -e "pak::pak('testthat')"
R -e "pak::pak('covr')"
R -e "pak::pak('diffviewer')"
R -e "pak::pak('diffdf')"

# Data wrangling and utilities
R -e "pak::pak('tidyverse')"
R -e "pak::pak('magrittr')"
R -e "pak::pak('glue')"
R -e "pak::pak('lifecycle')"
R -e "pak::pak('fastDummies')"

# Missing data
R -e "pak::pak('mice')"
R -e "pak::pak('naniar')"
R -e "pak::pak('Hotelling')"

# Modeling and diagnostics
R -e "pak::pak('caret')"
R -e "pak::pak('randomForest')"
R -e "pak::pak('pROC')"
R -e "pak::pak('smd')"

# Survival analysis
R -e "pak::pak('survival')"
R -e "pak::pak('survminer')"
R -e "pak::pak('simsurv')"

# Tables and reporting
R -e "pak::pak('broom')"
R -e "pak::pak('broom.helpers')"
R -e "pak::pak('tableone')"
R -e "pak::pak('gt')"
R -e "pak::pak('gtsummary')"
R -e "pak::pak('cardx')"
R -e "pak::pak('gridExtra')"
R -e "pak::pak('reactable')"
R -e "pak::pak('reactR')"
