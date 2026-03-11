# Takes an object of class smdi and styles it to a publication-ready gt table

This function takes either an object of class smdi or data.frame or
tibble as input and styles it to a publication-ready table based on the
gt package. The output is of class gt and can take further gt-based
arguments for customization.

## Usage

``` r
smdi_style_gt(
  smdi_object = NULL,
  include_little = TRUE,
  font_size = 13,
  tbl_width = 800
)
```

## Arguments

- smdi_object:

  object of class "smdi" or data.frame/tibble

- include_little:

  can be logical (TRUE/FALSE) for displaying Little's p-value that is
  part of an "smdi" object or a separate object of class "little"

- font_size:

  integer to determine table font size

- tbl_width:

  integer to determine table width

## Value

returns a formatted gt table object

## Details

**\[experimental\]**

## See also

[`gt`](https://gt.rstudio.com/reference/gt.html)

## Examples

``` r
library(smdi)
library(dplyr)

smdi_diagnose(
  data = smdi_data,
  covar = "egfr_cat",
  model = "cox",
  form_lhs = "Surv(eventtime, status)"
  ) %>%
smdi_style_gt()


  

Covariate
```
