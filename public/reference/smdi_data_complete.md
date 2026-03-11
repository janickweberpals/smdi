# smdi exemplary lung cancer dataset (with complete data)

Example dataset with complete cases.

## Usage

``` r
smdi_data_complete
```

## Format

### `smdi_data_complete`

A data frame with 2,500 rows and 14 columns:

- exposure:

  Treatment assignment variable (binary). Indicates initiation of the
  exposure of interest (1) versus a comparator regimen (0)

- age_num:

  Age at baseline in years

- female_cat:

  Is gender female (0 = no, 1 = yes)

- ecog_cat:

  ECOG performance score at baseline (0 versus 1)

- smoking_cat:

  Smoking status at baseline (0 = non-smoker, 1 = smoker)

- physical_cat:

  Physical activity at baseline (not active versus active)

- egfr_cat:

  EGFR mutation status (0 = wild-type, 1 = alteration)

- alk_cat:

  ALK transolcation mutation status (0 = wild-type, 1 = alteration)

- pdl1_num:

  PD-L1 cell staining biomarker in %

- histology_cat:

  Tumor histology (0 = nonsquamous, 1 = squamous)

- ses_cat:

  Socio-economic status (multi-categorical: 1-low, 2-middle, 3-high)

- copd_cat:

  COPD comorbidity at baseline

- eventtime:

  time to censoring event

- status:

  event indicator at time t; 0 = censored, 1 = deceased

## Source

https://janickweberpals.gitlab-pages.partners.org/smdi/articles/data_generation.html
