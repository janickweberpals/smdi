#' smdi exemplary lung cancer dataset (with complete data)
#'
#'
#' Report ...
#'
#' @format ## `smdi_data_complete`
#' A data frame with 1,000 rows and 14 columns:
#' \describe{
#'   \item{exposure}{Treatment assignment variable (binary). Indicates initiation of the exposure of interest (1) versus a comparator regimen (0)}
#'   \item{age_num}{Age at baseline in years}
#'   \item{female_cat}{Is gender female (0 = no, 1 = yes)}
#'   \item{ecog_cat}{ECOG performance score at baseline (0 versus 1)}
#'   \item{smoking_cat}{Smoking status at baseline (0 = non-smoker, 1 = smoker)}
#'   \item{physical_cat}{Physical activity at baseline (not active versus active)}
#'   \item{egfr_cat}{EGFR mutation status (0 = wild-type, 1 = alteration)}
#'   \item{alk_cat}{ALK transolcation mutation status (0 = wild-type, 1 = alteration)}
#'   \item{pdl1_num}{PD-L1 cell staining biomarker in %}
#'   \item{histology_cat}{Tumor histology (0 = nonsquamous, 1 = squamous)}
#'   \item{ses_cat}{Socio-economic status (multi-categorical: 1-low, 2-middle, 3-high)}
#'   \item{copd_cat}{COPD comorbidity at baseline}
#'   \item{eventtime}{time to censoring event}
#'   \item{status}{event indicator at time t; 0 = censored, 1 = deceased}
#'   ...
#' }
#' @source <https://janickweberpals.gitlab-pages.partners.org/smdi/articles/data_generation.html>
"smdi_data_complete"
