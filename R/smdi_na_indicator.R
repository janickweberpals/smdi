#' Create binary missing indicator variables by two different strategies
#'
#' @description
#'This function takes a dataframe and creates binary missing indicator variable. This can be realized with two
#'different approaches:
#'
#'Approach 1 (drop_NA_col = FALSE): creates a binary missing indicator variable for partially observed variables and retains both original and indicator variables.
#'
#'Approach 2 (drop_NA_col = TRUE): creates a binary missing indicator variable for partially observed variables and only retains indicator variables (and drops the original variables).
#'
#'Important: Make sure you have your variables format correct and avoid to include variables like ID variables, ZIP codes, dates, etc.
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation.
#' @param drop_NA_col logical, drop specified columns with NA (default) or retain those columns
#'
#' @return returns the dataframe with missing indicator variables (column names are ending on "_NA")
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr across
#' @importFrom tidyselect all_of
#'
#' @export
#'
#' @examples
#'library(smdi)
#'library(dplyr)
#'
#' smdi_data %>%
#'   smdi_na_indicator(drop_NA_col = FALSE) %>%
#'   glimpse()
#'
#' smdi_data %>%
#'   smdi_na_indicator(drop_NA_col = TRUE) %>%
#'   glimpse()
#'
smdi_na_indicator <- function(data = NULL,
                              covar = NULL,
                              drop_NA_col = TRUE
                              ){

  # initializing new variables

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}

  # check for missing covariates
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )


  # Strategy 1: columns with NA will be retained ----------------------------
  data_encoded <- data %>%
    # first we create a NA category for continuous covariates and then median impute the missings
    dplyr::mutate(dplyr::across(tidyselect::all_of(covar_miss), ~ ifelse(is.na(.x), 1, 0), .names = "{.col}_NA"))

  # Strategy 2: columns with NA will be dropped -----------------------------
  if(isTRUE(drop_NA_col)){

    data_encoded <- data_encoded %>%
      dplyr::select(-tidyselect::all_of(covar_miss))

    }


  return(data_encoded)

}

