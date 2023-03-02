#' Util helper to create binary missing indicator variables by two different strategies, one of which must be selected ("retain" and "drop")
#'
#' @description
#'This function takes a dataframe and creates binay missing indicator for all covariates which have at least one missing covariate. This can be realized with two
#'different approaches:
#'
#'Approach 1 ("retain"): "retains" variables with missing observations, i.e.
#'
#'Approach 2 ("drop"): only creates a binary missing indicator variable for variables with missing observations and "drops" the original variables with missing observations.
#'
#'Important: Make sure you have your variables format correct and avoid to include variables like ID variables, ZIP codes, dates, etc.
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation
#' @param na_strategy "retain" or "drop" according to the two possible strategies
#'
#' @return returns the dataframe with one-hot-encoded covariates with missing indicator variables (ending on "_NA")
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr summarize_all
#' @importFrom fastDummies dummy_cols
#' @importFrom stats median
#' @importFrom tidyselect all_of
#' @importFrom tidyselect everything
#' @importFrom tidyselect where
#'
#' @export
#'
#' @examples
#'library(smdi)
#'library(dplyr)
#'
#' smdi_data %>%
#'   smdi_na_indicator(na_strategy = "retain") %>%
#'   glimpse()
#'
#' smdi_data %>%
#'   smdi_na_indicator(na_strategy = "drop") %>%
#'   glimpse()
#'
smdi_na_indicator <- function(data = NULL,
                              covar = NULL,
                              na_strategy = c("retain", "drop")
                              ){

  # initializing new variables

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}
  if(!na_strategy %in% c("retain", "drop")){stop("<na_strategy>: you must select one of <retain> or <drop>. There is no default.")}

  if(na_strategy == "retain"){

    # Strategy 1 --------------------------------------------------------------
    # Continuous/numeric variables: we create binary missing indicator categories and the NA is imputed with the column median
    # Categorical variables: variable gets one-hot-encoded and a missing indicator category is assigned, i.e. all categories are 0 but NA category is 1

    # step 1.1: identify continuous variables
    cont_covars <- data %>%
      dplyr::select(tidyselect::where(is.numeric) & tidyselect::where(function(.x) nlevels(as.factor(.x)) > cont_n_levels)) %>%
      colnames()

    # step 1.2: execute
    data_encoded <- data %>%
      # first we create a NA category for continuous covariates and then median impute the missings
      dplyr::mutate(dplyr::across(c(cont_covars), ~ ifelse(is.na(.x), 1, 0), .names = "{.col}_NA")) %>%
      dplyr::mutate(dplyr::across(c(cont_covars), ~ ifelse(is.na(.x), stats::median(.x, na.rm = TRUE), .x)))

    }

  if(na_strategy == "drop"){

    # Strategy 2 --------------------------------------------------------------
    # Here, we just create binary missing indiactor variables for all included variables that have at least one missing value

    # step 2.1: run smdi_summarize to identify all variables with NAs
    na_covars <- smdi::smdi_summarize(data) %>%
      dplyr::pull(covariate)

    data_encoded <- data %>%
      # first we create a NA category for continuous covariates and then median impute the missings
      dplyr::mutate(dplyr::across(c(na_covars), ~ ifelse(is.na(.x), 1, 0), .names = "{.col}_NA")) %>%
      dplyr::select(-tidyselect::all_of(na_covars))

    }


  return(data_encoded)

}

