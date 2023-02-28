#' Util helper to create binary missing indicator variables by two different strategies which can be selected ("retain" and "drop")
#'
#' @description
#'This function takes a dataframe and creates binay missing indicator for all covariates which have at least one missing covariate. This can be with two
#'different approaches.
#'
#'Approach 1 ("retain"): "retains" information on partially observed covariates
#'Continuous/numeric variables: binary missing indicator categories are created and the NA values are imputed with the column median
#'Categorical variables: variable gets one-hot-encoded and a missing indicator category is assigned, i.e. all categories are 0 but the
#'newly created NA category is 1
#'
#'Approach 2 ("drop"): only creates a binary missing indicator variable and "drops" all variables with at least one missing value
#'Continuous/numeric variables: binary missing indicator categories are created and the NA values are imputed with the column median
#'Categorical variables: variable gets one-hot-encoded and a missing indicator category is assigned, i.e. all categories are 0 but the
#'newly created NA category is 1
#'
#'Important: Make sure you have your variables format correct and avoid to include variables like ID variables, ZIP codes, dates, etc.
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param cont_n_levels integer, number of unique levels of a variable to classify it as a continuous variable (as opposed to a categorical one). Default is 10. Only needed when strategy = "retain".
#' @param na_strategy "retain" or "drop" according to the two possible strategies
#' @param ... additional parameters
#'
#' @return returns the dataframe with one-hot-encoded covariates with missing indicator variables
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
#'   select(-id) %>%
#'   smdi_na_indicator(na_strategy = "drop") %>%
#'   glimpse()
#'
smdi_na_indicator <- function(data = NULL,
                              cont_n_levels = 10,
                              na_strategy = c("retain", "drop"),
                              ...
                              ){

  # initializing new variables

  # additional arguments
  add_args <- list(...)

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}
  if(!na_strategy %in% c("retain", "drop")){stop("na_strategy: you must select one of <retain> or <drop>. There is no default.")}

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
      dplyr::mutate(dplyr::across(c(cont_covars), ~ ifelse(is.na(.x), stats::median(.x, na.rm = TRUE), .x))) %>%
      # next we one-hot encode all remaining binary/categorical variables
      fastDummies::dummy_cols(
        ignore_na = FALSE,
        remove_selected_columns = TRUE,
        remove_first_dummy = TRUE
        ) %>%
      # fill up 0's of remaining categorical variables
      dplyr::mutate(dplyr::across(tidyselect::everything(), ~ ifelse(is.na(.x), 0, .x)))

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

