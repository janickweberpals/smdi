#' This is a utility function to help check input data and covariates provided
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically looks for and includes all columns with at least one missing observation
#'
#' @return returns the covariate vector for subsequent tasks or warnings/errors
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr pull
#' @importFrom dplyr pull
#' @importFrom glue glue
#' @importFrom tidyselect where
#' @importFrom tidyselect all_of
#'
#' @export
#'

smdi_check_covar <- function(data = NULL,
                             covar = NULL
                             ){

  # check if data is provided
  if(is.null(data)){stop("No dataframe provided.")}


  # if <covar> is specified -> select specified NA variables (and make sure all are NA), if not automatically find all that have at least one NA value ---

  # if <covar> is specified:
  if(!is.null(covar)){

    # give an error if not all covariates specified in <covar> are present in <data>
    if(any(!covar %in% colnames(data))){

      stop("Not all covariates specified in <covar> are present in the <data> provided.")

    }else{ # if all covariates in <covar> are present

      # check if there are variables specified in <covar> which are NOT missing
      covar_fully_obs <- data %>%
        dplyr::select(tidyselect::all_of(covar)) %>%
        dplyr::select(tidyselect::where(~sum(is.na(.x)) == 0))

      # give warning if not all of the specified <covar> variables have a missing => select only the ones with missing and return message
      if(ncol(covar_fully_obs) > 0){

        covar_fully_obs_collapse <- paste(names(covar_fully_obs), collapse = ", ")

        warning(glue::glue("<{covar_fully_obs_collapse}> specified as part of <covar> but does/do not contain any missing value. Please check that missing values are coded as <NA>. <{covar_fully_obs_collapse}> will not be considered as missing <covar>."))

        # drop fully observed covariates
        covar_miss <- data %>%
          dplyr::select(tidyselect::all_of(covar)) %>%
          dplyr::select(-tidyselect::all_of(names(covar_fully_obs))) %>%
          names()

        }else{ # if all <covar> are present and all have at least one NA

          # return covariates specified in <covar> for subsequent operations
          covar_miss <- covar

        } # end: all covariates are present and have at least one NA

      } # end: if all covariates are present

    # if covar is not specified, i.e. is null
    }else{

      # select all covariates that have at least one NA value
      covar_miss <- data %>%
        dplyr::select(tidyselect::where(~sum(is.na(.x)) > 0)) %>%
        names()

      } # end: if no <covar> are specified

    # give error if there are no covariates with at least one NA value
    if(length(covar_miss)==0){

      stop("Found no covariates with missing values. Please check that missing values are coded as <NA>.")

    }

  # return covariate vector
  return(covar_miss)

}
