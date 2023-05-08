#' This is a util function to help check input data and covariates provided
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation
#'
#' @return returns the covariate vector for subsequent tasks or warnings/errors
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr pull
#'
#' @export
#'

smdi_check_covar <- function(data = NULL,
                             covar = NULL
                             ){

  # check if data is provided
  if(is.null(data)){stop("No dataframe provided.")}


  # if <covar> is specified -> select variables, if not find all with at least one NA (latter if default) --------

  # if <covar> is specified:
  if(!is.null(covar)){

    # give an error of not all covariates specified in <covar> are present in <data>
    if(any(!covar %in% colnames(data))){

      stop("Not all covariates specified in <covar> are present in the <data> provided.")

    }else{ # if all covariates in <covar> are present

      # return covariates specified in <covar> for subsequent operations
      covar_miss <- covar

    }

  # if covar is not specified, i.e. is null
  }else{

    # select all covariates that have at least one NA value
    covar_miss <- data %>%
      dplyr::select(tidyselect::where(~sum(is.na(.x)) > 0)) %>%
      names()

    # give error if there are no covariates with at least one NA value
    if(length(covar_miss)==0){

      stop("Found no covariates with missing values. Please check that missing values are coded as <NA>.")

    }

  }

  # return covariate vector
  return(covar_miss)

}
