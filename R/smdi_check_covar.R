#' This is a util function to help check input data and covariates provided
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation
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

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}

  if(!is.null(covar)){

    # check if all provided covars are in df
    if(any(!covar %in% colnames(data))){

      stop("Not all covariates specified in <covar> are present in the <data> provided.")

    }else{

      covar_miss <- covar

    }

  }else{ # if covar = NULL

    # call smdi_summarize: select all covariates with missing values
    covar_miss <- smdi::smdi_summarize(data = data) %>%
      dplyr::pull(covariate)

    if(length(covar_miss)==0){

      stop("Found no covariates with missing values. Please either provide covariate names or check that missing values are coded as <NA>.")

    }

  }

  return(covar_miss)

}
