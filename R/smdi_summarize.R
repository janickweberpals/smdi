#' Util helper to automatically extract all covariates with partially observed covariates
#'
#' @description
#' This function takes a dataframe and automatically returns all columns which are partially observed with
#' the amount of observations missing and corresponding proportion assuming a one-row-per-patient
#' dataframe. This is an important utility function for many other functions in this package
#'
#'Important: avoid to include variables like ID variables, ZIP codes, dates, etc.
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#'
#' @return returns a table with covariate name, amount missing observations and proportion missing.
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr summarize_all
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom tidyselect everything
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'df %>%
#' smdi_summarize()
#' }

smdi_summarize <- function(data = NULL
                           ){

  # initializing new variables
  covariate <- n_miss <- perc_miss <- NULL

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}

  covar_miss <- data %>%
    dplyr::summarize_all(function(x) n_miss = sum(is.na(x))) %>%
    tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "covariate", values_to = "n_miss") %>%
    dplyr::filter(n_miss > 0) %>%
    dplyr::mutate(perc_miss = n_miss/nrow(data)*100) %>%
    dplyr::arrange(dplyr::desc(perc_miss), covariate) %>%
    dplyr::mutate(perc_miss = paste0(formatC(perc_miss, format = 'f', digits = 2), "%"))

  if(length(covar_miss$covariate)==0){

    message("Note: Found no covariates with missing values. Check that missing values are coded as <NA>.")

  }

  return(covar_miss)

}

