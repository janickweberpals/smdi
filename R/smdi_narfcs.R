#' Wrapper of NARFCS enhanced forked mice version: https://raw.githack.com/moreno-betancur/NARFCS/master/Vignette.html
#'
#' @description
#' This function takes a dataframe and returns all columns which are partially observed with
#' the amount of observations missing and corresponding proportion assuming a one-row-per-patient
#' dataframe.
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
#' smdi_narfcs()
#' }

smdi_narfcs <- function(data = NULL
                           ){



}

