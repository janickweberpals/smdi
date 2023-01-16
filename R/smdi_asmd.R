#' Compute mean/median absolute standardized mean differences
#'
#' @description
#' This function takes a dataframe and covariates which are only partially observed/missing.
#' The dataframe should consist of the exposure variable, the outcome variable(s), the partially observed covariates
#' and all other fully observed covariates which are deemed important for the final modeling
#' or which could be considered as auxiliary variables. If no partially observed covariates are provided,
#' the function automatically looks for all variables/columns with NA.
#'
#'Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with variable/column name(s) to investigate
#' @param median logical if the median (recommended) or mean of all absolute standardized mean differences should be computed
#'
#' @return returns mean/median absolute standardized mean differences
#'
#' @importFrom magrittr '%>%'
#' @importFrom tableone CreateTableOne
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'df %>%
#' smdi_asmd(covar = c("age", "gender", "bmi"))
#' }

smdi_asmd <- function(data = NULL,
                      covar = NULL,
                      median = TRUE
                      ){

  # initializing new variables
  xxx <- NULL

  # pre-checks
  if(is.null(strata)){stop("No stratum variable provided, can't compute differences.")}

  # covariate selection
  covar_miss_all <- smdi::smdi_summarize(data = data)

  if(!is.null(covar)){

    # from all missing covars, select and use only the specified ones
    covar_miss <- covar_miss_all %>%
      dplyr::filter(covariate %in% covar)

  }else if(is.null(covar)){

    covar_miss <- covar_miss_all

  }else{

    stop("Found no covariates with missing values. Check that missing values are coded as <NA>.")

  }


  # start applying smd computation over all partially observed covariates
  smd_loop <- function(i){

    data_tmp <- data %>%
      dplyr::mutate(na_indicator = ifelse(is.na(.data[[i]]), glue::glue("{i} not observed"), glue::glue("{i} observed")))

    smd_tmp <- tableone::CreateTableOne(
      data = data_tmp,
      vars = names(data_tmp),
      strata = "na_indicator",
      smd = TRUE
      ) %>%
      tableone::ExtractSmd() %>%
      stats::median()

  }

  smd_list <- lapply(i, )


  return(plot_summary)

}

