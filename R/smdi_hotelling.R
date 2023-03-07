#' Computes hotelling's multivariate t-test
#'
#' @description
#' Hotelling's multivariate t-test, which examines variable
#' differences conditional on having an observed covariate value or not.
#' As the power of statistical hypothesis tests can be influenced by
#' sample size, the combined investigation along with smdi_asmd() is highly recommended.
#'
#' @details
#' Wrapper of Hotelling::hotelling.test()
#' #' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @references
#' Hotelling H. The Generalization of Student’s Ratio. Ann Math Stat. 1931;2(3):360-378. doi:10.1214/aoms/1177732979
#'
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation and all remaining covariates will be used as predictors
#'
#' @return returns an asmd object with mean/median absolute standardized mean differences
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
#' @importFrom Hotelling hotelling.test
#' @importFrom tableone ExtractSmd
#' @importFrom tidyselect all_of
#' @importFrom tidyselect everything
#' @importFrom tidyselect where
#'
#' @export
#'
#' @examples
#'\dontrun{
#' library(smdi)
#' library(dplyr)
#'
#' }

smdi_hotelling <- function(data = NULL,
                           covar = NULL
                           ){


  #covariate <- `1 vs 2` <- NULL

  # pick missing indicator columns/partially observed covariates
  # check for missing covariates
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )

  # apply smdi_na_indicator for i to create missing
  # indicator variable
  strata_df <- smdi::smdi_na_indicator(
    data = data,
    covar = covar,
    drop_NA_col = TRUE
    )

  # start applying smd computation over all partially observed covariates
  smd_loop <- function(i){


    # create strata variable
    strata_var <- paste0(i, "_NA")

    # create matrices
    hotelling_matrix_missing <- data_miss_diagnostics %>%
      dplyr::filter(strata_var == 1) %>%
      dplyr::select(-strata_var) %>%
      as.matrix()

    hotelling_matrix_complete <- data_miss_diagnostics %>%
      dplyr::filter(coi_missing_indicator == 0) %>%
      dplyr::select(-coi_missing_indicator) %>%
      as.matrix()

    hotelling <- Hotelling::hotelling.test(hotelling_matrix_missing, hotelling_matrix_complete)

  }

}

#' @export
print.asmd <- function(x, ...){


  tbl <- do.call(rbind, lapply(x,'[[',4))
  cat("Summary ASMD table: \n")
  cat(tbl)

  return(tbl)

}


