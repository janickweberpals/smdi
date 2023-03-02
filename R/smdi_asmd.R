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
#' @param covar character covariate or covariate vector with variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation
#' @param na_strategy "retain" or "drop" covariates after creating misisng indicator variables; more info see ?smdi::smdi_na_indicator()
#' @param median logical if the median (recommended default) or mean of all absolute standardized mean differences should be computed
#' @param ... further arguments
#' @return returns mean/median absolute standardized mean differences
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
#' @importFrom tableone CreateTableOne
#' @importFrom tableone ExtractSmd
#' @importFrom tidyselect all_of
#' @importFrom tidyselect everything
#' @importFrom tidyselect where
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(smdi)
#' library(dplyr)
#'
#'df <- smdi_data %>%
#'select(-id) %>%
#'mutate(across(ends_with("cat"), as.factor))
#'df %>%
#' smdi_asmd(covar = c("age", "gender", "bmi"))
#' }

smdi_asmd <- function(data = NULL,
                      covar = NULL,
                      na_strategy = c("retain", "drop"),
                      median = TRUE,
                      ...
                      ){

  # additional arguments
  add_args <- list(...)

  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )


  # start applying smd computation over all partially observed covariates
  smd_loop <- function(i){

    # create missing indicator
    data_encoded <- data %>%
      smdi::smdi_na_indicator(
        na_strategy = na_strategy
      )

    # create tableone
    tbl1 <- tableone::CreateTableOne(
        data = data_tmp,
        vars = names(data_tmp %>% dplyr::select(-na_indicator)),
        strata = "na_indicator",
        # if multiple variables are missing, NA is an own category
        includeNA = TRUE
        )

    smd <- tableone::ExtractSmd(tbl1)

      as.data.frame() %>%
      tibble::rownames_to_column(var = "covariate") %>%
      dplyr::rename(smd = `1 vs 2`)

    if(isTRUE(median)){

      smd_summary <- tibble::tibble(
        covariate = paste(i),
        smd_median = stats::median(smd_tmp[,1])
        )

    }else{

      smd_summary <- tibble::tibble(
        covariate = paste(i),
        smd_mean = mean(smd_tmp[,1])
        )

    }

    return(smd_summary)

  }

  smd_list <- lapply(covar_miss$covariate, FUN = smd_loop)

  smd_return <- covar_miss_all %>%
    dplyr::left_join(
      do.call(rbind, smd_list),
      by = "covariate"
      )

  return(smd_return)

}

