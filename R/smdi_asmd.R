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
#' @param median logical if the median (recommended default) or mean of all absolute standardized mean differences should be computed
#'
#' @return returns mean/median absolute standardized mean differences
#'
#' @importFrom magrittr '%>%'
#' @importFrom tableone CreateTableOne
#' @importFrom tableone ExtractSmd
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
#' @importFrom stats median
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
  na_indicator <-  NULL

  # select covariates with missing values
  covar_miss_all <- smdi::smdi_summarize(data = data) %>%
    dplyr::pull(covariate)

  if(!is.null(covar)){

    # from all missing covars, select and use only the specified ones
    covar_miss <- covar_miss_all %>%
      dplyr::filter(covariate %in% covar)

  }else if(is.null(covar)){

    covar_miss <- covar_miss_all

  }else{

    stop("Found no covariates with missing values. Please either provide covariate names or check that missing values are coded as <NA>.")

  }


  # start applying smd computation over all partially observed covariates
  smd_loop <- function(i){

    data_tmp <- data %>%
      dplyr::mutate(na_indicator = ifelse(is.na(.data[[i]]), 1, 0)) %>%
      dplyr::select(-tidyselect::all_of(i))

    tbl1_tmp <- tableone::CreateTableOne(
      data = data_tmp,
      vars = names(data_tmp %>% dplyr::select(-na_indicator)),
      strata = "na_indicator",
      # if multiple variables are missing, NA is an own category
      includeNA = TRUE,
      smd = TRUE
      )

      tableone::ExtractSmd()

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

