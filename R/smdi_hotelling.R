#' Computes hotelling's multivariate t-test
#'
#' @description
#' Hotelling's multivariate t-test, which examines variable
#' differences conditional on having an observed covariate value or not.
#' As the power of statistical hypothesis tests can be influenced by
#' sample size, the combined investigation along with smdi_asmd() is highly recommended.
#'
#' @details
#' Wrapper of hotelling.test() (Hotelling package)
#' #' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @references
#' Hotelling H. The Generalization of Student’s Ratio. Ann Math Stat. 1931;2(3):360-378. doi:10.1214/aoms/1177732979
#'
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation and all remaining covariates will be used as predictors
#'
#' @return returns a hotelling object with statistics on hotellings test by covariate. S3 method print returns a summarized dataframe with hypothesis test p-values.
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom fastDummies dummy_cols
#' @importFrom Hotelling hotelling.test
#' @importFrom tibble rownames_to_column
#' @importFrom tidyselect all_of
#'
#' @export
#'
#' @examples
#'\dontrun{
#' library(smdi)
#' library(dplyr)
#'
#' smdi_data %>%
#'  smdi_hotteling()
#'
#' }

smdi_hotelling <- function(data = NULL,
                           covar = NULL
                           ){


  # initialize variables
  .data <- V1 <- NULL

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}

  # pick missing indicator columns/partially observed covariates
  # check for missing covariates
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )

  # apply smdi_na_indicator for datset to create missing
  # indicator variables
  data_encoded <- smdi::smdi_na_indicator(
    data = data,
    covar = covar,
    drop_NA_col = TRUE
    ) %>%
    fastDummies::dummy_columns(
      remove_most_frequent_dummy = TRUE,
      ignore_na = FALSE,
      remove_selected_columns = TRUE
      )

  # start applying smd computation over all partially observed covariates
  hotelling_loop <- function(i){

    # create strata variable
    strata_var <- paste0(i, "_NA")

    # create matrices
    hotelling_matrix_missing <- data_encoded %>%
      dplyr::filter(.data[[strata_var]] == 1) %>%
      dplyr::select(-tidyselect::all_of(strata_var)) %>%
      as.matrix()

    hotelling_matrix_complete <- data_encoded %>%
      dplyr::filter(.data[[strata_var]] == 0) %>%
      dplyr::select(-tidyselect::all_of(strata_var)) %>%
      as.matrix()

    hotelling <- Hotelling::hotelling.test(hotelling_matrix_missing, hotelling_matrix_complete)

    return(hotelling)

    }

  hotelling_out <- lapply(covar_miss, FUN = hotelling_loop)
  names(hotelling_out) <- covar_miss

  class(hotelling_out) <- "hotelling"

  return(hotelling_out)

  }


#' @export
print.hotelling <- function(x, ...){


  tbl <- do.call(rbind, lapply(x,'[[',2)) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "covariate") %>%
    dplyr::mutate(hotteling_p = V1) %>%
    dplyr::select(-V1)

  print(tbl)

  return(tbl)

}


