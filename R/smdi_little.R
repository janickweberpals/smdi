#' Computes Little's test
#'
#' @description
#' Little’s chi-squared test takes into account possible patterns of missingness across all variables in the dataset.
#' Rejection of the null hypothesis of this test would provide sufficient evidence to indicate that the data are (globally) not MCAR.
#' Please note that compared to smdi_hotelling(), this function tests for MCAR globally across all missing covariates.
#'
#'#' #' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @details
#' Wrapper of mcar_test (naniar package)
#'
#' @references
#' 	Little RJA. A Test of Missing Completely at Random for Multivariate Data with Missing Values.
#' 	J Am Stat Assoc. 1988;83(404):1198-1202. doi:10.1080/01621459.1988.10478722
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
#' @importFrom naniar hotelling.test
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

smdi_hotelling <- function(data = NULL
                           ){


  #covariate <- `1 vs 2` <- NULL

  little <- naniar::mcar_test(
    data_miss %>% dplyr::select(-coi_missing_indicator))

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


