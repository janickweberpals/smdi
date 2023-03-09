#' Computes Little's test
#'
#' @description
#' Little’s chi-squared test takes into account possible patterns of missingness across all variables in the dataset.
#' Rejection of the null hypothesis of this test would provide sufficient evidence to indicate that the data are (globally) not MCAR.
#' Please note that compared to smdi_hotelling, this function tests for MCAR globally across all missing covariates.
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
#' @param data dataframe or tibble object with partially observed/missing variables
#'
#' @return returns a hotelling object with statistics on hotellings test by covariate. S3 method print returns a summarized dataframe with hypothesis test p-values.
#'
#' @importFrom naniar mcar_test
#'
#' @export
#'
#' @examples
#' library(smdi)
#' library(dplyr)
#'
#' smdi_data %>%
#'  smdi_little()
#'

smdi_little <- function(data = NULL
                        ){

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}

  little <- naniar::mcar_test(data = data)

  class(little) <- "little"

  return(little)

  }
