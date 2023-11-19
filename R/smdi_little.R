#' Computes Little's test
#'
#' @description
#' Little’s chi-squared test takes into account possible patterns of missingness across all variables in the dataset.
#' Rejection of the null hypothesis of this test would provide sufficient evidence to indicate that the data are (globally) not MCAR.
#' Please note that compared to \link{smdi_hotelling}, this function tests for MCAR globally across all missing covariates.
#'
#'#' #' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @details
#' CAVE: Hotelling's and Little's show high susceptibility with large sample sizes and it is recommended to always interpret the results along with the other diagnostics.
#'
#' @seealso
#' \code{\link{mcar_test}}
#'
#' @references
#' Little RJA. A Test of Missing Completely at Random for Multivariate Data with Missing Values.
#' J Am Stat Assoc. 1988;83(404):1198-1202.
#'
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#'
#' @return returns a little object with statistics on little's test globally.
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

  # for little we need to one-hot-encode categorical variables
  # multi-categorical variables if exist
  if(any(sapply(data, function(.x) is.factor(.x) || is.character(.x)))){

    data_encoded <- data %>%
      fastDummies::dummy_columns(
        remove_most_frequent_dummy = TRUE,
        # as opposed to smdi_hotelling, for
        # smdi_little ignore_na is set to true
        # since for mcar_test, there need to be missing cells
        # (ignore_na = FALSE would create a missing indicator column)
        ignore_na = TRUE,
        remove_selected_columns = TRUE
        )

    }else{

    data_encoded <- data

    }

  # expect at least one column with missing values
  assertthat::assert_that(any(sapply(data, function(.x) is.na(.x))), msg = "<data> does not contain any column with missing values.")

  little <- naniar::mcar_test(data = data_encoded)

  class(little) <- "little"

  return(little)

  }
