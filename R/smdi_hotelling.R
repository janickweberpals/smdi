#' Computes hotelling's multivariate t-test
#'
#' @description
#' Hotelling's multivariate t-test, which examines variable
#' differences conditional on having an observed covariate value or not.
#' As the power of statistical hypothesis tests can be influenced by
#' sample size, the combined investigation along with smdi_asmd() is highly recommended.
#'
#' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @details
#' CAVE: Hotelling's and Little's show high susceptibility with large sample sizes and it is recommended to always interpret the results along with the other diagnostics.
#'
#' @seealso
#' \code{\link{hotelling.test}}
#'
#' @references
#' Hotelling H. The Generalization of Student’s Ratio. Ann Math Stat. 1931;2(3):360-378.
#'
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation and all remaining covariates will be used as predictors
#' @param n_cores integer, if >1, computations will be parallelized across amount of cores specified in n_cores (only UNIX systems)
#'
#' @return returns a hotelling object with statistics on hotellings test by covariate. That is, for each covar, the following outputs are provided:
#'
#' - stats: hotelling test statistics (for more information see \code{\link{hotelling.test}})
#'
#' - pval: p-value of hotelling test
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom fastDummies dummy_cols
#' @importFrom Hotelling hotelling.test
#' @importFrom parallel detectCores
#' @importFrom parallel mclapply
#' @importFrom tibble rownames_to_column
#' @importFrom tidyselect all_of
#'
#' @export
#'
#' @examples
#' library(smdi)
#'
#' smdi_hotelling(data = smdi_data)
#'

smdi_hotelling <- function(data = NULL,
                           covar = NULL,
                           n_cores = 1
                           ){


  # initialize variables
  .data <- V1 <- NULL

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}

  # n_cores on windows
  if(Sys.info()[["sysname"]]=="Windows"){
    message("Windows does not support parallelization based on forking. <n_cores> will be automatically set to 1.")
    n_cores = 1
  }

  # more cores than available
  if(n_cores > parallel::detectCores()){
    message("You specified more <n_cores> than you have available. The function will use all cores available to it.")
    }

  # check for missing covariate of interest
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )

  # apply smdi_na_indicator for datqset to create missing
  # indicator variables;
  # needs to be done for all variables with at least one NA
  data_encoded <- smdi::smdi_na_indicator(
    data = data,
    covar = smdi::smdi_check_covar(data = data),
    drop_NA_col = TRUE
    )

  # for hotelling we need to one-hot-encode categorical variables
  # applicable to multi-categorical variables if exist
  if(any(sapply(data_encoded, function(.x) is.factor(.x) || is.character(.x)))){

    data_encoded <- data_encoded %>%
      fastDummies::dummy_columns(
        remove_most_frequent_dummy = TRUE,
        # ignore_na = FALSE will make a dummy column for value_NA and
        # give a 1 in any row which has a NA value.
        ignore_na = FALSE,
        remove_selected_columns = TRUE
        )
    }

  # expect no column with missing values (all columns should be complete and NA's should be dummy variables after pre-processing)
  if(any(sapply(data_encoded, function(.x) is.na(.x)))){

    stop("After pre-processing the data, there are columns with NA. Please write an issue if you see this error.")

    }

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

  hotelling_out <- parallel::mclapply(covar_miss, FUN = hotelling_loop, mc.cores = n_cores)
  names(hotelling_out) <- covar_miss

  class(hotelling_out) <- "hotelling"

  return(hotelling_out)

  }



# generic print -----------------------------------------------------------

#' @export
print.hotelling <- function(x, ...){

  # initialize
  V1 <- NULL

  tbl <- do.call(rbind, lapply(x,'[[',2)) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "covariate") %>%
    dplyr::mutate(hotteling_p = ifelse(V1 < 0.001, "<.001", formatC(V1, format = "f", digits = 3))) %>%
    dplyr::select(-V1)

  return(print(tbl))

}


# generic summary ---------------------------------------------------------

#' @export
summary.hotelling <- function(object, ...){

  # initialize
  V1 <- NULL

  tbl <- do.call(rbind, lapply(object,'[[',2)) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "covariate") %>%
    dplyr::mutate(hotteling_p = ifelse(V1 < 0.001, "<.001", formatC(V1, format = "f", digits = 3))) %>%
    dplyr::select(-V1)

  return(tbl)

}


