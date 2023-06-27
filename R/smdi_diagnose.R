#' Computes three group missing data summary diagnostics
#'
#' @description
#' This function bundles and calls all three group diagnostics and returns the most important summary metrics.
#' For more information and details, please refer to the individual functions.
#'
#' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @details
#' Wrapper for individual diagnostics function.
#'
#' @seealso
#' \code{\link{smdi_asmd}}
#' \code{\link{smdi_hotelling}}
#' \code{\link{smdi_little}}
#' \code{\link{smdi_rf}}
#' \code{\link{smdi_outcome}}
#'
#' @references
#' TBD
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation and all remaining covariates will be used as predictors
#' @param median logical if the median (= TRUE; recommended default) or mean of all absolute standardized mean differences (asmd) should be computed (smdi_asmd)
#' @param includeNA logical, should missingness of other partially observed covariates be explicitly modeled for computation of absoulate standardized mean differences (default is FALSE)
#' @param ntree integer, number of trees for random forest missingness prediction model (defaults to 1000 trees)
#' @param train_test_ratio numeric vector to indicate the test/train split ratio for random forest missingness prediction model, e.g. c(.7, .3) is the default
#' @param set_seed seed for reproducibility of random forest missingness prediction model, defaults to 42
#' @param n_cores integer, if >1, computations will be parallelized across amount of cores specified in n_cores (only UNIX systems)
#' @param model character describing which outcome model to fit to assess the association between covar missingness indicator and outcome. Currently supported are models of type logistic, linear and cox (see smdi_outcome)
#' @param form_lhs string specifying the left-hand side of the outcome formula (see smdi_outcome)
#' @param exponentiated logical, should results of outcome regression to assess association between missingness and outcome be exponentiated (default is FALSE)
#'
#' @return smdi object including a summary table of all three smdi group diagnostics:
#'
#' **Group 1 diagnostic:**
#'
#' - asmd_{mean/median}: average/median absolute standardized mean difference (and min, max) of patient characteristics between those without (1) and with (0) observed covariate
#'
#' - hotteling_p: p-value of hotelling test. Rejecting the H0 means that Hotelling's test detects a significant difference in the distribution between patients without (1) and with (0) the observed covariate
#'
#' **Group 2 diagnostic:**
#'
#' - rf_auc: The area under the receiver operating curve (AUC) as a measure of the ability to predict the missingness of the partially observed covariate
#'
#'
#' **Group 3 diagnostic:**
#'
#' - estimate_crude: univariate association between missingness indicator of <covar> and outcome
#'
#' - estimate_adjusted: association between missingness indicator of <covar> and outcome conditional on other fully observed covariates and missing indicator variables of other partially observed covariates
#'
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(smdi)
#'
#' smdi_diagnose(
#'  data = smdi_data,
#'  model = "cox",
#'  form_lhs = "Surv(eventtime, status)"
#'  )
#'}
#'
smdi_diagnose <- function(data = NULL,
                          covar = NULL,

                          median = TRUE,
                          includeNA = FALSE,

                          train_test_ratio = c(.7, .3),
                          set_seed = 42,
                          ntree = 1000,
                          n_cores = 1,

                          model = c("logistic", "linear", "cox"),
                          form_lhs = NULL,
                          exponentiated = FALSE
                          ){

  # initialize
  #covariate <- `1 vs 2` <- term <- estimate <- conf.low <- conf.high <- NULL

  # check for missing covariates
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )

  # asmd --------------------------------------------------------------------
  asmd_out <- smdi::smdi_asmd(
    data = data,
    covar = covar_miss,
    median = median,
    includeNA = includeNA
    )

  tbl_asmd <- summary(asmd_out)
  tbl_asmd[[paste0(colnames(tbl_asmd)[[2]], "_min_max")]] <- paste0(tbl_asmd[[2]], " (", tbl_asmd[[3]], ", ", tbl_asmd[[4]], ")")
  tbl_asmd <- tbl_asmd[, c(1,5)]


  # hotelling ---------------------------------------------------------------
  hotelling_out <- smdi::smdi_hotelling(
    data = data,
    covar = covar_miss
    )

  tbl_hotelling <- summary(hotelling_out)


  # little ------------------------------------------------------------------
  little_out <- smdi::smdi_little(
    data = data
    )

  tbl_little <- glue::glue("p_little: {ifelse(little_out$p.value < 0.001, '<.001', formatC(little_out$p.value, format = 'f', digits = 3))}")

  # random forest -----------------------------------------------------------
  rf_out <- smdi::smdi_rf(
    data = data,
    covar = covar_miss,
    train_test_ratio = train_test_ratio,
    set_seed = set_seed,
    ntree = ntree
    )

  tbl_rf <- summary(rf_out)


  # outcome regression ------------------------------------------------------
  tbl_outcome <- smdi::smdi_outcome(
    data = data,
    model = model,
    form_lhs = form_lhs,
    exponentiated = exponentiated
    )

  # combine -----------------------------------------------------------------
  smdi_tbl_out <- tbl_asmd %>%
    dplyr::left_join(tbl_hotelling, by = "covariate") %>%
    dplyr::left_join(tbl_rf, by = "covariate") %>%
    dplyr::left_join(tbl_outcome, by = "covariate")

  smdi_out <- list(
    smdi_tbl = smdi_tbl_out,
    p_little = tbl_little
    )

  class(smdi_out) <- "smdi"

  return(smdi_out)

}

# generic print -----------------------------------------------------------

#' @export
print.smdi <- function(x, ...){

  cat("smdi summary table:")
  cat("\n")
  print(x$smdi_tbl)
  cat("\n")
  cat(x$p_little)

}

# generic summary ---------------------------------------------------------

#' @export
summary.smdi <- function(object, ...){

  tbl <- (object$smdi_tbl)

  return(tbl)

}
