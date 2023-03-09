#' Computes three group missing data summary diagnostics
#'
#' @description
#' This function
#'
#'#' #' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @details
#' Wrapper for individual diagnostics function.
#'
#' @seealso
#' [smdi_asmd()]
#' [smdi_hotteling()]
#' [smdi_little()]
#' [smdi_rf()]
#' [smdi_outcome()]
#'
#' @references
#' ...
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation and all remaining covariates will be used as predictors
#' @param model character describing which outcome model to fit to assess the association between covar missingness indicator and outcome. Currently supported are models of type logistic, linear and cox (see smdi_outcome)
#' @param form_lhs string specifying the left-hand side of the outcome formula (see smdi_outcome)
#' @param ... further arguments to pass on to smdi_asmd, smdi_hotelling, smdi_little, smdi_rf or smdi_outcome
#'
#' @return smdi object with summary table of all three smdi group diagnostics.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#'
#' @export
#'
#' @examples
#' library(smdi)
#'
#' smdi_outcome(
#'  data = smdi_data,
#'  model = "cox",
#'  form_lhs = "Surv(eventtime, status)"
#'  )
#'
#'
smdi_diagnose <- function(data = NULL,
                          covar = NULL,
                          model = c("logistic", "linear", "cox"),
                          form_lhs = NULL,
                          ...){

  # initialize
  #covariate <- `1 vs 2` <- term <- estimate <- conf.low <- conf.high <- NULL

  # additional arguments
  add_args <- list(...)

  # check for missing covariates
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )

  # asmd --------------------------------------------------------------------
  asmd_out <- smdi::smdi_asmd(
    data = data,
    covar = covar_miss,
    ...
    )

  tbl_asmd <- summary(asmd_out)


  # hotelling ---------------------------------------------------------------
  hotelling_out <- smdi::smdi_hotelling(
    data = data,
    covar = covar_miss,
    ...
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
    ...
    )

  tbl_rf <- summary(rf_out)


  # outcome regression ------------------------------------------------------
  tbl_outcome <- smdi::smdi_outcome(
    data = data,
    model = "cox",
    form_lhs = "Surv(eventtime, status)",
    ...
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
