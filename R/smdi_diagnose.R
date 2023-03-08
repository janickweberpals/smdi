#' Computes three group missing data summary diagnostics
#'
#' @description
#' This function
#'
#'#' #' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @details
#' The function automatically fits a crude and adjusted outcome model. The currently supported models are logistic (glm), linear (lm) and cox (survival).
#' For adjusted models, the function uses all available covariates found in the dataset specified with the <data> parameter. If covariates should not
#' be include in the outcome model, these covariates should be dropped beforehand (as with all other functions in the smdi package).
#'
#' The left-hand side of the formula (<form_lhs>) needs to specify the outcome in one of the following ways:
#' - glm (binary): character of column name with binary outcome, e.g. "MACE"
#' - lm (continuous): character of column name with binary outcome, e.g. "WEIGHT_LOSS"
#' - cox (time-to-event): LHS specifying time-to-event outcome, e.g. "Surv(TIME, STATUS)"
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
#' @param exponentiated logical, should results of the fitted outcome model be exponentiated (see smdi_outcome)
#' @param ... further arguments to pass on to smdi_asmd, smdi_hotelling, smdi_little, smdi_rf or smdi_outcome
#'
#' @return smdi object with details of all wrapped functions. print() returns convenient summary table
#'
#' @importFrom broom tidy
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom stats as.formula
#' @importFrom stats glm
#' @importFrom stats lm
#' @importFrom stringr str_remove
#' @importFrom survival coxph
#' @importFrom survival Surv
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
#' )
#'
#'
smdi_diagnose <- function(data = NULL,
                          covar = NULL,
                          model = c("logistic", "linear", "cox"),
                          form_lhs = NULL,
                          ...){

  # initialize
  covariate <- `1 vs 2` <- term <- estimate <- conf.low <- conf.high <- NULL

  # additional arguments
  add_args <- list(...)

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}
  if(is.null(form_lhs)){stop("No <form_lhs> provided.")}
  if(is.null(model) || !model %in% c("logistic", "linear", "cox")){stop("<model> either not specified or not of type logistic, linear or cox")}


  # check for missing covariates
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )

  # asmd --------------------------------------------------------------------
  asmd_out <- smdi::smdi_asmd(
    data = data,
    covar = covar_miss
    )


  # hotelling ---------------------------------------------------------------
  hotelling_out <- smdi::smdi_hotelling(
    data = data,
    covar = covar_miss
    )


  # little ------------------------------------------------------------------
  little_out <- smdi::smdi_little(
    data = data
    )


  # random forest -----------------------------------------------------------
  rf_out <- smdi::smdi_rf(
    data = data,
    covar = covar_miss
    )


  # summary table
  smdi_tbl <- print(asmd_out)

  # combine -----------------------------------------------------------------
  smdi_out <- c(
    asmd = asmd_out,
    hotelling = hotelling_out,
    little = list(little_out),
    rf = rf_out
    )


  return(smdi_out)

}

#' @export
print.asmd <- function(x, ...){


  smdi_table <- x
  print(smdi_table)

  return(smdi_table)

}
