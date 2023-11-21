#' Computes association between missingness and outcome
#'
#' @description
#' This function fits outcome models with a covariate missingness indicator(s) of the covariates specified with *covar*.
#' The estimates are computed by univariate and adjusted models on all other prognostic covariates
#' in the dataset. Based on the underlying missingness mechanism, the estimate for the covariate missingness indicator
#' may indicate a meaningful difference in the outcome between patients with vs w/o
#' the observed confounder conditional on other covariates that could explain that difference.
#'
#' @param model `r lifecycle::badge("deprecated")` `model = "logistic"` is no
#'   longer supported in favor of model = "glm" and the new glm_family parameter
#'   which can be used to specify any glm model.
#'
#' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @details
#' The function automatically fits a univariate and adjusted outcome model. The currently supported models are glm (glm), linear (lm) and cox (survival).
#' For adjusted models, the function uses all available covariates found in the dataset specified with the data parameter. If covariates should not
#' be include in the outcome model, these covariates should be dropped beforehand (as with all other functions in the smdi package).
#'
#' The left-hand side of the formula (form_lhs) needs to specify the outcome in one of the following ways:
#'
#' - glm (binary): character of column name with binary outcome, e.g. "MACE"
#'
#' - lm (continuous): character of column name with binary outcome, e.g. "WEIGHT_LOSS"
#'
#' - cox (time-to-event): LHS specifying time-to-event outcome, e.g. "Surv(TIME, STATUS)"
#'
#' @references
#' ...
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation and all remaining covariates will be used as predictors
#' @param model character describing which outcome model to fit to assess the association between covar missingness indicator and outcome. Currently supported are models of type glm, linear and cox
#' @param glm_family glm family object to specify a certain family of generalized linear models (e.g., binomial, gaussian, Gamma, poisson, etc.). For all options see ?stats::family
#' @param form_lhs string specifying the left-hand side of the outcome formula (see details)
#' @param exponentiated logical, should results be exponentiated (default is FALSE)
#' @param n_cores integer, if >1, computations will be parallelized across amount of cores specified in n_cores (only UNIX systems)
#'
#' @seealso
#' \code{\link{stats}}
#' \code{\link{survival}}
#'
#' @return returns a tibble with univariate and adjusted estimates for each partially observed covar:
#'
#' - estimate_univariate: univariate association between missingness indicator of covar and outcome
#'
#' - estimate_adjusted: association between missingness indicator of covar and outcome conditional on other fully observed covariates and missing indicator variables of other partially observed covariates
#'
#' @importFrom broom tidy
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom parallel detectCores
#' @importFrom parallel mclapply
#' @importFrom stats as.formula
#' @importFrom stats binomial gaussian glm lm Gamma inverse.gaussian poisson quasi quasibinomial quasipoisson
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
smdi_outcome <- function(data = NULL,
                         covar = NULL,
                         model = c("glm", "linear", "cox"),
                         glm_family = NULL,
                         form_lhs = NULL,
                         exponentiated = FALSE,
                         n_cores = 1
                         ){

  # initialize
  term <- covariate <- estimate <- conf.low <- conf.high <- estimate_univariate <- estimate_adjusted <- V1 <- NULL

  # superseded 'model' parameter
  if (model == "logistic") {
    lifecycle::deprecate_stop(
      when = "0.3.0",
      what = "smdi_outcome(model = 'logistic')",
      details = "'logistic' is no longer supported and was superseded with 'glm' along with the <glm_family> parameter to specify any glm model supported by stats::glm family."
    )
  }

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}
  if(is.null(form_lhs)){stop("No <form_lhs> provided.")}
  if(is.null(model) || !model %in% c("glm", "linear", "cox")){stop("<model> either not specified or not of type glm, linear or cox")}
  if(model %in% c("linear", "cox") & !is.null(glm_family)){stop("<glm_family> is specified although <model> is either 'linear' or 'cox'")}
  if(any(is.na(data[[form_lhs]]))){stop("Outcome variable <form_lhs> contains missing observations for which smdi_outcome is not suited.")}

  # if model is glm but no family is specified
  # logistic regression is default if nothing is specified
  if(model == "glm" & is.null(glm_family)){

    glm_family <- binomial(link = "logit")
    message("<glm_family> not specified. Using 'binomial(link = 'logit')' as default.")

    }

  # n_cores on windows
  if(Sys.info()[["sysname"]]=="Windows"){
    warning("Windows does not support parallelization based on forking. <n_cores> will be set to 1.")
    n_cores = 1
  }

  # more cores than available
  if(n_cores > parallel::detectCores()){
    warning("You specified more <n_cores> than you have available. The function will use all cores available to it.")
  }

  # check for missing covariate of interest
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )

  # apply smdi_na_indicator for datset to create missing
  # indicator variables;
  # needs to be done for all variables with at least one NA
  data_encoded <- smdi::smdi_na_indicator(
    data = data,
    covar = smdi::smdi_check_covar(data = data),
    drop_NA_col = TRUE
    )

  # univariate outcome results ---------------------------------------------------
  univariate_loop <- function(i){

    # create strata variable
    target_var <- paste0(i, "_NA")

    form_univariate <- stats::as.formula(paste(form_lhs, "~", target_var))

    # fit model
    if(model == "glm"){

      univariate_fit <- stats::glm(form_univariate, family = glm_family, data = data_encoded)

    }else if(model == "linear"){

      univariate_fit <- stats::lm(form_univariate, data = data_encoded)

    }else if(model == "cox"){

      form_univariate <- stats::as.formula(paste("survival::", form_lhs, "~", target_var))
      univariate_fit <- survival::coxph(form_univariate, data = data_encoded)

    }

    univariate_result <- univariate_fit %>%
      broom::tidy(exponentiate = exponentiated, conf.int = TRUE) %>%
      # filter out intercept term
      dplyr::filter(term != "(Intercept)") %>%
      # go back to initial covariate name to be consistent
      dplyr::mutate(covariate = stringr::str_remove(term, "_NA")) %>%
      # summarize estimate
      dplyr::mutate(dplyr::across(c(estimate, conf.low, conf.high), ~ formatC(.x, format = "f", digits = 2))) %>%
      dplyr::mutate(estimate_univariate = glue::glue("{estimate} (95% CI {conf.low}, {conf.high})")) %>%
      dplyr::select(covariate, estimate_univariate)

    return(univariate_result)

  }

  # collect results over all covar_miss
  univariate_out <- do.call(rbind, parallel::mclapply(covar_miss, FUN = univariate_loop, mc.cores = n_cores))


  # adjusted outcome results ------------------------------------------------
  form_adjusted <- as.formula(paste(form_lhs, "~ ."))

  if(model == "glm"){

    adjusted_fit <- stats::glm(form_adjusted, family = "binomial", data = data_encoded)

  }else if(model == "linear"){

    adjusted_fit <- stats::lm(form_adjusted, data = data_encoded)

  }else if(model == "cox"){

    form_adjusted <- stats::as.formula(paste("survival::", form_lhs, "~ ."))
    adjusted_fit <- survival::coxph(form_adjusted, data = data_encoded)

  }

  adjusted_out <- adjusted_fit %>%
    broom::tidy(exponentiate = exponentiated, conf.int = TRUE) %>%
    # go back to initial covariate name to be consistent
    dplyr::mutate(covariate = stringr::str_remove(term, "_NA")) %>%
    # filter out intercept term and other covariate estimates
    dplyr::filter(covariate %in% c(covar_miss)) %>%
    dplyr::mutate(dplyr::across(c(estimate, conf.low, conf.high), ~ formatC(.x, format = "f", digits = 2))) %>%
    dplyr::mutate(estimate_adjusted = glue::glue("{estimate} (95% CI {conf.low}, {conf.high})")) %>%
    dplyr::select(covariate, estimate_adjusted)


  # combine univariate and adjusted results
  results_out <- univariate_out %>%
    dplyr::left_join(adjusted_out, by = "covariate")

  return(results_out)

}
