#' Wrapper of NARFCS enhanced forked mice version: https://raw.githack.com/moreno-betancur/NARFCS/master/Vignette.html
#'
#' @description
#' This function is a wrapper of the NARFCS extension of the package `mice` by M. Moreno-Betancur, F. Leacy, D. Tompsett and I. White.
#' The function pefroams a sensitivity analysis based on the Not At Random Fully Conditional Specification (NARFCS) procedure. The wrapper
#' intends to give users a more friendly version of the package, especially in situations with many covariates. The function itself follows
#' the Formula syntax for fully conditionally specified models. For a great introduction to the overall NARFCS procedure
#' and implementation, please visit <https://raw.githack.com/moreno-betancur/NARFCS/master/Vignette.html>
#'
#' @param i numeric sensitivity parameter delta; can also be a range of deltas if used in combination with lapply
#' @param data dataframe or tibble object with partially observed/missing variables. Ideally consists only of continuous or binary covariates
#' @param id character column name with (patient) identifier variable
#' @covar character covariate or covariate vector with variable/column name(s) to investigate; restricted to one at this time and must be continuous or binary
#' @missing_indicator_var = character indicating name of the  missing indicator variable(s) for covar for narfcs (optional)
#' @predictor_covar character covariate vector for narfcs imputation procedure, if not specified takes all available variables in dataset
#' @n_impute number of imputed datasets per delta parameter value
#'
#' @return returns a table with covariate name, amount missing observations and proportion missing.
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom dplyr rename_all
#' @importFrom tidyselect all_of
#' @importFrom fastDummies dummy_cols
#' @importFrom stringr str_replace_all
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'df %>%
#' smdi_narfcs()
#' }

smdi_narfcs <- function(i, # sensitivity parameter delta
                        data = NULL, # dataframe
                        id_var = NULL, # id
                        covar = NULL, # character covariate or covariate vector with variable/column name(s) to investigate; restricted to one at this time
                        missing_indicator_var = NULL, # missing indicator variable(s) for Ycont and Ybin for narfcs (optional)
                        predictor_covar = NULL, # covariate vector for narfcs and cox regression; this should also include your exposure of interest!
                        n_impute = 30, # amount of imputed datasets per iteration/delta parameter
                        verbose = FALSE){

  # initial checks

  # function currently takes either a single continuous or a single binary target variables
  if(length(covar) > 1){

    stop("You specified more than one target parameter. Function can for now only handle one at a time.")

  }

  # covar needs to be continuous or binary
  if(!class(data[[covar]]) %in% c("numeric", "integer") & !(nlevels(factor(data[[covar]])) == 2)){

    stop("covar must be either numeric or binary")

  }

  # step 1: prepare the data

  # determine covar class
  if(class(data[[covar]]) %in% c("numeric", "integer") & nlevels(factor(data[[covar]])) > 2){

    target_parameter_cont <- covar
    target_parameter_binary <- NULL

    message(glue::glue("Note: target parameter/covariate '{covar}' will be handled as a continuous covariate"))

  }else if(nlevels(factor(data[[covar]])) == 2){

    target_parameter_binary <- covar
    target_parameter_cont <- NULL

    message(glue::glue("Note: target parameter/covariate '{covar}' will be handled as a binary covariate"))

  }else{

    stop("Couldn't determine covar class. Make sure covar is continuous or binary")

  }


  # get predictor vector ---------------------------------------------------

  # if no pre-defined predictor_covar are specified we use all columns
  # in the dataset but the id variable and the target parameter
  if(is.null(predictor_covar)){

    predictor_covar <- data %>%
      dplyr::select(-tidyselect::all_of(c(id_var, covar))) %>%
      names()

  }


  # for narfcs procedure function, categorical (predictor) variables need to be one hot encoded
  # if there are any categorical/factor variables => create binary dummies
  categorical_covars <- data %>%
    dplyr::select(-tidyselect::all_of(c(id_var, covar))) %>%
    dplyr::select(tidyselect::where(function(.x) is.character(.x) | is.factor(.x))) %>%
    names()

  if(length(categorical_covars) > 0){

    data_predictors <- data %>%
      dplyr::select(tidyselect::all_of(c(id_var, predictor_covar))) %>%
      fastDummies::dummy_cols(
        select_columns = categorical_covars,
        remove_most_frequent_dummy = TRUE,
        remove_selected_columns = TRUE,
        ignore_na = FALSE # categorical variables with NA will automatically have a missing indicator
        ) %>%
      # fix column names
      dplyr::rename_all(function(x) make.names(stringr::str_replace_all(x, " |-", "_")))

    }else{

      data_predictors <- data %>%
        dplyr::select(tidyselect::all_of(c(id_var, predictor_covar)))

  }

  # store new predictor names vector for subsequent tasks
  predictor_var_vec <- names(data_predictors %>% dplyr::select(-tidyselect::all_of(id_var)))

  # make a new clean dataframe
  data <- data[, c(id_var, covar)] %>%
    dplyr::left_join(data_predictors, by = id_var)

  # for narfcs we merge "missing_indicator_var" (optional) and "predictor_var_vec" into "covariates"
  covariates <- c(missing_indicator_var, predictor_var_vec)

  # merge and select columns in specific order (order is important)
  data_in <- data[, c(id_var, covar)] %>%
    dplyr::left_join(data_predictors, by = id_var) %>%
    dplyr::select(
      tidyselect::all_of(target_parameter_cont),
      tidyselect::all_of(target_parameter_binary),
      tidyselect::all_of(covariates)
      )

  # step 2: prepare Formula syntax for narfcs

  # delta-adjustment of i for imputing target parameter
  # i.e. imputation model for target parameter is target parameter = intcpt + \beta_X * X + \beta_Z * Z + ...  + 10
  # set-up list with sensitivity parameter values
  pSens <- rep(list(list("")), ncol(data_in))
  names(pSens) <- names(data_in)

  # specify our sensitivity parameter for Y
  for(j in c(target_parameter_cont, target_parameter_binary)){

    # specify our sensitivity parameter for Y
    pSens[[j]] <- list(i) # this is Y

  }

  # automate method parameter given input parameters
  method_target_cont <- paste0(rep("normSens", length(target_parameter_cont))) # cont. Y parameter
  method_target_binary <- paste0(rep("logregSens", length(target_parameter_binary))) # binary Y parameter
  covar_vector <- paste0(rep("", length(covariates))) # covariates not to be narfcs imputed
  method <- c(method_target_cont, method_target_binary, covar_vector)

  # automate form parameter given input parameters
  if(!is.null(target_parameter_cont)){
    # impute cont Y as linear regression on intercept and non-imputable covariates (covar_vector)
    form_target_cont <- paste0("~ 1 +", paste0(c(target_parameter_binary, covariates), collapse = "+"))
  }else{
    form_target_cont <- NULL
  }

  if(!is.null(target_parameter_binary)){
    # impute cont Y as linear regression intercept and non-imputable covariates (covar_vector)
    form_target_binary <- paste0("~ 1 +", paste0(c(target_parameter_cont, covariates, "status", "time_indicator"), collapse = "+"))

    data_in[[target_parameter_binary]] <- as.factor(data_in[[target_parameter_binary]])

  }else{
    form_target_binary <- NULL
  }

  form <- c(form_target_cont, form_target_binary, covar_vector)

  # automate formSens parameter given input parameters
  formSens_target <-  paste0(rep("~1", length(target_parameter_cont) + length(target_parameter_binary))) # Apply a sensitivity delta-adjustment to the intercept in the Y imputation model, do not impute covariates
  formSens <- c(formSens_target, covar_vector)

  # NARFCS analysis using formula syntax
  impNARFCS <- mice::mice(data_in,

                          m = n_impute,

                          # Impute Y using normSens, do not impute covariates
                          method = method,

                          # Impute Y as linear regression on intercept, and covariates, do not impute covariates
                          form = form,

                          # number of chained equations that mice will run; we set it to 1 as for now we just look at one missing variable at a time
                          maxit = 1,

                          # Apply a sensitivity delta-adjustment to the intercept in the Y imputation model, do not impute covariates
                          formSens = formSens,

                          # Get the delta-adjustment parameter from pSens
                          parmSens = pSens,
                          seed = 42,
                          print = F
                          )

  # long format of imputed dataset
  impNARFCS_long <- mice::complete(impNARFCS, action = "long", include = TRUE)
  impNARFCS_long_mids <- mice::as.mids(impNARFCS_long)

  # specify cox formula
  rhs <- paste0(c(target_parameter_cont, target_parameter_binary, predictor_var_dummy), collapse = "+")
  form <- as.formula(paste("survival::Surv(time_indicator, status)", "~", rhs))

  # pool results from cox regression
  cox_fit <- with(data = impNARFCS_long_mids, expr = survival::coxph(formula(paste(format(form), collapse = ""))))
  cox_result <- summary(pool(cox_fit), conf.int = TRUE, exponentiate = TRUE) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var="variable") %>%
    dplyr::mutate(delta = i) %>%
    dplyr::mutate(hr = exp(est)) %>%
    dplyr::mutate(lcl = exp(`lo 95`)) %>%
    dplyr::mutate(ucl = exp(`hi 95`)) %>%
    dplyr::select(variable, delta, hr, lcl, ucl, est, se)

  return(cox_result)

  # return(
  #   list(
  #     mice.object = impNARFCS,
  #     imputed.data.long = impNARFCS_long,
  #     imputed.mids.object = impNARFCS_long_mids, # that's the object we use for pooled analysis
  #     outcome.call = form,
  #     cox.result = cox_result
  #     )
  #   )

}


# plot results ------------------------------------------------------------
# NARFCS plot function
narfcs_plot <- function(narfcs_results_df = NULL){

  plot <- narfcs_results_df %>%
    ggplot2::ggplot(aes(x = delta, y = hr)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(aes(ymin = lcl, ymax = ucl), linetype = 2, alpha = 0.25) +
    ggplot2::labs(title = "Not at random fully conditional specification (NARFCS) sensitivity analysis",
                  x = expression(delta),
                  y = "Hazard ratio (HR)") +
    ggplot2::theme_bw()

  return(plot)

}


