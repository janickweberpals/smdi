#' Computes random forest-based AUC
#'
#' @description
#' The function trains and fits a random forest model to assess the ability to predict missingness for
#' the specified covariate(s). If missing indicator can be predicted as a function of observed covariates,
#' MAR may be a likely scenario and would imply that imputation may be feasible.
#'
#' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @details
#' The random forest utilizes the randomForest engine.
#'
#' @seealso
#' \code{\link{randomForest}}
#'
#' @references
#' Weberpals J,  Sondhi A, Jiang C, Yerram P, Taylor MD,  Samant M, Cherng ST. A Systematic Approach Towards Missing Lab Data in Electronic Health Records: A Case Study in Non-Small Cell Lung Cancer and Multiple Myeloma. 37th International Conference on Pharmacoepidemiology & Therapeutic Risk Management 2021. Pharmacoepidemiol Drug Saf 2021; 30:36-36.
#' https://onlinelibrary.wiley.com/doi/10.1002/pds.5305
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation and all remaining covariates will be used as predictors
#' @param ntree integer, number of trees (defaults to 1000 trees)
#' @param train_test_ratio numeric vector to indicate the test/train split ratio, e.g. c(.7, .3) which is the default
#' @param set_seed seed for reproducibility, defaults to 42
#' @param n_cores integer, if >1, computations will be parallelized across amount of cores specified in n_cores (only UNIX systems)
#'
#' @return rf object: list that contains the ROC AUC value and corresponding variable importance in training dataset (latter as ggplot object)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_identity
#' @importFrom ggplot2 theme_bw
#' @importFrom glue glue
#' @importFrom naniar mcar_test
#' @importFrom parallel detectCores
#' @importFrom parallel mclapply
#' @importFrom pROC roc
#' @importFrom randomForest randomForest
#' @importFrom stats predict
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @importFrom tibble rownames_to_column
#' @importFrom tidyselect all_of
#'
#' @export
#'
#' @examples
#' library(smdi)
#'
#' smdi_rf(data = smdi_data)
#'

smdi_rf <- function(data = NULL,
                    covar = NULL,
                    train_test_ratio = c(.7, .3),
                    set_seed = 42,
                    ntree = 1000,
                    n_cores = 1
                    ){

  # initialize
  .data <- MeanDecreaseAccuracy <- V1 <- covariate <- rf_auc <- NULL

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}

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

  # start applying smd computation over all partially observed covariates
  rf_loop <- function(i){

    # create strata variable
    target_var <- paste0(i, "_NA")

    # format missing indicator (target_var) variable correctly
    data_encoded <- data_encoded %>%
      dplyr::mutate(target_var = as.factor(.data[[target_var]])) %>%
      dplyr::select(-tidyselect::all_of(target_var))

    # use x% of dataset as training set and y% as test set
    set.seed(set_seed)
    sample <- sample(c(TRUE, FALSE), nrow(data_encoded), replace = TRUE, prob = train_test_ratio)
    train  <- data_encoded[sample, ]
    test   <- data_encoded[!sample, ]

    set.seed(set_seed)
    rf <- randomForest::randomForest(as.factor(target_var) ~ ., data = train, ntree = ntree, importance = TRUE)

    # evaluate on test set
    rf_test <- stats::predict(rf, newdata = test, type = "response", importance = T)

    rf.roc <- pROC::roc(response = test$target_var, predictor = as.numeric(as.character(rf_test)), quiet = TRUE)
    auc <- pROC::auc(rf.roc)[[1]]

    rf_tbl_out <- tibble::tibble(
      covariate = stringr::str_remove(target_var, "_NA"),
      rf_auc = auc
      ) %>%
      dplyr::mutate(rf_auc = formatC(rf_auc, format = "f", digits = 3))

    rf_plot_out <- rf$importance %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "covariate") %>%
      ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(as.factor(covariate), MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
      ggplot2::geom_point(size = 3, color = "darkblue") +
      ggplot2::labs(
        x = "Covariate",
        y = "Mean decrease in accuracy",
        title = glue::glue("Covariate importance for predicting {stringr::str_remove(target_var, '_NA')} (training set)")
      ) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw()

    rf_out <- list(
      rf_table = rf_tbl_out,
      rf_plot = rf_plot_out
      )

    return(rf_out)

  }

  # run the function for each covariate
  rf_out <- parallel::mclapply(covar_miss, FUN = rf_loop, mc.cores = n_cores)
  names(rf_out) <- covar_miss

  # assign class
  class(rf_out) <- "rf"

  return(rf_out)

}

# generic print -----------------------------------------------------------

#' @export
print.rf <- function(x, ...){

  tbl <- do.call(rbind, lapply(x,'[[',1))

  return(print(tbl))

}

# generic summary ---------------------------------------------------------

#' @export
summary.rf <- function(object, ...){

  tbl <- do.call(rbind, lapply(object,'[[',1))

  return(tbl)

}
