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
#' The random forest utilizes the \link{randomForest} engine.
#'
#' CAVE: If the missingness indicator variables of other partially observed covariates (indicated by suffix _NA) have an extremely high variable importance (combined with an unusually high AUC),
#' this might be an indicator of a monotone missing data pattern. In this case it is advisable to exclude other partially observed covariates and run missingness diagnostics separately.
#'
#' @seealso
#' \code{\link{randomForest}}
#'
#' @references
#' Sondhi A, Weberpals J, Yerram P, Jiang C, Taylor M, Samant M, Cherng S. A systematic approach towards missing lab data in electronic health records: A case study in non-small cell lung cancer and multiple myeloma. CPT Pharmacometrics Syst Pharmacol. 2023 Jun 15. <doi: 10.1002/psp4.12998.> Epub ahead of print. PMID: 37322818.
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation and all remaining covariates will be used as predictors
#' @param ntree integer, number of trees (defaults to 1000 trees)
#' @param train_test_ratio numeric vector to indicate the test/train split ratio, e.g. c(.7, .3) which is the default
#' @param tune logical,if TRUE, a 5-fold cross validation is performed combined with a random search for the optimal number of optimal number of variables randomly sampled as candidates at each split (mtry). FALSE is the default due to potentially extensive computation times.
#' @param set_seed seed for reproducibility, defaults to 42
#' @param n_cores integer, if >1, computations will be parallelized across amount of cores specified in n_cores (only UNIX systems)
#'
#' @return returns an rf object which comes as a list that contains the ROC AUC value and corresponding variable importance in training dataset (latter as ggplot object). That is, for each covar, the following outputs are provided:
#'
#' - rf_table: The area under the receiver operating curve (AUC) as a measure of the ability to predict the missingness of the partially observed covariate
#'
#' - rf_plot: ggplot object illustrating the variable importance for the prediction made expressed by the mean decrease in accuracy per predictor.
#' That is how much would the accuracy of the prediction (# of correct predictions/Total # of predictions made) decrease, had we left out this specific predictor.
#'
#' - OOB: estimated OOB error for each investigated partially observed confounder (indicates the performance of the random forest model for data points that are not used in training a tree.)
#'
#' @importFrom caret trainControl train
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
#' @export
#'
#' @examples
#' library(smdi)
#'
#' smdi_rf(data = smdi_data, covar = "ecog_cat")
#'

smdi_rf <- function(data = NULL,
                    covar = NULL,
                    train_test_ratio = c(.7, .3),
                    set_seed = 42,
                    ntree = 1000,
                    n_cores = 1,
                    tune = FALSE
                    ){

  # initialize
  .data <- MeanDecreaseAccuracy <- V1 <- covariate <- rf_auc <- imp_tmp <- . <- NULL

  # pre-checks
  if(is.null(data)){stop("No dataframe provided.")}

  # n_cores on windows
  if(Sys.info()[["sysname"]]=="Windows"){
    message("Windows does not support parallelization based on forking. <n_cores> will be set to 1.")
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

    # tune mtry if desired
    if(tune){

      # cv 5 folds repeat 1 time & random search for mtry
      control <- caret::trainControl(
        method = 'repeatedcv',
        number = 5,
        repeats = 1,
        search = "random"
        )

      set.seed(set_seed)
      rf_train <- caret::train(
        as.factor(target_var) ~ . ,
        data = train,
        ntree = ntree,
        method = "rf",
        metric = 'Accuracy',
        trControl = control
        )

      set.seed(set_seed)
      rf <- randomForest::randomForest(
        as.factor(target_var) ~ .,
        data = train,
        ntree = ntree,
        mtry = rf_train$bestTune$mtry
        )

      # compute OOB for cross-validated rf model
      conf <- rf$confusion[,-ncol(rf$confusion)]
      oob <- glue::glue("Estimated OOB error for {i}: {formatC((1 - (sum(diag(conf))/sum(conf)))*100, 4)}%")

      }else{

        set.seed(set_seed)
        rf <- randomForest::randomForest(as.factor(target_var) ~ ., data = train, ntree = ntree, importance = TRUE)

        # compute OOB
        conf <- rf$confusion[,-ncol(rf$confusion)]
        oob <- glue::glue("Estimated OOB error for {i}: {formatC((1 - (sum(diag(conf))/sum(conf)))*100, 4)}%")

        }

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
      {{. ->> imp_tmp}} %>%
      ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(as.factor(covariate), MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
      ggplot2::geom_point(size = 3, color = "darkblue") +
      ggplot2::labs(
        x = "Covariate",
        y = "Mean decrease in accuracy",
        title = glue::glue("Covariate importance for predicting {stringr::str_remove(target_var, '_NA')} (training set)")
      ) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw()

      # we add a message for very high AUC values
      # to make analyst aware to check for monotonicity
      # we choose AUC of .9 as cut-off
      if(auc > 0.9){
        cat("\n")
        message("Important note: \n")
        message(glue::glue("AUC for predicting covariate {rf_tbl_out$covariate} is very high (>0.9)."))

        # determine most important predictor
        imp_var_message <- imp_tmp %>%
          dplyr::filter(MeanDecreaseAccuracy == max(MeanDecreaseAccuracy, na.rm=T)) %>%
          dplyr::pull(covariate)

        message(glue::glue("Predictor with highest importance: {imp_var_message}."))
        message("Check for potentially underlying monotone missing data pattern. \n")
        cat("\n")
        }

    rf_out <- list(
      rf_table = rf_tbl_out,
      rf_plot = rf_plot_out,
      OOB = oob
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
