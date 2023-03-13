#' Computes mean/median absolute standardized mean differences between observed and missing observations
#'
#' @description
#' This function takes a dataframe with covariates which are partially observed/missing and returns the
#' median/average absolute standardized mean difference (asmd) and more details for every specified covariate in covar
#' (if NULL all covariates with at least one NA are considered).
#'
#' Important: don't include variables like ID variables, ZIP codes, dates, etc.
#'
#' @details
#' The asmd may be one indicator as to how much patient characteristics differ between patients with and without an observed
#' value for a partially observed covariate. If the median/average asmd is above a certain threshold this may indicate
#' imbalance in patient covariate distributions which may be indicative of the partially observed covariate following a
#' missing at random (MAR) mechanims, i.e. the missingness is explainable by other observed covariates. Similarly,
#' no imbalance between observed covariates may be indicative that missingness cannot be explained with observed
#' covariates and the underlying missingness mechanism may be completely at random (MCAR) or not at random (e.g.
#' missingness is only associated with unobserved factors or through the partially observed covariate itself).
#'
#' A clear cut-off is hard to determine and analogues to propensity scores,
#' some researchers have proposed that a standardized difference of 0.1 (10 per cent)
#' denotes meaningful imbalance in the baseline covariate.
#'
#' The asmd is computed for every covariate one-by-one and not jointly. If there is multivariate
#' missingness, i.e. more than just one missing covariate exists, you can decide what should
#' happen with the other partially observed 'predictor' covariates using the <includeNA> parameter.
#' That is, if <includeNA> is set to FALSE (default), only the asmd between observed cases will be computed,
#' and if <includeNA> is set to TRUE, missingness is modeled as an explicit category (categorical covariates only).
#'
#' If any other behavior is desired, data transformations for example with the \code{\link{smdi_na_indicator}} function, may make sense
#' before calling the function.
#'
#' The dataframe should generally consist of the exposure variable, the outcome variable(s), the partially observed covariates
#' and all other fully observed covariates which are deemed important for the final modeling
#' and (optionally) which could be considered as auxiliary variables. If no partially observed covariates are provided,
#' the function automatically looks for all variables/columns with NA (powered by the \code{\link{smdi_summarize}} function)
#'
#' @seealso
#' \code{\link{CreateTableOne}}
#'
#' @references
#' Austin PC. Balance diagnostics for comparing the distribution of baseline covariates between treatment groups in propensity-score matched samples. Stat Med. 2009 Nov 10;28(25):3083-107.
#'
#' Normand SLT, Landrum MB, Guadagnoli E, Ayanian JZ, Ryan TJ, Cleary PD, McNeil BJ. Validating recommendations for coronary angiography following an acute myocardial infarction in the elderly: a matched analysis using propensity scores. Journal of Clinical Epidemiology. 2001;54:387–398.
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation and all remaining covariates will be used as predictors
#' @param median logical if the median (= TRUE; recommended default) or mean of all absolute standardized mean differences (asmd) should be computed
#' @param includeNA logical, should missingness of other partially observed covariates be explicitly modeled (default is FALSE)
#'
#' @return returns an asmd object with mean/median absolute standardized mean differences
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr summarize_all
#' @importFrom fastDummies dummy_cols
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_identity
#' @importFrom ggplot2 theme_bw
#' @importFrom glue glue
#' @importFrom stats median
#' @importFrom tableone CreateTableOne
#' @importFrom tableone ExtractSmd
#' @importFrom tibble tibble
#' @importFrom tibble rownames_to_column
#' @importFrom tidyselect all_of
#' @importFrom tidyselect everything
#' @importFrom tidyselect where
#'
#' @export
#'
#' @examples
#' library(smdi)
#' library(dplyr)
#'
#' # S3 print method
#' asmd <- smdi_asmd(data = smdi_data)
#' asmd
#'
#' # let's look at the first variable
#' # we can check the complete covariate distribution
#' asmd$pdl1_num$asmd_table1
#'

smdi_asmd <- function(data = NULL,
                      covar = NULL,
                      median = TRUE,
                      includeNA = FALSE
                      ){


  covariate <- `1 vs 2` <- NULL

  # pick missing indicator columns/partially observed covariates
  # check for missing covariates
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )

  # start applying smd computation over all partially observed covariates
  smd_loop <- function(i){

    # apply smdi_na_indicator for i to create missing
    # indicator variable
    strata_df <- smdi::smdi_na_indicator(
      data = data,
      covar = i,
      drop_NA_col = TRUE
      )

    # create strata variable
    strata_var <- paste0(i, "_NA")

    # create tableone
    tbl1 <- tableone::CreateTableOne(
        data = strata_df,
        # all covariates except strata covariate
        vars = names(strata_df)[ !names(strata_df) == strata_var],
        # strata covariate
        strata = strata_var,
        # if multiple variables are missing, NA is an own category
        includeNA = includeNA,
        smd = TRUE
        )

    # extract smd and compute median/mean
    smd <- tableone::ExtractSmd(tbl1) %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "covariate") %>%
      dplyr::filter(covariate != i) %>%
      dplyr::rename(smd = `1 vs 2`)

    # asmd plot
    asmd_plot <- smd %>%
      ggplot2::ggplot(
        ggplot2::aes(
          y = forcats::fct_reorder(covariate, smd),
          x = smd,
          color = ifelse(smd < 0.1, "darkgreen", "firebrick")
          )
        ) +
      ggplot2::geom_point(size = 3) +
      ggplot2::labs(
        title = glue::glue("ASMD plot for covariate {i}"),
        x = "Absolute standardized mean difference [ASMD]",
        y = "",
        color = "asmd < 0.1",
        caption = glue::glue("ASMD is computed as the asmd between patients with and without observed '{i}'")
        ) +
      ggplot2::scale_color_identity() +
      ggplot2::theme_bw()

    if(isTRUE(median)){

      asmd_aggregate <- tibble::tibble(
        covariate = paste(i),
        asmd_median = stats::median(smd$smd)
        )

    }else{

      asmd_aggregate <- tibble::tibble(
        covariate = paste(i),
        asmd_mean = mean(smd$smd)
        )

    }

    # assemble lapply output object
    return(
      list(
        asmd_covar = i,
        asmd_table1 = print(tbl1, smd = TRUE, printToggle = FALSE),
        asmd_plot = asmd_plot,
        asmd_aggregate = asmd_aggregate
        )
      )

    }

  # iterate above analyses overall specified
  # partially observed covariates
  asmd_out <- lapply(covar_miss, FUN = smd_loop)
  names(asmd_out) <- covar_miss

  class(asmd_out) <- "asmd"

  return(asmd_out)

}

# generic print -----------------------------------------------------------

#' @export
print.asmd <- function(x, ...){

  tbl <- do.call(rbind, lapply(x,'[[',4)) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ formatC(.x, format = "f", digits = 3)))

  return(print(tbl))

}

# generic summary ---------------------------------------------------------

#' @export
summary.asmd <- function(object, ...){

  tbl <- do.call(rbind, lapply(object,'[[',4)) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ formatC(.x, format = "f", digits = 3)))

  return(tbl)

}


