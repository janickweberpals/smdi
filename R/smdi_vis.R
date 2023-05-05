#' Quick and elegant visualization of partially observed/missing variables
#'
#' @description
#' This function takes a dataframe and outputs a nicely formatted
#' ggplot2 vertical barchart plot that visualizes the proportion
#' missing for a given variable (vector) or all existent missing
#' variables. Results can also be stratified by another variable
#' in which case the proportion missing refers to the amount of
#' patients in the respective stratum.
#'
#' Important: Function assumes the data is in a one-row-per-patient format.
#'
#' @param data dataframe or tibble object with partially observed/missing variables. Assumes a a one-row-per-patient format
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation
#' @param strata character name of variable/column by which results should be stratified
#'
#' @return returns ggplot2 graph displaying selected or automatically identified variables by percent missing
#'
#' @importFrom magrittr '%>%'
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme_bw
#' @importFrom glue glue
#'
#' @export
#'
#' @examples
#' library(smdi)
#'
#' smdi_vis(data = smdi_data)
#'

smdi_vis <- function(data = NULL,
                     covar = NULL,
                     strata = NULL
                     ){

  # initializing new variables
  # tip: https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  n_miss <- covariate <- prop_miss <- prop_miss_label <- .data <- NULL

  # run smdi_summary to run check on covariates and
  # compute the # and % missingness for <covar> and potentially by <strata>
  data_summary <- smdi_summarize(
    data = data,
    covar = covar,
    strata = strata
    )

  # plot missingness
  perc_max <- max(data_summary$prop_miss)

  plot_summary <- data_summary %>%
    dplyr::mutate(covariate = forcats::fct_reorder(covariate, prop_miss)) %>%
    ggplot2::ggplot(ggplot2::aes(x = covariate, y = prop_miss)) +
    ggplot2::geom_bar(stat="identity", fill = "firebrick4", color = "black") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, perc_max + (perc_max/10))) +
    ggplot2::geom_text(ggplot2::aes(label= prop_miss_label), nudge_y = perc_max/15) +
    ggplot2::labs(
      x = "",
      y = "% missing"
      ) +
    ggplot2::theme_bw()

  # if <strata> is specified plot faceted and add notes
  if(!is.null(strata)){

    plot_summary <- plot_summary +
      ggplot2::facet_wrap(~.data[[strata]]) +
      ggplot2::labs(
        subtitle = glue::glue("Results stratified by {strata}"),
        caption = glue::glue("% refer to the number of observations in each stratum of {strata}.")
        )

  }

  return(plot_summary)

}

# Other visualizations as re-exports for missing data patterns #1: upsetR plot
#' @export
#' @importFrom naniar gg_miss_upset
naniar::gg_miss_upset

# Other visualizations as re-exports for missing data patterns #1: mice md.pattern matrix
#' @export
#' @importFrom mice md.pattern
mice::md.pattern
