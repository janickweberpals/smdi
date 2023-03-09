#' Quick and elegant visualization of partially observed/missing variables
#'
#' @description
#' This function takes a dataframe and outputs a nicely formatted
#' ggplot2 vertical barchart plot that visualizes the proportion
#' missing for a given variable (vector) or the top n missing
#' variables. Results can also be stratified by another variable.
#' The function assumes a one-row-per-patient dataframe.
#'
#' @param data dataframe or tibble object with partially observed/missing variables
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation
#' @param strata character name of variable/column by which results should be stratified
#'
#' @return returns ggplot2 graph displaying selected or chosen variables by percent missing
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr slice_max
#' @importFrom dplyr summarize_all
#' @importFrom forcats fct_reorder
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom tidyselect everything
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
  n_miss <- covariate <- perc_miss <- perc_miss_label <- .data <- NULL

  # check for missing covariates
  # if they are not specified, all NAs are returned
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )

  # additional checks
  if(length(covar_miss) > 20){message(">20 covariates with NA. Consider less covariates for better visualization results.")}

  if(!is.null(strata)){

    if(!strata %in% names(data)){stop("Strata variable not present in data.")}

    if(sum(is.na(data[[strata]])) > 0){warning("Strata variable has NA.")}

    # group data
    data <- data %>%
      dplyr::group_by(.data[[strata]])

    }

  # now compute exact percentage missing
  data_summary <- data %>%
    dplyr::summarize(dplyr::across(tidyselect::all_of(covar_miss), ~ sum(is.na(.x))), .drop = "groups") %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(covar_miss), names_to = "covariate", values_to = "n_miss") %>%
    dplyr::mutate(perc_miss = n_miss/nrow(data)*100) %>%
    dplyr::mutate(perc_miss_label = paste0(formatC(perc_miss, format = 'f', digits = 2), "%")) %>%
    dplyr::arrange(dplyr::desc(perc_miss), covariate)

  # plot missingness
  perc_max <- max(data_summary$perc_miss)

  plot_summary <- data_summary %>%
    dplyr::mutate(covariate = forcats::fct_reorder(covariate, perc_miss)) %>%
    ggplot2::ggplot(ggplot2::aes(x = covariate, y = perc_miss)) +
    ggplot2::geom_bar(stat="identity", fill = "firebrick4", color = "black") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, perc_max + (perc_max/10))) +
    ggplot2::geom_text(ggplot2::aes(label= perc_miss_label), nudge_y = perc_max/15) +
    ggplot2::labs(x = "", y = "% missing",
                  caption = glue::glue("Results refer to dataset with n = {formatC(nrow(data), big.mark = ',')} patients")) +
    ggplot2::theme_bw()

  if(!is.null(strata)){

    plot_summary <- plot_summary +
      ggplot2::facet_wrap(~.data[[strata]]) +
      ggplot2::labs(
        subtitle = glue::glue("Results stratified by {strata}")
      )

  }

  return(plot_summary)

}

