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
#' @param covar character covariate or covariate vector with variable/column name(s) to investigate
#' @param top_n_covar integer, display top n missing covariates
#' @param strata character name of variable/column by which results should be stratified
#'
#' @return returns ggplot2 graph displaying selected or chosen variables by percent missing
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr summarize_all
#' @importFrom dplyr arrange
#' @importFrom dplyr slice_max
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom tidyselect everything
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 facet_wrap
#' @importFrom glue glue
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'df %>%
#' smdi_vis(covar = "age")
#' }

smdi_vis <- function(data = NULL, # dataframe
                     covar = NULL, # covariate or covariate character vector to investigate
                     top_n_covar = NULL, # integer. display top n missing covariates
                     strata = NULL # visualizations stratified
                     ){

  # initializing new variables
  # tip: https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  n_miss <- covariate <- perc_miss <- perc_miss_label <- NULL

  # define covariate vector and implement basic checks
  if(!is.null(covar)){

    if(length(covar) > 20){message("consider less covariates for better visualization results.")}

    if(!is.null(top_n_covar)){message("both <covar> nor <top_n_covar> were specified; displaying results for <covar>")}

    covar_vec <- covar

  }else if(!is.null(top_n_covar)){

    if(top_n_covar > 20){message("consider less covariates for better visualization results.")}

    covar_vec <- data %>%
      dplyr::summarize_all(function(x) n_miss = sum(is.na(x))) %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "covariate", values_to = "n_miss") %>%
      dplyr::arrange(dplyr::desc(n_miss)) %>%
      dplyr::slice_max(n_miss, n = top_n_covar) %>%
      dplyr::pull(covariate)

    if(!is.null(strata)){

      if(strata %in% covar_vec){

        warning(glue::glue("strata variable <{strata}> amongst {top_n_covar} missing covariates!"))
        covar_vec <- covar_vec[!covar_vec %in% strata]

      }
    }

  }else{

    stop("neither <covar> nor <top_n_covar> specified! You must specify one of the two.")

  }


  # in case results are stratified, group it by stratifying variable +> select covars
  if(!is.null(strata)){

    # strata variable (helper variable easier to operate with)
    data["strata"] <- data[strata]

    # group df by stratifying variable
    data <- data %>%
      dplyr::select(tidyselect::all_of(covar_vec), strata) %>%
      dplyr::group_by(strata)

  }else{

    # in case results are not stratified
    data <- data %>%
      dplyr::select(tidyselect::all_of(covar_vec))

  }

  # now compute exact percentage missing
  data_summary <- data %>%
    dplyr::select(tidyselect::all_of(covar_vec)) %>%
    dplyr::summarize_all(function(x) n_miss = sum(is.na(x))) %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(covar_vec), names_to = "covariate", values_to = "n_miss") %>%
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
      ggplot2::facet_wrap(~strata)

  }

  return(plot_summary)

}

