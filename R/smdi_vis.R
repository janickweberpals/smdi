# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# function assumes a one-row-per-patient dataframe

smdi_vis <- function(
    data = NULL, # dataframe
    covar = NULL, # covariate vector to investigate
    top_n_covar = NULL, # integer. display top n missing covariates
    strata = NULL # visualizations stratified
    ){

  # define covariate vector and implement basic checks
  if(!is.null(covar)){

    if(length(covar) > 20){message("consider less covariates for better visualization results.")}
    if(!is.null(top_n_covar)){message("both <covar> nor <top_n_covar> were specified, displaying results for <covar>")}

    covar_vec <- covar

  }else if(!is.null(top_n_covar)){

    if(length(top_n_covar) > 20){message("consider less covariates for better visualization results.")}

    covar_vec <- data %>%
      dplyr::summarize_all(function(x) n_miss = sum(is.na(x))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "covariate", values_to = "n_miss") %>%
      dplyr::arrange(desc(n_miss)) %>%
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
    dplyr::summarize_all(function(x) n_miss = sum(is.na(x))) %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(covar_vec), names_to = "covariate", values_to = "n_miss") %>%
    dplyr::mutate(perc_miss = n_miss/nrow(data)*100) %>%
    dplyr::mutate(perc_miss_label = paste0(formatC(perc_miss, format = 'f', digits = 2), "%")) %>%
    dplyr::arrange(desc(perc_miss), covariate)

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

