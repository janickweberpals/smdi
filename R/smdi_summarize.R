#' Utility helper to give a light summary of partially observed covariates
#'
#' @description
#' This function takes a dataframe and automatically returns the amount and proportion of
#' missing for partially observed covariates assuming a one-row-per-patient
#' dataframe. This is an important utility function for other functions in this package.
#' Results can also be stratified by another variable
#' in which case the proportion missing refers to the amount of
#' patients in the respective stratum.
#'
#' @param data dataframe or tibble object with partially observed/missing variables. Assumes a a one-row-per-patient format.
#' @param covar character covariate or covariate vector with partially observed variable/column name(s) to investigate. If NULL, the function automatically includes all columns with at least one missing observation.
#' @param strata character name of variable/column by which results should be stratified
#'
#' @return returns count and proportion of missing values. If <strata> is specified, the returned proportion refers to the amount of
#' patients in the respective stratum.
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#'
#' @export
#'
#' @examples
#' library(smdi)
#'
#' smdi_vis(data = smdi_data)
#'

smdi_summarize <- function(data = NULL,
                           covar = NULL,
                           strata = NULL
                           ){

  # initializing new variables
  # tip: https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  n_miss <- covariate <- prop_miss <- prop_miss_label <- .data <- NULL

  # checks
  if(is.null(data)){stop("No dataframe provided.")}

  # check for specified/missing covariates
  covar_miss <- smdi::smdi_check_covar(
    data = data,
    covar = covar
    )

  # checks and grouping in case <strata> is specified
  if(!is.null(strata)){

    # check if <strata> variable is present in <data>
    if(!strata %in% names(data)){stop("Strata variable not present in data.")}

    # raise warning to user if strata variable is not a character or factor or has more than 10 unique levels (in this case the variable likely has wrong encoding)
    if(!class(data[[strata]]) %in% c("factor", "character") | nlevels(factor(data[[strata]])) > 10){

      warning("Strata variable is not a character/factor or has > 10 unique levels. Consider re-categorizing.")

      }

    # return a warning if <strata> variable itself has NA values
    if(sum(is.na(data[[strata]])) > 0){

      # assign an 'unknown' <strata> level
      data[[strata]] <- ifelse(is.na(data[[strata]]), "Unknown", data[[strata]])

      # remove <strata> from covar_miss
      covar_miss <- covar_miss[!covar_miss == strata]

      warning("Strata variable has NA. Additional 'Unknown' stratum level was added.")

      }

    # group data for summarizing NA by stratum
    data <- data %>%
      dplyr::group_by(.data[[strata]])

    }

  # now compute count and percentage missing
  data_summary <- data %>%
    dplyr::summarize(
      dplyr::across(
        tidyselect::all_of(covar_miss),
        .fns = list(
          n_miss = ~ sum(is.na(.x)),
          prop_miss = ~ sum(is.na(.x))/dplyr::n()*100 # n is denominator size => stratum (if grouped) or total (if ungrouped)
          ),
        .names = "{.col}_{.fn}"),
      .groups = "drop"
      ) %>%
    tidyr::pivot_longer(
      cols = -{{strata}},
      names_to = c("covariate", ".value"),
      names_pattern = "(.+)_(n_miss|prop_miss)"
      ) %>%
    dplyr::mutate(prop_miss_label = paste0(formatC(prop_miss, format = 'f', digits = 2), "%")) %>%
    # sort by prop missing and covariate
    dplyr::arrange(dplyr::desc(prop_miss), covariate)

  return(data_summary)

}
