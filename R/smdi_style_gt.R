#' Takes an object of class smdi and styles it to a publication-ready gt table
#'
#' @description
#' This function takes either an object of class smdi or data.frame or tibble as
#' input and styles it to a publication-ready table based on the gt package.
#' The output is of class gt and can take further gt-based arguments for
#' customization.
#'
#' @param smdi_object object of class smdi or data.frame or tibble
#' @param include_little can be logical (TRUE/FALSE) or an object of class 'little'
#' @param font_size integer to determine table font size
#' @param tbl_width integer to determine table width
#'
#'
#' @return returns a formatted gt table object
#'
#' @importFrom magrittr '%>%'
#' @importFrom gt gt tab_footnote html cells_column_labels cols_label tab_options px
#' @importFrom glue glue
#' @importFrom methods is
#'
#' @seealso
#' \code{\link{gt}}
#'
#' @export
#'
#' @examples
#'library(smdi)
#'library(dplyr)
#'
#' diagnostics <- smdi_diagnose(
#'   data = smdi_data,
#'   covar = NULL, # NULL includes all covariates with at least one NA
#'   model = "cox",
#'   form_lhs = "Surv(eventtime, status)"
#'   ) %>%
#'   smdi_style_gt()
#'
smdi_style_gt <- function(smdi_object = NULL,
                          include_little = TRUE,
                          font_size = 13,
                          tbl_width = 800
                          ){


  asmd_median_min_max <- hotteling_p <- rf_auc <- estimate_crude <- estimate_adjusted <- NULL

  # check if smdi object or table
  if(methods::is(smdi_object, "smdi")){

    smdi_table <- smdi_object$smdi_tbl

  }else if(any(class(smdi_object) %in% c("data.frame", "tibble"))){

    smdi_table <- smdi_object

  }else{

    stop("<smdi_object> is not of type smdi, data.frame or tibble")

  }

  # little (if specified)
  if(isTRUE(include_little)){

    little_foot <- glue::glue("{stringr::str_replace(smdi_object$p_little, '_', ' ')}, ")

    }else if(methods::is(include_little, "little")){

    little_foot <- glue::glue("p little: {ifelse(include_little$p.value < .001, '<.001', include_little$p.value)}, ")

    }else{

    little_foot <- ""

    }

  # general abbrevations
  foot_abbr <- "ASMD = Median absolute standardized mean difference across all covariates, AUC = Area under the curve, \U03B2 = beta coefficient, CI = Confidence interval, max = Maximum, min = Minimum"

  smdi_gt <- smdi_table %>%

    # make into a gt table
    gt::gt() %>%

    # Abbrevations
    gt::tab_footnote(
      footnote = glue::glue("{little_foot} Abbreviations: {foot_abbr}")
      ) %>%

    gt::cols_label(
      covariate = "Covariate",
      asmd_median_min_max= "ASMD (min/max)",
      hotteling_p = gt::md("p<sub>Hotelling</sub>"),
      rf_auc = "AUC",
      estimate_crude = gt::md("\U03B2<sub>crude</sub> (95% CI)"),
      estimate_adjusted = "\U03B2 (95% CI)"
      ) %>%

    # add footnotes describing three group diagnostics
    gt::tab_footnote(
      footnote = "Group 1 diagnostic: Differences in patient characteristics between patients with and without covariate",
      locations = gt::cells_column_labels(
        columns = c(asmd_median_min_max, hotteling_p)
        )
      ) %>%

    gt::tab_footnote(
      footnote = "Group 2 diagnostic: Ability to predict missingness",
      locations = gt::cells_column_labels(
        columns = rf_auc
        )
      ) %>%

    gt::tab_footnote(
      footnote = "Group 3 diagnostic: Assessment if missingness is associated with the outcome (crude, adjusted)",
      locations = gt::cells_column_labels(
        columns = c(estimate_crude, estimate_adjusted)
        )
      ) %>%

    gt::tab_options(
      table.width = gt::px(tbl_width),
      data_row.padding = gt::px(3),
      table.font.size = font_size
      ) %>%

    gt::cols_align(align = "left")


  return(smdi_gt)

}

