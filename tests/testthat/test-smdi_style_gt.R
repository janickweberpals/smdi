# Throw error when smdi_object is not of eligible type
test_that("smdi_style_gt formats eligible input objects", {
  # Create a sample smdi object
  smdi_obj <- list(
    smdi_tbl = data.frame(
      covariate = c("A", "B", "C"),
      asmd_median_min_max = c(0.1, 0.2, 0.3),
      hotteling_p = c(0.01, 0.02, 0.03),
      rf_auc = c(0.8, 0.9, 0.7),
      estimate_univariate = c(0.5, 0.6, 0.4),
      estimate_adjusted = c(0.4, 0.3, 0.2)
    ),
    p_little = "p_little: 0.001"
  )

  # expect error since smdi_obj is of class list
  expect_error(gt_table <- smdi_style_gt(smdi_object = smdi_obj), "<smdi_object> is not of type smdi, data.frame or tibble")

})

test_that("smdi_style_gt throws an error for unsupported object type", {
  # Create an unsupported object
  unsupported_obj <- c("A", "B", "C")

  # Test if an error is thrown
  expect_error(smdi_style_gt(smdi_object = unsupported_obj), "<smdi_object> is not of type smdi, data.frame or tibble")
})

# Test cases for smdi_style_gt function when little is TRUE
test_that("smdi_style_gt formats smdi object correctly with include_little = TRUE", {
  # Create a sample smdi object
  smdi_obj <- list(
    smdi_tbl = data.frame(
      covariate = c("A", "B", "C"),
      asmd_median_min_max = c(0.1, 0.2, 0.3),
      hotteling_p = c(0.01, 0.02, 0.03),
      rf_auc = c(0.8, 0.9, 0.7),
      estimate_univariate = c(0.5, 0.6, 0.4),
      estimate_adjusted = c(0.4, 0.3, 0.2)
    ),
    p_little = "p_little: 0.001"
  )

  # pretend we have smdi object
  class(smdi_obj) <- "smdi"

  # Format smdi object to gt table
  gt_table <- smdi_style_gt(smdi_object = smdi_obj, include_little = TRUE)

  # Test table properties
  expect_s3_class(gt_table, "gt_tbl")

  # Number of rows excluding column labels and data
  expect_equal(nrow(gt_table$`_data`), 3)

  # Test column labels
  expect_equal(gt_table$`_boxhead`$var, c("covariate", "asmd_median_min_max", "hotteling_p", "rf_auc", "estimate_univariate", "estimate_adjusted"))

  # Test table content
  expect_equal(gt_table$`_data`$covariate, c("A", "B", "C"))
  expect_equal(gt_table$`_data`$asmd_median_min_max, c(0.1, 0.2, 0.3))
  expect_equal(gt_table$`_data`$hotteling_p,  c(0.01, 0.02, 0.03))
  expect_equal(gt_table$`_data`$rf_auc, c(0.8, 0.9, 0.7))
  expect_equal(gt_table$`_data`$estimate_univariate, c(0.5, 0.6, 0.4))
  expect_equal(gt_table$`_data`$estimate_adjusted, c(0.4, 0.3, 0.2))

  # Little's test in footnote
  expect_equal(gt_table$`_footnotes`$footnotes[[1]][1], "p little: 0.001,  Abbreviations: ASMD = Median absolute standardized mean difference across all covariates, AUC = Area under the curve, beta = beta coefficient, CI = Confidence interval, max = Maximum, min = Minimum")

  # Other footnotes
  ## ASMD and p hoteling (Group 1 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[2]], "Group 1 diagnostic: Differences in patient characteristics between patients with and without covariate")
  expect_equal(gt_table$`_footnotes`$footnotes[[3]], "Group 1 diagnostic: Differences in patient characteristics between patients with and without covariate")

  ## AUC (Group 2 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[4]], "Group 2 diagnostic: Ability to predict missingness")

  ## univariate and adjusted beta coefficient (Group 3 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[5]], "Group 3 diagnostic: Assessment if missingness is associated with the outcome (univariate, adjusted)")
  expect_equal(gt_table$`_footnotes`$footnotes[[6]], "Group 3 diagnostic: Assessment if missingness is associated with the outcome (univariate, adjusted)")
})

# Test cases for smdi_style_gt function when little is FALSE
test_that("smdi_style_gt handles include_little = FALSE", {

  # Create a sample smdi object
  smdi_obj <- list(
    smdi_tbl = data.frame(
      covariate = c("A", "B", "C"),
      asmd_median_min_max = c(0.1, 0.2, 0.3),
      hotteling_p = c(0.01, 0.02, 0.03),
      rf_auc = c(0.8, 0.9, 0.7),
      estimate_univariate = c(0.5, 0.6, 0.4),
      estimate_adjusted = c(0.4, 0.3, 0.2)
    ),
    p_little = "p_little: 0.001"
  )

  # pretend we have smdi object
  class(smdi_obj) <- "smdi"

  # Format smdi object to gt table
  gt_table <- smdi_style_gt(smdi_object = smdi_obj, include_little = FALSE)

  # Test table properties
  expect_s3_class(gt_table, "gt_tbl")

  # Number of rows excluding column labels and data
  expect_equal(nrow(gt_table$`_data`), 3)

  # Test column labels
  expect_equal(gt_table$`_boxhead`$var, c("covariate", "asmd_median_min_max", "hotteling_p", "rf_auc", "estimate_univariate", "estimate_adjusted"))

  # Test table content
  expect_equal(gt_table$`_data`$covariate, c("A", "B", "C"))
  expect_equal(gt_table$`_data`$asmd_median_min_max, c(0.1, 0.2, 0.3))
  expect_equal(gt_table$`_data`$hotteling_p,  c(0.01, 0.02, 0.03))
  expect_equal(gt_table$`_data`$rf_auc, c(0.8, 0.9, 0.7))
  expect_equal(gt_table$`_data`$estimate_univariate, c(0.5, 0.6, 0.4))
  expect_equal(gt_table$`_data`$estimate_adjusted, c(0.4, 0.3, 0.2))

  # NO Little's test in footnote, just abbreviations
  expect_equal(gt_table$`_footnotes`$footnotes[[1]][1], " Abbreviations: ASMD = Median absolute standardized mean difference across all covariates, AUC = Area under the curve, beta = beta coefficient, CI = Confidence interval, max = Maximum, min = Minimum")

  # Other footnotes
  ## ASMD and p hoteling (Group 1 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[2]], "Group 1 diagnostic: Differences in patient characteristics between patients with and without covariate")
  expect_equal(gt_table$`_footnotes`$footnotes[[3]], "Group 1 diagnostic: Differences in patient characteristics between patients with and without covariate")

  ## AUC (Group 2 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[4]], "Group 2 diagnostic: Ability to predict missingness")

  ## univariate and adjusted beta coefficient (Group 3 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[5]], "Group 3 diagnostic: Assessment if missingness is associated with the outcome (univariate, adjusted)")
  expect_equal(gt_table$`_footnotes`$footnotes[[6]], "Group 3 diagnostic: Assessment if missingness is associated with the outcome (univariate, adjusted)")
})

# Throw warning if include_little = TRUE and smdi_object is not an object of class smdi
test_that("smdi_style_gt throws error if include_little = TRUE and smdi_object is not an object of class smdi", {

  # Create a sample smdi object
  smdi_obj <- data.frame(
      covariate = c("A", "B", "C"),
      asmd_median_min_max = c(0.1, 0.2, 0.3),
      hotteling_p = c(0.01, 0.02, 0.03),
      rf_auc = c(0.8, 0.9, 0.7),
      estimate_univariate = c(0.5, 0.6, 0.4),
      estimate_adjusted = c(0.4, 0.3, 0.2)
      )

  # Format smdi object to gt table
  expect_warning(gt_table <- smdi_style_gt(smdi_object = smdi_obj, include_little = TRUE))

  # Test table properties
  expect_s3_class(gt_table, "gt_tbl")

  # Number of rows excluding column labels and data
  expect_equal(nrow(gt_table$`_data`), 3)

  # Test column labels
  expect_equal(gt_table$`_boxhead`$var, c("covariate", "asmd_median_min_max", "hotteling_p", "rf_auc", "estimate_univariate", "estimate_adjusted"))

  # Test table content
  expect_equal(gt_table$`_data`$covariate, c("A", "B", "C"))
  expect_equal(gt_table$`_data`$asmd_median_min_max, c(0.1, 0.2, 0.3))
  expect_equal(gt_table$`_data`$hotteling_p,  c(0.01, 0.02, 0.03))
  expect_equal(gt_table$`_data`$rf_auc, c(0.8, 0.9, 0.7))
  expect_equal(gt_table$`_data`$estimate_univariate, c(0.5, 0.6, 0.4))
  expect_equal(gt_table$`_data`$estimate_adjusted, c(0.4, 0.3, 0.2))

  # NO Little's test in footnote, just abbreviations
  expect_equal(gt_table$`_footnotes`$footnotes[[1]][1], " Abbreviations: ASMD = Median absolute standardized mean difference across all covariates, AUC = Area under the curve, beta = beta coefficient, CI = Confidence interval, max = Maximum, min = Minimum")

  # Other footnotes
  ## ASMD and p hoteling (Group 1 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[2]], "Group 1 diagnostic: Differences in patient characteristics between patients with and without covariate")
  expect_equal(gt_table$`_footnotes`$footnotes[[3]], "Group 1 diagnostic: Differences in patient characteristics between patients with and without covariate")

  ## AUC (Group 2 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[4]], "Group 2 diagnostic: Ability to predict missingness")

  ## univariate and adjusted beta coefficient (Group 3 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[5]], "Group 3 diagnostic: Assessment if missingness is associated with the outcome (univariate, adjusted)")
  expect_equal(gt_table$`_footnotes`$footnotes[[6]], "Group 3 diagnostic: Assessment if missingness is associated with the outcome (univariate, adjusted)")

})

# smdi format table correctly if smdi_object is a tibble/df and include_little is a little's object
test_that("smdi_style_gt handles non-smdi object with little's object", {
  # Create a sample data frame
  df <- data.frame(
    covariate = c("A", "B", "C"),
    asmd_median_min_max = c(0.1, 0.2, 0.3),
    hotteling_p = c(0.01, 0.02, 0.03),
    rf_auc = c(0.8, 0.9, 0.7),
    estimate_univariate = c(0.5, 0.6, 0.4),
    estimate_adjusted = c(0.4, 0.3, 0.2)
  )

  # create a little's object
  little_obj <- list(
    statistic = 801.0,
    df = 86,
    p.value = 0.005,
    missing.patterns = 9
  )
  class(little_obj) <- "little"

  gt_table <- smdi_style_gt(smdi_object = df, include_little = little_obj)

  # Test table properties
  expect_s3_class(gt_table, "gt_tbl")

  # Number of rows excluding column labels and data
  expect_equal(nrow(gt_table$`_data`), 3)

  # Test column labels
  expect_equal(gt_table$`_boxhead`$var, c("covariate", "asmd_median_min_max", "hotteling_p", "rf_auc", "estimate_univariate", "estimate_adjusted"))

  # Test table content
  expect_equal(gt_table$`_data`$covariate, c("A", "B", "C"))
  expect_equal(gt_table$`_data`$asmd_median_min_max, c(0.1, 0.2, 0.3))
  expect_equal(gt_table$`_data`$hotteling_p,  c(0.01, 0.02, 0.03))
  expect_equal(gt_table$`_data`$rf_auc, c(0.8, 0.9, 0.7))
  expect_equal(gt_table$`_data`$estimate_univariate, c(0.5, 0.6, 0.4))
  expect_equal(gt_table$`_data`$estimate_adjusted, c(0.4, 0.3, 0.2))

  # NO Little's test in footnote, just abbreviations
  expect_equal(gt_table$`_footnotes`$footnotes[[1]][1], "p little: 0.005,  Abbreviations: ASMD = Median absolute standardized mean difference across all covariates, AUC = Area under the curve, beta = beta coefficient, CI = Confidence interval, max = Maximum, min = Minimum")

  # Other footnotes
  ## ASMD and p hoteling (Group 1 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[2]], "Group 1 diagnostic: Differences in patient characteristics between patients with and without covariate")
  expect_equal(gt_table$`_footnotes`$footnotes[[3]], "Group 1 diagnostic: Differences in patient characteristics between patients with and without covariate")

  ## AUC (Group 2 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[4]], "Group 2 diagnostic: Ability to predict missingness")

  ## univariate and adjusted beta coefficient (Group 3 diagnostic)
  expect_equal(gt_table$`_footnotes`$footnotes[[5]], "Group 3 diagnostic: Assessment if missingness is associated with the outcome (univariate, adjusted)")
  expect_equal(gt_table$`_footnotes`$footnotes[[6]], "Group 3 diagnostic: Assessment if missingness is associated with the outcome (univariate, adjusted)")

})
