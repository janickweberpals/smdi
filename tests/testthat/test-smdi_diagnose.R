# Define a test case
test_that("smdi_diagnose computes three group missing data summary diagnostics", {
  # Create a sample dataset with missing values
  data <- data.frame(time =  rexp(100, rate = 0.2),
                     event = rbinom(100, 1, 0.5),
                     covariate1 = rnorm(100),
                     covariate2 = rnorm(100),
                     covariate3 = rnorm(100),
                     covariate4 = sample(c(1, 2, NA), 100, replace = TRUE))

  # Call the smdi_diagnose function
  result <- smdi_diagnose(data = data, model = "cox", form_lhs = "Surv(time, event)")

  # Check if the result is of class "smdi"
  expect_s3_class(result, "smdi")

  # Check if the summary table contains the expected columns
  expect_named(result$smdi_tbl, c("covariate", "asmd_median_min_max", "hotteling_p", "rf_auc", "estimate_crude", "estimate_adjusted"))

  # Check if the p_little value is computed
  expect_true(grepl("p_little:", result$p_little))

})

# Test print.smdi function
test_that("print.smdi function prints smdi summary table", {
  # Create a sample smdi object
  smdi_tbl <- data.frame(
    covariate = c("A", "B", "C"),
    asmd_median_min_max = c(0.1, 0.2, 0.3),
    hotteling_p = c(0.01, 0.02, 0.03),
    rf_auc = c(0.8, 0.9, 0.7),
    estimate_crude = c(0.5, 0.6, 0.4),
    estimate_adjusted = c(0.4, 0.3, 0.2)
  )
  p_little <- "p_little: <.001"
  smdi_obj <- list(smdi_tbl = smdi_tbl, p_little = p_little)
  class(smdi_obj) <- "smdi"

  # Capture the output of print.smdi function
  output <- capture_output(print(smdi_obj))

  # Check if the output contains the smdi summary table
  expect_match(output, "smdi summary table:\n  covariate asmd_median_min_max hotteling_p rf_auc estimate_crude\n1         A                 0.1        0.01    0.8            0.5\n2         B                 0.2        0.02    0.9            0.6\n3         C                 0.3        0.03    0.7            0.4\n  estimate_adjusted\n1               0.4\n2               0.3\n3               0.2\n\np_little: <.001")

})

# Test summary.smdi function
test_that("summary.smdi function returns smdi summary table", {
  # Create a sample smdi object
  smdi_tbl <- data.frame(
    covariate = c("A", "B", "C"),
    asmd_median_min_max = c(0.1, 0.2, 0.3),
    hotteling_p = c(0.01, 0.02, 0.03),
    rf_auc = c(0.8, 0.9, 0.7),
    estimate_crude = c(0.5, 0.6, 0.4),
    estimate_adjusted = c(0.4, 0.3, 0.2)
  )
  p_little <- "p_little: <.001"
  smdi_obj <- list(smdi_tbl = smdi_tbl, p_little = p_little)
  class(smdi_obj) <- "smdi"

  # Call summary.smdi function
  summary_tbl <- summary(smdi_obj)

  # Check if the returned object is a data frame
  expect_s3_class(summary_tbl, "data.frame")

  # Check if the summary table has the correct column names
  expect_equal(colnames(summary_tbl), c("covariate", "asmd_median_min_max", "hotteling_p", "rf_auc", "estimate_crude", "estimate_adjusted"))

  # Check if the summary table has the correct values
  expect_equal(summary_tbl[,1], c("A", "B", "C"))
  expect_equal(summary_tbl[,2], c(0.1, 0.2, 0.3))
  expect_equal(summary_tbl[,3], c(0.01, 0.02, 0.03))
  expect_equal(summary_tbl[,4], c(0.8, 0.9, 0.7))
  expect_equal(summary_tbl[,5], c(0.5, 0.6, 0.4))
  expect_equal(summary_tbl[,6], c(0.4, 0.3, 0.2))
})
