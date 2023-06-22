# Test case: No dataframe provided
test_that("No dataframe provided", {
  expect_error(smdi_rf(), "No dataframe provided.")
})

# Test the smdi_rf function
test_that("smdi_rf function computes random forest-based AUC correctly", {
  # Set up test data
  set.seed(42)
  data <- data.frame(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(NA, 2, NA, 4, 5),
    var3 = c(1, NA, 3, 4, 5),
    var4 = c(1, 2, 3, 4, NA)
  )

  # Call the smdi_rf function
  result <- smdi_rf(data = data, covar = "var2", ntree = 100, train_test_ratio = c(0.7, 0.3))

  # Check the class of the result
  expect_s3_class(result, "rf")

  # Check the structure of the result
  expect_named(result, c("var2"))
  expect_s3_class(result$var2$rf_table, "tbl_df")
  expect_s3_class(result$var2$rf_plot, "ggplot")

  # Check the AUC value
  expect_equal(result$var2$rf_table$rf_auc, "0.500", tolerance = 0.0001)

  # Check the importance plot
  expect_true(grepl("Covariate importance", result$var2$rf_plot$labels$title))
  expect_true(grepl("Mean decrease in accuracy", result$var2$rf_plot$labels$y))
})

# Test the print.rf function
test_that("print.rf function prints rf object correctly", {
  # Set up test data
  set.seed(42)
  data <- data.frame(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(NA, 2, NA, 4, 5),
    var3 = c(1, NA, 3, 4, NA),
    var4 = c(1, NA, 3, 4, NA)
  )

  # Call the smdi_rf function
  result <- smdi_rf(data = data, covar = "var2", ntree = 100, train_test_ratio = c(0.7, 0.3))
  expect_equal(ncol(print(result)), 2)
  expect_equal(nrow(print(result)), 1)

  # Call the smdi_rf function with all missing variables
  result <- smdi_rf(data = data, ntree = 100, train_test_ratio = c(0.7, 0.3))
  expect_equal(ncol(print(result)), 2)
  expect_equal(nrow(print(result)), 3)
})

# Test the summary.rf function
test_that("summary.rf function returns rf object summary correctly", {
  # Set up test data
  set.seed(42)
  data <- data.frame(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(NA, 2, NA, 4, 5),
    var3 = c(1, NA, 3, 4, NA),
    var4 = c(1, NA, 3, 4, NA)
  )

  # Call the smdi_rf function
  result <- smdi_rf(data = data, covar = "var2", ntree = 100, train_test_ratio = c(0.7, 0.3))
  expect_equal(ncol(summary(result)), 2)
  expect_equal(nrow(summary(result)), 1)

  # Call the smdi_rf function with all missing variables
  result <- smdi_rf(data = data, ntree = 100, train_test_ratio = c(0.7, 0.3))
  expect_equal(ncol(summary(result)), 2)
  expect_equal(nrow(summary(result)), 3)
})

# check n_cores parameter
# test_that("specified to available n cores", {
#   set.seed(42)
#   data_monotone <- data.frame(
#     var1 = rnorm(nrow(smdi_data_complete), mean = 5, sd = 0.5),
#     var2 = rnorm(nrow(smdi_data_complete), mean = 10, sd = 2.25)
#   )
#   data_monotone[3:503, "var1"] <- NA
#   data_monotone[1:500, "var2"] <- NA
#   expect_warning(smdi_rf(data = data_monotone, n_cores = 200), "You specified more <n_cores> than you have available. The function will use all cores available to it.")
# })

# Test monotonicity case
test_that("message to analyst in case of high AUCs as an indication for monotonicity", {
  set.seed(42)
  data_monotone <- data.frame(
    var1 = rnorm(nrow(smdi_data_complete), mean = 5, sd = 0.5),
    var2 = rnorm(nrow(smdi_data_complete), mean = 10, sd = 2.25)
    )
  data_monotone[3:503, "var1"] <- NA
  data_monotone[1:500, "var2"] <- NA

  expect_message(result <- smdi_rf(data = data_monotone), "Important note:")
  expect_true(result$var1$rf_table$rf_auc > 0.9)
  expect_true(result$var2$rf_table$rf_auc > 0.9)
})
