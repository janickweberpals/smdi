# Check if an error is thrown when no dataframe is provided
test_that("Error is thrown when no dataframe is provided", {
  expect_error(smdi_hotelling(), "No dataframe provided.")
})

# Check if hotelling object is returned
test_that("Hotelling object is returned", {
  data <- data.frame(x = c(1, 2, NA), y = c(3, NA, 5))
  result <- smdi_hotelling(data = data)
  expect_true(is(result, "hotelling"))
})

# Check if the number of hotelling objects matches the number of covariates
test_that("Number of hotelling objects matches number of covariates", {
  data <- data.frame(x = c(1, 2, NA, 9), y = c(3, NA, 5, 8), z = c(0, 0, 0, 1))
  result <- smdi_hotelling(data = data)
  expect_equal(length(result), 2)
})

# Check if hotelling object contains expected covariates
test_that("Hotelling object contains expected covariates", {
  data <- data.frame(x = c(1, 2, NA), y = c(3, NA, 5))
  result <- smdi_hotelling(data = data)
  expect_true(all(c("x" %in% names(result), "y" %in% names(result))))
})

# Check if hotelling object contains expected output variables
test_that("Hotelling object contains expected output variables", {
  data <- data.frame(x = c(1, 2, NA, 9), y = c(3, NA, 5, 8), z = c(0, 0, 0, 1))
  result <- smdi_hotelling(data = data)
  expect_true(all(c("stats" %in% names(result$x), "pval" %in% names(result$x))))
})

# Check if hotelling object returns numeric p-values
test_that("Hotelling object returns correct p-values", {
  data <- data.frame(x = c(1, 2, NA), y = c(3, NA, 5))
  result <- smdi_hotelling(data = data)
  expect_true(is.numeric(result$x$pval))
  expect_true(is.numeric(result$y$pval))
})

# Check if dummy_columns function is applied correctly
test_that("dummy_columns function is applied correctly", {
  data <- data.frame(x = c("A", "B", "A"), y = c("C", "D", "D"))
  expected <- data.frame(x_A = c(1, 0, 1), x_B = c(0, 1, 0), y_C = c(1, 0, 0), y_D = c(0, 1, 1))
  result <- data %>%
    fastDummies::dummy_columns(
      remove_most_frequent_dummy = TRUE,
      ignore_na = FALSE,
      remove_selected_columns = TRUE
    )
  expect_equal(ncol(result), 2)
})

# Check if print function produces the expected output
test_that("print function produces the expected output", {
  data <- data.frame(x = c(1, 2, NA), y = c(3, NA, 5))
  result <- smdi_hotelling(data = data)
  expect_equal(ncol(print(result)), 2)
  expect_equal(nrow(print(result)), 2)
})

# Check if summary function produces the expected output
test_that("summary function produces the expected output", {
  data <- data.frame(x = c(1, 2, NA), y = c(3, NA, 5))
  result <- smdi_hotelling(data = data)
  expect_equal(ncol(summary(result)), 2)
  expect_equal(nrow(summary(result)), 2)
})

# Check n_core warning
# test_that("specified to available n cores", {
#   data <- data.frame(x = c(1, 2, NA), y = c(3, NA, 5))
#   expect_warning(smdi_hotelling(data = data, n_cores = 200), "You specified more <n_cores> than you have available. The function will use all cores available to it.")
# })
