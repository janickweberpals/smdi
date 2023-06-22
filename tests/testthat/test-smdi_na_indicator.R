# Test case for the scenario when no dataframe is provided
test_that("No dataframe provided", {
  expect_error(smdi_na_indicator(), "No dataframe provided.")
})

# Define a test case
test_that("Create binary missing indicator variables", {
  # Create a sample dataframe
  data <- data.frame(a = c(1, 2, NA), b = c(NA, 4, 5), c = c(7, NA, 9))
  covar <- c("a", "b", "c")

  # Test approach 1 (drop_NA_col = FALSE)
  result_approach1 <- smdi_na_indicator(data = data, covar = covar, drop_NA_col = FALSE)
  expect_equal(names(result_approach1), c("a", "b", "c", "a_NA", "b_NA", "c_NA"))
  expect_identical(result_approach1$a_NA, c(0, 0, 1))
  expect_identical(result_approach1$b_NA, c(1, 0, 0))
  expect_identical(result_approach1$c_NA, c(0, 1, 0))

  # Test approach 2 (drop_NA_col = TRUE)
  result_approach2 <- smdi_na_indicator(data = data, covar = covar, drop_NA_col = TRUE)
  expect_equal(names(result_approach2), c("a_NA", "b_NA", "c_NA"))
  expect_identical(result_approach2$a_NA, c(0, 0, 1))
  expect_identical(result_approach2$b_NA, c(1, 0, 0))
  expect_identical(result_approach2$c_NA, c(0, 1, 0))
})

# Test case for the scenario when no covariate is specified and drop_NA_col = FALSE
test_that("No covariate specified - drop_NA_col = FALSE", {
  data <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 4), c = c(5, 6, 7))
  expected_output <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 4), c = c(5, 6, 7), a_NA = c(0, 1, 0), b_NA = c(1, 0, 0))
  result <- smdi_na_indicator(data = data, drop_NA_col = FALSE)
  expect_equal(result, expected_output)
})

# Test case for the scenario when no covariate is specified and drop_NA_col = TRUE
test_that("No covariate specified - drop_NA_col = TRUE", {
  data <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 4), c = c(5, 6, 7))
  expected_output <- data.frame(c = c(5, 6, 7), a_NA = c(0, 1, 0), b_NA = c(1, 0, 0))
  result <- smdi_na_indicator(data = data, drop_NA_col = TRUE)
  expect_equal(result, expected_output)
})

# Test case for the scenario when covariate is specified and drop_NA_col = FALSE
test_that("Covariate specified - drop_NA_col = FALSE", {
  data <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 4), c = c(5, 6, 7))
  covar <- c("a", "c")
  expected_output <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 4), c = c(5, 6, 7), a_NA = c(0, 1, 0))
  expected_warning <- "<c> specified as part of <covar> but does/do not contain any missing value. Please check that missing values are coded as <NA>. <c> will not be considered as missing <covar>."
  expect_warning(result <- smdi_na_indicator(data = data, covar = covar, drop_NA_col = FALSE), expected_warning)
  expect_equal(result, expected_output)
})

# Test case for the scenario when covariate is specified and drop_NA_col = TRUE
test_that("Covariate specified - drop_NA_col = TRUE", {
  data <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 4), c = c(5, 6, 7))
  covar <- c("a", "c")
  expected_output <- data.frame(b = c(NA, 2, 4), c = c(5, 6, 7), a_NA = c(0, 1, 0))
  expected_warning <- "<c> specified as part of <covar> but does/do not contain any missing value. Please check that missing values are coded as <NA>. <c> will not be considered as missing <covar>."
  expect_warning(result <- smdi_na_indicator(data = data, covar = covar, drop_NA_col = TRUE), expected_warning)
  expect_equal(result, expected_output)
})
