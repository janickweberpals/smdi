# Test case for the scenario when no dataframe is provided
test_that("No dataframe provided", {
  expect_error(smdi_check_covar(), "No dataframe provided.")
})

# Test case for the scenario when not all covariates specified in <covar> are present in the <data> provided
test_that("Not all covariates present", {
  data <- data.frame(a = 1:5, b = 6:10)
  covar <- c("a", "b", "c")
  expect_error(smdi_check_covar(data = data, covar = covar), "Not all covariates specified in <covar> are present in the <data> provided.")
})

# Test case for the scenario when some specified covariates in <covar> do not have missing values
test_that("Some specified covariates fully observed", {
  data <- data.frame(a = c(1, 2, NA), b = c(NA, 4, 5), c = c(7, 8, 9), d = c(10, 11, 12))
  covar <- c("a", "b", "c", "d")
  expected_warning <- "<c, d> specified as part of <covar> but does/do not contain any missing value. Please check that missing values are coded as <NA>. <c, d> will not be considered as missing <covar>."
  expected_output <- c("a", "b")
  expect_warning(result <- smdi_check_covar(data = data, covar = covar), expected_warning)
  expect_equal(result, expected_output)
})

# Test case for the scenario when all specified covariates in <covar> do not have missing values
# Test case: No covariates with missing values and <covar> specified
test_that("Throw error and warning if there are no covariates with missing values and <covar> is specified.", {
  set.seed(42)
  data <- data.frame(a = rbinom(15, 1, 0.5), b = rbinom(15, 1, 0.5), c = rnorm(15, mean = 0, sd = 1))
  expect_warning(expect_error(smdi_check_covar(data = data, covar = "a")))
})

# Test case for the scenario when <covar> is specified and does really only include covariates with at least one missing value
test_that("Covariates with missing values and <covar> specified", {
  data <- data.frame(a = c(1, 2, NA), b = c(NA, 4, 5), c = c(7, 8, 9))
  result <- smdi_check_covar(data = data, covar = c("a", "b"))
  expected_output <- c("a", "b")
  expect_equal(result, expected_output)
})

# Test case for the scenario when covar is not specified and there are covariates with missing values
test_that("Covariates with missing values and <covar> not specified", {
  data <- data.frame(a = c(1, 2, NA), b = c(NA, 4, 5), c = c(7, 8, 9))
  expected_output <- c("a", "b")
  result <- smdi_check_covar(data = data)
  expect_equal(result, expected_output)
})

# Test case for the scenario when covar is not specified and there are no covariates with missing values
test_that("No covariates with missing values", {
  data <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  expect_error(smdi_check_covar(data = data), "Found no covariates with missing values. Please check that missing values are coded as <NA>.")
})

