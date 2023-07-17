# Check if an error is thrown when no dataframe is provided
test_that("Error is thrown when no dataframe is provided", {
  expect_error(smdi_summarize(), "No dataframe provided.")
})

# Test case: Strata variable not present in dataframe
test_that("Strata variable not present in dataframe", {
  data <- data.frame(a = c(1, NA, NA, 1), b = c(4, 5, 6, 1), c = c(7, 8, 9, 1))
  expect_error(smdi_summarize(data = data, strata = "strata"), "Strata variable not present in data.")
})

# Test case: Strata variable  is a numeric
test_that("Throw warning if strata variableis numeric and/or has more than 10 different unique levels", {
  set.seed(42)
  data <- data.frame(a = c(NA, NA, rbinom(13, 1, 0.5)), b = rbinom(15, 1, 0.5), c = rnorm(15, mean = 0, sd = 1), strata = rnorm(15, mean = 0, sd = 1))
  expect_warning(smdi_summarize(data = data, strata = "strata"), "Strata variable is not a character/factor or has > 10 unique levels. Consider re-categorizing.")
})

# Test case: Strata variable has NA
test_that("Throw warning if strata variable has NA in itself", {
  data <- data.frame(a = c(1, NA, NA, 1), b = c(4, 5, 6, 1), c = c(7, 8, 9, 1), strata = c("A", NA, "B", "B"))
  expect_warning(smdi_summarize(data = data, strata = "strata"), "Strata variable has NA. Additional 'Unknown' stratum level was added.")
})

# Test case: Missing covariates with no strata
test_that("Missing covariates with no strata", {
  data <- data.frame(a = c(1, NA, 3, 4), b = c(4, NA, 6, 7), c = c(7, 8, 9, 10))
  result_summary <- smdi_summarize(data = data)

  # Check the number of missing values
  expect_equal(result_summary$n_miss, c(1, 1))

  # Check the proportion of missing values
  expect_equal(result_summary$prop_miss, c(25, 25))

  # Check the labels for proportion missing values
  expect_equal(result_summary$prop_miss_label, c("25.00%", "25.00%"))

  # Check the order of covariates
  expect_equal(result_summary$covariate, c("a", "b"))
})

# Test case: Missing covariates with strata
test_that("Missing covariates with strata", {
  data <- data.frame(a = c(1, NA, NA, 1), b = c(4, 5, 6, 1), c = c(7, 8, 9, 1), strata = c("A", "A", "B", "B"))
  result_summary <- smdi_summarize(data = data, strata = "strata")

  # Check the number of missing values
  expect_equal(result_summary$n_miss, c(1, 1))

  # Check the proportion of missing values
  expect_equal(result_summary$prop_miss, c(50, 50))

  # Check the labels for proportion missing values
  expect_equal(result_summary$prop_miss_label, c("50.00%", "50.00%"))

  # Check the order of covariates
  expect_equal(result_summary$covariate, c("a",  "a"))

  # Check the order of strata
  expect_equal(result_summary$strata, c("A", "B"))
})

# Test case: No covariates with missing values
test_that("Throw warning if there are no covariates with missing values.", {
  set.seed(42)
  data <- data.frame(a = rbinom(15, 1, 0.5), b = rbinom(15, 1, 0.5), c = rnorm(15, mean = 0, sd = 1))
  expect_error(smdi_summarize(data = data), "Found no covariates with missing values. Please check that missing values are coded as <NA>.")
})

# Test case: No covariates with missing values and <covar> specified
test_that("Throw error and warning if there are no covariates with missing values and covar is not specified.", {
  set.seed(42)
  data <- data.frame(a = rbinom(15, 1, 0.5), b = rbinom(15, 1, 0.5), c = rnorm(15, mean = 0, sd = 1))
  expect_error(smdi_summarize(data = data))
})

# Test case: No covariates with missing values and <covar> specified
test_that("Throw error and warning if there are no covariates with missing values and covar is specified.", {
  set.seed(42)
  data <- data.frame(a = rbinom(15, 1, 0.5), b = rbinom(15, 1, 0.5), c = rnorm(15, mean = 0, sd = 1))
  expect_warning(expect_error(smdi_summarize(data = data, covar = "a")))
})
