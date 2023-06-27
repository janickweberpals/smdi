# Unit tests for the smdi_outcome function
test_that("smdi_outcome returns the expected output", {
  # Test case 1: Check if function throws an error when no dataframe is provided
  expect_error(smdi_outcome(), "No dataframe provided.")
})

# n_cores on windows
test_that("Test n_cores OS dependency", {
  set.seed(42)
  data <- data.frame(x = c(NA, NA, rbinom(97, 1, 0.5), NA), y = c(rbinom(97, 1, 0.5), NA, NA, NA), z = rnorm(100))
  if(isTRUE(Sys.info()[["sysname"]]=="Windows")){
    expect_warning(smdi_outcome(data = data, n_cores = 2))
  }else{
    expect_no_warning(smdi_outcome(data = data, model = "linear", form_lhs = "z", n_cores = 2))
  }
})

test_that("No LHS form provided", {
  # Test case 2: Check if function throws an error when no form_lhs is provided
  expect_error(smdi_outcome(data = iris), "No <form_lhs> provided.")
})

test_that("Invalid LHS provided", {
  # Test case 3: Check if function throws an error when an invalid model type is specified
  expect_error(smdi_outcome(data = iris, form_lhs = "Sepal.Length", model = "invalid_model"), "<model> either not specified or not of type logistic, linear or cox")
})


# Test case for logistic model with binary outcome
test_that("Logistic model with binary outcome works correctly", {
  data <- data.frame(outcome = rbinom(100, 1, 0.5),
                     covariate1 = rnorm(100),
                     covariate2 = rnorm(100),
                     covariate3 = rnorm(100),
                     covariate4 = sample(c(1, 2, NA), 100, replace = TRUE))

  result <- smdi_outcome(data = data, model = "logistic", form_lhs = "outcome")

  # Perform assertions on the result
  expect_true("covariate4" %in% result$covariate)
  expect_equal(nrow(result), 1)
})

# Test case for linear model with continuous outcome
test_that("Linear model with continuous outcome works correctly", {
  data <- data.frame(outcome = rnorm(100),
                     covariate1 = rnorm(100),
                     covariate2 = rnorm(100),
                     covariate3 = rnorm(100),
                     covariate4 = sample(c(1, 2, NA), 100, replace = TRUE))

  result <- smdi_outcome(data = data, model = "linear", form_lhs = "outcome")

  # Perform assertions on the result
  expect_true("covariate4" %in% result$covariate)
  expect_equal(nrow(result), 1)
})

# Test case for Cox model with time-to-event outcome
test_that("Cox model with time-to-event outcome works correctly", {
  library(survival)
  data <- data.frame(time =  rexp(100, rate = 0.2),
                     event = rbinom(100, 1, 0.5),
                     covariate1 = rnorm(100),
                     covariate2 = rnorm(100),
                     covariate3 = rnorm(100),
                     covariate4 = sample(c(1, 2, NA), 100, replace = TRUE))

  result <- smdi_outcome(data = data, model = "cox", form_lhs = "Surv(time, event)")

  # Perform assertions on the result
  expect_true("covariate4" %in% result$covariate)
  expect_equal(nrow(result), 1)
})


