# n_cores on windows
test_that("Test n_cores OS dependency", {
  set.seed(42)
  data <- data.frame(x = c(NA, NA, rbinom(97, 1, 0.5), NA), y = c(rbinom(97, 1, 0.5), NA, NA, NA), z = rnorm(100))
  if(isTRUE(Sys.info()[["sysname"]]=="Windows")){
    expect_warning(smdi_asmd(data = data, n_cores = 2))
  }else{
      expect_no_warning(smdi_asmd(data = data, n_cores = 2))
    }
})

# Define a test context
test_that("smdi_asmd function tests", {

  # Test 1: Check if the function throws an error when no dataframe is provided
  expect_error(smdi_asmd(), "No dataframe provided.")

  # Test 2: Check if the function returns the expected output for a valid input
  data <- data.frame(
    covariate1 = c(1, 2, NA, 4, 5, NA, 7, 8, NA),
    covariate2 = c(NA, 2, 3, NA, 5, 6, 7, 8, 9),
    outcome = c(0, 1, 1, 0, 0, 1, 1, 0, 0)
  )

  result <- smdi_asmd(data)

  # Perform assertions on the output
  expect_true("covariate1" %in% names(result))
  expect_true("covariate2" %in% names(result))

  expect_named(result$covariate1, c("asmd_covar", "asmd_table1", "asmd_plot", "asmd_aggregate"))
  expect_named(result$covariate2, c("asmd_covar", "asmd_table1", "asmd_plot", "asmd_aggregate"))

  # Test 3: Check if the function handles missing covariate argument correctly
  result_missing_covar <- smdi_asmd(data, covar = NULL)

  # Perform assertions on the output
  expect_true("covariate1" %in% names(result_missing_covar))
  expect_true("covariate2" %in% names(result_missing_covar))

  expect_named(result_missing_covar$covariate1, c("asmd_covar", "asmd_table1", "asmd_plot", "asmd_aggregate"))
  expect_named(result_missing_covar$covariate2, c("asmd_covar", "asmd_table1", "asmd_plot", "asmd_aggregate"))

  # Test 4: Check if the function handles the median argument correctly
  result_mean <- smdi_asmd(data, median = FALSE)

  # Perform assertions on the output
  expect_named(result_mean$covariate1$asmd_aggregate, c("covariate", "asmd_mean", "asmd_min", "asmd_max"))
  expect_named(result_mean$covariate2$asmd_aggregate, c("covariate", "asmd_mean", "asmd_min", "asmd_max"))

  expect_true(all(names(result$covariate1$asmd_aggregate) %in% c("covariate", "asmd_median", "asmd_min", "asmd_max")))
  expect_true(all(names(result$covariate2$asmd_aggregate) %in% c("covariate", "asmd_median", "asmd_min", "asmd_max")))

  # Add more tests as needed

})
