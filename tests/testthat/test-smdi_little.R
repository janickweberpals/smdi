# Test case 1: Check if the function returns a "little" object
test_that("function returns a 'little' object", {
  data <- data.frame(
    var1 = c(1, 2, NA, 4),
    var2 = c(NA, 2, 3, 4),
    var3 = c(1, NA, 3, 4)
  )
  result <- smdi_little(data)
  expect_s3_class(result, "little")
})

# Test case 2: Check if the function handles missing data correctly
test_that("function handles missing data correctly", {
  data <- data.frame(
    var1 = c(1, 2, NA, 4),
    var2 = c(NA, 2, 3, 4),
    var3 = c(1, NA, 3, 4)
  )
  result <- smdi_little(data)
  expect_equal(result$p.value, 0.24, tolerance = 0.01)
  expect_true(is.numeric(result$p.value))
})

# Test case 3: Check if the function raises an error for missing input data
test_that("function raises an error for missing input data", {
  expect_error(smdi_little(), "No dataframe provided.")
})
