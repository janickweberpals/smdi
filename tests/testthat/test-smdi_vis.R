# Test case: No dataframe provided
test_that("No dataframe provided", {
  expect_error(smdi_vis(), "No dataframe provided.")
})

# Test case: All variables missing
test_that("All variables missing", {
  data <- data.frame(a = c(NA, NA, NA), b = c(NA, NA, NA), c = c(NA, NA, NA))
  obj <- smdi_vis(data)
  expect_s3_class(obj, "ggplot")
})

# Test case: stratified plot
test_that("Stratified plot", {
  data <- data.frame(a = c(1, NA, NA, 3), b = c(1, 0, 1, 1), S = c("A", "B", "A", "B"))
  obj <- smdi_vis(data, strata = "S")
  expect_s3_class(obj, "ggplot")
})
