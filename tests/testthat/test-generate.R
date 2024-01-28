
test_that('Function generation works', {
  dose <- c(0.1, 0.5, 1, 2, 5, 10)
  arg_c <- list(dose = dose, fn = log_logistic, error_sd = 1, b = 1, c = 2, d = 10, e = 5)
  expect_no_error(do.call(generate_continuous_data, arg_c))

})
