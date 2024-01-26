

test_that('Constraints work', {
  arg_list <- list(drc::ryegrass,
                   response = 'rootl',
                   dose = 'conc',
                   type = c('stimulation'),
                   vary_slope = TRUE,
                   model_type = 'log-logistic',
                   log_transform_dose = FALSE,
                   normalized_response = FALSE,
                   dose_log = c('none', 'log10', 'natural'),
                   normalize_response = FALSE,
                   fixed_params = list('c' = 1),
                   family = c("continuous", "binomial", "Poisson"),
                   constraints = list(upper = list('e' = 20),
                                      lower = list('b' = 0.1)))
  expect_no_error(do.call(dose_response_analysis, arg_list))
  arg_list$constraints$upper[['c']] <- 2
  expect_error(do.call(dose_response_analysis, arg_list), "Put restrictions")
})

test_that('Model works', {
  arg_list <- list(drc::ryegrass,
                   response = 'rootl',
                   dose = 'conc',
                   type = c('stimulation'),
                   vary_slope = TRUE,
                   model_type = 'log-logistic',
                   log_transform_dose = FALSE,
                   normalized_response = FALSE,
                   dose_log = c('none', 'log10', 'natural'),
                   normalize_response = FALSE,
                   family = c("continuous", "binomial", "Poisson"),
                   constraints = NULL)
  expect_no_error(do.call(dose_response_analysis, arg_list))
})

test_that("Error for fixed parameters and varying slope", {
  arg_list <- list(drc::ryegrass,
                   response = 'rootl',
                   dose = 'conc',
                   type = c('stimulation'),
                   vary_slope = TRUE,
                   model_type = 'log-logistic',
                   log_transform_dose = FALSE,
                   normalized_response = FALSE,
                   dose_log = c('none', 'log10', 'natural'),
                   normalize_response = FALSE,
                   family = c("continuous", "binomial", "Poisson"),
                   fixed_params = list('b' = 1.2),
                   constraints = NULL)
  expect_error(do.call(dose_response_analysis, arg_list), 'Slope inconsistent with')

})
