

test_that('Constraints work', {
  arg_list <- list(drc::ryegrass,
                   response = 'rootl',
                   dose = 'conc',
                   type = c('stimulation'),
                   vary_slope = TRUE,
                   model_type = 'log-logistic',
                   log_transform_dose = FALSE,
                   dose_log = c('none', 'log10', 'natural'),
                   fixed_params = list('c' = 1),
                   family = c("continuous"),
                   constraints = list(upper = list('e' = 20),
                                      lower = list('b' = 0.1)))
  expect_no_error(do.call(dose_response_analysis, arg_list))

  arg_list$constraints$upper[['c']] <- 2
  expect_error(do.call(dose_response_analysis, arg_list), "Put restrictions")

  # Test that vary slope produces error with constraints on b
  arg_list$vary_slope <- FALSE
  arg_list$constraints$upper[['b']] <- 1.5
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
                   dose_log = c('none'),
                   family = c("continuous"),
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
                   dose_log = c('none', 'log10', 'natural'),
                   family = c("continuous"),
                   fixed_params = list('b' = 1.2),
                   constraints = NULL)
  expect_error(do.call(dose_response_analysis, arg_list), 'Slope inconsistent with')

})

test_that("Log transformation", {
  arg_list <- list(drc::ryegrass,
                   response = 'rootl',
                   dose = 'conc',
                   type = c('stimulation'),
                   vary_slope = FALSE,
                   model_type = 'log-logistic',
                   log_transform_dose = FALSE,
                   dose_log = c('natural'),
                   family = c("continuous"),
                   constraints = NULL)

  # 4 cases
  expect_error(do.call(dose_response_analysis, arg_list), 'Dose log incompatible')
  arg_list$dose_log <- 'none'
  arg_list$log_transform_dose <- TRUE
  expect_error(do.call(dose_response_analysis, arg_list), 'Dose log incompatible')
  arg_list$dose_log <- 'natural'
  expect_no_error(do.call(dose_response_analysis, arg_list))

})
