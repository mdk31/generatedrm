


#' Title
#'
#' @param dat
#' @param response
#' @param dose
#' @param type
#' @param vary_slope
#' @param model_type
#' @param log_transform_dose
#' @param normalized_response
#' @param dose_log
#' @param normalize_response
#' @param family
#' @param fixed_params
#' @param constraints
#'
#' @return
#' @export
#'
#' @examples
dose_response_analysis <- function(dat, response, dose, type = c('stimulation', 'inhibition'),
                                   vary_slope = FALSE,
                                   model_type = 'log-logistic',
                                   log_transform_dose = FALSE,
                                   normalized_response = FALSE,
                                   dose_log = c('none', 'log10', 'natural'),
                                   normalize_response = FALSE,
                                   family = c("continuous", "binomial", "Poisson"),
                                   fixed_params = NULL,
                                   constraints = list()){

  model_type <- match.arg(model_type)
  type <- match.arg(type)
  logDose <- match.arg(dose_log)
  log_base_map <- c(log10 = 10, natural = exp(1))

  fn_params <- c("b", "c", "d", "e")

  if(!is.null(fixed_params)){
    # Fixed parameters passed
    assertthat::assert_that(all(!is.null(names(fixed_params)) && names(fixed_params) %in% fn_params))
    params <- utils::modifyList(setNames(as.list(rep(NA, length(fn_params))), fn_params), fixed_params)
  } else{
      params <- setNames(as.list(rep(NA, length(fn_params))), fn_params)
  }

  params <- unlist(params)
  nonfix_names <- names(params)[is.na(params)]
  fix_names <- names(params)[!is.na(params)]
  nonfix <- length(nonfix_names)
  assertthat::assert_that(nonfix > 0, msg = "All parameters are fixed!")

  if(vary_slope){
    # Do not fix slope
    assertthat::assert_that(is.na(params[['b']]), msg = "Slope inconsistent with fixed parameter settings")
    # Set slope paramaeter based on type of model
  } else{
    slope_param <- ifelse(type == 'stimulation', 1, -1)
    assertthat::assert_that(is.na(params[['b']]) || params[['b']] == slope_param, msg = "You fixed the slope but to a value inconsistent with DR type")
  }

  log_base_map <- c(log10 = 10, natural = exp(1))
  # Transform dose
  if(log_transform_dose && model_type == 'log-logistic'){
    fct <- drc::LL2.4(fixed = unlist(params), names = names(params))
  } else{
    fct <- drc::LL.4(fixed = unlist(params), names = names(params))
  }

  # Retrieve the corresponding value
  if (logDose != 'none') {
    logDose <- log_base_map[[dose_log]]
  } else {
    logDose <- NULL  # or some default behavior for 'none'
  }

  if(length(constraints) > 0){
    # Get non-fixed parameters
    if(!is.null(constraints$upper)){
      assertthat::assert_that(is.list(constraints$upper), msg = 'Upper limits must be named list')
      assertthat::assert_that(!any(is.null(names(constraints$upper))) && all(names(constraints$upper) %in% fn_params),
                              msg = 'Upper constraints must be named and match parameter names')
      # Check that constraints are not on fixed parameters
      assertthat::assert_that(!any(names(constraints$upper) %in% fix_names), msg = 'Put restrictions on parameters but fixed them as well')
      upperl <- utils::modifyList(setNames(as.list(rep(Inf, length(nonfix_names))), nonfix_names), constraints$upper)
      upperl <- unlist(upperl)
    } else{
      upperl <- NULL
    }
    if(!is.null(constraints$lower)){
      assertthat::assert_that(is.list(constraints$lower), msg = 'Lower limits must be named list')
      assertthat::assert_that(!any(is.null(names(constraints$lower))) && all(names(constraints$lower) %in% fn_params),
                              msg = 'Lower constraints must be named and match parameter names')      # Check that constraints are not on fixed parameters
      assertthat::assert_that(!any(names(constraints$lower) %in% fix_names), msg = 'Put restrictions on parameters but fixed them as well')
      lowerl <- utils::modifyList(setNames(as.list(rep(-Inf, length(nonfix_names))), nonfix_names), constraints$lower)
      lowerl <- unlist(lowerl)
    } else{
      lowerl <- NULL
    }
  } else{
    upperl <- lowerl <- NULL
  }


  drm <- drc::drm(dat, logDose = logDose, fct = fct, upperl = upperl, lowerl = lowerl)
  return(drm)

}

# # Function to plot with confidence intervals
# plot_drc_with_confidence <- function(drc_model, data, dose_column, response_column) {
#   # Generate a sequence of doses
#   dose_seq <- seq(min(data[[dose_column]]), max(data[[dose_column]]), length.out = 100)
#
#   # Create a data frame for the dose sequence
#   pred_data <- data.frame(Dose = dose_seq)
#
#   # Predict responses and confidence intervals at these doses
#   preds <- predict(drc_model, newdata = pred_data, interval = "confidence")
#
#   # Add predictions and confidence intervals to the data frame
#   pred_data$Response <- preds[,1]
#   pred_data$Lower <- preds[,2]
#   pred_data$Upper <- preds[,3]
#   browser()
#
#   # Create the plot
#   ggplot(data, aes_string(x = dose_column, y = response_column)) +
#     geom_point() +
#     geom_line(data = pred_data, aes(x = Dose, y = Response)) +
#     geom_ribbon(data = pred_data, aes(x = Dose, ymin = Lower, ymax = Upper), alpha = 0.2, fill="blue") +
#     theme_minimal() +
#     ggtitle("Dose-Response Curve with Confidence Intervals")
# }
#
#
# plot_drc_with_confidence(ryegrass.m0, ryegrass, 'conc', 'rootl')
