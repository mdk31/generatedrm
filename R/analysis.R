


#' Dose Response Analysis
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
dose_response_analysis <- function(dat, response, dose,
                                   type = c('stimulation', 'inhibition'),
                                   vary_slope = FALSE,
                                   model_type = 'log-logistic',
                                   log_transform_dose = FALSE,
                                   constant = 0.01,
                                   normalized_response = FALSE,
                                   dose_log = c('none', 'log10', 'natural'),
                                   family = c("continuous", "binomial"),
                                   fixed_params = NULL,
                                   constraints = list()){

  form <- paste0(response, ' ~ ', dose)

  model_type <- match.arg(model_type)
  type <- match.arg(type)
  logDose <- match.arg(dose_log)
  family <- match.arg(family)
  log_base_map <- c(log10 = 10, natural = exp(1))

  fn_params <- c("b", "c", "d", "e")

  if(!is.null(fixed_params)){
    # Fixed parameters passed
    assertthat::assert_that(all(!is.null(names(fixed_params))) && all(names(fixed_params) %in% fn_params))
    params <- utils::modifyList(setNames(as.list(rep(NA, length(fn_params))), fn_params), fixed_params)
  } else{
      params <- setNames(as.list(rep(NA, length(fn_params))), fn_params)
  }


  if(vary_slope){
    # Do not fix slope
    assertthat::assert_that(is.na(params[['b']]), msg = "Slope inconsistent with fixed parameter settings")
  } else{
    # DRC paramatrizes the slope differently
    if(is.na(params[['b']])){
      slope_param <- ifelse(type == 'stimulation', -1, +1)
      params[['b']] <- slope_param
    }
  }

  params <- unlist(params)
  nonfix_names <- names(params)[is.na(params)]
  fix_names <- names(params)[!is.na(params)]
  nonfix <- length(nonfix_names)
  assertthat::assert_that(nonfix > 0, msg = "All parameters are fixed!")

  log_base_map <- c(log10 = 10, natural = exp(1))


  # Retrieve the corresponding value
  if (logDose != 'none') {
    assertthat::assert_that(log_transform_dose, msg = "Provided transformation but log_transform_dose set to FALSE")
    logDose <- log_base_map[[dose_log]]
  } else {
    logDose <- NULL
  }
  # Transform dose
  if(log_transform_dose){
    if(is.null(logDose)){
      warning('logDose set to none, but transformation requested, using natural logs')
      base <- exp(1)
    } else{
      base <- logDose
    }
    dat[[dose]] <- log(ifelse(dat[[dose]] == 0, dat[[dose]] + constant, dat[[dose]]), base = base)
    fct <- drc::LL2.4(fixed = unlist(params), names = names(params))
  }

  if(model_type == 'log-logistic'){
    if(log_transform_dose){
      # Use the log-transformed parametrization
      fct <- drc::LL2.4(fixed = unlist(params), names = names(params))
    } else{
      # Use the standard
      fct <- drc::LL.4(fixed = unlist(params), names = names(params))
    }
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

  drm <- drc::drm(formula = as.formula(form), data = dat, logDose = logDose,
                  fct = fct, upperl = upperl, lowerl = lowerl, type = family)
  return(drm)

}

# # Function to plot with confidence intervals
#' Plot Results
#'
#' @param drc_model
#' @param dose_column
#' @param response_column
#'
#' @return
#' @export
#'
#' @examples
plot_drc_with_confidence <- function(drc_model, dose_column, response_column) {
  # Generate a sequence of doses
  data <- drc_model$origData

  doselab <- ifelse(!is.null(drc_model$logDose), 'log(dose)', 'dose')

  dose_seq <- seq(min(data[[dose_column]]), max(data[[dose_column]]), length.out = 100)

  # Create a data frame for the dose sequence
  pred_data <- data.frame(Dose = dose_seq)

  # Predict responses and confidence intervals at these doses
  preds <- suppressWarnings(predict(drc_model, newdata = pred_data, interval = "confidence"))

  # Add predictions and confidence intervals to the data frame
  pred_data$Response <- preds[,1]
  pred_data$Lower <- preds[,2]
  pred_data$Upper <- preds[,3]

  # Create the plot
  ggplot2::ggplot(data, ggplot2::aes_string(x = dose_column, y = response_column)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(data = pred_data, ggplot2::aes(x = Dose, y = Response), inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = pred_data, ggplot2::aes(x = Dose, ymin = Lower, ymax = Upper),
                         alpha = 0.2, fill="blue", inherit.aes = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::xlab(doselab) +
    ggplot2::ggtitle("Dose-Response Curve with Confidence Intervals")
}

# Function to create a kable table from drm model output
#' Create Table
#'
#' @param drm_model
#' @param format
#'
#' @return
#' @export
#'
#' @examples
create_drm_table <- function(drm_model, format = 'markdown') {
  # Extract model coefficients and confidence intervals
  coefs <- coef(summary(drm_model))
  conf_int <- confint(drm_model)

  # Combine information into a single data frame
  results <- data.frame(
    Estimate = coefs[, "Estimate"],
    Std.Error = coefs[, "Std. Error"],
    `t value` = coefs[, "t-value"],
    `Pr(>|t|)` = coefs[, "p-value"],
    `Lower CI` = conf_int[, 1],
    `Upper CI` = conf_int[, 2]
  )

  # Create kable table
  knitr::kable(results, format = format)
}

# Example usage
# drm_model <- drm(response ~ dose, data = your_data, fct = your_model)
# create_drm_table(drm_model)

