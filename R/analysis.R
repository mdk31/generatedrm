#'Dose Response Analysis
#' This function performs a dose-response analysis using various model types and options.
#' It allows for handling both stimulation and inhibition type responses with options
#' for fixing parameters, log-transforming doses, and applying constraints on model parameters.
#'
#' @param dat A data frame containing the data to be analyzed.
#' @param response A string naming the column in 'dat' that contains the response variable.
#' @param dose A string naming the column in 'dat' that contains the dose variable.
#' @param type Character string indicating the type of response; either 'stimulation' or 'inhibition'.
#'             Default is 'stimulation'. Only used if slope is not specified.
#' @param vary_slope Logical, if TRUE, the slope of the response curve is not fixed and is estimated from the data.
#'                   Default is FALSE.
#' @param model_type Character string specifying the model type to be used for analysis.
#'                   Default is 'log-logistic'.
#' @param log_transform_dose Logical, set to TRUE if doses are log-transformed before entering function.
#'                           Default is FALSE.
#' @param dose_log Character string indicating the type of logarithm that has been applied to dose values.
#'                 Options are 'none', 'log10', 'natural'. Default is 'none'.
#' @param family Character string indicating the type of response variable.
#'               Options are 'continuous' for continuous responses and 'binomial' for binomial responses.
#'               Default is 'continuous'.
#' @param fixed_params A named list of parameters to be kept fixed during the analysis.
#'                     Default is NULL.
#' @param constraints A list containing 'upper' and 'lower' named lists to set constraints on parameters.
#'                    Default is an empty list.
#'
#' @return An object of class 'drm' (dose-response model) as returned by drc::drm().
#'
#' @export
#'
#' @examples
#' # Example data frame
#' data_frame <- data.frame(dose = 1:10, response = rnorm(10))
#' # Example usage
#' result <- dose_response_analysis(dat = data_frame, response = "response", dose = "dose")
#' print(result)
dose_response_analysis <- function(dat, response, dose,
                                   type = c('stimulation', 'inhibition'),
                                   vary_slope = FALSE,
                                   model_type = 'log-logistic',
                                   log_transform_dose = FALSE,
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

  fn_keys <- list('log-logistic' = c("b", "c", "d", "e"))
  fn_params <- fn_keys[[model_type]]

  if(!is.null(fixed_params)){
    # Fixed parameters passed
    assertthat::assert_that(all(!is.null(names(fixed_params))) && all(names(fixed_params) %in% fn_params))
    params <- utils::modifyList(stats::setNames(as.list(rep(NA, length(fn_params))), fn_params), fixed_params)
  } else{
      params <- stats::setNames(as.list(rep(NA, length(fn_params))), fn_params)
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
      upperl <- utils::modifyList(stats::setNames(as.list(rep(Inf, length(nonfix_names))), nonfix_names), constraints$upper)
      upperl <- unlist(upperl)
    } else{
      upperl <- NULL
    }
    if(!is.null(constraints$lower)){
      assertthat::assert_that(is.list(constraints$lower), msg = 'Lower limits must be named list')
      assertthat::assert_that(!any(is.null(names(constraints$lower))) && all(names(constraints$lower) %in% fn_params),
                              msg = 'Lower constraints must be named and match parameter names')      # Check that constraints are not on fixed parameters
      assertthat::assert_that(!any(names(constraints$lower) %in% fix_names), msg = 'Put restrictions on parameters but fixed them as well')
      lowerl <- utils::modifyList(stats::setNames(as.list(rep(-Inf, length(nonfix_names))), nonfix_names), constraints$lower)
      lowerl <- unlist(lowerl)
    } else{
      lowerl <- NULL
    }
  } else{
    upperl <- lowerl <- NULL
  }

  drm <- drc::drm(formula = stats::as.formula(form), data = dat, logDose = logDose,
                  fct = fct, upperl = upperl, lowerl = lowerl, type = family)
  return(drm)

}

#' Plot Dose-Response Curve with Confidence Intervals
#'
#' This function plots a dose-response curve using a model object from the 'drc' package.
#' It includes confidence intervals to visually assess the uncertainty in the model predictions.
#'
#' @param drc_model An object of class 'drc', typically the result of fitting a dose-response model
#'                  using the 'drm' function from the 'drc' package.
#' @param dose_column The name of the column in the original data used for doses.
#' @param response_column The name of the column in the original data used for responses.
#'
#' @return A ggplot object representing the dose-response curve with confidence intervals.
#'         The plot includes the original data points, the fitted dose-response curve,
#'         and a shaded area representing the confidence intervals.
#'
#' @export
#'
#' @examples
#' data_frame <- data.frame(dose = 1:10, response = rnorm(10))
#' result <- dose_response_analysis(dat = data_frame, response = "response", dose = "dose")
#' plot_drc_with_confidence(result, "dose", "response")
plot_drc_with_confidence <- function(drc_model, dose_column, response_column) {
  # Generate a sequence of doses
  data <- drc_model$origData

  doselab <- ifelse(!is.null(drc_model$logDose), 'log(dose)', 'dose')

  dose_seq <- seq(min(data[[dose_column]]), max(data[[dose_column]]), length.out = 100)

  # Create a data frame for the dose sequence
  pred_data <- data.frame(Dose = dose_seq)

  # Predict responses and confidence intervals at these doses
  preds <- suppressWarnings(stats::predict(drc_model, newdata = pred_data, interval = "confidence"))

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

#' Create a Summary Table from a Dose-Response Model
#'
#' This function generates a summary table from a dose-response model object.
#' The table includes estimates, standard errors, t-values, p-values, and confidence intervals
#' for the model parameters. The table can be formatted in various styles using the 'knitr' package.
#'
#' @param drm_model An object of class 'drm', typically the result of fitting a dose-response model
#'                  using the 'drm' function from the 'drc' package.
#' @param format A character string specifying the output format of the table.
#'               Possible values include 'markdown', 'html', 'latex', etc.
#'               Default is 'markdown'.
#'
#' @return A character string containing the formatted table, which can be displayed in R Markdown
#'         documents or other formats supported by the 'knitr' package.
#'
#' @export
#'
#' @examples
#' data_frame <- data.frame(dose = 1:10, response = rnorm(10))
#' result <- dose_response_analysis(dat = data_frame, response = "response", dose = "dose")
#' create_drm_table(result, format = 'markdown')
create_drm_table <- function(drm_model, format = 'markdown') {
  # Extract model coefficients and confidence intervals
  coefs <- stats::coef(summary(drm_model))
  conf_int <- stats::confint(drm_model)

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

