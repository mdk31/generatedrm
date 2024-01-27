


#' Title
#'
#' @param dose
#' @param fn
#' @param error_sd
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
generate_continuous_data <- function(dose, fn, error_sd = 1, ...){
  n <- length(dose)
  assertthat::assert_that(length(error_sd) == 1 | length(error_sd) == n)
  # Generate errors
  errors <- stats::rnorm(n, mean = 0, sd = 1)*error_sd
  mean_response <- fn(dose, ...)
  y <- mean_response + errors
  data.frame(dose = dose, response = y)

}

generate_binomial_data <- function(dose, trials, fn, ...){
  n <- length(dose)
  assertthat::assert_that(length(trials) == n)
  probs <- fn(dose, ...)
  y <- stats::rbinom(n, size = trials, prob = probs)
}

generate_count_data <- function(dose, fn, ...){
  n <- length(dose)
  lambda <- fn(dose, ...)
  assertthat::assert_that(length(lambda) == n)
  y <- stats::rpois(n, lambda)

}



#' Title
#'
#' @param x
#' @param bottom
#' @param top
#' @param e
#' @param slope
#'
#' @return
#' @export
#'
#' @examples
log_logistic <- function(x, bottom, top, e, slope){
  bottom + (top - bottom)/(1 + exp(slope*(log(x) - log(e))))
}


