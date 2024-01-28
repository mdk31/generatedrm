


#' Generate Data for Dose Response
#'
#' These functions generate continuous (normally-distributed), binomial, and count (Poisson) data.
#'
#' @param dose A numeric vector of doses. The number of observations is the length of this vector
#' @param fn A function for the mean response
#' @param error_sd Standard deviation of the error
#' @param ... Additional arguments passed to `fn` function
#'
#' @return A data frame with two columns: `dose` and `response`, where `response` is the
#'         calculated response for each dose, including the added error.
#' @export
#'
#' @examples
#' my_fn <- function(dose) { dose^2 } # example dose-response function
#' data <- generate_continuous_data(1:10, my_fn, error_sd = 0.5)
#' print(data)
#' @rdname generate
generate_continuous_data <- function(dose, fn, error_sd = 1, ...){
  n <- length(dose)
  assertthat::assert_that(length(error_sd) == 1 | length(error_sd) == n)
  # Generate errors
  errors <- stats::rnorm(n, mean = 0, sd = 1)*error_sd
  mean_response <- fn(dose, ...)
  y <- mean_response + errors
  data.frame(dose = dose, response = y)

}

#' @rdname generate
#' @param trials The number of trials
generate_binomial_data <- function(dose, trials, fn, ...){
  n <- length(dose)
  assertthat::assert_that(length(trials) == n)
  probs <- fn(dose, ...)
  y <- stats::rbinom(n, size = trials, prob = probs)
}

#' @rdname generate
generate_count_data <- function(dose, fn, ...){
  n <- length(dose)
  lambda <- fn(dose, ...)
  assertthat::assert_that(length(lambda) == n)
  y <- stats::rpois(n, lambda)

}



#' Log-Logistic Function
#'
#' Main model used to generate mean responses. Most commonly used in fitting dose-respones relationships
#'
#' @param x A numeric vector of doses
#' @param c The lower limit
#' @param b The Hill slope
#' @param d The upper limit
#' @param e The EC/IC50, the response halfway between the upper and lower limit
#'
#' @return A numeric vector of mean responses
#' @export
log_logistic <- function(x, c, d, e, b){
  c + (d - c)/(1 + exp(b*(log(x) - log(e))))
}


