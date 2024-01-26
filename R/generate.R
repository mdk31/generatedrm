


generate_continuous_data <- function(dose, fn, error_sd = 1, ...){
  n <- length(dose)
  assertthat::assert_that(length(error_sd) == 1 | length(error_sd) == n)
  # Generate errors
  errors <- stats::rnorm(n, mean = 0, sd = 1)*error_sd
  mean_response <- fn(dose, ...)
  y <- mean_response + errors
  return(y)

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


log_logistic <- function(x, b, c, d, e){
  c + (d - c)/(1 + exp(b*(log(x) - log(e))))
}


