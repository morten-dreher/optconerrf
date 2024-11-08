#' Internal function to get second-stage level for inverse normal test
#'
#' @name findAlpha2
#'
#' @description Helper function to identify second-stage significance level for the inverse normal test
#'
#' @template param_alpha2_iterate
#' @template param_alpha
#' @template param_alpha1
#' @template param_alpha0
#' @template param_weights_stages
#'
#' @importFrom stats qnorm
#'
#' @return Distance of the current value of alpha2 to the value that solves the level condition.
#' @keywords internal

findAlpha2 <- function(alpha2, alpha, alpha1, alpha0, weights) {
  correlationMatrix <- matrix(data = c(1, weights[1], weights[1], 1), nrow = 2)
  return(-mvtnorm::pmvnorm(upper = c(qnorm(1-alpha2), stats::qnorm(1-alpha1)), corr = correlationMatrix) +
           mvtnorm::pmvnorm(upper = c(qnorm(1-alpha2), stats::qnorm(1-alpha0)), corr = correlationMatrix) + alpha0 - alpha)
}
