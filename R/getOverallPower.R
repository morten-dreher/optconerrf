#' Calculate the overall power
#' @name getOverallPower
#'
#' @description Calculate the overall power
#'
#' @template param_design
#' @template param_delta1_overallPower
#'
#' @return The overall power of the design at the provided effect size.
#'

getOverallPower <- function(design, delta1) {

  ncp1 <- delta1 * sqrt(design$firstStageInformation)

  # Calculate probability to reject at the second stage for given delta
  secondStageRejection <- function(firstStagePValue) {
    (1-pnorm(qnorm(1 - getOptimalConditionalError(firstStagePValue, design = design))- sqrt(getSecondStageInformation(firstStagePValue, design = design)) * delta1)) *
    exp(qnorm(1 - firstStagePValue) * ncp1 - ncp1^2 / 2)
  }

  integral <- stats::integrate(f = secondStageRejection, lower = design$alpha1, upper = design$alpha0)$value

  overallPower <- 1 - pnorm(qnorm(1 - design$alpha1) - ncp1) + integral

  return(overallPower)
}
