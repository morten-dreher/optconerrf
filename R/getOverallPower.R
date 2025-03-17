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

getOverallPower <- function(design, delta) {

  ncp <- delta * sqrt(design$firstStageInformation)

  # Calculate probability to reject at the second stage for given delta
  secondStageRejection <- function(firstStagePValue) {
    (1-pnorm(qnorm(1 - getOptimalConditionalError(firstStagePValue, design = design))- sqrt(getSecondStageInformation(firstStagePValue, design = design)) * delta)) *
    exp(qnorm(1 - firstStagePValue) * ncp - ncp^2 / 2)
  }

  integral <- stats::integrate(f = secondStageRejection, lower = design$alpha1, upper = design$alpha0)$value

  OverallPower <- 1 - pnorm(qnorm(1 - design$alpha1) - ncp) + integral

  return(OverallPower)
}
