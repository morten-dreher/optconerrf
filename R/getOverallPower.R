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

  if (!is.na(design$conditionalPower)) {
    power <- 1 - pnorm(qnorm(1 - design$alpha1) - ncp1) + design$conditionalPower * (pnorm(qnorm(1 - design$alpha1) - ncp1) - pnorm(qnorm(1 - design$alpha0) - ncp1))
  } else {
    secondStageRejection <- function(firstStagePValue) {
      design$conditionalPowerFunction(firstStagePValue) * exp(qnorm(1 - firstStagePValue) * ncp1 - ncp1^2 / 2)
    }
    integral <- stats::integrate(f = secondStageRejection, lower = design$alpha1, upper = design$alpha0)$value
    power <- 1 - pnorm(qnorm(1 - design$alpha1) - ncp1) + integral
  }

  return(power)
}
