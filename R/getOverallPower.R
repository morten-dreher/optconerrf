#' Calculate the overall power
#' @name getOverallPower
#'
#' @description Calculate the overall power
#'
#' @template param_design
#' @template param_alternative
#'
#' @return The overall power of the design at the provided effect size.
#' @export

getOverallPower <- function(design, alternative) {

  ncp1 <- alternative * sqrt(design$firstStageInformation)

  # Calculate probability to reject at the second stage for given delta
  secondStageRejection <- function(firstStagePValue) {
    (1-pnorm(qnorm(1 - getOptimalConditionalError(firstStagePValue, design = design))- sqrt(getSecondStageInformation(firstStagePValue, design = design)) * alternative)) *
    exp(qnorm(1 - firstStagePValue) * ncp1 - ncp1^2 / 2)
  }

  integral <- stats::integrate(f = secondStageRejection, lower = design$alpha1, upper = design$alpha0)$value

  overallPower <- 1 - pnorm(qnorm(1 - design$alpha1) - ncp1) + integral

  return(overallPower)
}

getOverallPower <- Vectorize(FUN = getOverallPower, vectorize.args = "alternative")
