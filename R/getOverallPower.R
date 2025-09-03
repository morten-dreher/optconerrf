#' Calculate the overall power
#' @name getOverallPower
#'
#' @description Calculate the overall power and other operating characteristics of a design.
#'
#' @details
#' This function is used to evaluate the overall performance of a design.
#' The probabilities for first-stage futility, first-stage efficacy and overall efficacy (i.e., overall power) are saved in an object of class \code{PowerResultsOptimalConditionalError}.
#'
#' @template param_design
#' @template param_alternative
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()], [getSimulationResults()]
#'
#' @examples
#' # Get a design
#' design <- getDesignOptimalConditionalErrorFunction(
#' alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#' delta1 = 0.25, likelihoodRatioDistribution = "fixed", deltaLR = 0.25,
#' firstStageInformation = 80, useInterimEstimate = FALSE,
#' )
#'
#' getOverallPower(design, alternative = 0.25)
#'
#' @return The overall power of the design at the provided effect size.
#' @export

getOverallPower <- function(design, alternative) {
  ncp1 <- alternative * sqrt(design$firstStageInformation)

  firstStageFutility <- numeric(length(alternative))
  firstStageEfficacy <- numeric(length(alternative))
  overallPower <- numeric(length(alternative))

  for (i in 1:length(alternative)) {
    # Early decision probabilities
    firstStageFutility[i] <- stats::pnorm(
      stats::qnorm(1 - design$alpha0) - ncp1[i]
    )
    firstStageEfficacy[i] <- 1 -
      stats::pnorm(stats::qnorm(1 - design$alpha1) - ncp1[i])

    # Calculate probability to reject at the second stage for given delta
    secondStageRejection <- function(firstStagePValue) {
      (1 -
        stats::pnorm(
          stats::qnorm(
            1 - getOptimalConditionalError(firstStagePValue, design = design)
          ) -
            sqrt(getSecondStageInformation(firstStagePValue, design = design)) *
              alternative[i]
        )) *
        exp(qnorm(1 - firstStagePValue) * ncp1[i] - ncp1[i]^2 / 2)
    }

    integral <- stats::integrate(
      f = secondStageRejection,
      lower = design$alpha1,
      upper = design$alpha0
    )$value

    overallPower[i] <- firstStageEfficacy[i] + integral
  }

  powerResults <- new(
    "PowerResultsOptimalConditionalError",
    alternative = alternative,
    firstStageFutility = firstStageFutility,
    firstStageEfficacy = firstStageEfficacy,
    overallPower = overallPower
  )

  return(powerResults)
}
