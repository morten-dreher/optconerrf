#' Simulate trials
#'
#' @description
#' Simulate the rejection probability for a given design and alternative.
#'
#' @details
#' Simulates the probabilities of overall rejection as well as early futility and early efficacy for the provided scenario and design.
#' This is done by generating random normally distributed test statistics and calculating their p-values.
#'
#' @template param_design
#' @param maxNumberOfIterations Number of trials to be simulated.
#' @template param_alternative
#' @param seed An optional seed for reproducibility.
#'
#' @return An object of class \code{SimulationResultsOptimalConditionalError} containing the simulation results.
#'
#' @importFrom methods new
#' @importFrom stats pnorm
#' @importFrom stats rnorm
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()], [getOverallPower()]
#'
#' @export
#' @examples
#' \dontrun{
#' design <- getDesignOptimalConditionalErrorFunction(
#'  alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, delta1 = 0.25,
#'  useInterimEstimate = FALSE,
#'  conditionalPower = 0.9, likelihoodRatioDistribution = "maxlr",
#'  firstStageInformation = 10
#' )
#'
#' # Simulate under the null hypothesis and for a mean difference of 0.5
#' getSimulationResults(
#'  design = design, alternative = c(0, 0.5)
#' )
#'
#' }

getSimulationResults <- function(design, maxNumberOfIterations = 10000, alternative, seed = NULL) {

  # If seed provided, use it
  if(!is.null(seed)) {
    set.seed(seed)
  }

  # Create empty vectors
  firstStageRejections <- numeric()
  firstStageFutility <- numeric()
  overallRejections <- numeric()

  # For each item of the alternative, perform simulations
  for(i in 1:length(alternative)) {

    # Generate first-stage p-values with desired effect size
    firstStagePValues <- 1-stats::pnorm(stats::rnorm(n = maxNumberOfIterations, mean = alternative[i]*sqrt(design$firstStageInformation)))

    # Proportion of early rejections
    firstStageRejections <- c(firstStageRejections, sum(firstStagePValues <= design$alpha1)/length(firstStagePValues))
    # Proportion of early futility stops
    firstStageFutility <- c(firstStageFutility, sum(firstStagePValues > design$alpha0)/length(firstStagePValues))

    optimalConditionalErrors <- getOptimalConditionalError(
      firstStagePValue = firstStagePValues, design = design)

    # Generate second-stage p-values with desired effect size
    secondStagePValues <- 1-stats::pnorm(stats::rnorm(n = maxNumberOfIterations,
                                                      mean = alternative[i]*sqrt(getSecondStageInformation(
                                                        firstStagePValue = firstStagePValues, design = design))))

    # Proportion of overall rejections
    overallRejections <- c(overallRejections, sum(secondStagePValues <= optimalConditionalErrors)/length(secondStagePValues))

  }

  # Create new object
  simulationResults <- new(
    "SimulationResultsOptimalConditionalError",
    alternative = alternative,
    firstStageFutility = firstStageFutility,
    firstStageEfficacy = firstStageRejections,
    overallPower = overallRejections,
    maxNumberOfIterations = maxNumberOfIterations
  )

  return(simulationResults)
}
