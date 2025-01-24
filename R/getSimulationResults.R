#' Simulate trials
#'
#' @description
#' Simulate the rejection probability for a given design and alternative.
#'
#'
#' @template param_design
#' @param maxNumberOfIterations Number of trials to be simulated.
#' @param alternative Assumed relative effect size.
#'
#' @return An object of class \code{SimulationResultsOptimalConditionalError}
#'

getSimulationResults <- function(design, maxNumberOfIterations = 10000, alternative, seed = NULL) {

  if(!is.null(seed)) {
    set.seed(seed)
  }

  firstStageRejections <- numeric()
  firstStageFutility <- numeric()
  overallRejections <- numeric()

  for(i in 1:length(alternative)) {

    firstStagePValues <- 1-pnorm(rnorm(n = maxNumberOfIterations, mean = alternative[i]*design$firstStageInformation))

    firstStageRejections <- c(firstStageRejections, sum(firstStagePValues <= design$alpha1)/length(firstStagePValues))
    firstStageFutility <- c(firstStageFutility, sum(firstStagePValues > design$alpha0)/length(firstStagePValues))

    optimalConditionalErrors <- getOptimalConditionalError(
      firstStagePValue = firstStagePValues, design = design)

    secondStagePValues <- 1-pnorm(rnorm(n = maxNumberOfIterations, mean = alternative[i]*design$firstStageInformation))

    overallRejections <- c(overallRejections, sum(secondStagePValues <= optimalConditionalErrors)/length(secondStagePValues))

  }

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
