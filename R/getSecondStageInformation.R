#' Calculate the Second-stage Information
#' @name getSecondStageInformation
#'
#' @description Calculate second-stage information for given first-stage p-value and design.
#'
#' @details The second-stage information \eqn{I_{2}} is calculated given a first-stage p-value \eqn{p_1} as:
#' \deqn{I_{2}(p_1) = \frac{(\Phi^{-1}(1-\alpha_2(p_1)) + \Phi^{-1}(CP))^2}{\Delta_1^2} = \frac{\nu(\alpha_2(p_1))}{\Delta_1^2},}
#' where
#' \itemize{
#'    \item \eqn{\alpha_2(p_1)} is the conditional error function
#'    \item \eqn{CP} is the target conditional power
#'    \item \eqn{\Delta_1} is the assumed treatment effect (expressed as a mean difference).
#' }
#' The conditional error is calculated according to the specification provided in the \code{design} argument.
#' For p-values smaller or equal to the first-stage efficacy boundary as well as p-values greater than the first-stage futility boundary,
#' the returned information is 0 (since the trial is ended early in both cases).
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @return The second-stage information.
#' @export
#'
#' @examples
#' \dontrun{
#' design <- getDesignOptimalConditionalErrorFunction(
#'   alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5,
#'   conditionalPower = 0.9, delta1 = 0.25, useInterimEstimate = FALSE,
#'   firstStageInformation = 40, likelihoodRatioDistribution = "maxlr"
#' )
#'
#' getSecondStageInformation(
#'   firstStagePValue = c(0.05, 0.1, 0.2), design = design
#' )
#' }
#'
#' @template reference_optimal
getSecondStageInformation <- function(firstStagePValue, design) {
  # For p-values outside of the continuation region, return information 0
  if ((firstStagePValue <= design$alpha1 && design$alpha1 > 0) || firstStagePValue > design$alpha0) {
    information <- 0
  } else {
    # For design with interim estimate, apply effect restrictions
    if (design$useInterimEstimate) {
      effect <- min(
        max(qnorm(1 - firstStagePValue) / design$firstStageInformation, design$delta1Min),
        design$delta1Max
      )
    }
    # For design without interim estimate, use fixed effect
    else {
      effect <- design$delta1
    }

    # Calculate conditional error
    conditionalError <- getOptimalConditionalError(
      firstStagePValue = firstStagePValue, design = design
    )

    # Check if conditional power function should be used
    if(!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
      conditionalPower <- design$conditionalPowerFunction(firstStagePValue)
    }
    else {
      conditionalPower <- design$conditionalPower
    }

    information <- (getNu(alpha = conditionalError, conditionalPower = conditionalPower)) / (effect^2)
  }
  return(information)
}

getSecondStageInformation <- Vectorize(
  FUN = getSecondStageInformation,
  vectorize.args = c("firstStagePValue")
)
