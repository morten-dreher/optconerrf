#' Calculate the Optimal Conditional Error
#' @name getOptimalConditionalError
#'
#' @details The optimal conditional error \eqn{\alpha_2} given a first-stage p-value \eqn{p_1} is calculated as:
#' \deqn{\alpha_2(p_1)=\psi(-e^{c_0} \cdot \frac{\Delta_1^2}{l(p_1)}).}
#'
#' The level constant \eqn{c_0} as well as the specification of the effect size \eqn{\Delta_1} and the likelihood ratio \eqn{l(p_1)}
#' must be contained in the \code{design} object (see \code{?getDesignOptimalConditionalErrorFunction}).
#' Early stopping rules are supported, i.e., for \eqn{p_1 \leq \alpha_1}, the returned conditional error is 1 and for \eqn{p_1 > \alpha_0}, the returned conditional error is 0.
#'
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @return Value of the optimal conditional error function.
#' @export
#'
#' @template reference_optimal
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()]
#'
#' @examples
#' # Create a design
#' design <- getDesignOptimalConditionalErrorFunction(
#' alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#' delta1 = 0.5, firstStageInformation = 40, useInterimEstimate = FALSE,
#' likelihoodRatioDistribution = "fixed", deltaLR = 0.5)
#'
#' # Calculate optimal conditional error
#' getOptimalConditionalError(
#' firstStagePValue = c(0.1, 0.2, 0.3), design = design
#' )

getOptimalConditionalError <- function(firstStagePValue, design) {
  # Check if firstStagePValue lies outside early decision boundaries
  if (firstStagePValue <= design$alpha1 && design$alpha1 != 0) {
    return(1)
  } else if (firstStagePValue > design$alpha0) {
    return(0)
  }

  # If monotonisation constants specified and monotonisation enforced, perform non-increasing transformation
  if (
    design$enforceMonotonicity &&
      !is.null(unlist(design$monotonisationConstants))
  ) {
    Q <- getMonotoneFunction(
      x = firstStagePValue,
      fun = getQ,
      design = design
    )
  } else {
    Q <- getQ(firstStagePValue = firstStagePValue, design = design)
  }

  # Check if conditional power function should be used
  if (!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
    conditionalPower <- design$conditionalPowerFunction(firstStagePValue)
  } else {
    conditionalPower <- design$conditionalPower
  }

  #Calculate the constraints
  C_max <- getConstraintC_max(firstStagePValue = firstStagePValue, design = design)
  C_min <- getConstraintC_min(firstStagePValue = firstStagePValue, design = design)

  #Handling of the special case firstStagePValue=0 and no early stopping
  if (firstStagePValue == 0 && design$alpha1 == 0) {
    # Calculate the specified conditional power for a firstStagePValue of 0
    if (!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
      conditionalPower_0 <- design$conditionalPowerFunction(0)
    } else {
      conditionalPower_0 <- design$conditionalPower
    }
    return(min(C_max, conditionalPower_0))
  }

  return(max(
    C_min,
    min(
      C_max,
      getPsi(
        nuPrime = (-exp(design$levelConstant) / Q),
        conditionalPower = conditionalPower,
        design = design,
        firstStagePValue = firstStagePValue
      )
    )
  ))
}

getOptimalConditionalError <- Vectorize(
  FUN = getOptimalConditionalError,
  vectorize.args = c("firstStagePValue")
)
