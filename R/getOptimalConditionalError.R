#' Calculate the Optimal Conditional Error
#' @name getOptimalConditionalError
#'
#' @details The optimal conditional error \eqn{\alpha_2} given a first-stage p-value \eqn{p_1} is calculated as:
#' \deqn{\alpha_2(p_1)=\psi(-e^{c_0} \cdot \frac{\vartheta_1^2}{l(p_1)}).}
#'
#' The level constant \eqn{c_0} as well as the specification of the effect size \eqn{\vartheta_1} and the likelihood ratio \eqn{l(p_1)}
#' must be contained in the \code{design} object (see \code{?getDesignOptimalConditionalErrorFunction}).
#' For \eqn{p_1 \leq \alpha_1}, the returned conditional error is 1 and for \eqn{p_1 > \alpha_0}, the returned conditional error is 0.
#'
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @return Value of the optimal conditional error function.
#' @export
#'
#' @references Brannath, W. & Bauer, P. (2004). Optimal conditional error functions for the control of conditional power. Biometrics, 60 (3), 715–723. https://doi.org/10.1111/j.0006-341X.2004.00221.x
#'
#' @examples
#' # Create a design
#' design <- getDesignOptimalConditionalErrorFunction(
#' alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#' delta1 = 1, firstStageInformation = 1, useInterimEstimate = FALSE,
#' likelihoodRatioDistribution = "fixed", deltaLR = 1)
#'
#' # Calculate optimal conditional error
#' getOptimalConditionalError(
#' firstStagePValue = c(0.1, 0.2, 0.3), design = design
#' )
#'

getOptimalConditionalError <- function(firstStagePValue, design) {

  # Check if firstStagePValue lies outside early decision boundaries
  if(firstStagePValue <= design$alpha1) {
    return(1)
  }
  else if(firstStagePValue > design$alpha0) {
    return(0)
  }

  # If monotonisation constants specified and monotonisation enforced, perform non-increasing transformation
  if(design$enforceMonotonicity && !is.null(unlist(design$monotonisationConstants))) {
    Q <- getMonotoneFunction(
      x = firstStagePValue, fun = getQ, design = design,
      printConstant = FALSE)
  }
  else {
    Q <- getQ(firstStagePValue = firstStagePValue, design = design)
  }

  return(max(design$minimumConditionalError, min(design$maximumConditionalError, getPsi(nuPrime = (-exp(design$levelConstant)/Q), conditionalPower = design$conditionalPower))))

}

getOptimalConditionalError <- Vectorize(FUN = getOptimalConditionalError, vectorize.args = c("firstStagePValue"))
