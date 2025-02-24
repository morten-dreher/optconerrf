#' Calculate the Optimal Conditional Error
#' @name getOptimalConditionalError
#'
#' @details The optimal conditional error \eqn{\alpha_2} given a first-stage p-value \eqn{p_1} is calculated as:
#' \deqn{\alpha_2(p_1)=\psi(-e^{c_0} \cdot \frac{\vartheta_1^2}{l(p_1)}).}
#'
#' The level constant \eqn{c_0} as well as the specification of the effect size \eqn{\vartheta_1=\Delta_1\cdot \sqrt{I_1}} and the likelihood ratio \eqn{l(p_1)}
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
#' @references Brannath, W. & Bauer, P. (2004). Optimal conditional error functions for the control of conditional power. Biometrics, 60 (3), 715–723. https://doi.org/10.1111/j.0006-341X.2004.00221.x
#'
#' @examples
#' \dontrun{
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
#' }

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
      x = firstStagePValue, fun = getQ, design = design)
  }
  else {
    Q <- getQ(firstStagePValue = firstStagePValue, design = design)
  }

  #Check if interim estimate is used
  if(design$useInterimEstimate) {
    delta1 <- min(max(qnorm(1-firstStagePValue)/sqrt(design$firstStageInformation), design$delta1Min), design$delta1Max)
  }
  # Otherwise use fixed effect
  else {
    delta1 <- design$delta1
  }

  # Check if conditional power function should be used
  if(!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
    conditionalPower <- design$conditionalPowerFunction(firstStagePValue)
  }
  else {
    conditionalPower <- design$conditionalPower
  }

  C_max <- design$maximumConditionalError
  C_min <- design$minimumConditionalError

  #If minimumSecondStageInformation is given, use this instead of maximumConditionalError
  if(design$minimumSecondStageInformation > 0){
    C_max <- 1 - pnorm(delta1* sqrt(design$minimumSecondStageInformation)-qnorm(conditionalPower))
  }

  #If maximumSecondStageInformation is given, use this instead of minimumConditionalError
  if(design$maximumSecondStageInformation < Inf){
    C_min <- 1 - pnorm(delta1* sqrt(design$maximumSecondStageInformation)-qnorm(conditionalPower))
  }

  return(max(C_min, min(C_max, getPsi(nuPrime = (-exp(design$levelConstant)/Q), conditionalPower = conditionalPower))))

}

getOptimalConditionalError <- Vectorize(FUN = getOptimalConditionalError, vectorize.args = c("firstStagePValue"))
