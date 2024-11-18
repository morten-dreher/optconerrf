#' Calculate the Optimal Conditional Error
#' @name getOptimalConditionalError
#'
#' @details The optimal conditional error \eqn{\alpha_2} given a first-stage p-value \eqn{p_1} is calculated as:
#' \deqn{\alpha_2(p_1)=\psi(-e^{c_0} \cdot \frac{\vartheta_1^2}{l(p_1)}).}
#' The level constant \eqn{c_0} ensures that the optimal conditional error function meets level condition to maintain the overall type I error rate and can be found using \code{getLevelConstant()}. Specifying it via the argument \code{constant} accelerates calculations. \cr
#' By \eqn{\vartheta_1}, an assumed effect size is specified. A fixed effect choice (specified via \code{ncp1}) will be absorbed in the level constant. When using an interim estimate, a lower cut-off \eqn{\vartheta_0} (on the non-centrality parameter scale) must be specified via the argument \code{ncp0}. \cr
#' Several distributional assumptions can be used for the likelihood ratio \eqn{l(p_1)}, for details see the documentation of \code{getLikelihoodRatio()}. \cr
#' The function \eqn{\psi} is the inverse of \eqn{\nu'(\alpha) = -2 \cdot(\Phi^{-1}(1-\alpha) + \Phi^{-1}(1-CP)) / \phi(\Phi^{-1}(1-\alpha))}.
#' The function \eqn{\nu'} can only be inverted if the conditional power \eqn{CP} is restricted to values \eqn{1-\Phi(2) \leq CP \leq \Phi(2)}. \cr
#' The argument \code{monotonisationConstants} can be calculated beforehand using the function \code{getMonotonisationConstants()} and provided to \code{getOptimalConditionalError()}
#' to calculate the monotone (non-increasing) optimal conditional error function. If this argument is omitted, the resulting optimal conditional error function may not be non-increasing.
#' Note that the argument \code{constant} must be calibrated accordingly, i.e., with or without monotone transformation, by \code{getLevelConstant()}.
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
#' alpha=0.025, alpha1=0.001, alpha0=0.5, conditionalPower=0.9,
#' delta1=1, firstStageInformation=1,
#' likelihoodRatioDistribution="fixed", deltaLR=1)
#'
#' # Calculate optimal conditional error
#' getOptimalConditionalError(
#' firstStagePValue=c(0.1,0.2,0.3), design=design
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

  # Identify effect size for conditional power
  # Fixed effect

  if(!is.null(design$ncp1)) {
    effect <- design$ncp1
  }
  else if(!is.null(design$ncp0)) {
    effect <- design$ncp0

    effect <- stats::qnorm(1-firstStagePValue)

    if(effect < design$ncp0) {
      effect <- design$ncp0
    }
  }
  else {
    stop("Effect must be specified in design object either via a fixed ncp1 or a minimum ncp0")
  }

  # Calculate Q

  # If monotonisation constants specified, perform non-increasing transformation
  if(!is.null(design$monotonisationConstants)) {
    Q <- getMonotoneFunction(
      x = firstStagePValue, fun = getQ, design = design,
      printConstant = FALSE)
  }
  else {
    Q <- getQ(firstStagePValue = firstStagePValue, design = design)
  }

  return(min(max(getPsi(nuPrime = (-exp(design$levelConstant)/Q), conditionalPower = design$conditionalPower), design$minimumConditionalError), design$maximumConditionalError))
}

getOptimalConditionalError <- Vectorize(FUN = getOptimalConditionalError, vectorize.args = c("firstStagePValue"))
