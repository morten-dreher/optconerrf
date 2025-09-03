#' Calculate Expected Second-stage Information
#' @name getExpectedSecondStageInformation
#'
#' @description Calculate the expected second-stage information using the optimal conditional error function with specific assumptions.
#'
#' @details {The expected second-stage information is calculated as:
#'  \deqn{\mathbb{E}(I_{2})=\int_{\alpha_1}^{\alpha_0}\frac{\nu(\alpha_2(p_1)) \cdot l(p_1)}{\Delta_1^2} dp_1,}
#'    where
#'    \itemize{
#'        \item \eqn{\alpha_1, \alpha_0} are the first-stage efficacy and futility boundaries
#'        \item \eqn{\alpha_2(p_1)} is the optimal conditional error calculated for \eqn{p_1}
#'        \item \eqn{l(p_1)} is the "true" likelihood ratio under which to calculate the expected sample size. This can be different from the likelihood ratio used to calibrate the optimal conditional error function.
#'        \item \eqn{\Delta_1} is the assumed treatment effect to power for, expressed as a mean difference. It may depend on the interim data (i.e., \eqn{p_1}) in case \code{useInterimEstimate = TRUE} was specified for the design object.
#'        \item \eqn{\nu(\alpha_2(p_1)) = (\Phi^{-1}(1-\alpha_2(p_1))+\Phi^{-1}(CP))^2} is a factor calculated for the specific assumptions about the optimal conditional error function and the target conditional power \eqn{CP}.
#' }}
#'
#' @template param_design
#' @template param_likelihoodRatioDistribution_expected
#' @param ... {Additional parameters required for the specification of \code{likelihoodRatioDistribution}}.
#'
#' @return Expected second-stage information.
#'
#' @examples
#' # Get a design
#' design <- getDesignOptimalConditionalErrorFunction(
#' alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#' delta1 = 0.25, likelihoodRatioDistribution = "fixed", deltaLR = 0.25,
#' firstStageInformation = 80, useInterimEstimate = FALSE,
#' )
#' # Calculate expected information under correct specification
#' getExpectedSecondStageInformation(design)
#'
#' # Calculate expected information under the null hypothesis
#' getExpectedSecondStageInformation(
#'  design = design, likelihoodRatioDistribution = "fixed", deltaLR = 0
#' )
#'
#' @export
#' @seealso [getDesignOptimalConditionalErrorFunction()], [getSecondStageInformation()]
#' @template reference_optimal

getExpectedSecondStageInformation <- function(
  design,
  likelihoodRatioDistribution = NULL,
  ...
) {
  # Integrate over a helper function from alpha1 to alpha0
  return(
    stats::integrate(
      f = integrateExpectedInformation,
      lower = design$alpha1,
      upper = design$alpha0,
      design = design,
      likelihoodRatioDistribution = likelihoodRatioDistribution,
      ... = ...
    )$value
  )
}
