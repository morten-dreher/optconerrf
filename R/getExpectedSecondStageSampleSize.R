#' Calculate Expected Second-stage Sample Size
#' @name getExpectedSecondStageSampleSize
#'
#' @description Calculate the expected second-stage sample size using the optimal conditional error function with specific assumptions.
#'
#' @details {The expected second-stage sample size is calculated as:
#'  \deqn{\mathbb{E}(n_{2,t})=\int_{\alpha_1}^{\alpha_0}\frac{\sigma^2 \cdot (r+1) \cdot \nu(\alpha_2(p_1)) \cdot l(p_1)}{\Delta_1^2} dp_1,}
#'    where
#'    \itemize{
#'        \item \eqn{\alpha_1, \alpha_0} are the efficacy and futility boundaries
#'        \item \eqn{\sigma} is the assumed (shared) standard deviation of the treatment effect
#'        \item \eqn{r} is the allocation ratio between treatment and control group (\eqn{r=n_t/n_c})
#'        \item \eqn{\alpha_2(p_1)} is the optimal conditional error calculated for \eqn{p_1}
#'        \item \eqn{l(p_1)} is the "true" likelihood ratio under which to calculate the expected sample size. This can be different from the likelihood ratio used to calibrate the optimal conitional error function.
#'        \item \eqn{\Delta_1} is the assumed treatment effect to power for, expressed as a mean difference
#'        \item \eqn{\nu(\alpha_2(p_1)) = (\Phi^{-1}(1-\alpha_2(p_1))+\Phi^{-1}(CP))^2} is a factor calculated for the specific assumptions about the optimal conditional error function and the target conditional power \eqn{CP}.
#' }
#' To use a truncated interim estimate of the treatment effect instead of a fixed effect \eqn{\Delta_1}, the minimum value \eqn{\Delta_0} can be specified via \code{delta0}.
#' When this is done, an additional specification of the first-stage sample size via \code{firstStageSampleSize} (and possibly, \code{standardDeviation} and \code{allocationRatio}) is necessary, to enable rescaling between non-centrality parameters and mean differences. \cr
#' In case the argument \code{monotonisationConstants} is calculated beforehand using the function \code{getMonotonisationConstants()}, it can be provided to \code{getExpectedSecondStageSampleSize()}
#' to calculate the expected second-stage sample size using the monotone (non-increasing) transformation of the optimal conditional error function.
#' If the argument is omitted, the expected sample size of the second stage is calculated using a potentially non-monotone optimal conditional error function.}
#'
#' @template param_design
#' @template param_distDelta
#' @template param_allocationRatio
#' @template param_standardDeviation
#' @param ... {Additional parameters required for \code{getOptimalConditionalError()} and the specification of \code{distDelta}. \cr
#' Must also include an effect specification to calculate the second-stage sample size, given as:
#' \itemize{
#' \item \code{delta1} for a fixed effect or
#' \item \code{delta0} for the lower cut-off of an interim estimate
#' }}
#'
#' @return Expected second-stage sample size of the treatment group.
#'
#' @examples
#' # Get a design
#' design <- getDesignOptimalConditionalErrorFunction(
#' alpha=0.025, alpha1=0.001, alpha0=0.5, conditionalPower=0.9,
#' ncp1=1, dist="fixed", ncpLR=1)
#' # Calculate expected sample size under correct specification
#' getExpectedSecondStageSampleSize(
#' design, distDelta="fixed", ncpLRDelta=1, delta1=0.25)
#'
#'
#' @export
#'
#' @references Brannath, W., & Bauer, P. (2004). Optimal conditional error functions for the control of conditional power. Biometrics, 60 (3), 715–723. https://doi.org/10.1111/j.0006-341X.2004.00221.x

getExpectedSecondStageSampleSize <- function(design, distDelta, allocationRatio = 1, standardDeviation = 1, ...) {


  # Integrate over a helper function from alpha1 to alpha0
  return(stats::integrate(f = integrateExpectedSampleSize, lower = design$alpha1, upper = design$alpha0, dist = design$dist, levelConstant = design$levelConstant,
                          conditionalPower = design$conditionalPower, alpha0 = design$alpha0, alpha1 = design$alpha1, distDelta = distDelta,
                          monotonisationConstants = design$monotonisationConstants, design = design, ... = ...)$value
         * (allocationRatio + 1) * standardDeviation^2)
}
