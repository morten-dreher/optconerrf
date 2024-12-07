#' Calculate the Second-stage Information
#' @name getSecondStageInformation
#'
#' @description Calculate second-stage information for given alpha, conditional power and effect size
#'
#' @details The second-stage information \eqn{I_{2}} is calculated given a first-stage p-value \eqn{p_1} as:
#' \deqn{I_{2}(p_1) = \frac{(\Phi^{-1}(1-\alpha_2(p_1)) + \Phi^{-1}(CP))^2}{\Delta_1^2} = \frac{\nu(\alpha_2(p_1))}{\Delta_1^2},}
#' where
#' \itemize{
#'    \item \eqn{\alpha_2(p_1)} is the second-stage significance level (e.g., a conditional error)
#'    \item \eqn{CP} is the target conditional power
#'    \item \eqn{\Delta_1} is the assumed treatment effect (expressed as a mean difference).
#' }
#'
#' @param conditionalError Conditional error rate of the design.
#' @template param_conditionalPower
#' @param delta1 The effect size to power for.
#'
#' @return The second-stage information.
#' @export
#'
#' @examples
#' getSecondStageInformation(
#' conditionalError = 0.05, conditionalPower = 0.9, delta1 = 0.25)
#'
#'
#' @references Brannath, W. & Bauer, P. (2004). Optimal conditional error functions for the control of conditional power. Biometrics, 60 (3), 715–723. https://doi.org/10.1111/j.0006-341X.2004.00221.x
#'
getSecondStageInformation <- function(conditionalError, conditionalPower, delta1) {
  return((getNu(alpha = conditionalError, conditionalPower = conditionalPower))
         /(delta1^2))
}

getSecondStageInformation <- Vectorize(FUN = getSecondStageInformation,
                                      vectorize.args = c("conditionalError", "conditionalPower", "delta1"))
