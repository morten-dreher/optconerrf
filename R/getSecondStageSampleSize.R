#' Calculate the Second-stage Sample Size
#' @name getSecondStageSampleSize
#'
#' @description Calculate second-stage sample size for given alpha, conditional power and effect size
#'
#' @details The second-stage sample size \eqn{n_{2,t}} of the treatment group is calculated given a first-stage p-value \eqn{p_1} as:
#' \deqn{n_{2,t}(p_1) = \frac{\sigma^2\cdot(r+1)\cdot (\Phi^{-1}(1-\alpha_2(p_1)) + \Phi^{-1}(CP))^2}{\Delta_1^2} = \frac{\sigma^2\cdot(r+1) \cdot \nu(\alpha_2(p_1))}{\Delta_1^2},}
#' where
#' \itemize{
#'    \item \eqn{\sigma} is the assumed (shared) standard deviation of the treatment effect
#'    \item \eqn{r} is the allocation ratio between treatment and control group (\eqn{r=n_t/n_c})
#'    \item \eqn{\alpha_2(p_1)} is the second-stage significance level (e.g., a conditional error)
#'    \item \eqn{CP} is the target conditional power
#'    \item \eqn{\Delta_1} is the assumed treatment effect (expressed as a mean difference).
#' }
#'
#' @param conditionalError Conditional error rate of the design.
#' @template param_conditionalPower
#' @param delta1 Effect size to power for.
#' @template param_allocationRatio
#' @template param_standardDeviation
#'
#' @return The second-stage sample size of the treatment group.
#' @export
#'
#' @examples
#' getSecondStageSampleSize(
#' conditionalError=0.05, conditionalPower=0.9, delta1=0.25)
#'
#'
#' @references Brannath, W., & Bauer, P. (2004). Optimal conditional error functions for the control of conditional power. Biometrics, 60 (3), 715–723. https://doi.org/10.1111/j.0006-341X.2004.00221.x
#'
getSecondStageSampleSize <- function(conditionalError, conditionalPower, delta1, allocationRatio = 1, standardDeviation = 1) {
  return(((standardDeviation^2)*(allocationRatio+1)*getNu(alpha = conditionalError, conditionalPower = conditionalPower))/(delta1^2))
}

getSecondStageSampleSize <- Vectorize(FUN = getSecondStageSampleSize,
                                      vectorize.args = c("conditionalError", "conditionalPower", "delta1"))
