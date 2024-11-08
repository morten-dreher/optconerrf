#' Get Second-stage Level for Inverse Normal Method
#' @name getAlpha2InverseNormal
#'
#' @description Find second-stage significance level for inverse normal test (\code{getInverseNormalCombinationFunction()}) and inverse normal conditional error (\code{getInverseNormalConditionalError()}).
#'
#' @template param_alpha
#' @template param_alpha1
#' @template param_alpha0
#' @template param_weights_stages
#'
#' @return Second-stage significance level of the inverse normal test.
#'
#' @export
#'
#' @examples
#' getAlpha2InverseNormal(alpha=0.025, alpha1=0.001, alpha0=0.5)
#'

getAlpha2InverseNormal <- function(alpha, alpha1, alpha0, weights = 1/sqrt(c(2, 2))) {
  # Solve the helper function findAlpha2
  alpha2 <- stats::uniroot(f = findAlpha2, lower = 0, upper = 1, alpha = alpha, alpha1 = alpha1, alpha0 = alpha0, weights = weights)$root
  return(alpha2)
}
