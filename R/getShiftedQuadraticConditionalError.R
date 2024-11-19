#' Calculated Shifted Quadratic Conditional Error
#'
#' @name getShiftedQuadraticConditionalError
#'
#' @details The shifted quadratic conditional error function can be used to illustrate conditional error functions with different monotonicity.
#' It is defined as:
#' \deqn{\alpha_2(p_1) = c\cdot (p_1-s)^2.}
#' To meet overall level condition, the constant \eqn{c} is determined. The choice of the shift value \eqn{s \in [0,1]} results in a conditional error function which is increasing (\eqn{s=0}), decreasing (\eqn{s=1}) or not strictly monotone (\eqn{s \in ]0,1[}).
#' The form of the shifted quadratic conditional error function is based on mathematical convenience, as exact calculations of the type I error rate can be performed.
#' For practical use cases, refer to \code{getCircularConditionalError()}, \code{getInverseNormalConditionalError()} and \code{getOptimalConditionalError()}.
#'
#' @template param_firstStagePValue
#' @template param_alpha
#' @param shift Shift value. Should be between 0 and 1.
#'
#' @examples
#' getShiftedQuadraticConditionalError(
#' firstStagePValue = 0.1, alpha = 0.025, shift = 0.5)
#'
#' @return Shifted quadratic conditional error.
#' @export

getShiftedQuadraticConditionalError <- function(firstStagePValue, alpha, shift) {
  constant <- alpha/(shift^2-shift+(1/3))
  return(constant*(firstStagePValue-shift)^2)
}

getShiftedQuadraticConditionalError <- Vectorize(FUN = getShiftedQuadraticConditionalError)
