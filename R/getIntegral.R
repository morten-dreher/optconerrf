#' Internal calculation of the integral over psi
#' @name getIntegral
#'
#' @description Helper function that integrates over the optimal conditional error function. Used to find appropriate level constant.
#'
#' @details Internal function called by \code{findLevelConstant()} that should be solved (i.e., root should be found).
#'
#' @template param_constant_integrate
#' @template param_design
#'
#' @return Distance of integral over psi to (alpha-alpha1).
#' @keywords internal

getIntegral <- function(constant, design) {
  # If there are no monotonisation constants, use standard integration
  if(length(design$monotonisationConstants) == 0) {
    integral <- stats::integrate(
      f = getInnerPsi, lower = design$alpha1, upper = design$alpha0, constant = constant,
      design = design)$value
  }
  # If monotonisation constants exist, use alternate integration routine better adapted to constant functions
  else {
    integral <- getIntegralWithConstants(
      constant = constant, design = design
    )
  }
  return(integral - (design$alpha-design$alpha1))
}
