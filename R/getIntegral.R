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
  enforceBasicIntegration <- isTRUE(as.logical(getOption(
    "optconerrf.enforce.basic.integration",
    FALSE
  )))

  designHasConstraints <- (design$maximumConditionalError < 1) ||
    (design$minimumConditionalError > 0) ||
    (design$minimumSecondStageInformation > 0) ||
    (design$maximumSecondStageInformation < Inf)

  # In the following cases, use basic integration:
  # - enforced through option (see above)
  # - design has constraints
  # - monotonicity of design is not enforced
  # - design has no monotonisation constants
  # - design uses a conditional power function

  if (
    enforceBasicIntegration ||
      designHasConstraints ||
      !design$enforceMonotonicity ||
      is.null(unlist(design$monotonisationConstants)) ||
      !is.null(suppressWarnings(body(design$conditionalPowerFunction)))
  ) {
    integral <- stats::integrate(
      f = getInnerPsi,
      lower = design$alpha1,
      upper = design$alpha0,
      constant = constant,
      design = design
    )$value
  } else {
    # If monotonisation constants exist, use alternative integration routine better adapted to constant functions
    integral <- getIntegralWithConstants(
      constant = constant,
      design = design
    )
  }
  return(integral - (design$alpha - design$alpha1))
}
