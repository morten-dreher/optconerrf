#' Get Level Constant for Optimal Conditional Error Function
#' @name getLevelConstant
#'
#' @description Find the constant required such that the conditional error function meets the overall level condition.
#'
#' @details The level condition is defined as:
#' \deqn{\alpha = \alpha_1 + \int_{\alpha_1}^{\alpha_0} \alpha_2(p_1)dp_1.}
#' The constant \eqn{c_0} of the optimal conditional error function is calibrated such that it meets the level condition.
#' For a valid design, the additional following condition must be met to be able to exhaust the level \eqn{\alpha}:
#' \deqn{\alpha_1 + CP(\alpha_0-\alpha_1)>\alpha.}
#' This condition is checked by \code{getLevelConstant()} and the execution is terminated if it is not met. \cr
#'
#' @template param_design
#'
#' @return A list that contains the constant (element \code{$root}) and other components provided by \code{uniroot()}.
#'
#' @export
#'
#'
#' @references Brannath, W. & Bauer, P. (2004). Optimal conditional error functions for the control of conditional power. Biometrics, 60 (3), 715–723. https://doi.org/10.1111/j.0006-341X.2004.00221.x
#' @references Brannath, W. & Dreher, M. (2024). Optimal monotone conditional error functions. https://arxiv.org/abs/2402.00814

getLevelConstant <- function(design) {
  # Check basic condition for decision rules
  # Fixed conditional power
  if(!is.na(design$conditionalPower)) {
    if (design$alpha1 + design$conditionalPower * (design$alpha0 - design$alpha1) <= design$alpha) {
      stop("(alpha1 + conditionalPower*(alpha0-alpha1)) must exceed alpha, otherwise no level constant fully exhausting alpha can be found.")
    }
  }
  # Conditional power function
  else if(!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
    if(stats::integrate(f = design$conditionalPowerFunction, lower = design$alpha1, upper = design$alpha0)$value
    <= design$alpha - design$alpha1) {
      stop("Integral over conditional power function from alpha1 to alpha0 must exceed (alpha-alpha1), otherwise no level constant fully exhausting alpha can be found.")
    }
  }
  # Unexpected issue
  else {
    stop("Unexpected error: both conditionalPower and conditionalPowerFunction are specified inappropriately.")
  }

  # Find the level constant.
  # Expects an error if specified non-centrality parameter is very large or very small
  tryCatch(
    expr = {
      stats::uniroot(
        f = getIntegral, lower = design$levelConstantMinimum,
        upper = design$levelConstantMaximum, design = design,
        tol = 1e-16
      )
    },
    error = function(e) {
      # This specific error may occur if the given non-centrality parameter is too small or too large or if the
      # provided constraints are not suitable and is handled separately
      if (e$message == "f() values at end points not of opposite sign") {
        stop("Root finding for level constant failed. Try changing the search interval via arguments levelConstantMinimum and levelConstantMaximum. \n Alternatively, the constraints on the optimal conditional error function or second-stage information may not be appropriate.")
      }
      # Print all other errors directly
      else {
        stop(e)
      }
    }
  )
}
