#' Internal variation of \code{getPsi()} used by \code{findLevelConstant()}
#' @name getInnerPsi
#'
#' @description Calculate psi for the given scenario, incl. constant
#' @details Internal function called by \code{getIntegral()} that calculates a point value of \code{getPsi()}.
#'
#' @template param_firstStagePValue_integrate
#' @template param_constant_integrate
#' @template param_design
#'
#' @return Point value of \code{getPsi()}.
#' @keywords internal

getInnerPsi <- function(firstStagePValue, constant, design) {

  # If monotonisation constants provided, perform non-increasing transformation
  if(design$enforceMonotonicity) {
    Q <- getMonotoneFunction(
      x = firstStagePValue, fun = getQ, argument = "firstStagePValue",
      design = design, printConstant = FALSE)
  } else {
    Q <- getQ(firstStagePValue = firstStagePValue, design = design)
  }

  inner <- -exp(constant)/Q
  return(pmin(pmax(getPsi(nuPrime = inner, conditionalPower = design$conditionalPower),
                   design$minimumConditionalError), design$maximumConditionalError))
}
