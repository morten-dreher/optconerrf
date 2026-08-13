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
      design = design)
  } else {
    Q <- getQ(firstStagePValue = firstStagePValue, design = design)
  }

  # Calculate the value to be supplied to getPsi
  inner <- -exp(constant)/Q

  # Identify conditional power to be supplied to getPsi and to be used for (information) constraints
  # Check if conditional power function should be used
  if(!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
    conditionalPower <- design$conditionalPowerFunction(firstStagePValue)
  } else {
    conditionalPower <- design$conditionalPower
  }

  #Calculate the constraints
  C_max <- getConstraintC_max(firstStagePValue = firstStagePValue, design = design)
  C_min <- getConstraintC_min(firstStagePValue = firstStagePValue, design = design)

  return(pmin(pmax(getPsi(nuPrime = inner, conditionalPower = conditionalPower, design = design, firstStagePValue = firstStagePValue),
                   C_min), C_max))
}
