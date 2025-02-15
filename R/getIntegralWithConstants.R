#' Calculate the integral over a partially constant function
#'
#' @description This function aims to provide a more accurate integration routine than the \code{integrate()} function
#' for a partially constant function.
#'
#' @template param_constant_integrate
#' @template param_design
#'
#' @return Integral over the partially constant function.
#' @keywords internal
#'

getIntegralWithConstants <- function(constant, design) {

  # Number of non-decreasing intervals of the optimal conditional error function
  numberOfIntervals <- length(design$monotonisationConstants$qs)

  # Integrate over the first (non-constant) part of the function
  firstPart <- stats::integrate(
    f = getInnerPsi, lower = design$alpha1, upper = design$monotonisationConstants$dls[1],
    constant = constant, design = design)$value

  # Integrate over the final (non-constant) part of the function
  lastPart <- stats::integrate(
    f = getInnerPsi, lower = design$monotonisationConstants$dus[numberOfIntervals], upper = design$alpha0,
    constant = constant, design = design)$value

  # Calculate integral for all constant parts
  constantParts <- 0
  for (x in 1:numberOfIntervals){
    newPart <- min(max(getPsi(nuPrime = -exp(constant)/design$monotonisationConstants$qs[x],
                   conditionalPower = design$conditionalPower), design$minimumConditionalError),
        design$maximumConditionalError)*
      (design$monotonisationConstants$dus[x] - design$monotonisationConstants$dls[x])
    constantParts <- constantParts + newPart
  }

  # Calculate integral for all remaining non-constant parts
  nonConstantParts <- 0
  if (numberOfIntervals>1){
    for (x in 1:(numberOfIntervals-1)){
      newPart <- stats::integrate(
        f = getInnerPsi, lower = design$monotonisationConstants$dus[x], upper = design$monotonisationConstants$dls[x+1],
        constant = constant, design = design)$value
      nonConstantParts <- nonConstantParts + newPart
    }
  }

  # Return the sum of all parts
  return(sum(c(firstPart, lastPart, constantParts, nonConstantParts)))
}
