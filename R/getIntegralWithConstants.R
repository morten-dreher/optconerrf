# TODO: extend to more than one interval
#' Calculate the integral over a partially constant function
#'
#' @template param_constant_integrate
#' @template param_design
#'
getIntegralWithConstants <- function(constant, design) {
  part1 <- stats::integrate(
    f = getInnerPsi, lower = design$alpha1, upper = design$monotonisationConstants$dls[1],
    constant = constant, design = design)$value

  # Manually calculate integral over constant part of function
  # The first-stage p-value to be used is slightly shifted from the lower bound
  part2 <- min(max(getPsi(nuPrime = -exp(constant)/getQ(
    firstStagePValue = design$monotonisationConstants$dls[1]+1e-6,
    design = design), conditionalPower = design$conditionalPower), design$minimumConditionalError),
    design$maximumConditionalError)*
    (design$monotonisationConstants$dus[1] - design$monotonisationConstants$dls[1])


  part3 <- stats::integrate(
    f = getInnerPsi, lower = design$monotonisationConstants$dus[1], upper = design$alpha0,
    constant = constant, design = design)$value

  return(sum(c(part1, part2, part3)))
}
