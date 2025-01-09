# TODO: extend to more than one interval
#' Calculate the integral over a partially constant function
#'
#' @description This function aims to provide a more accurate integration routine than the \code{integrate()} function
#' for a partially constant function.
#'
#' @template param_constant_integrate
#' @template param_design
#'

#Neu
getIntegralWithConstants <- function(constant, design) {

  #number of non-decreasing intervals of the optimal conditional error function
  n <- length(design$monotonisationConstants$qs)

  # Integrate over the first (non-constant) part of the function
  first_part <- stats::integrate(
    f = getInnerPsi, lower = design$alpha1, upper = design$monotonisationConstants$dls[1],
    constant = constant, design = design)$value

  # Integrate over the final (non-constant) part of the function
  last_part <- stats::integrate(
    f = getInnerPsi, lower = design$monotonisationConstants$dus[n], upper = design$alpha0,
    constant = constant, design = design)$value

  # Calculate integral for all constant parts
  const_parts <- 0
  for (x in 1:n){
    new_part <- min(max(getPsi(nuPrime = -exp(constant)/design$monotonisationConstants$qs[1],
                   conditionalPower = design$conditionalPower), design$minimumConditionalError),
        design$maximumConditionalError)*
      (design$monotonisationConstants$dus[x] - design$monotonisationConstants$dls[x])
    const_parts <- const_parts + new_part
  }

  # Calculate integral for all remaining non-constant parts
  non_const_parts <- 0
  if (n>1){
    for (x in 1:(n-1)){
      new_part <- stats::integrate(
        f = getInnerPsi, lower = design$monotonisationConstants$dus[x], upper = design$monotonisationConstants$dls[x+1],
        constant = constant, design = design)$value
      non_const_parts <- non_const_parts + new_part
    }
  }

  # Return the sum of all parts
  return(sum(c(first_part, last_part, const_parts, non_const_parts)))
}

########## Alt ############
# getIntegralWithConstants <- function(constant, design) {
#   # Integrate over the first (non-constant) part of the function
#   part1 <- stats::integrate(
#     f = getInnerPsi, lower = design$alpha1, upper = design$monotonisationConstants$dls[1],
#     constant = constant, design = design)$value
#
#   # Manually calculate integral over the constant part of function
#   # For this, the q provided in the monotonisation constants can simply be used
#   part2 <- min(max(getPsi(nuPrime = -exp(constant)/design$monotonisationConstants$qs[1],
#                           conditionalPower = design$conditionalPower), design$minimumConditionalError),
#     design$maximumConditionalError)*
#     (design$monotonisationConstants$dus[1] - design$monotonisationConstants$dls[1])
#
#   # Integrate over the final (non-constant) part of the function
#   part3 <- stats::integrate(
#     f = getInnerPsi, lower = design$monotonisationConstants$dus[1], upper = design$alpha0,
#     constant = constant, design = design)$value
#
#   # Return the sum of the individual integrals
#   return(sum(c(part1, part2, part3)))
# }
