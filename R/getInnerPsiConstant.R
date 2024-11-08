#' Integration routine called via \code{getIntegralWithConstants()}.
#'
#' @template param_firstStagePValue
#' @template param_constant_integrate
#' @template param_design
getInnerPsiConstant <- function(firstStagePValue, constant, design) {
  Q <- getQ(firstStagePValue = firstStagePValue, design = design)

  inner <- -exp(constant)/Q

  return(pmin(pmax(getPsi(nuPrime = inner, conditionalPower = design$conditionalPower),
                   design$minimumConditionalError), design$maximumConditionalError))
}
