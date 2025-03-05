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

  C_max_Info <- NULL
  C_min_Info <- NULL

  # Constraints on conditional error scale
  C_max_cond_error <- design$maximumConditionalError
  C_min_cond_error <- design$minimumConditionalError

  # Identify conditional power to be supplied to getPsi and to be used for (information) constraints
  # Check if conditional power function should be used
  if(!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
    conditionalPower <- design$conditionalPowerFunction(firstStagePValue)

    #Calculate possibly data-dependent constraints
    #Identify effect for constraints
    if(design$useInterimEstimate) {
      delta1 <- pmin(pmax(qnorm(1-firstStagePValue)/sqrt(design$firstStageInformation), design$delta1Min), design$delta1Max)
    }
    else {
      delta1 <- design$delta1
    }

    #If minimumSecondStageInformation is given, use this instead of maximumConditionalError
    if(design$minimumSecondStageInformation > 0){
      C_max_Info <- 1 - pnorm(delta1* sqrt(design$minimumSecondStageInformation)-qnorm(conditionalPower))
    }

    #If maximumSecondStageInformation is given, use this instead of minimumConditionalError
    if(design$maximumSecondStageInformation < Inf){
      C_min_Info <- 1 - pnorm(delta1* sqrt(design$maximumSecondStageInformation)-qnorm(conditionalPower))
    }
  }
  # Constant conditional power
  else {
    conditionalPower <- design$conditionalPower

    #Check if interim estimate is used
    if(design$useInterimEstimate) {
      delta_C_max <- min(qnorm(1-design$alpha1)/sqrt(design$firstStageInformation),design$delta1Max)
      delta_C_min <- max(qnorm(1-design$alpha0)/sqrt(design$firstStageInformation),design$delta1Min)
    }
    # Otherwise use fixed effect
    else {
      delta_C_max <- design$delta1
      delta_C_min <- design$delta1
    }

    #If minimumSecondStageInformation is given, use this instead of maximumConditionalError
    if(design$minimumSecondStageInformation > 0){
      C_max_Info <- 1 - pnorm(delta_C_max* sqrt(design$minimumSecondStageInformation)-qnorm(conditionalPower))
    }

    #If maximumSecondStageInformation is given, use this instead of minimumConditionalError
    if(design$maximumSecondStageInformation < Inf){
      C_min_Info <- 1 - pnorm(delta_C_min* sqrt(design$maximumSecondStageInformation)-qnorm(conditionalPower))
    }
  }

  #Use the constraint that is the stronger restriction
  C_max <- min(C_max_Info, C_max_cond_error)
  C_min <- max(C_min_Info, C_min_cond_error)


  return(pmin(pmax(getPsi(nuPrime = inner, conditionalPower = conditionalPower),
                   C_min), C_max))
}
