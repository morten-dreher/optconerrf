#' Calculates the constraints.
#' @name getConstraints
#'
#' @details Internal function that calculates the constraints.
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @return List with the lower and upper constraint
#'
#'

getConstraints <- function(firstStagePValue, design){

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
    } else {
      delta1 <- design$delta1
    }

    #If minimumSecondStageInformation is given, use this
    if(design$minimumSecondStageInformation > 0){
      C_max_Info <- 1 - pnorm(delta1* sqrt(design$minimumSecondStageInformation)-qnorm(conditionalPower))
    }

    #If maximumSecondStageInformation is given, use this
    if(design$maximumSecondStageInformation < Inf){
      C_min_Info <- 1 - pnorm(delta1* sqrt(design$maximumSecondStageInformation)-qnorm(conditionalPower))
    }
  } else {
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

    #If minimumSecondStageInformation is given, use this
    if(design$minimumSecondStageInformation > 0){
      C_max_Info <- 1 - pnorm(delta_C_max* sqrt(design$minimumSecondStageInformation)-qnorm(conditionalPower))
    }

    #If maximumSecondStageInformation is given, use this
    if(design$maximumSecondStageInformation < Inf){
      C_min_Info <- 1 - pnorm(delta_C_min* sqrt(design$maximumSecondStageInformation)-qnorm(conditionalPower))
    }
  }

  #Use the constraint that is the stronger restriction
  C_max <- min(C_max_Info, C_max_cond_error)
  C_min <- max(C_min_Info, C_min_cond_error)

  return(list("C_min" = C_min, "C_max" = C_max))

}
