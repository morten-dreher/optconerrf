#' Calculate conditional error from second-stage information
#'
#' @template param_firstStagePValue
#' @param secondStageInformation The second-stage information for which to calculate the corresponding conditional error.
#' @template param_design
#'
#' @return The conditional error which corresponds to the provided second-stage information.
getConditionalErrorFromSecondStageInformation <- function(firstStagePValue, secondStageInformation, design) {

  if(design$useInterimEstimate) {
    delta1 <- min(max(qnorm(1-firstStagePValue)/sqrt(design$firstStageInformation), design$delta1Min), design$delta1Max)
  }
  else {
    delta1 <- design$delta1
  }

  if(!is.na(design$conditionalPower)) {
    conditionalPower <- design$conditionalPower
  }

  conditionalError <- 1-pnorm(sqrt(secondStageInformation) * delta1 - qnorm(conditionalPower))

  return(conditionalError)

}
