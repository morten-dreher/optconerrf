#' Calculate the overall power
#' @name getOverallPower
#'
#' @description Calculate the overall power
#'
#' @template param_design
#'
#'

getOverallPower <- function(design, delta){

  ncp <- delta*sqrt(design$firstStageInformation)

  if (!is.na(design$conditionalPower)){
    power <- 1 - pnorm(qnorm(1-design$alpha1)-ncp)+design$conditionalPower*(pnorm(qnorm(1-design$alpha1)-ncp)-pnorm(qnorm(1-design$alpha0)-ncp))
  }else{
    integrate <- function(firstStagePValue){design$conditionalPowerFunction(firstStagePValue)*exp(qnorm(1-firstStagePValue)*ncp-ncp^2/2)}
    integral <- stats::integrate(f = integrate, lower = design$alpha1, upper = design$alpha0)$value
    power <- 1 - pnorm(qnorm(1-design$alpha1)-ncp)+integral
  }

  return(power)
}

