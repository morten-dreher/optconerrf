#' Calculate Maximum Second-stage Information
#' @name getMaxInformation
#'
#' @description Calculate the maximum information
#'
#' @template param_design
#'
#'

getMaxInformation <- function(design) {

  maxInfo <- getSecondStageInformation(firstStagePValue = design$alpha0, design = design)

  return(maxInfo)
}
