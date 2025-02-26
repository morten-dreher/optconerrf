#' Calculate Minimum and Maximum Second-stage Information
#' @name getMinMaxInformation
#'
#' @description Calculate the minimum and maximum information
#'
#' @template param_design
#' @param delta_min Minimum for treatment effect.
#' @param delta_max Maximum for treatment effect.
#'
#'

getMinMaxInformation <- function(design, delta_min = NULL, delta_max = NULL) {
  minInfo_alpha <- NULL
  maxInfo_alpha <- NULL
  maxInfo_cond_error <- NULL
  minInfo_cond_error <- NULL

  #Achtung, hier wird zur Berechnung automatisch
  if (design$alpha1 != 0) {
    epsilon <- 1e-17
    minInfo_alpha <- getSecondStageInformation(firstStagePValue = design$alpha1 + epsilon, design = design)
  }
  maxInfo_alpha <- getSecondStageInformation(firstStagePValue = design$alpha0, design = design)

#Hier gibt es Probleme mit der Anpassung
  if ((design$maximumConditionalError < design$conditionalPower) && !is.null(delta_max)) {
    minInfo_cond_error <- getNu(alpha = design$maximumConditionalError,conditionalPower = design$conditionalPower) / (delta_max^2)
  }
  if (design$minimumConditionalError != 0) {
    maxInfo_cond_error <- getNu(alpha = design$minimumConditionalError, conditionalPower = design$conditionalPower) / (delta_min^2)
  }

  minInfo <- max(minInfo_alpha, minInfo_cond_error)
  maxInfo <- min(maxInfo_alpha, maxInfo_cond_error)

  return(c("minInfo" = minInfo, "maxInfo" = maxInfo))
}
