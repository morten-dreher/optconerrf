#' Calculate Expected Second-stage Information
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

  if (design$alpha1 != 0) {
    epsilon <- 1e-17
    estimated_delta <- qnorm(1 - (design$alpha1 + epsilon)) * design$firstStageInformation^(-1 / 2)
    # delta <- max(delta_min, estimated_delta)
    delta <- min(delta_max, estimated_delta)
    opti <- getOptimalConditionalError(
      firstStagePValue = design$alpha1 + epsilon,
      design = design
    )

    minInfo_alpha <- getSecondStageInformation(
      firstStagePValue = design$alpha1 + epsilon,
      design = design
    )
  }
  estimated_delta <- qnorm(1 - design$alpha0) * design$firstStageInformation^(-1 / 2)
  # delta <- min(delta_max, estimated_delta)
  delta <- max(delta_min, estimated_delta)
  opti <- getOptimalConditionalError(
    firstStagePValue = design$alpha0,
    design = design
  )
  maxInfo_alpha <- getSecondStageInformation(
    firstStagePValue = design$alpha0,
    design = design
  )
  print(paste0("maxInfo_alpha ", maxInfo_alpha))

  if ((design$maximumConditionalError < design$conditionalPower) && !is.null(delta_max)) {
    minInfo_cond_error <-
      getNu(
        alpha = design$maximumConditionalError,
        conditionalPower = design$conditionalPower
      ) / (delta_max^2)
    print(paste0("minInfo_cond_error ", minInfo_cond_error))
  }
  if (design$minimumConditionalError != 0) {
    maxInfo_cond_error <-
      getNu(
        alpha = design$minimumConditionalError,
        conditionalPower = design$conditionalPower
      ) / (delta_min^2)
    print(paste0("maxInfo_cond_error ", maxInfo_cond_error))
  }

  minInfo <- max(minInfo_alpha, minInfo_cond_error)
  maxInfo <- min(maxInfo_alpha, maxInfo_cond_error)

  return(c("minInfo" = minInfo, "maxInfo" = maxInfo))
}
