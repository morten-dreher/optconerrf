#' Create a design object for the optimal conditional error function.
#' @name getDesignOptimalConditionalErrorFunction
#'
#' @description This function returns a design object in form of a list which contains all important parameters for the optimal CEF.
#'
#'
#' @details
#' From the given user specifications, the constant to achieve level condition for control of the overall type I error rate as well as the constants to ensure a non-increasing optimal CEF are automatically calculated.
#' The level constant is calculated via the helper function \code{getLevelConstant()} and the monotonisation constants are calculated via
#' the function \code{getMonotonisationConstants()}. See the respective help pages for more details.
#'
#' @template param_alpha
#' @template param_alpha1
#' @template param_alpha0
#' @template param_conditionalPower
#' @template param_delta1
#' @template param_likelihoodRatioDistribution
#' @template param_firstStageInformation
#' @template param_useInterimEstimate
#' @param minimumConditionalError Lower boundary for the optimal conditional error function. Default 0 (no restriction).
#' @param maximumConditionalError Upper boundary for the optimal conditional error function. Default value is 1, however, the optimal conditional error function is inherently bounded by the conditional power (assuming no early efficacy stop).
#' @template param_levelConstantMinimum
#' @template param_levelConstantMaximum
#' @template param_enforceMonotonicity
#' @param ... Additional arguments required for the specification of the likelihood ratio.
#'
#' @importFrom methods new
#'
#' @return A list of the optimal conditional error design which can be passed on to other package functions.
#'
#' @examples
#' getDesignOptimalConditionalErrorFunction(
#' alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#' delta1 = 0.5, likelihoodRatioDistribution = "fixed", deltaLR = 1,
#' firstStageInformation = 2, useInterimEstimate = FALSE)
#'
#' getDesignOptimalConditionalErrorFunction(
#' alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#' delta1 = 0.5, likelihoodRatioDistribution = "maxlr",
#' firstStageInformation = 2, useInterimEstimate = TRUE)
#'
#' @export
#'
getDesignOptimalConditionalErrorFunction <- function(
    alpha, alpha1, alpha0, conditionalPower = NA_real_, delta1 = NA_real_,
    delta1Min = NA_real_, delta1Max = NA_real_, ncp1 = NA_real_,
    ncp1Min = NA_real_, ncp1Max = NA_real_, useInterimEstimate = TRUE,
    firstStageInformation, likelihoodRatioDistribution,
    minimumConditionalError = 0, maximumConditionalError = 1,
    levelConstantMinimum = 0, levelConstantMaximum = 10,
    enforceMonotonicity = TRUE, ...) {

  design <- new(
    "TrialDesignOptimalConditionalError",
    alpha = alpha,
    alpha1 = alpha1,
    alpha0 = alpha0,
    conditionalPower = conditionalPower,
    delta1 = delta1,
    delta1Min = delta1Min,
    delta1Max = delta1Max,
    ncp1 = ncp1,
    ncp1Min = ncp1Min,
    ncp1Max = ncp1Max,
    firstStageInformation = firstStageInformation,
    useInterimEstimate = useInterimEstimate,
    likelihoodRatioDistribution = likelihoodRatioDistribution,
    minimumConditionalError = minimumConditionalError,
    maximumConditionalError = maximumConditionalError,
    levelConstantMinimum = levelConstantMinimum,
    levelConstantMaximum = levelConstantMaximum,
    enforceMonotonicity = enforceMonotonicity,
    ... = ...
    )

  return(design)

}
