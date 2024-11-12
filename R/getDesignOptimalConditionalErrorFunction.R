#' Create a design object for the optimal conditional error function.
#' @name getDesignOptimalConditionalErrorFunction
#'
#' @description This function returns a design object in form of a list which contains all important parameters for the optimal CEF.
#'
#'
#' @details
#' From the given user specifications, the constant to achieve level condition for control of the overall type I error rate as well as the constants to ensure a non-increasing optimal CEF are automatically calculated.
#' The level constant is calculated via the helper function \code{getLevelConstant()} and the monotonisation constants are calculated via
#' the function\code{getMonotonisationConstants()}. See the respective help pages for more details.
#'
#' @template param_alpha
#' @template param_alpha1
#' @template param_alpha0
#' @template param_conditionalPower
#' @template param_ncp1
#' @template param_ncp0
#' @template param_dist
#' @param minimumConditionalError Lower boundary for the optimal conditional error function. Default 0 (no restriction).
#' @param maximumConditionalError Upper boundary for the optimal conditional error function. Default value is 1, however, the optimal conditional error function is inherently bounded by the conditional power (assuming no early efficacy stop).
#' @template param_levelConstantMinimum
#' @template param_levelConstantMaximum
#' @param ... Additional arguments required for the specification of the likelihood ratio.
#'
#' @return A list of the optimal conditional error design which can be passed on to other package functions.
#'
#' @examples
#' getDesignOptimalConditionalErrorFunction(
#' alpha=0.025, alpha1=0.001, alpha0=0.5, conditionalPower=0.9,
#' ncp1=1, dist="fixed", ncpLR=1)
#'
#' @export
#'
getDesignOptimalConditionalErrorFunction <- function(
    alpha, alpha1, alpha0, conditionalPower, ncp1 = NULL, ncp0 = NULL,
    dist, minimumConditionalError = 0, maximumConditionalError = 1,
    levelConstantMinimum = 0, levelConstantMaximum = 10, ...) {

  # Get additional arguments (parameters of likelihood ratio distribution)
  additionalArguments <- list(...)

  # Create a design list
  designList <- list(
    "alpha" = alpha,
    "alpha1" = alpha1,
    "alpha0" = alpha0,
    "conditionalPower" = conditionalPower,
    "dist" = dist,
    "minimumConditionalError" = minimumConditionalError,
    "maximumConditionalError" = maximumConditionalError
  )

  # Determine distribution and ensure that required additional parameters are specified
  # Fixed parameter(s) for likelihood ratio
  if(dist == "fixed") {
    designList$ncpLR <- unlist(additionalArguments[["ncpLR"]])
    designList$weights <- unlist(additionalArguments[["weights"]])

    if(is.null(unlist(additionalArguments[["ncpLR"]]))) {
      stop("Argument ncpLR must be provided for fixed likelihood case")
    }

    if(is.null(designList$weights)) {
      designList$weights <- rep(1/length(designList$ncpLR), length(designList$ncpLR))
    }
  }
  # Normally distributed prior
  else if(dist == "normal") {
    designList$ncpLR <- additionalArguments[["ncpLR"]]
    designList$tauLR <- additionalArguments[["tauLR"]]

    if(is.null(additionalArguments[["ncpLR"]]) || is.null(additionalArguments[["tauLR"]])) {
      stop("Arguments ncpLR and tauLR must be specified for normal likelihood case")
    }
  }
  # Exponentially distributed prior
  else if(dist == "exp") {
    designList$kap0 <- additionalArguments[["kap0"]]

    if(is.null(additionalArguments[["kap0"]])) {
      stop("Argument kap0 must be specified for exponential likelihood case")
    }
  }
  # Uniformly distributed prior
  else if(dist == "unif") {
    designList$delMax <- additionalArguments[["delMax"]]

    if(is.null(additionalArguments[["delMax"]])) {
      stop("Argument delMax must be specified for uniform likelihood case")
    }
  }
  # Maximum likelihood ratio
  else if(dist == "maxlr") {

  }
  else {
    stop("Distribution not matched.")
  }

  # Next, determine effect specification for conditional power
  # ncp0 specified -> use of interim estimate with cutoff ncp0
  if(!is.null(ncp0) && is.null(ncp1)) {
    designList$ncp0 <- ncp0

    # Get monotonisation constants
    designList$monotonisationConstants <- getMonotonisationConstants(
      fun = getQ, lower = alpha1, upper = alpha0, argument = "firstStagePValue",
      design = designList
    )

    # Get level constant
    designList$levelConstant <- getLevelConstant(
      design = designList, levelConstantMinimum = levelConstantMinimum,
      levelConstantMaximum = levelConstantMaximum
    )$root

  }
  # ncp1 specified -> use fixed effect size ncp1
  else if(!is.null(ncp1) && is.null(ncp0)) {
    designList$ncp1 <- ncp1

    # Get monotonisation constants
    designList$monotonisationConstants <- getMonotonisationConstants(
      fun = getQ, lower = alpha1, upper = alpha0, argument = "firstStagePValue",
      design = designList
    )

    # Get level constant
    designList$levelConstant <- getLevelConstant(
      design = designList, levelConstantMinimum = levelConstantMinimum,
      levelConstantMaximum = levelConstantMaximum
    )$root
  }
  else {
    stop("Effect size for conditional power must be specified either as a fixed effect ncp1 or by a lower cut-off ncp0 for an interim estimate.")
  }


  return(designList)

}
