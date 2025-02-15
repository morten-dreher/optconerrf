#' Integrate over information
#'
#' @description Internal function used by \code{getExpectedSecondStageInformation()} to calculate the integral over the information.
#'
#' @template param_firstStagePValue
#' @template param_likelihoodRatioDistribution
#' @template param_design
#' @param ... Additional arguments needed for \code{getOptimalConditionalError()} and \code{getLikelihoodRatio()}.
#'
#' @return Integral over the information of the second stage
#'
#' @keywords internal

integrateExpectedInformation <- function(firstStagePValue, design, likelihoodRatioDistribution, ...) {

  # Calculate optimal conditional error function
  optimalCondErr <- getOptimalConditionalError(
    firstStagePValue = firstStagePValue, design = design)

  # Identify how the likelihood ratio should be calculated
  likelihoodRatio <- NA
  args <- list(...)

  # Fixed effect
  # If NULL, use specification in design object
  if(is.null(likelihoodRatioDistribution)) {
    ghostDesign <- design
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  else if(likelihoodRatioDistribution == "fixed") {
    deltaLR <- unlist(args["deltaLR"])
    weights <- unlist(args["weightsDeltaLR"])

    # Ensure argument specified
    if(is.null(deltaLR)) {
      stop("Argument deltaLR must be provided for fixed likelihood ratio case.")
    }
    if(is.null(weights)) {
      weights <- rep(1/length(deltaLR), length(deltaLR))
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("likelihoodRatioDistribution" = likelihoodRatioDistribution,
                        "deltaLR" = deltaLR, "weightsDeltaLR" = weights,
                        "firstStageInformation" = design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Normal prior for effect
  else if(likelihoodRatioDistribution == "normal") {
    ncpLR <- unlist(args["deltaLR"])
    tauLR <- unlist(args["tauLR"])

    # Ensure arguments specified
    if(is.null(ncpLR) || is.null(tauLR)) {
      stop("Arguments deltaLR and tauLR must be provided for fixed likelihood ratio case.")
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("likelihoodRatioDistribution" = likelihoodRatioDistribution,
                        "deltaLR" = deltaLR, "tauLR" = tauLR,
                        "firstStageInformation" = design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Exponential prior for effect
  else if(likelihoodRatioDistribution == "exp") {

    kap0 <- unlist(args["kappaLR"])

    # Ensure argument specified
    if(is.null(kap0)) {
      stop("Argument kappaLR must be specified for exponential likelihood case.")
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("likelihoodRatioDistribution" = likelihoodRatioDistribution,
                        "kappaLR" = kap0,
                        "firstStageInformation" = design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Uniform prior for effect
  else if(likelihoodRatioDistribution == "unif") {

    delMax <- unlist(args["deltaMaxLR"])

    # Ensure argument specified
    if(is.null(delMax)) {
      stop("Argument deltaMaxLR must be specified for uniform likelihood case.")
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("likelihoodRatioDistribution" = likelihoodRatioDistribution,
                        "deltaMaxLR" = delMax,
                        "firstStageInformation" = design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Maximum likelihood ratio
  else if(likelihoodRatioDistribution == "maxlr") {
    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("likelihoodRatioDistribution" = likelihoodRatioDistribution)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Unknown distribution specified
  else {
    stop("Distribution not matched.")
  }

  # Identify effect size to calculate second-stage information
  # The effect is the same as the one specified in the design object.

  # Fixed effect case
  if(!design$useInterimEstimate) {
    delta1 <- design$delta1
  }
  # Interim estimate
  else {
    # Apply restrictions that are given in the design object
    delta1 <- pmin(pmax(design$delta1Min, qnorm(1-firstStagePValue)), design$delta1Max)
  }

  return((getNu(alpha=optimalCondErr,  conditionalPower = design$conditionalPower)*likelihoodRatio) / (delta1^2))

}
