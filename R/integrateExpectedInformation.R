#' Integrate over information
#'
#' @description Internal function used by \code{getExpectedSecondStageInformation()} to calculate the integral over the information.
#'
#' @template param_firstStagePValue
#' @template param_distDelta
#' @template param_design
#' @param ... Additional arguments needed for \code{getOptimalConditionalError()} and \code{getLikelihoodRatio()}.
#'
#' @return Integral over the information of the second stage
#'
#' @keywords internal

integrateExpectedInformation <- function(firstStagePValue, design, distDelta, ...) {

  # Calculate optimal conditional error function
  optimalCondErr <- getOptimalConditionalError(
    firstStagePValue = firstStagePValue, design = design)

  # Identify how the likelihood ratio should be calculated
  likelihoodRatio <- NA
  args <- list(...)

  # Fixed effect
  if(distDelta == "fixed") {
    ncpLR <- unlist(args["deltaLR"])
    weights <- unlist(args["weightsDeltaLR"])

    # Ensure argument specified
    if(is.null(ncpLR)) {
      stop("Argument ncpLRDelta must be provided for fixed likelihood ratio case.")
    }
    if(is.null(weights)) {
      weights <- rep(1/length(ncpLR), length(ncpLR))
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("likelihoodRatioDistribution" = distDelta, "deltaLR" = ncpLR, "weightsDeltaLR" = weights,
                        "firstStageInformation" = design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Normal prior for effect
  else if(distDelta == "normal") {
    ncpLR <- unlist(args["deltaLR"])
    tauLR <- unlist(args["tauLR"])

    # Ensure arguments specified
    if(is.null(ncpLR) || is.null(tauLR)) {
      stop("Arguments ncpLRDelta and tauLRDelta must be provided for fixed likelihood ratio case.")
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("likelihoodRatioDistribution" = distDelta, "deltaLR" = ncpLR, "tauLR" = tauLR,
                        "firstStageInformation" = design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Exponential prior for effect
  else if(distDelta == "exp") {

    kap0 <- unlist(args["kappaLR"])

    # Ensure argument specified
    if(is.null(kap0)) {
      stop("Argument kap0Delta must be specified for exponential likelihood case.")
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("likelihoodRatioDistribution" = distDelta, "kappaLR" = kap0,
                        "firstStageInformation" = design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Uniform prior for effect
  else if(distDelta == "unif") {

    delMax <- unlist(args["deltaMaxLR"])

    # Ensure argument specified
    if(is.null(delMax)) {
      stop("Argument delMaxDelta must be specified for uniform likelihood case.")
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("likelihoodRatioDistribution" = distDelta, "deltaMaxLR" = delMax,
                        "firstStageInformation" = design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Maximum likelihood ratio
  else if(distDelta == "maxlr") {
    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("likelihoodRatioDistribution" = distDelta)

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
    # Use maximum of estimated effect and minimum effect
    delta1 <- pmax(qnorm(1-firstStagePValue)/sqrt(design$firstStageInformation),
                   design$ncp1/sqrt(design$firstStageInformation))
  }

  return((getNu(alpha=optimalCondErr,  conditionalPower = design$conditionalPower)*likelihoodRatio) / (delta1^2))

}
