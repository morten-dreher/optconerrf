#' Integrate over sample size
#'
#' @description Internal function used by \code{getExpectedSecondStageSampleSize()} to calculate the integral over the sample size.
#'
#' @template param_firstStagePValue
#' @template param_distDelta
#' @template param_design
#' @template param_allocationRatio
#' @template param_standardDeviation
#' @param ... Additional arguments needed for \code{getOptimalConditionalError()} and \code{getLikelihoodRatio()}.
#'
#' @return Integral over the sample size
#'
#' @keywords internal

integrateExpectedSampleSize <- function(firstStagePValue, design, distDelta,
                                        allocationRatio = 1, standardDeviation = 1, ...) {

  # Calculate optimal conditional error function
  optimalCondErr <- getOptimalConditionalError(
    firstStagePValue = firstStagePValue, design = design)

  # Identify how the likelihood ratio should be calculated
  likelihoodRatio <- NA
  args <- list(...)


  # Fixed effect
  if(distDelta == "fixed") {
    ncpLR <- unlist(args["ncpLRDelta"])
    weights <- unlist(args["weightsDelta"])

    # Ensure argument specified
    if(is.null(ncpLR)) {
      stop("Argument ncpLRDelta must be provided for fixed likelihood ratio case.")
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("dist" = distDelta, "ncpLR" = ncpLR, "weights" = weights)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Normal prior for effect
  else if(distDelta == "normal") {
    ncpLR <- unlist(args["ncpLRDelta"])
    tauLR <- unlist(args["tauLRDelta"])

    # Ensure arguments specified
    if(is.null(ncpLR) || is.null(tauLR)) {
      stop("Arguments ncpLRDelta and tauLRDelta must be provided for fixed likelihood ratio case.")
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("dist" = distDelta, "ncpLR" = ncpLR, "tauLR" = tauLR)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Exponential prior for effect
  else if(distDelta == "exp") {

    kap0 <- unlist(args["kap0Delta"])

    # Ensure argument specified
    if(is.null(kap0)) {
      stop("Argument kap0Delta must be specified for exponential likelihood case.")
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("dist" = distDelta, "kap0" = kap0)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Uniform prior for effect
  else if(distDelta == "unif") {

    delMax <- unlist(args["delMaxDelta"])

    # Ensure argument specified
    if(is.null(delMax)) {
      stop("Argument delMaxDelta must be specified for uniform likelihood case.")
    }

    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("dist" = distDelta, "delMax" = delMax)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Maximum likelihood ratio
  else if(distDelta == "maxlr") {
    # Create a list that acts as a design object to calculate true likelihood ratio
    ghostDesign <- list("dist" = distDelta)

    # Calculate likelihood ratio
    likelihoodRatio <- getLikelihoodRatio(
      firstStagePValue = firstStagePValue, design = ghostDesign
    )
  }
  # Unknown distribution specified
  else {
    stop("Distribution not matched.")
  }

  # Identify effect size to calculate second-stage sample size
  # -----
  # The effect is the same as the one specified in the design object.
  # In the current implementation, this means that it is provided on the non-centrality parameter scale.
  # -----

  # Fixed effect case
  if("delta1" %in% names(args)) {
    delta1 <- unlist(args["delta1"])
  }
  # Interim estimate (specified via minimum cut-off)
  else if("delta0" %in% names(args)) {
    delta0 <- unlist(args["ncp0"])

    # Use maximum of estimated effect and minimum effect
    delta1 <- pmax(qnorm(1-firstStagePValue), delta0)
  }
  # Unknown effect specification given
  else {
    stop("Effect size for which to calculate the expected second-stage sample size must be specified via delta1 for a fixed effect or via delta0 for the minimum effect size of an interim estimate.")
  }

  return((getNu(alpha=optimalCondErr,  conditionalPower = design$conditionalPower)*likelihoodRatio) / (delta1^2))

}
