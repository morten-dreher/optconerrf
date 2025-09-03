#' Calculate Likelihood Ratio
#'
#' @name getLikelihoodRatio
#'
#' @description Calculate the likelihood ratio of a p-value for a given distribution.
#'
#' @details The calculation of the likelihood ratio for a first-stage p-value \eqn{p_1} is done based on a distributional assumption, specified in the \code{design} object.
#' The different options require different parameters, elaborated in the following.
#' \itemize{
#'    \item \code{likelihoodRatioDistribution="fixed"}: calculates the likelihood ratio for a fixed \eqn{\Delta}. The non-centrality parameter of the likelihood ratio \eqn{\vartheta} is then computed as \code{deltaLR}*\code{sqrt(firstStageInformation)} and the likelihood ratio is calculated as:
#'          \deqn{l(p_1) = e^{\Phi^{-1}(1-p_1)\vartheta - \vartheta^2/2}.} \code{deltaLR} may also contain multiple elements, in which case a weighted likelihood ratio is calculated for the given values. Unless positive weights that sum to 1 are provided by the argument \code{weightsDeltaLR}, equal weights are assumed.
#'    \item \code{likelihoodRatioDistribution="normal"}: calculates the likelihood ratio for a normally distributed prior of \eqn{\vartheta} with mean \code{deltaLR}*\code{sqrt(firstStageInformation)} (\eqn{\mu}) and standard deviation \code{tauLR}*\code{sqrt(firstStageInformation)} (\eqn{\sigma}). The parameters \code{deltaLR} and \code{tauLR} must be specified on the mean difference scale.
#'          \deqn{l(p_1) = (1+\sigma^2)^{-\frac{1}{2}}\cdot e^{-(\mu/\sigma)^2/2 + (\sigma\Phi^{-1}(1-p_1) + \mu/\sigma)^2 / (2\cdot (1+\sigma^2))}}
#'    \item \code{likelihoodRatioDistribution="exp"}: calculates the likelihood ratio for an exponentially distributed prior of \eqn{\vartheta} with mean \code{kappaLR}*\code{sqrt(firstStageInformation)} (\eqn{\eta}). The likelihood ratio is then calculated as:
#'          \deqn{l(p_1) = \eta \cdot \sqrt{2\pi} \cdot e^{(\Phi^{-1}(1-p_1)-\eta)^2/2} \cdot \Phi(\Phi^{-1}(1-p_1)-\eta)}
#'    \item \code{likelihoodRatioDistribution="unif"}: calculates the likelihood ratio for a uniformly distributed prior of \eqn{\vartheta} on the support \eqn{[0, \Delta\cdot\sqrt{I_1}]}, where \eqn{\Delta} is specified as \code{deltaMaxLR} and \eqn{I_1} is the \code{firstStageInformation}.
#'          \deqn{l(p_1) = \frac{\sqrt{2\pi}}{\Delta\cdot\sqrt{I_1}} \cdot e^{\Phi^{-1}(1-p_1)^2/2} \cdot (\Phi(\Delta\cdot\sqrt{I_1} - \Phi^{-1}(1-p_1))-p_1)}
#'    \item \code{likelihoodRatioDistribution="maxlr"}: the non-centrality parameter \eqn{\vartheta} is estimated from the data and no additional parameters must be specified. The likelihood ratio is estimated from the data as:
#'          \deqn{l(p_1) = e^{max(0, \Phi^{-1}(1-p_1))^2/2}}
#'          The maximum likelihood ratio is always restricted to effect sizes \eqn{\vartheta \geq 0} (corresponding to \eqn{p_1 \leq 0.5}).
#' }
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @return The value of the likelihood ratio for the given specification.
#' @export
#'
#' @examples
#' # Get a design
#' design <- getDesignOptimalConditionalErrorFunction(
#' alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#' delta1 = 0.25, likelihoodRatioDistribution = "fixed", deltaLR = 0.25,
#' firstStageInformation = 80, useInterimEstimate = FALSE,
#' )
#'
#' getLikelihoodRatio(firstStagePValue = c(0.05, 0.1, 0.2), design = design)
#'
#' @template reference_optimal
#' @references Hung, H. M. J., O’Neill, R. T., Bauer, P. & Kohne, K. (1997). The behavior of the p-value when the alternative hypothesis is true. Biometrics. https://www.jstor.org/stable/2533093

getLikelihoodRatio <- function(firstStagePValue, design) {
  # Initialise likelihood ratio
  likelihoodRatio <- NA
  # Fixed effect case
  if (design$likelihoodRatioDistribution == "fixed") {
    # Get ncp and weights

    # Ensure that ncp argument is provided
    # This is a fallback protection, as the design function should already ensure this
    if (is.null(design$deltaLR)) {
      stop(
        "Argument deltaLR required for fixed likelihood case, but not found in design object."
      )
    }

    ncp <- design$deltaLR * sqrt(design$firstStageInformation)
    weights <- design$weightsDeltaLR

    # If weights argument was not specified, automatically use equal weights
    if (is.null(weights)) {
      weights <- rep(1 / length(ncp), length(ncp))
    }

    # Ensure that weights are positive and sum up to 1
    # This is a fallback protection, as the design function should already ensure this
    if (sum(weights) != 1 || any(weights < 0)) {
      stop("weightsDeltaLR must be positive and sum up to 1")
    }

    # Calculate likelihood ratio
    likelihoodRatio <- exp(qnorm(1 - firstStagePValue) * ncp - ncp^2 / 2) %*%
      weights
  } else if (design$likelihoodRatioDistribution == "normal") {
    # Normal prior
    # Get ncp and tau

    # Ensure that arguments were specified
    # This is a fallback protection, as the design function should already ensure this
    if (is.null(design$deltaLR) || is.null(design$tauLR)) {
      stop(
        "Arguments deltaLR and tauLR required for normal likelihood case, but not found in design object."
      )
    }

    ncp <- design$deltaLR * sqrt(design$firstStageInformation)
    tau <- design$tauLR * sqrt(design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- (1 / sqrt(1 + tau^2)) *
      exp(
        -(ncp / tau)^2 /
          2 +
          (tau * qnorm(1 - firstStagePValue) + (ncp / tau))^2 /
            (2 * (1 + tau^2))
      )
  } else if (design$likelihoodRatioDistribution == "exp") {
    # Exponential prior
    # Get ncp

    # Ensure that argument was specified
    # This is a fallback protection, as the design function should already ensure this
    if (is.null(design$kappaLR)) {
      stop(
        "Argument kappaLR required for exponential likelihood case, but not found in design object."
      )
    }

    ncp <- design$kappaLR * sqrt(design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- sqrt(2 * pi) *
      ncp *
      exp((stats::qnorm(1 - firstStagePValue) - ncp)^2 / 2) *
      stats::pnorm(stats::qnorm(1 - firstStagePValue) - ncp)
  } else if (design$likelihoodRatioDistribution == "unif") {
    # Uniform prior
    # Get ncp

    # Ensure that argument was specified
    # This is a fallback protection, as the design function should already ensure this
    if (is.null(design$deltaMaxLR)) {
      stop(
        "Argument deltaMaxLR required for uniform likelihood case, but not found in design object."
      )
    }

    ncp <- design$deltaMaxLR * sqrt(design$firstStageInformation)

    # Calculate likelihood ratio
    likelihoodRatio <- sqrt(2 * pi) *
      exp(stats::qnorm(1 - firstStagePValue)^2 / 2) *
      (stats::pnorm(ncp - stats::qnorm(1 - firstStagePValue)) -
        firstStagePValue) /
      ncp
  } else if (design$likelihoodRatioDistribution == "maxlr") {
    # Maximum likelihood ratio case
    # Calculate likelihood ratio
    likelihoodRatio <- exp(max(0, stats::qnorm(1 - firstStagePValue))^2 / 2)
  } else {
    stop("Distribution not matched.")
  }

  # Return likelihood ratio
  return(unname(likelihoodRatio))
}

getLikelihoodRatio <- Vectorize(getLikelihoodRatio, "firstStagePValue")
