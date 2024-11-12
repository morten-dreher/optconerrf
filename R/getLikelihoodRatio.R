#' Calculate Likelihood Ratio
#'
#' @name getLikelihoodRatio
#'
#' @description Calculate the likelihood ratio of a p-value for a given distribution.
#'
#' @details The calculation of the likelihood ratio for a first-stage p-value \eqn{p_1} is done based on a distributional assumption, specified via the argment \code{dist}.
#' The different options require different parameters, elaborated in the following.
#' \itemize{
#'    \item \code{dist="fixed"}: calculates the likelihood ratio for a fixed \eqn{\vartheta}. The non-centrality parameter of the likelihood ratio \eqn{\vartheta} must then be specified via \code{ncpLR} to calculate the likelihood ratio as:
#'          \deqn{l(p_1) = e^{\Phi^{-1}(1-p_1)\vartheta - \vartheta^2/2}}
#'    \item \code{dist="normal"}: calculates the likelihood ratio for a normally distributed prior of \eqn{\vartheta} with mean \code{ncpLR} (\eqn{\vartheta}) and standard deviation \code{tauLR} (\eqn{\tau}). Both parameters must be specified on the non-centrality parameter scale.
#'          \deqn{l(p_1) = (1+\tau^2)^{-\frac{1}{2}}\cdot e^{-(\vartheta/\tau)^2/2 + (\tau\Phi^{-1}(1-p_1) + \vartheta/\tau)^2 / (2\cdot (1+\tau^2))}}
#'    \item \code{dist="exp"}: calculates the likelihood ratio for an exponentially distributed prior of \eqn{\vartheta} with parameter \code{kap0} (\eqn{\kappa}), which is the mean of the exponential distribution, calculated as \eqn{1/\vartheta} as:
#'          \deqn{l(p_1) = \kappa \cdot \sqrt{2\pi} \cdot e^{(\Phi^{-1}(1-p_1)-\kappa)^2/2} \cdot \Phi(\Phi^{-1}(1-p_1)-\kappa)}
#'    \item \code{dist="unif"}: calculates the likelihood ratio for a uniformly distributed prior of \eqn{\vartheta} on the support \eqn{[0, \Delta]}, where \eqn{\Delta} is specified as \code{delMax}.
#'          \deqn{l(p_1) = \frac{\sqrt{2\pi}}{\Delta} \cdot e^{\Phi^{-1}(1-p_1)^2/2} \cdot (\Phi(\Delta - \Phi^{-1}(1-p_1))-p_1)}
#'    \item \code{dist="maxlr"}: the non-centrality parameter \eqn{\vartheta} is estimated from the data and no additional parameters must be specified. The likelihood ratio is estimated from the data as:
#'          \deqn{l(p_1) = e^{max(0, \Phi^{-1}(1-p_1))^2/2}}
#'          The maximum likelihood ratio is always restricted to effect sizes \eqn{\vartheta \geq 0}. (respectively \eqn{p_1 \leq 0.5}).
#' }
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @return The value of the likelihood ratio for the given specification.
#' @export
#'
#' @references Brannath, W. & Bauer, P. (2004). Optimal conditional error functions for the control of conditional power. Biometrics, 60 (3), 715–723. https://doi.org/10.1111/j.0006-341X.2004.00221.x
#' @references Hung, H. M. J., O’Neill, R. T., Bauer, P. & Kohne, K. (1997). The behavior of the p-value when the alternative hypothesis is true. Biometrics. http://www.jstor.org/stable/2533093

getLikelihoodRatio <- function(firstStagePValue, design) {
  # initialise likelihood ratio, get ... arguments
  likelihoodRatio <- NA
  # Fixed likelihood case
  if(design$dist == "fixed"){

    # Get ncp and weights
    ncp <- design$ncpLR
    weights <- design$weights

    # Ensure that ncp argument is provided
    if(is.null(ncp)) {
      stop("Argument ncpLR must be provided for fixed likelihood case")
    }

    # If weights argument was not specified, automatically use equal weights
    if(is.null(weights)) {
      weights <- rep(1/length(ncp), length(ncp))
    }

    # Ensure that weights sum up to 1
    if(sum(weights) != 1) {
      stop("weights must sum up to 1")
    }

    # Calculate likelihood ratio
    likelihoodRatio <- exp(qnorm(1-firstStagePValue)*ncp-ncp^2/2) %*% weights
  }
  # Normal prior
  else if(design$dist == "normal") {

    # Get ncp and tau
    ncp <- design$ncpLR
    tau <- design$tauLR

    # Ensure that arguments were specified
    if(is.null(ncp) || is.null(tau)) {
      stop("Arguments ncpLR and tauLR must be specified for normal likelihood case")
    }

    # Calculate likelihood ratio
    likelihoodRatio <- (1/sqrt(1+tau^2))*exp(-(ncp/tau)^2/2+(tau*qnorm(1-firstStagePValue)+(ncp/tau))^2/(2*(1+tau^2)))
  }
  # Exponential prior
  else if(design$dist == "exp"){
    # Get kap0
    kap0 <- design$kap0

    # Ensure that argument was specified
    if(is.null(kap0)) {
      stop("Argument kap0 must be specified for exponential likelihood case")
    }

    # Calculate likelihood ratio
    likelihoodRatio <- sqrt(2*pi)*kap0*exp((qnorm(1-firstStagePValue)-kap0)^2/2)*pnorm(qnorm(1-firstStagePValue)-kap0)
  }
  # Uniform prior
  else if(design$dist == "unif"){
    # Get delMax
    delMax <- design$delMax

    # Ensure that argument was specified
    if(is.null(delMax)) {
      stop("Argument delMax must be specified for uniform likelihood case")
    }

    # Calculate likelihood ratio
    likelihoodRatio <- sqrt(2*pi)*exp(qnorm(1-firstStagePValue)^2/2)*(pnorm(delMax-qnorm(1-firstStagePValue))-firstStagePValue)/delMax
  }
  # Maximum likelihood ratio case
  else if(design$dist == "maxlr") {
    # Calculate likelihood ratio
    likelihoodRatio <- exp(max(0, qnorm(1-firstStagePValue))^2/2)
  }
  else {
    stop("Distribution not matched.")
  }

  # Return likelihood ratio
  return(unname(likelihoodRatio))
}

getLikelihoodRatio <- Vectorize(getLikelihoodRatio, "firstStagePValue")
