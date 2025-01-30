#' @param distDelta Distribution under which to calculate the expected sample size, passed to \code{getLikelihoodRatio()}. Specified in the same way as \code{likelihoodRatioDistribution}, but values are allowed to be different. \cr
#' Options are \code{"fixed", "normal", "exp", "unif", "maxlr"} for fixed, normally distributed, exponentially distributed, uniformly distributed prior in the likelihood ratio and maximum likelihood ratio, respectively.
#' Each case requires different additional specifications. \code{"fixed"} requires the parameter \code{deltaLR} which provides the mean difference under which to calculate the likelihood ratio.
#' If \code{deltaLR} contains multiple values, they can be weighted using an additional argument \code{weightsDeltaLR}. \code{"normal"} requires parameters \code{deltaLR} and \code{tauLR} for the mean and standard deviation of the normal distribution (both on mean difference scale).
#' \code{"exp"} requires the parameter \code{kappaLR} which is the mean of the exponential distribution (on the mean difference scale).
#' \code{"unif"} requires the specification of \code{deltaMaxLR}, which is the maximum of the support for the uniform likelihood ratio distribution (on the mean difference scale).
#' By default, \code{distDelta = NULL}, in which case the likelihood ratio specification of \code{design} is used to calculate the expected second-stage information.

