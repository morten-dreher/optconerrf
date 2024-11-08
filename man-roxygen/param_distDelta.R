#' @param distDelta Distribution under which to calculate the expected sample size, passed to \code{getLikelihoodRatio()}. Specified in the same way as \code{dist}, but values are allowed to be different. \cr
#' Options are \code{"fixed", "normal", "exp", "unif", "maxlr"} for fixed, normally distributed, exponentially distributed, uniformly distributed and maximum likelihood ratio, respectively.
#' Each case requires different additional specifications. \code{"fixed"} requires the parameter \code{ncpDelta} which provides the non-centrality parameter under which to calculate the likelihood ratio.
#' If \code{ncpLR} contains multiple values, they can be weighted using an additional argument \code{weightsDelta}. \code{"normal"} requires parameters \code{ncpDelta} and \code{tauDelta} for the mean and standard deviation of the normal distribution (both on non-centrality parameter scale).
#' \code{"exp"} requires the parameter \code{kap0Delta} which is the mean of the exponential distribution (on the non-centrality parameter scale).
#' \code{"unif"} requires the specification of \code{delMaxDelta}, which is the maximum of the support for the uniform likelihood ratio distribution (on the non-centrality parameter scale).

