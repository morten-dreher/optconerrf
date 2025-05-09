#' @param likelihoodRatioDistribution The distribution to be used for the effect size of the likelihood ratio in the calculation of the expected second-stage information. Options are \code{"fixed", "normal", "exp", "unif", "maxlr"} for fixed effect size, normally distributed, exponentially distributed, uniformly distributed prior of the effect size and maximum likelihood ratio, respectively.
#' Each case requires different additional specifications: \cr
#' \itemize{
#' \item \code{likelihoodRatioDistribution="fixed"} uses one (or more) fixed effect sizes for the likelihood ratio and requires the parameter \code{deltaLR} which provides the mean difference under which to calculate the likelihood ratio. If \code{deltaLR} contains multiple values, they may be weighted using an additional argument \code{weightsDeltaLR}. Omitting \code{weightsDeltaLR} automatically leads to equal weighting.
#' \item \code{likelihoodRatioDistribution="normal"} uses a normal prior for the effect size and requires parameters \code{deltaLR} and \code{tauLR} for the mean and standard deviation of the normal distribution (both on mean difference scale).
#' \item \code{likelihoodRatioDistribution="exp"} uses an exponential prior for the effect size and requires the parameter \code{kappaLR} which is the mean of the exponential distribution (on the mean difference scale).
#' \item \code{likelihoodRatioDistribution="unif"} uses a uniform prior for the effect size and requires the specification of \code{deltaMaxLR}, which is the maximum of the support for the uniform likelihood ratio distribution (on the mean difference scale).
#' \item \code{likelihoodRatioDistribution="maxlr"} estimates the non-centrality parameter to be used for the likelihood ratio from the data. No additional parameters must be specified.
#' }
#' The default is \code{likelihoodRatioDistribution=NULL}.
#' In this case, the likelihood ratio distribution under which the expected second-stage information is calculated is taken directly from the design object.
