#' @param dist The distribution to be used for the effect size of the likelihood ratio in the optimal conditional error function. Options are \code{"fixed", "normal", "exp", "unif", "maxlr"} for fixed effect size, normally distributed, exponentially distributed, uniformly distributed prior of the effect size and maximum likelihood ratio, respectively.
#' Each case requires different additional specifications. \cr
#' \itemize{
#' \item \code{dist="fixed"} uses one (or more) fixed effect sizes for the likelihood ratio and requires the parameter \code{ncpLR} which provides the non-centrality parameter under which to calculate the likelihood ratio. If \code{ncpLR} contains multiple values, they may be weighted using an additional argument \code{weights}. Omitting \code{weights} automatically leads to equal weighting.
#' \item \code{dist="normal"} uses a normal prior for the effect size and requires parameters \code{ncpLR} and \code{tau} for the mean and standard deviation of the normal distribution (both on non-centrality parameter scale).
#' \item \code{dist="exp"} uses an exponential prior for the effect size and requires the parameter \code{kap0} which is the mean of the exponential distribution (on the non-centrality parameter scale).
#' \item \code{dist="unif"} uses a uniform prior for the effect size and requires the specification of \code{delMax}, which is the maximum of the support for the uniform likelihood ratio distribution (on the non-centrality parameter scale).
#' \item \code{dist="maxlr"} estimates the non-centrality parameter to be used for the likelihood ratio from the data. No additional parameters must be specified.
#' }
