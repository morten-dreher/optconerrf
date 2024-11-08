#' Calculate Non-centrality Parameter
#' @name getNonCentralityParameter
#'
#' @description Calculates the non-centrality parameter for given design parameters.
#'
#' @details The non-centrality parameter \eqn{\vartheta} is calculated as
#' \deqn{\vartheta = \frac{\Delta}{\sigma}\cdot \sqrt{\frac{n_t}{r+1}},}
#'    where
#'    \itemize{
#'        \item \eqn{\Delta} is the effect size, specified as a mean difference (argument \code{delta})
#'        \item \eqn{\sigma} is the standard deviation of the effect (equal between groups, argument \code{standardDeviation})
#'        \item \eqn{n_t} is the sample size of the treatment group (argument \code{firstStageSampleSize})
#'        \item \eqn{r} is the allocation ratio between the groups (\eqn{r=n_t/n_c}, argument \code{allocationRatio}).
#'    }
#'    A minimum effect size (mean difference scale) can be specified via the argument \code{delta0}.
#'
#'
#' @template param_delta
#' @template param_firstStageSampleSize
#' @template param_standardDeviation
#' @template param_allocationRatio
#' @template param_delta0
#'
#' @return (Truncated) Non-centrality parameter.
#'
#' @examples
#' getNonCentralityParameter(
#' delta=0.25, firstStageSampleSize=100)
#'
#' @export
getNonCentralityParameter <- function(delta, firstStageSampleSize, standardDeviation = 1, allocationRatio = 1, delta0 =-Inf) {
  if(delta < delta0) {
    delta <- delta0
  }
  return((delta/standardDeviation)*sqrt(firstStageSampleSize/(allocationRatio + 1)))
}

getNonCentralityParameter <- Vectorize(FUN = getNonCentralityParameter, vectorize.args = c("delta", "firstStageSampleSize"))
