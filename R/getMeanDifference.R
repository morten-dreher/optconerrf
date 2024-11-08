#' Calculate Mean Difference from Non-centrality Parameter
#'
#' @name getMeanDifference
#'
#' @description Calculate the mean difference from specified design parameters.
#'
#' @details The calculation of the mean difference \eqn{\Delta} is done as:
#' \deqn{\Delta = \vartheta \cdot \sigma \cdot \sqrt{\frac{r+1}{n_t}},}
#' where
#'    \itemize{
#'        \item \eqn{\vartheta} is the effect size, specified as a non-centrality parameter (argument \code{ncp})
#'        \item \eqn{\sigma} is the standard deviation of the effect (equal between groups, argument \code{standardDeviation})
#'        \item \eqn{n_t} is the sample size of the treatment group (argument \code{firstStageSampleSize})
#'        \item \eqn{r} is the allocation ratio between the groups (\eqn{r=n_t/n_c}, argument \code{allocationRatio}).
#'    }
#'    A minimum effect size (non-centrality parameter scale) can be specified via the argument \code{ncp0}.
#'
#' @param ncp Treatment effect as a non-centrality parameter. Must be a numeric value.
#' @template param_firstStageSampleSize
#' @template param_standardDeviation
#' @template param_allocationRatio
#' @template param_ncp0
#'
#' @return Mean difference for a specified non-centrality parameter.
#' @export
#'
#' @examples
#' getMeanDifference(
#' ncp=1.768, firstStageSampleSize=100)
#'
getMeanDifference <- function(ncp, firstStageSampleSize, standardDeviation = 1, allocationRatio = 1, ncp0 = -Inf) {
  ncp <- pmax(ncp0, ncp)
  delta <- (ncp*standardDeviation)/sqrt(firstStageSampleSize/(allocationRatio+1))
  return(delta)
}

getMeanDifference <- Vectorize(FUN = getMeanDifference, vectorize.args = c("ncp"))
