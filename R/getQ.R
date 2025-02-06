#' Calculate Q
#' @name getQ
#' @description Calculate the ratio of likelihood ratio and squared effect size.
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @details For more information on how to specify the likelihood ratio, see \code{?getLikelihoodRatio()}.
#' In case the optimal conditional error function is ever increasing in the first-stage p-value \eqn{p_1}, a monotone transformation of \code{getQ()}
#' is needed for logical consistency and type I error rate control. See \code{?getMonotonisationConstants()} for more information. \cr
#'
#' @importFrom stats qnorm
#'
#' @return Ratio of likelihood ratio and squared effect size.
#' @export
#'
#'
#' @references Brannath, W. & Dreher, M. (2024). Optimal monotone conditional error functions. https://arxiv.org/abs/2402.00814

getQ <- function(firstStagePValue, design) {

  effect <- NA
  likelihoodRatio <- NA

  if(design$useInterimEstimate) {
    effect <- min(max(design$ncp1Min, stats::qnorm(1-firstStagePValue)), design$ncp1Max)
  }
  else {
    effect <- design$ncp1
  }

  likelihoodRatio <- getLikelihoodRatio(firstStagePValue = firstStagePValue, design = design)

  Q <- likelihoodRatio/(effect^2)

  return(Q)
}

getQ <- Vectorize(getQ, vectorize.args = "firstStagePValue")
