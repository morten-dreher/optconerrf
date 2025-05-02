#' Calculate Q
#' @name getQ
#' @description Calculate the ratio of likelihood ratio and squared effect size.
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @details For more information on how to specify the likelihood ratio, see \code{?getLikelihoodRatio()}.
#' In case the optimal conditional error function is ever increasing in the first-stage p-value \eqn{p_1}, a monotone transformation of \code{getQ()}
#' is needed for logical consistency and type I error rate control. \cr
#' The formula for \eqn{Q(p_1)} is:
#' \deqn{Q(p_1) = l(p_1) / \Delta_1^2,}
#' where \eqn{l(p_1)} is the likelihood ratio and \eqn{\Delta_1} is the effect size at which the conditional power should be achieved.
#' The effect size may also depend on the interim data (i.e., on \eqn{p_1}) in case \code{useInterimEstimate = TRUE} was specified for the design object.
#'
#' @importFrom stats qnorm
#'
#' @return Ratio of likelihood ratio and squared effect size.
#' @export
#'
#' @template reference_monotone

getQ <- function(firstStagePValue, design) {

  # Initialise effect and likelihood ratio
  effect <- NA
  likelihoodRatio <- NA

  # When using interim estimate, apply the restrictions given in the design
  if(design$useInterimEstimate) {
    effect <- min(max(design$ncp1Min, stats::qnorm(1-firstStagePValue)), design$ncp1Max)/sqrt(design$firstStageInformation)
  }
  # Fixed effect case
  else {
    effect <- design$delta1
  }

  likelihoodRatio <- getLikelihoodRatio(firstStagePValue = firstStagePValue, design = design)

  Q <- likelihoodRatio/(effect^2)

  return(Q)
}

getQ <- Vectorize(getQ, vectorize.args = "firstStagePValue")
