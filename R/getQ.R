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
#'
#' @return Ratio of likelihood ratio and squared effect size.
#' @export
#'
#'
#' @references Brannath, W. & Dreher, M. (2024). Optimal monotone conditional error functions. https://arxiv.org/abs/2402.00814

getQ <- function(firstStagePValue, design) {

  effect <- NA
  likelihoodRatio <- NA

  if(!is.null(design$ncp1)) {
    effect <- design$ncp1
  }
  else if(!is.null(design$ncp0)) {
    effect <- design$ncp0

    effect <- stats::qnorm(1-firstStagePValue)

    if(effect < design$ncp0) {
      effect <- design$ncp0
    }
  }
  else {
    stop("Effect must be specified in design object either via a fixed ncp1 or a minimum ncp0")
  }

  likelihoodRatio <- getLikelihoodRatio(firstStagePValue = firstStagePValue, design = design)

  Q <- likelihoodRatio/(effect^2)

  return(Q)
}

getQ <- Vectorize(getQ, vectorize.args = "firstStagePValue")
