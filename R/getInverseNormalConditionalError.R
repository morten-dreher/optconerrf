#' Calculate Inverse Normal Conditional Error
#'
#' @name getInverseNormalConditionalError
#'
#' @description Calculate the conditional error of the inverse normal method.
#'
#' @details The inverse normal conditional error is calculated for a first-stage p-value \eqn{p_1 \in ]\alpha_1, \alpha_0]} as:
#' \deqn{\alpha_2(p_1) = 1-\Phi(\frac{\Phi^{-1}(1-\alpha_2) - w_1 \Phi^{-1}(1-p_1)}{w_2}),}
#' where
#' \itemize{
#'    \item \eqn{\alpha_2} is the second-stage significance level (see `getAlpha2InverseNormal()`)
#'    \item \eqn{w_1, w_2} are the stage-wise weights
#' }
#'
#' @template param_alpha
#' @template param_alpha1
#' @template param_alpha0
#' @template param_firstStagePValue
#' @template param_weights_stages
#' @template param_alpha2_specify
#'
#' @return Conditional error of the inverse normal combination function.
#'
#' @export
#'
#' @examples
#' getInverseNormalConditionalError(
#' alpha=0.025, alpha0=0.5, alpha1=0.001, firstStagePValue=0.09)
#'
#' @references Wassmer, G. & Brannath, W. (2016). Group sequential and confirmatory adaptive designs in clinical trials. Springer. https://doi.org/https://doi.org/10.1007/978-3-319-32562-0
#'
getInverseNormalConditionalError <- function(alpha, alpha0, alpha1, firstStagePValue, weights = c(sqrt(0.5), sqrt(0.5)), alpha2 = NULL) {

  if(is.null(alpha2)) {
    alpha2 <- getAlpha2InverseNormal(alpha = alpha, alpha1 = alpha1, alpha0 = alpha0, weights = weights)
  }

  conditionalError <- NA
  if(firstStagePValue <= alpha1) {
    conditionalError <- 1
  }
  else if(firstStagePValue > alpha0) {
    conditionalError <- 0
  }
  else if(firstStagePValue <= alpha0 && firstStagePValue > alpha1){
    conditionalError <- 1-pnorm((qnorm(1-alpha2)-weights[1]*qnorm(1-firstStagePValue))/weights[2])
  }
  return(conditionalError)
}

getInverseNormalConditionalError <- Vectorize(FUN = getInverseNormalConditionalError, vectorize.args = c("firstStagePValue"))
