#' Calculate Inverse Normal Combination Function
#' @name getInverseNormalCombinationFunction
#'
#' @description Calculate the result of the inverse normal combination function.
#'
#' @template param_firstStagePValue
#' @param secondStagePValue p-value of the second stage.
#' @template param_weights_stages
#'
#' @return Value of the inverse normal combination function.
#'
#' @export
#'
#' @details
#' The inverse normal combination function for stage-wise p-values \eqn{p_1, p_2} is provided in Wassmer & Brannath (2016) as:
#' \deqn{
#' C(p_1,p_2) = 1-\Phi(w_1\Phi^{-1}(1-p_1)+w_2\Phi^{-1}(1-p_2)),
#' }
#' where \eqn{w_1, w_2} are positive stage-wise weights subject to \eqn{w_1^2+w_2^2=1}.
#'
#' @references Wassmer, G. & Brannath, W. (2016). Group sequential and confirmatory adaptive designs in clinical trials. Springer. https://doi.org/https://doi.org/10.1007/978-3-319-32562-0
#'
#' @examples
#' getInverseNormalCombinationFunction(
#' firstStagePValue=0.1, secondStagePValue=0.08)
#'
getInverseNormalCombinationFunction <- function(firstStagePValue, secondStagePValue, weights = 1/sqrt(c(2, 2))) {
  return(1-pnorm(weights[1]*qnorm(1-firstStagePValue) + weights[2]*qnorm(1-secondStagePValue)))
}

getInverseNormalCombinationFunction <- Vectorize(FUN = getInverseNormalCombinationFunction, vectorize.args = c("firstStagePValue", "secondStagePValue"))
