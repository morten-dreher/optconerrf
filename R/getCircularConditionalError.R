#' Get Circular Conditional Error
#'
#' @name getCircularConditionalError
#'
#' @description Calculate circular conditional error according to Proschan & Hunsberger (1995).
#'
#' @details For specified early decision boundaries \eqn{\alpha_0, \alpha_1}, the circular conditional error function is calculated for a first-stage p-value \eqn{p_1 \in ]\alpha_1, \alpha_0]} as:
#' \deqn{\alpha_2(p_1) = 1-\Phi(\sqrt{(\Phi^{-1}(1-\alpha_1))^2 - (\Phi^{-1}(1-p_1))^2}).}
#' For a tabulation of early decision boundaries, refer to Wassmer & Brannath (2016).
#'
#' @template param_firstStagePValue
#' @template param_alpha1
#' @template param_alpha0
#'
#' @return Value of the circular conditional error function.
#' @export
#'
#' @examples
#' getCircularConditionalError(firstStagePValue=0.1, alpha1=0.01170, alpha0=0.5)
#'
#' @references Proschan, M. A., & Hunsberger, S. A. (1995). Designed extension of studies based on conditional power. Biometrics. http://www.jstor.org/stable/2533262
#' @references Wassmer, G., & Brannath, W. (2016). Group sequential and confirmatory adaptive designs in clinical trials. Springer. https://doi.org/https://doi.org/10.1007/978-3-319-32562-0

getCircularConditionalError <- function(firstStagePValue, alpha1, alpha0) {

  circularConditionalError <- NA
  if(firstStagePValue <= alpha1) {
    circularConditionalError <- 1
  }
  else if((firstStagePValue > alpha1)  && (firstStagePValue <= alpha0)) {
    circularConditionalError <- 1-pnorm(sqrt((qnorm(1-alpha1))^2 - (qnorm(1-firstStagePValue))^2))
  }
  else if(firstStagePValue > alpha0) {
    circularConditionalError <- 0
  }

  return(circularConditionalError)
}

getCircularConditionalError <- Vectorize(FUN = getCircularConditionalError, vectorize.args = c("firstStagePValue"))
