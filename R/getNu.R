#' Calculate Nu
#' @name getNu
#'
#' @description Calculate the factor which relates \eqn{\alpha_2} to the second-stage information for given conditional power.
#'
#' @details Note that this function uses factor 1 instead of factor 2 (Brannath & Bauer 2004). This has no impact on the optimal conditional error function, as constant factors are absorbed by the level constant \eqn{c_0}. \cr
#' The calculation is:
#' \deqn{\nu(\alpha_2(p_1)) = (\Phi^{-1}(1-\alpha_2(p_1)) + \Phi^{-1}(CP))^2.}
#' @template param_alpha_cerr
#' @template param_conditionalPower
#'
#' @return Factor linking information and \eqn{\alpha_2}.
#' @export
#'
#' @examples
#' getNu(alpha = 0.05, conditionalPower = 0.9)
#'
#' # Returns 0 if alpha exceeds conditionalPower
#' getNu(alpha = 0.8, conditionalPower = 0.7)
#'
#' @template reference_optimal

getNu <- function(alpha, conditionalPower) {
  nu <- 0
  if(any(alpha > conditionalPower)) {
    warning("alpha/conditional error should not exceed conditionalPower. Information is otherwise 0")
  }
  else{
    nu <- (qnorm(1-alpha)+qnorm(conditionalPower))^2
  }
  return(nu)
}

getNu <- Vectorize(FUN = getNu, vectorize.args = c("alpha", "conditionalPower"))
