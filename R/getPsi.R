#' Calculate Psi, the Inverse of Nu Prime
#' @name getPsi
#'
#' @description Get point-wise values of psi (inverse of nu prime)
#'
#' @param nuPrime The function value to be inverted.
#' @template param_conditionalPower
#'
#' @return The value of alpha which corresponds to nuPrime and lies between 0 and conditionalPower.
#' @export
#'
#' @details
#' The function \eqn{psi} is the inverse of:
#' \deqn{\nu'(\alpha) = -2 \cdot(\Phi^{-1}(1-\alpha) + \Phi^{-1}(1-CP)) / \phi(\Phi^{-1}(1-\alpha))}.
#' It can only be found if the conditional power \eqn{CP} is restricted to values \eqn{1-\Phi(2) \leq CP \leq \Phi(2)}.
#'
#' @examples
#' getPsi(getNuPrime(alpha = 0.05, conditionalPower = 0.9), conditionalPower = 0.9)

getPsi <- function(nuPrime, conditionalPower) {

 rootlist <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
                     lower = 0, upper = conditionalPower, tol = 1e-16)
 return(rootlist$root)
}

getPsi <- Vectorize(FUN = getPsi, vectorize.args = c("nuPrime"))
