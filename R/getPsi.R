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

#
# getPsi <- function(nuPrime, design){
#
# if(design$conditionalPower > pnorm(2) | design$conditionalPower < 1 - pnorm(2)){
#  u_01 <- 1-pnorm(-qnorm(design$conditionalPower)/2+sqrt(qnorm(design$conditionalPower)^2/4-1))
#  u_02 <- 1-pnorm(-qnorm(design$conditionalPower)/2-sqrt(qnorm(design$conditionalPower)^2/4-1))
#  x <- exp(design$levelConstant)/Q
#  if(-x < getNuPrime(alpha = u_02, conditionalPower = design$conditionalPower)){
#
#    rootlist <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
#            lower = 0, upper = u_01, tol = 1e-16)
#    return(rootlist$root)
#
#  } else if (-x > getNuPrime(alpha = u_01, conditionalPower = design$conditionalPower)){
#
#    rootlist <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
#            lower = u_02, upper = design$conditionalPower, tol = 1e-16)
#    return(rootlist$root)
#
#  }
#  else{
#    #Berechne Aopt_1 und Aopt_2
#    rootlist1 <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
#    lower = 0, upper = u_01, tol = 1e-16)
#    psi_lower <- rootlist1$root
#    rootlist2 <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
#                         lower = u_02, upper = design$conditionalPower, tol = 1e-16)
#    psi_upper <- rootlist2$root
#    quotient <- getNu(alpha = psi_lower, conditionalPower = conditionalPower) - getNu(alpha = psi_upper, conditionalPower = conditionalPower)/(psi_upper - psi_lower)
#    if (quotient > x){
#      return(psi_lower)
#    } else {
#      return(psi_upper)
#    }
#  }
# } else {
#    rootlist <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
#                       lower = 0, upper = design$conditionalPower, tol = 1e-16)
#    return(rootlist$root)
# }
# }
