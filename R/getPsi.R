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
#' If the conditional power \eqn{CP} lies outside of the range \eqn{1-\Phi(2) \leq CP \leq \Phi(2)}, the calculation is slightly more complicated.
#'
#' @examples
#' getPsi(getNuPrime(alpha = 0.05, conditionalPower = 0.9), conditionalPower = 0.9)


getPsi <- function(nuPrime, conditionalPower){

 # If the conditional power is between 1-pnorm(2) and pnorm(2) nu prime is monotone and we can build the inverse directly
 if(pnorm(-2) <= conditionalPower && conditionalPower <= pnorm(2)){
   rootlist <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
                       lower = 0, upper = conditionalPower, tol = 1e-16)
   return(rootlist$root)

 # If the conditional power is not between 1-pnorm(2) and pnorm(2) nu prime is not monotone and we need to build the inverse differently
 } else {
   # Calculate the minimum and the maximum of NuPrime(u)
   u_max <- 1-pnorm(-qnorm(conditionalPower)/2+sqrt(qnorm(conditionalPower)^2/4-1))
   u_min <- 1-pnorm(-qnorm(conditionalPower)/2-sqrt(qnorm(conditionalPower)^2/4-1))
   NuPrime_u_max <- getNuPrime(alpha = u_max, conditionalPower = conditionalPower)
   NuPrime_u_min <- getNuPrime(alpha = u_min, conditionalPower = conditionalPower)

   if(nuPrime > NuPrime_u_max){

     rootlist <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
                         lower = u_min, upper = conditionalPower, tol = 1e-16)
     return(rootlist$root)

   } else if (nuPrime < NuPrime_u_min){

     rootlist <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
                         lower = 0, upper = u_max, tol = 1e-16)
     return(rootlist$root)

   } else {

     # Calculate psi_lower and psi_upper
     rootlist1 <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
                          lower = 0, upper = u_max, tol = 1e-16)
     psi_lower <- rootlist1$root
     rootlist2 <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
                          lower = u_min, upper = conditionalPower, tol = 1e-16)
     psi_upper <- rootlist2$root
     # Calculate the quotient that is needed to decide if psi_lower or psi_upper is used
     quotient <- getNu(alpha = min(conditionalPower, psi_upper), conditionalPower = conditionalPower) -
          getNu(alpha = psi_lower, conditionalPower = conditionalPower)/(min(psi_upper, conditionalPower)- psi_lower)
     if (quotient <= nuPrime){
       return(psi_upper)
     } else {
       return(psi_lower)
     }
   }
 }
}

getPsi <- Vectorize(FUN = getPsi, vectorize.args = c("nuPrime"))
