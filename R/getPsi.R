#' Calculate Psi, the Inverse of Nu Prime
#' @name getPsi
#'
#' @description Get point-wise values of psi (inverse of nu prime)
#'
#' @param nuPrime The function value to be inverted.
#' @template param_conditionalPower
#'
#' @return The value of alpha which corresponds to nuPrime and lies between 0 and \code{conditionalPower}.
#' @export
#'
#' @details
#' The function \eqn{\psi} is the inverse of:
#' \deqn{\nu'(\alpha) = -2 \cdot(\Phi^{-1}(1-\alpha) + \Phi^{-1}(1-CP)) / \phi(\Phi^{-1}(1-\alpha))}.
#' If the conditional power \eqn{CP} lies outside of the range \eqn{1-\Phi(2) \leq CP \leq \Phi(2)}, the calculation is slightly more complicated.
#' The argument \code{conditionalPower} is either the fixed target conditional power or the value of the conditional power function at the corresponding first-stage p-value.
#'
#' @examples
#' # Returns 0.05
#' getPsi(getNuPrime(alpha = 0.05, conditionalPower = 0.9), conditionalPower = 0.9)


getPsi <- function(nuPrime, conditionalPower, design, firstStagePValue){

 # If the conditional power is between 1-pnorm(2) and pnorm(2) nu prime is monotone and we can build the inverse directly
 if((pnorm(-2) <= conditionalPower & conditionalPower <= pnorm(2))){
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
     #In this case the constraints already need to be considered when building the inverse
     C_max <- getConstraintC_max(firstStagePValue = firstStagePValue, design = design)
     C_min <- getConstraintC_min(firstStagePValue = firstStagePValue, design = design)

     # Calculate psi_lower_c and psi_upper_c
     rootlist1 <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
                          lower = 0, upper = u_max, tol = 1e-16)
     psi_lower_c <- rootlist1$root
     psi_lower_c <- pmax(C_min, pmin(C_max, rootlist1$root))
     rootlist2 <- uniroot(f=function(alpha){getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime},
                          lower = u_min, upper = conditionalPower, tol = 1e-16)
     psi_upper_c <- pmax(C_min, pmin(C_max, rootlist2$root))
     psi_upper_c <- rootlist2$root
     # Calculate the quotient that is needed to decide if psi_lower_c or psi_upper_c is used
     quotient <- getNu(alpha = psi_upper_c, conditionalPower = conditionalPower) -
          getNu(alpha = psi_lower_c, conditionalPower = conditionalPower)/(psi_upper_c - psi_lower_c)
     if (quotient <= nuPrime){
       return(psi_upper_c)
     } else {
       return(psi_lower_c)
     }
   }
 }
}

getPsi <- Vectorize(FUN = getPsi, vectorize.args = c("nuPrime", "conditionalPower"))
