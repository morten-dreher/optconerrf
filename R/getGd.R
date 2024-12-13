#' Calculate the integral that should be maximized
#' @name getGd
#'
#' @description Calculate the integral that should be maximized
#'
#'
#'

getGd<- function (Aopt, design){
  #needs package cubature, Q muss vermutlich noch anders berechnet werden, siehe Funktion getOptimalConditionalError
  f <- function(x){getNuPrime(x[1])*getQ(x[2])+exp(design$levelConstant)}
  adaptIntegrate(f, lowerLimit = c(design$alpha1, Aopt), upperLimit = c(design$alpha0, design$conditionalPower))
}
