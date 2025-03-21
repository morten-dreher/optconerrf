#' Calculate the Derivate of Nu
#' @name getNuPrime
#'
#' @description Calculates the derivative of nu for a given conditional error and conditional power.
#'
#' @details The function \eqn{\nu'} is defined as
#' \deqn{\nu'(p_1) = -2 \cdot (\Phi^{-1}(1-\alpha_2(p_1)) + \Phi^{-1}(CP))/\phi(\Phi^{-1}(1-\alpha_2(p_1))).}
#' Note that in this implementation, the the factor -2 is used instead of -4, which is used in by Brannath & Bauer (2004), who explicitly investigate the setting of a balanced two-group trial.
#'
#' @template param_alpha_cerr
#' @template param_conditionalPower
#'
#' @return Value for nu prime.
#' @export
#'
#' @examples
#' getNuPrime(alpha = 0.05, conditionalPower = 0.9)
#'
#' @references Brannath, W. & Bauer, P. (2004). Optimal conditional error functions for the control of conditional power. Biometrics, 60 (3), 715–723. https://doi.org/10.1111/j.0006-341X.2004.00221.x
#'
getNuPrime <- function(alpha, conditionalPower) {

    return(-2*(qnorm(1-alpha)+qnorm(conditionalPower))/dnorm(qnorm(1-alpha)))
}

getNuPrime <- Vectorize(FUN = getNuPrime, vectorize.args = "alpha")
