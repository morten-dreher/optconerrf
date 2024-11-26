#' Return Monotone Function Values
#'
#' @name getMonotoneFunction
#'
#' @description Applies the provided monotonisation constants to a specified, possibly non-monotone function. The returned function values are non-increasing.
#' If the constants are not specified, they are determined automatically.
#'
#' @details The exact monotonisation process is outlined in Brannath & Dreher (2024), but specified in terms of the first-stage test statistic \eqn{z_1} rather than the first-stage p-value \eqn{p_1}. \cr
#' The algorithm can easily be translated to the use of p-values by switching the maximum and minimum functions, i.e., replacing \eqn{min\{q, Q(z_1)\}} by \eqn{max\{q, Q(p_1)\}} and \eqn{min\{q, Q(z_1)\}} by \eqn{max\{q, Q(p_1\}}.
#'
#' @param x Argument values.
#' @template param_fun_mono
#' @template param_lower_mono
#' @template param_upper_mono
#' @template param_argument_mono
#' @template param_nSteps_mono
#' @template param_epsilon_mono
#' @template param_numberOfIterationsQ
#' @template param_design
#' @param printConstant Logical. Specifies whether the intervals and constants should be printed.
#'
#' @return Monotone function values.
#' @export
#'
#'
#' @references Brannath, W. & Dreher, M. (2024). Optimal monotone conditional error functions. https://arxiv.org/abs/2402.00814

getMonotoneFunction <- function(x, fun, lower=NULL, upper=NULL, argument=NULL, nSteps = 10^4, epsilon = 10^(-5), numberOfIterationsQ = 10^4, design, printConstant = TRUE) {

  # If monotonisation is enforced, do it
  if(design$enforceMonotonicity) {
    out <- design$monotonisationConstants
  }
  else {
    out <- list()
  }

  # If the length of object out is 0, the function is already non-increasing
  if(length(out)==0) {
    modFunctionValues <- fun(x, design = design)
  }
  # If the length of out is larger than 0, a transformation is necessary
  else {
    pos_lower <- apply(outer(x, out$dls, ">="), 1, sum)
    pos_upper <- apply(outer(x, out$dus, ">"), 1, sum) + 1
    modFunctionValues <- ifelse(pos_lower == pos_upper, yes = out$qs[pmax(1, pos_lower)], fun(x, design = design))
  }
  return(modFunctionValues)
}
