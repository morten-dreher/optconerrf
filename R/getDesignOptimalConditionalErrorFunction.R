#' Create a design object for the optimal conditional error function.
#' @name getDesignOptimalConditionalErrorFunction
#'
#' @description This function returns a design object which contains all important parameters for the specification of the optimal conditional error function.
#' The returned object is of class \code{TrialDesignOptimalConditionalError} and can be passed to other package functions.
#'
#'
#' @details
#' The design object contains the information required to determine the specific setting of the optimal conditional error function and can be passed to other package functions.
#' From the given user specifications, the constant to achieve level condition for control of the overall type I error rate as well as the constants to ensure a non-increasing optimal CEF (if required) are automatically calculated.
#'
#' @section Likelihood ratio distribution:
#' To calculate the optimal conditional error function, an assumption about the true parameter under which the second-stage information is to be minimised is required.
#' Various options are available and can be specified via the argument \code{likelihoodRatioDistribution}:
#' \itemize{
#'    \item \code{likelihoodRatioDistribution="fixed"}: calculates the likelihood ratio for a fixed \eqn{\Delta}. The non-centrality parameter of the likelihood ratio \eqn{\vartheta} is then computed as \code{deltaLR}*\code{sqrt(firstStageInformation)} and the likelihood ratio is calculated as:
#'          \deqn{l(p_1) = e^{\Phi^{-1}(1-p_1)\vartheta - \vartheta^2/2}.} \code{deltaLR} may also contain multiple elements, in which case a weighted likelihood ratio is calculated for the given values. Unless positive weights that sum to 1 are provided by the argument \code{weightsDeltaLR}, equal weights are assumed.
#'    \item \code{likelihoodRatioDistribution="normal"}: calculates the likelihood ratio for a normally distributed prior of \eqn{\vartheta} with mean \code{deltaLR}*\code{sqrt(firstStageInformation)} (\eqn{\mu}) and standard deviation \code{tauLR}*\code{sqrt(firstStageInformation)} (\eqn{\sigma}). The parameters \code{deltaLR} and \code{tauLR} must be specified on the mean difference scale.
#'          \deqn{l(p_1) = (1+\sigma^2)^{-\frac{1}{2}}\cdot e^{-(\mu/\sigma)^2/2 + (\sigma\Phi^{-1}(1-p_1) + \mu/\sigma)^2 / (2\cdot (1+\sigma^2))}}
#'    \item \code{likelihoodRatioDistribution="exp"}: calculates the likelihood ratio for an exponentially distributed prior of \eqn{\vartheta} with mean \code{kappaLR}*\code{sqrt(firstStageInformation)} (\eqn{\eta}). The likelihood ratio is then calculated as:
#'          \deqn{l(p_1) = \kappa \cdot \sqrt{2\pi} \cdot e^{(\Phi^{-1}(1-p_1)-\eta)^2/2} \cdot \Phi(\Phi^{-1}(1-p_1)-\eta)}
#'    \item \code{likelihoodRatioDistribution="unif"}: calculates the likelihood ratio for a uniformly distributed prior of \eqn{\vartheta} on the support \eqn{[0, \Delta\cdot\sqrt{I_1}]}, where \eqn{\Delta} is specified as \code{deltaMaxLR} and \eqn{I_1} is the \code{firstStageInformation}.
#'          \deqn{l(p_1) = \frac{\sqrt{2\pi}}{\Delta\cdot\sqrt{I_1}} \cdot e^{\Phi^{-1}(1-p_1)^2/2} \cdot (\Phi(\Delta\cdot\sqrt{I_1} - \Phi^{-1}(1-p_1))-p_1)}
#'    \item \code{likelihoodRatioDistribution="maxlr"}: the non-centrality parameter \eqn{\vartheta} is estimated from the data and no additional parameters must be specified. The likelihood ratio is estimated from the data as:
#'          \deqn{l(p_1) = e^{max(0, \Phi^{-1}(1-p_1))^2/2}}
#'          The maximum likelihood ratio is always restricted to effect sizes \eqn{\vartheta \geq 0} (corresponding to \eqn{p_1 \leq 0.5}).
#' }
#'
#' @section Effect for conditional power:
#' For the treatment effect at which the target conditional power should be achieved, either a fixed effect or an interim estimate can be used.
#' The usage of a fixed effect is indicated by setting \code{useInterimEstimate=FALSE}, in which case the fixed effect is provided by \code{delta1} on the mean difference scale or by \code{ncp1} on the non-centrality parameter scale (i.e., \code{delta1*sqrt(firstStageInformation)}).
#' For an interim estimate, specified by \code{useInterimEstimate=TRUE}, a lower cut-off for the interim estimate must be provided, either by \code{delta1Min} on the mean difference scale, or \code{ncp1Min} on the non-centrality parameter scale.
#' In addition, an upper limit of the estimate may be analogously provided by \code{delta1Max} or \code{ncp1Max}.
#'
#' @section Monotonicity:
#' By default, the optimal conditional error function returned by \code{getDesignOptimalConditionalErrorFunction()} is transformed to be non-increasing in the first-stage p-value \eqn{p_1} if found to be increasing on any interval.
#' The necessary intervals and constants for the transformation are calculated by \code{getMonotonisationConstants()}.
#' Although not recommended for the operating characteristics of the design, the transformation may be omitted by setting \code{enforceMonotonicity=FALSE}.
#'
#' @section Constraints:
#' In some applications, it may be feasible to restrict the optimal conditional error function by a lower and/or upper limit.
#' These constraints can be directly implemented on the function by using the arguments \code{minimumConditionalError} and \code{maximumConditionalError}.
#' By default, \code{minimumConditionalError=0} and \code{maximumConditionalError=1}, i.e., no constraints are applied.
#' Alternatively, the constraints may also be specified on the second-stage information via \code{minimumSecondStageInformation} and \code{maximumSecondStageInformation}.
#' If the constraints are specified on the information scale, \code{minimumConditionalError} is calculated from \code{maximumSecondStageInformation} and \code{maximumConditionalError}
#' is calculated from \code{minimumSecondStageInformation}. This is done under the assumptions that the maximum second-stage information occurs for \eqn{p_1 = \alpha_0} and the
#' minimum second-stage information is achieved for \eqn{p_1} just above \eqn{\alpha_1} (for the exact calculation, \eqn{p_1 = \alpha_1+1e-17} is used).
#' Additional specifications of \code{minimumConditionalError} and \code{maximumConditionalError} will be ignored if \code{maximumSecondStageInformation} and
#' \code{minimumSecondStageInformation} are provided.
#'
#' @section Level constant:
#' The level constant is determined by the helper function \code{getLevelConstant()}. It is identified using the \code{uniroot()} function and by default, the interval between 0 and 10 is searched for the level constant.
#' In specific settings, the level constant may lie outside of this interval. In such cases, the search interval can be changed by altering the parameters \code{levelConstantMinimum} and \code{levelConstantMaximum}. \cr
#' If inappropriate constraints to the optimal conditional error function are provided via \code{minimumConditionalError} and \code{maximumConditionalError}
#' or \code{minimumSecondStageInformation} and \code{maximumSecondStageInformation}, it may be impossible to find a level constant which exhausts the full alpha level.
#'
#' @section Generic functions:
#' The \code{print()} and \code{plot()} functions are available for objects of class \code{TrialDesignOptimalConditionalError}.
#' For details, see \code{?print.TrialDesignOptimalConditionalError} and \code{?plot.TrialDesignOptimalConditionalError}.
#'
#' @template param_alpha
#' @template param_alpha1
#' @template param_alpha0
#' @template param_conditionalPower
#' @template param_ncp1
#' @template param_ncp1Min
#' @template param_ncp1Max
#' @template param_delta1
#' @template param_delta1Min
#' @template param_delta1Max
#' @template param_likelihoodRatioDistribution
#' @template param_firstStageInformation
#' @template param_useInterimEstimate
#' @template param_minimumSecondStageInformation
#' @template param_maximumSecondStageInformation
#' @template param_minimumConditionalError
#' @template param_maximumConditionalError
#' @template param_levelConstantMinimum
#' @template param_levelConstantMaximum
#' @template param_enforceMonotonicity
#' @param ... Additional arguments required for the specification of the likelihood ratio.
#'
#' @importFrom methods new
#'
#' @return An object of class \code{TrialDesignOptimalConditionalError}, which can be passed to other package functions.
#'
#' @examples
#' \dontrun{
#' # Create a design with fixed parameter for the likelihood ratio and a
#' # fixed effect for conditional power. The second-stage information is
#' # restricted by a minimum and maximum.
#' getDesignOptimalConditionalErrorFunction(
#'   alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#'   delta1 = 0.5, likelihoodRatioDistribution = "fixed", deltaLR = 1,
#'   firstStageInformation = 2, useInterimEstimate = FALSE,
#'   minimumSecondStageInformation = 2, maximumSecondStageInformation = 40
#' )
#'
#' # Create a design using the maximum likelihood ratio and an interim
#' # estimate for the effect for conditional power.
#' getDesignOptimalConditionalErrorFunction(
#'   alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#'   delta1Min = 0.5, likelihoodRatioDistribution = "maxlr",
#'   firstStageInformation = 2, useInterimEstimate = TRUE
#' )
#' }
#'
#' @export
#'
getDesignOptimalConditionalErrorFunction <- function(
    alpha, alpha1, alpha0, conditionalPower = NA_real_, delta1 = NA_real_,
    delta1Min = NA_real_, delta1Max = Inf, ncp1 = NA_real_,
    ncp1Min = NA_real_, ncp1Max = Inf, useInterimEstimate = TRUE,
    firstStageInformation, likelihoodRatioDistribution,
    minimumSecondStageInformation = 0, maximumSecondStageInformation = Inf,
    minimumConditionalError = 0, maximumConditionalError = 1,
    conditionalPowerFunction = NA,
    levelConstantMinimum = 0, levelConstantMaximum = 10,
    enforceMonotonicity = TRUE, ...) {
  design <- new(
    "TrialDesignOptimalConditionalError",
    alpha = alpha,
    alpha1 = alpha1,
    alpha0 = alpha0,
    conditionalPower = conditionalPower,
    delta1 = delta1,
    delta1Min = delta1Min,
    delta1Max = delta1Max,
    ncp1 = ncp1,
    ncp1Min = ncp1Min,
    ncp1Max = ncp1Max,
    firstStageInformation = firstStageInformation,
    useInterimEstimate = useInterimEstimate,
    likelihoodRatioDistribution = likelihoodRatioDistribution,
    minimumSecondStageInformation = minimumSecondStageInformation,
    maximumSecondStageInformation = maximumSecondStageInformation,
    minimumConditionalError = minimumConditionalError,
    maximumConditionalError = maximumConditionalError,
    conditionalPowerFunction = conditionalPowerFunction,
    levelConstantMinimum = levelConstantMinimum,
    levelConstantMaximum = levelConstantMaximum,
    enforceMonotonicity = enforceMonotonicity,
    ... = ...
  )

  return(design)
}
