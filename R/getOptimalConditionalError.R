#' Calculate the Optimal Conditional Error
#' @name getOptimalConditionalError
#'
#' @details The optimal conditional error \eqn{\alpha_2} given a first-stage p-value \eqn{p_1} is calculated as:
#' \deqn{\alpha_2(p_1)=\psi(-e^{c_0} \cdot \frac{\Delta_1^2}{l(p_1)}).}
#'
#' The level constant \eqn{c_0} as well as the specification of the effect size \eqn{\Delta_1} and the likelihood ratio \eqn{l(p_1)}
#' must be contained in the \code{design} object (see \code{?getDesignOptimalConditionalErrorFunction}).
#' Early stopping rules are supported, i.e., for \eqn{p_1 \leq \alpha_1}, the returned conditional error is 1 and for \eqn{p_1 > \alpha_0}, the returned conditional error is 0.
#'
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @return Value of the optimal conditional error function.
#' @export
#'
#' @template reference_optimal
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()]
#'
#' @examples
#' # Create a design
#' design <- getDesignOptimalConditionalErrorFunction(
#' alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#' delta1 = 0.5, firstStageInformation = 40, useInterimEstimate = FALSE,
#' likelihoodRatioDistribution = "fixed", deltaLR = 0.5)
#'
#' # Calculate optimal conditional error
#' getOptimalConditionalError(
#' firstStagePValue = c(0.1, 0.2, 0.3), design = design
#' )

getOptimalConditionalError <- function(firstStagePValue, design) {
  # Check if firstStagePValue lies outside early decision boundaries
  if (firstStagePValue <= design$alpha1 && design$alpha1 != 0) {
    return(1)
  } else if (firstStagePValue > design$alpha0) {
    return(0)
  }

  # If monotonisation constants specified and monotonisation enforced, perform non-increasing transformation
  if (
    design$enforceMonotonicity &&
      !is.null(unlist(design$monotonisationConstants))
  ) {
    Q <- getMonotoneFunction(
      x = firstStagePValue,
      fun = getQ,
      design = design
    )
  } else {
    Q <- getQ(firstStagePValue = firstStagePValue, design = design)
  }

  #Take constraints into account (minimumConditionalError, maximumConditionalError,
  #minimumSecondStageInformation, maximumSecondStageInformation)

  C_max_Info <- NULL
  C_min_Info <- NULL

  C_max_cond_error <- design$maximumConditionalError
  C_min_cond_error <- design$minimumConditionalError

  # Check if conditional power function should be used
  if (!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
    conditionalPower <- design$conditionalPowerFunction(firstStagePValue)

    #Check if interim estimate is used
    if (design$useInterimEstimate) {
      delta1 <- min(
        max(
          qnorm(1 - firstStagePValue) / sqrt(design$firstStageInformation),
          design$delta1Min
        ),
        design$delta1Max
      )
    } else {
      # Otherwise use fixed effect
      delta1 <- design$delta1
    }
    #Calculate constraint based on minimumSecondStageInformation
    if (design$minimumSecondStageInformation > 0) {
      C_max_Info <- 1 -
        pnorm(
          delta1 *
            sqrt(design$minimumSecondStageInformation) -
            qnorm(conditionalPower)
        )
    }

    #Calculate constraint based on maximumSecondStageInformation
    if (design$maximumSecondStageInformation < Inf) {
      C_min_Info <- 1 -
        pnorm(
          delta1 *
            sqrt(design$maximumSecondStageInformation) -
            qnorm(conditionalPower)
        )
    }
  } else {
    conditionalPower <- design$conditionalPower

    #Check if interim estimate is used
    if (design$useInterimEstimate) {
      delta_C_max <- min(
        qnorm(1 - design$alpha1) / sqrt(design$firstStageInformation),
        design$delta1Max
      )
      delta_C_min <- max(
        qnorm(1 - design$alpha0) / sqrt(design$firstStageInformation),
        design$delta1Min
      )
    } else {
      # Otherwise use fixed effect
      delta_C_max <- design$delta1
      delta_C_min <- design$delta1
    }

    #Calculate constraint based on minimumSecondStageInformation
    if (design$minimumSecondStageInformation > 0) {
      C_max_Info <- 1 -
        pnorm(
          delta_C_max *
            sqrt(design$minimumSecondStageInformation) -
            qnorm(conditionalPower)
        )
    }

    #Calculate constraint based on maximumSecondStageInformation
    if (design$maximumSecondStageInformation < Inf) {
      C_min_Info <- 1 -
        pnorm(
          delta_C_min *
            sqrt(design$maximumSecondStageInformation) -
            qnorm(conditionalPower)
        )
    }
  }

  #Use the constraint that is the stronger restriction
  C_max <- min(C_max_Info, C_max_cond_error)
  C_min <- max(C_min_Info, C_min_cond_error)

  #Handling of the special case firstStagePValue=0 and no early stopping
  if (firstStagePValue == 0 && design$alpha1 == 0) {
    # Calculate the specified conditional power for a firstStagePValue of 0
    if (!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
      conditionalPower_0 <- design$conditionalPowerFunction(0)
    } else {
      conditionalPower_0 <- design$conditionalPower
    }
    return(min(C_max, conditionalPower_0))
  }

  return(max(
    C_min,
    min(
      C_max,
      getPsi(
        nuPrime = (-exp(design$levelConstant) / Q),
        conditionalPower = conditionalPower
      )
    )
  ))
}

getOptimalConditionalError <- Vectorize(
  FUN = getOptimalConditionalError,
  vectorize.args = c("firstStagePValue")
)
