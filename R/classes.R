#' @name TrialDesignOptimalConditionalError
#' @title Optimal Conditional Error Design
#' @description
#' A class for a trial design object using the optimal conditional error function.
#' @details
#' This object should not be created directly; use \code{getDesignOptimalConditionalErrorFunction()} with suitable arguments to create a design.
#'
TrialDesignOptimalConditionalError <- setRefClass(
  Class = "TrialDesignOptimalConditionalError",
  fields = list(
    alpha = "numeric",
    alpha1 = "numeric",
    alpha0 = "numeric",
    conditionalPower = "numeric",
    conditionalPowerFunction = "function",
    delta1 = "numeric",
    delta1Min = "numeric",
    delta1Max = "numeric",
    firstStageInformation = "numeric",
    useInterimEstimate = "logical",
    likelihoodRatioDistribution = "character",
    deltaLR = "numeric",
    weightsDeltaLR = "numeric",
    tauLR = "numeric",
    kappaLR = "numeric",
    deltaMaxLR = "numeric",
    levelConstant = "numeric",
    monotonisationConstants = "list",
    minimumSecondStageInformation = "numeric",
    maximumSecondStageInformation = "numeric",
    minimumConditionalError = "numeric",
    maximumConditionalError = "numeric",
    levelConstantMinimum = "numeric",
    levelConstantMaximum = "numeric",
    ncp1 = "numeric",
    ncp1Min = "numeric",
    ncp1Max = "numeric",
    enforceMonotonicity = "logical"
  ),
  methods = list(
    initialize = function(
    alpha = NA_real_,
    alpha1 = NA_real_,
    alpha0 = NA_real_,
    conditionalPower = NA_real_,
    conditionalPowerFunction = NA,
    delta1 = NA_real_,
    delta1Min = NA_real_,
    delta1Max = NA_real_,
    firstStageInformation = NA_real_,
    useInterimEstimate = TRUE,
    likelihoodRatioDistribution = "",
    deltaLR = NA_real_,
    weightsDeltaLR = NA_real_,
    tauLR = NA_real_,
    kappaLR = NA_real_,
    deltaMaxLR = NA_real_,
    levelConstant = NA_real_,
    monotonisationConstants,
    minimumSecondStageInformation = 0,
    maximumSecondStageInformation = Inf,
    minimumConditionalError = 0,
    maximumConditionalError = 1,
    levelConstantMinimum = 0,
    levelConstantMaximum = 10,
    ncp1 = NA_real_,
    ncp1Min = NA_real_,
    ncp1Max = NA_real_,
    enforceMonotonicity = TRUE
    ) {

      # Range checks for numeric variables
      # General range checks
      rangeCheck(variable = alpha, range = c(0, 1), allowedEqual = FALSE)
      rangeCheck(variable = alpha1, range = c(0, 1), allowedEqual = TRUE)
      rangeCheck(variable = alpha0, range = c(0, 1), allowedEqual = TRUE)

      # Context-related range checks
      rangeCheck(variable = alpha1, range = c(0, alpha), allowedEqual = TRUE)
      rangeCheck(variable = alpha0, range = c(alpha1, 1), allowedEqual = TRUE)

      if(is.na(conditionalPower) && is.null(suppressWarnings(body(conditionalPowerFunction)))) {
        stop("Must specify either conditionalPower or a valid conditionalPowerFunction.")
      }
      else {
        if(!is.na(conditionalPower)) {
          rangeCheck(variable = conditionalPower, range = c(0, 1), allowedEqual = FALSE)

          if(!is.null(suppressWarnings(body(conditionalPowerFunction)))) {
            warning("Both conditionalPower and conditionalPowerFunction are provided. Using conditionalPower and ignoring conditionalPowerFunction.")
          }
        }
        else if(!is.null(suppressWarnings(body(conditionalPowerFunction)))) {

          # Check if function is increasing
          # Grid of values
          pValueGrid <- seq(from = alpha1, to = alpha0, length.out = 50)
          condPowerValues <- conditionalPowerFunction(pValueGrid)

          # Any function value larger than previous?
          if(any(condPowerValues[2:(length(condPowerValues))] > condPowerValues[1:(length(condPowerValues)-1)])) {
            warning("Conditional power function should not be increasing in the first-stage p-value.")
          }

          if(useInterimEstimate && (minimumSecondStageInformation > 0 || maximumSecondStageInformation < Inf)) {
            warning("Use of conditional power function, interim estimate and information constraints may lead to non-monotone conditional error function.")
          }

          .self$conditionalPowerFunction <- conditionalPowerFunction
        }
      }

      rangeCheck(variable = firstStageInformation, range = c(0, Inf), allowedEqual = FALSE)

      # Set initial parameters
      .self$alpha <- alpha
      .self$alpha1 <- alpha1
      .self$alpha0 <- alpha0
      .self$conditionalPower <- conditionalPower
      .self$firstStageInformation <- firstStageInformation
      .self$likelihoodRatioDistribution <- likelihoodRatioDistribution
      .self$useInterimEstimate <- useInterimEstimate

      if(levelConstantMinimum >= levelConstantMaximum) {
        stop("levelConstantMinimum must be smaller than levelConstantMaximum.")
      }

      .self$levelConstantMinimum <- levelConstantMinimum
      .self$levelConstantMaximum <- levelConstantMaximum

      # Derive effect sizes for conditional power
      # When using an interim estimate, derive minimal or maximal effects
      if(useInterimEstimate) {
        # Neither lower limit provided -> error
        if((is.na(ncp1Min) && (is.na(delta1Min)))) {
          stop("Must provide a lower limit for the interim estimate by using ncp1Min or delta1Min.")
        }
        else if(!is.na(delta1Min)) {
          rangeCheck(variable = delta1Min, range = c(0, Inf), allowedEqual = FALSE)
          .self$delta1Min <- delta1Min
          .self$delta1Max <- delta1Max

          if(!is.na(ncp1Min)) {
            warning("Both ncp1Min and delta1Min are provided. Using delta1Min and ignoring ncp1Min.")
          }

          .self$ncp1Min <- delta1Min * sqrt(firstStageInformation)
          .self$ncp1Max <- delta1Max * sqrt(firstStageInformation)
        }
        else if(!is.na(ncp1Min)) {
          rangeCheck(variable = ncp1Min, range = c(0, Inf), allowedEqual = FALSE)
          .self$ncp1Min <- ncp1Min
          .self$ncp1Max <- ncp1Max

          .self$delta1Min <- ncp1Min / sqrt(firstStageInformation)
          .self$delta1Max <- ifelse(ncp1Max == Inf, Inf, ncp1Max/sqrt(firstStageInformation))
        }
        else {
          stop("Unexpected error occured during determination of restrictions for interim estimate.")
        }

      }
      # When not using an interim estimate, derive fixed effects
      else {
        # If non-centrality parameter was not specified, calculate it from delta1
        if(!is.na(delta1)) {
          rangeCheck(variable = delta1, range = c(0, Inf), allowedEqual = FALSE)
          .self$delta1 <- delta1

          if(!is.na(ncp1)) {
            warning("Both delta1 and ncp1 are provided. Using delta1 and ignoring ncp1.")
          }
          .self$ncp1 <- delta1*sqrt(firstStageInformation)
        }
        # If delta1 was not specified, calculate it from ncp1
        else if(!is.na(ncp1)) {
          rangeCheck(variable = ncp1, range = c(0, Inf), allowedEqual = FALSE)
          .self$ncp1 <- ncp1
          .self$delta1 <- ncp1/sqrt(firstStageInformation)
        }
        # Else, none of ncp1 and delta1 were specified
        else {
          stop("Must specify delta1 or ncp1 when using a fixed effect for conditional power.")
        }
      }

      # Range checks for constraints
      # General range checks
      rangeCheck(variable = minimumConditionalError, range = c(0, 1), allowedEqual = TRUE)
      rangeCheck(variable = maximumConditionalError, range = c(0, 1), allowedEqual = TRUE)

      rangeCheck(variable = minimumSecondStageInformation, range = c(0, Inf), allowedEqual = TRUE)
      rangeCheck(variable = maximumSecondStageInformation, range = c(0, Inf), allowedEqual = TRUE)

      if(maximumSecondStageInformation == 0) {
        stop("Maximum second-stage information must be larger than 0.")
      }

      # Context-related range checks
      rangeCheck(variable = minimumConditionalError, range = c(0, maximumConditionalError), allowedEqual = TRUE,
                 hint = "It must not exceed maximumConditionalError.")
      rangeCheck(variable = maximumConditionalError, range = c(minimumConditionalError, 1), allowedEqual = TRUE,
                 hint = "It must not be smaller than minimumConditionalError.")

      rangeCheck(variable = minimumSecondStageInformation, range = c(0, maximumSecondStageInformation),
                 allowedEqual = TRUE, hint = "It must not exceed maximumSecondStageInformation.")
      rangeCheck(variable = maximumSecondStageInformation, range = c(minimumSecondStageInformation, Inf),
                 allowedEqual = TRUE, hint = "It must not be smaller than minimumSecondStageInformation.")


      # Identify constraints for minimum conditional error / maximum second-stage information
      .self$minimumConditionalError <- minimumConditionalError
      .self$maximumSecondStageInformation <- maximumSecondStageInformation

      # Identify constraints for maximum conditional error / minimum second-stage information
      .self$maximumConditionalError <- maximumConditionalError
      .self$minimumSecondStageInformation <- minimumSecondStageInformation

      .self$enforceMonotonicity <- enforceMonotonicity

      # Identify specific distribution parameters
      if(likelihoodRatioDistribution == "fixed") {
        if(any(is.na(deltaLR))) {
          stop("Must provide deltaLR for fixed effect in likelihood ratio.")
        }
        else {
          .self$deltaLR <- deltaLR
          # If any of the weights are NA, use equal weights
          if(any(is.na(weightsDeltaLR))) {
            .self$weightsDeltaLR <- rep(1/length(deltaLR), length(deltaLR))
            # For multiple effects, tell the user that equal weights are used.
            if(length(deltaLR) > 1) {
              message("At least one entry in weightsDeltaLR is NA. Using equal weights for effects in fixed likelihood ratio.")
            }
          }
          else {
            rangeCheck(variable = weightsDeltaLR, range = c(0, 1), allowedEqual = TRUE)
            # Check if weightsDeltaLR and deltaLR are of equal length
            if(length(weightsDeltaLR) != length(deltaLR)) {
              stop("Must provide exactly one weight in weightsDeltaLR per entry of deltaLR.")
            }
            # Verify that weightsDeltaLR sums to 1
            if(sum(weightsDeltaLR) != 1) {
              stop("Weights in weightsDeltaLR must sum to 1.")
            }
            .self$weightsDeltaLR <- weightsDeltaLR
          }
        }
      }
      else if(likelihoodRatioDistribution == "normal") {
        if(is.na(deltaLR) || is.na(tauLR)) {
          stop("Must provide deltaLR and tauLR for normal prior in likelihood ratio.")
        }
        else {

          rangeCheck(variable = tauLR, range = c(0, Inf), allowedEqual = FALSE)

          .self$deltaLR <- deltaLR
          .self$tauLR <- tauLR
        }
      }
      else if(likelihoodRatioDistribution == "exp") {
        if(is.na(kappaLR)) {
          stop("Must provide kappaLR for exponential prior in likelihood ratio.")
        }
        else {
          rangeCheck(variable = kappaLR, range = c(0, Inf), allowedEqual = FALSE)
          .self$kappaLR <- kappaLR
        }
      }
      else if(likelihoodRatioDistribution == "unif") {
        if(is.na(deltaMaxLR)) {
          stop("Must provide deltaMaxLR for uniform prior in likelihood ratio.")
        }
        else {
          rangeCheck(variable = deltaMaxLR, range = c(0, Inf), allowedEqual = FALSE)
          .self$deltaMaxLR <- deltaMaxLR
        }
      }
      else if(likelihoodRatioDistribution == "maxlr") {

      }
      else {
        stop("Distribution not matched. likelihoodRatioDistribution should be one of 'fixed', 'normal', 'exp', 'unif' or 'maxlr'.")
      }

      # Calculate monotonisation constants
      .self$monotonisationConstants <- getMonotonisationConstants(
        fun = "getQ", lower = alpha1, upper = alpha0, argument = "firstStagePValue",
        design = .self
      )

      # Calculate level constant
      .self$levelConstant <- getLevelConstant(
        design = .self
      )$root
    }
  )
)

#' @name SimulationResultsOptimalConditionalError
#' @title Simulation results for optimal conditional error design
#' @description
#' A class for simulation results of the optimal conditional error function.
#'
SimulationResultsOptimalConditionalError <- setRefClass(
  Class = "SimulationResultsOptimalConditionalError",
  fields = list(
    alternative = "numeric",
    firstStageFutility = "numeric",
    firstStageEfficacy = "numeric",
    overallPower = "numeric",
    maxNumberOfIterations = "numeric"
  )
)
