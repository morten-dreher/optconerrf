TrialDesignOptimalConditionalError <- setRefClass(
  Class = "TrialDesignOptimalConditionalError",
  fields = list(
    alpha = "numeric",
    alpha1 = "numeric",
    alpha0 = "numeric",
    conditionalPower = "numeric",
    conditionalPowerFunction = "function",
    delta1 = "numeric",
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
    minimumConditionalError = "numeric",
    maximumConditionalError = "numeric",
    levelConstantMinimum = "numeric",
    levelConstantMaximum = "numeric",
    ncp1 = "numeric",
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
    minimumConditionalError = 0,
    maximumConditionalError = 1,
    levelConstantMinimum = 0,
    levelConstantMaximum = 10,
    ncp1 = NA,
    enforceMonotonicity = TRUE
    ) {

      # Range checks for numeric variables
      rangeCheck(variable = alpha, range = c(0, 1), allowedEqual = FALSE)
      rangeCheck(variable = alpha1, range = c(0, alpha), allowedEqual = TRUE)
      rangeCheck(variable = alpha0, range = c(alpha1, 1), allowedEqual = TRUE)

      if(is.na(conditionalPower) && is.na(conditionalPowerFunction)) {
        stop("Must specify either conditionalPower or conditionalPowerFunction")
      }
      else {
        if(!is.na(conditionalPower)) {
          rangeCheck(variable = conditionalPower, range = c(0, 1), allowedEqual = FALSE)
        }
      }

      rangeCheck(variable = delta1, range = c(0, Inf), allowedEqual = FALSE)
      rangeCheck(variable = firstStageInformation, range = c(0, Inf), allowedEqual = FALSE)
      rangeCheck(variable = minimumConditionalError, range = c(0, 1), allowedEqual = TRUE)
      rangeCheck(variable = maximumConditionalError, range = c(0, 1), allowedEqual = TRUE)

      # Set initial parameters
      .self$alpha <- alpha
      .self$alpha1 <- alpha1
      .self$alpha0 <- alpha0
      .self$conditionalPower <- conditionalPower
      .self$delta1 <- delta1
      .self$firstStageInformation <- firstStageInformation
      .self$likelihoodRatioDistribution <- likelihoodRatioDistribution
      .self$useInterimEstimate <- useInterimEstimate
      .self$levelConstantMinimum <- levelConstantMinimum
      .self$levelConstantMaximum <- levelConstantMaximum

      # If non-centrality parameter was not specified, calculate it
      if(is.na(ncp1)) {
        .self$ncp1 <- delta1*sqrt(firstStageInformation)
      }
      else {
        rangeCheck(variable = ncp1, range = c(0, Inf), allowedEqual = FALSE)
        .self$ncp1 <- ncp1
      }

      .self$minimumConditionalError <- minimumConditionalError
      .self$maximumConditionalError <- maximumConditionalError

      .self$enforceMonotonicity <- enforceMonotonicity

      # Identify specific distribution parameters
      if(likelihoodRatioDistribution == "fixed") {
        if(any(is.na(deltaLR))) {
          stop("Must provide deltaLR for fixed effect in likelihood ratio")
        }
        else {
          .self$deltaLR <- deltaLR
          if(is.na(weightsDeltaLR)) {
            .self$weightsDeltaLR <- rep(1/length(deltaLR), length(deltaLR))
          }
        }
      }
      else if(likelihoodRatioDistribution == "normal") {
        if(is.na(deltaLR) || is.na(tauLR)) {
          stop("Must provide deltaLR and tauLR for normal prior in likelihood ratio")
        }
        else {

          rangeCheck(variable = tauLR, range = c(0, Inf), allowedEqual = FALSE)

          .self$deltaLR <- deltaLR
          .self$tauLR <- tauLR
        }
      }
      else if(likelihoodRatioDistribution == "exp") {
        if(is.na(kappaLR)) {
          stop("Must provide kappaLR for exponential prior in likelihood ratio")
        }
        else {
          rangeCheck(variable = kappaLR, range = c(0, Inf), allowedEqual = FALSE)
          .self$kappaLR <- kappaLR
        }
      }
      else if(likelihoodRatioDistribution == "unif") {
        if(is.na(deltaMaxLR)) {
          stop("Must provide deltaMaxLR for uniform prior in likelihood ratio")
        }
        else {
          rangeCheck(variable = deltaMaxLR, range = c(0, Inf), allowedEqual = FALSE)
          .self$deltaMaxLR <- deltaMaxLR
        }
      }
      else if(likelihoodRatioDistribution == "maxlr") {

      }
      else {
        stop("Distribution not matched.")
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
