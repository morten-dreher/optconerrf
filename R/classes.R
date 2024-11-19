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
    likelihoodRatioDistribution = NA,
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
    enforceMonotonicity = TRUE
    ) {
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

      .self$ncp1 <- delta1*sqrt(firstStageInformation)

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
          .self$deltaLR <- deltaLR
          .self$tauLR <- tauLR
        }
      }
      else if(likelihoodRatioDistribution == "exp") {
        if(is.na(kappaLR)) {
          stop("Must provide kappaLR for exponential prior in likelihood ratio")
        }
        else {
          .self$kappaLR <- kappaLR
        }
      }
      else if(likelihoodRatioDistribution == "unif") {
        if(is.na(deltaMaxLR)) {
          stop("Must provide deltaMaxLR for uniform prior in likelihood ratio")
        }
        else {
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
        design = .self, levelConstantMinimum = levelConstantMinimum,
        levelConstantMaximum = levelConstantMaximum
      )$root
    }
  )
)
