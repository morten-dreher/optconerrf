test_that("Correct result for optimal conditional error function", {
  setting_1 <- read.csv(test_path("testdata", "ocef_setting1.csv"))

  # Comparison conditional error functions - fixed delta
  design_fixed_delta_1 <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPower = 0.8,
    likelihoodRatioDistribution = "fixed",
    ncp1 = 1,
    deltaLR = 1 / sqrt(170 / 2),
    firstStageInformation = 170 / 2,
    useInterimEstimate = FALSE
  )
  cond_error <- getOptimalConditionalError(
    setting_1$p1[-1],
    design_fixed_delta_1
  )
  expect_equal(cond_error, setting_1$fixed[-1], tolerance = 1e-4)
  testthat::expect_equal(
    object = getOptimalConditionalError(
      firstStagePValue = c(0, 1),
      design = design_fixed_delta_1
    ),
    expected = c(1, 0)
  )

  # Skip remaining tests on CRAN
  skip_on_cran()

  # Comparison conditional error functions - maxlr

  design_maxlr_1 <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPower = 0.8,
    likelihoodRatioDistribution = "maxlr",
    ncp1 = 1,
    deltaLR = 1 / sqrt(170 / 2),
    firstStageInformation = 170 / 2,
    useInterimEstimate = FALSE
  )
  cond_error <- getOptimalConditionalError(setting_1$p1[-1], design_maxlr_1)
  expect_equal(cond_error, setting_1$maxlr[-1], tolerance = 1e-4)

  # Comparison conditional error functions - normal

  design_normal_1 <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPower = 0.8,
    likelihoodRatioDistribution = "normal",
    ncp1 = 1,
    deltaLR = 1 / sqrt(170 / 2),
    tauLR = 0.6 / sqrt(170 / 2),
    firstStageInformation = 170 / 2,
    useInterimEstimate = FALSE
  )
  cond_error <- getOptimalConditionalError(setting_1$p1[-1], design_normal_1)
  expect_equal(cond_error, setting_1$normal[-1], tolerance = 1e-4)

  # Comparison conditional error functions - exponential

  design_exp_1 <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPower = 0.8,
    likelihoodRatioDistribution = "exp",
    ncp1 = 1,
    deltaLR = 1 / sqrt(170 / 2),
    kappaLR = 1 / sqrt(170 / 2),
    firstStageInformation = 170 / 2,
    useInterimEstimate = FALSE
  )
  cond_error <- getOptimalConditionalError(setting_1$p1[-1], design_exp_1)
  expect_equal(cond_error, setting_1$exp[-1], tolerance = 1e-4)

  # Comparison conditional error functions - uniform

  design_unif_1 <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPower = 0.8,
    likelihoodRatioDistribution = "unif",
    ncp1 = 1,
    deltaLR = 1 / sqrt(170 / 2),
    deltaMaxLR = 2 * 1 / sqrt(170 / 2),
    firstStageInformation = 170 / 2,
    useInterimEstimate = FALSE
  )
  cond_error <- getOptimalConditionalError(setting_1$p1[-1], design_unif_1)
  expect_equal(cond_error, setting_1$unif[-1], tolerance = 1e-4)

  # Comparison conditional error functions - maxlr with interim estimate

  design_maxlr_interim_1 <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPower = 0.8,
    likelihoodRatioDistribution = "maxlr",
    firstStageInformation = 1,
    delta1Min = (qnorm(0.8) - qnorm(0.025)) / 4,
    useInterimEstimate = TRUE
  )
  cond_error <- getOptimalConditionalError(
    setting_1$p1[-1],
    design_maxlr_interim_1
  )
  expect_equal(cond_error, setting_1$maxlr_interim[-1], tolerance = 1e-4)

  # Comparison conditional error functions - maxlr with constraints

  C_min <- pnorm(qnorm(0.9) - sqrt(2) * 2.3)
  C_max <- pnorm(qnorm(0.9) - sqrt(0.5) * 2.3)

  design_maxlr_constr_1 <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPower = 0.9,
    likelihoodRatioDistribution = "maxlr",
    ncp1 = 1,
    firstStageInformation = 1,
    useInterimEstimate = FALSE,
    minimumConditionalError = C_min,
    maximumConditionalError = C_max
  )
  cond_error <- getOptimalConditionalError(
    setting_1$p1[-1],
    design_maxlr_constr_1
  )
  expect_equal(cond_error, setting_1$maxlr_constr[-1], tolerance = 1e-4)

  # Comparison conditional error functions - uniform with constraints

  design_unif_constr_1 <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPower = 0.9,
    likelihoodRatioDistribution = "unif",
    ncp1 = 1,
    deltaMaxLR = 2 / sqrt(170 / 2),
    firstStageInformation = 170 / 2,
    useInterimEstimate = FALSE,
    minimumConditionalError = C_min,
    maximumConditionalError = C_max
  )
  cond_error <- getOptimalConditionalError(
    setting_1$p1[-1],
    design_unif_constr_1
  )
  expect_equal(cond_error, setting_1$unif_constr[-1], tolerance = 1e-4)

  # Additional regression testing

  # Design with conditional power function and interim estimate

  design_cpfun_interim <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.05,
    alpha1 = 0.0001,
    alpha0 = 0.6,
    conditionalPowerFunction = function(x) {
      1 - x
    },
    useInterimEstimate = TRUE,
    delta1Min = 0.2,
    likelihoodRatioDistribution = "maxlr",
    firstStageInformation = 45
  )

  expect_equal(
    getOptimalConditionalError(
      firstStagePValue = c(0.01, 0.025, 0.05, 0.1, 0.2),
      design = design_cpfun_interim
    ),
    c(0.54225673, 0.23556328, 0.16033201, 0.12023572, 0.06174099),
    tolerance = 1e-4
  )

  # Design with conditional power function and fixed effect
  design_cpfun_fixed <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.05,
    alpha1 = 0.0001,
    alpha0 = 0.6,
    conditionalPowerFunction = function(x) {
      1 - x
    },
    useInterimEstimate = FALSE,
    delta1 = 0.25,
    likelihoodRatioDistribution = "maxlr",
    firstStageInformation = 45
  )

  expect_equal(
    getOptimalConditionalError(
      c(0.01, 0.025, 0.05, 0.1, 0.2),
      design = design_cpfun_fixed
    ),
    c(0.987465221, 0.391909962, 0.170156530, 0.082921382, 0.043223507),
    tolerance = 1e-4
  )

  # Design with conditional power function, fixed effect, information constraints
  design_cpfun_fixed_constr <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.05,
    alpha1 = 0.0001,
    alpha0 = 0.6,
    conditionalPowerFunction = function(x) {
      1 - x
    },
    useInterimEstimate = FALSE,
    delta1 = 0.25,
    likelihoodRatioDistribution = "maxlr",
    firstStageInformation = 110,
    minimumSecondStageInformation = 1,
    maximumSecondStageInformation = 300
  )

  expect_equal(
    getOptimalConditionalError(
      c(0.01, 0.025, 0.05, 0.1, 0.2),
      design = design_cpfun_fixed_constr
    ),
    c(0.981069106, 0.396877529, 0.171916330, 0.083727926, 0.043628517),
    tolerance = 1e-4
  )

  # Design with fixed CP and information constraints
  design_inf_constraints <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.000158,
    alpha0 = 0.5,
    conditionalPower = 0.9,
    delta1 = 0.25,
    useInterimEstimate = FALSE,
    firstStageInformation = 50,
    likelihoodRatioDistribution = "maxlr",
    minimumSecondStageInformation = 30,
    maximumSecondStageInformation = 200
  )

  expect_equal(
    getOptimalConditionalError(
      c(0.01, 0.025, 0.05, 0.1, 0.2),
      design = design_inf_constraints
    ),
    c(0.362933432, 0.156691207, 0.086154255, 0.049504082, 0.030525989),
    tolerance = 1e-4
  )
})
