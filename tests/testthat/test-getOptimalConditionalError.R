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
})
