test_that("Class objects correctly created", {
  # Create a design object using interim estimate
  design_interim <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5,
    conditionalPower = 0.9, useInterimEstimate = TRUE,
    delta1Min = 0.2, delta1Max = 1,
    firstStageInformation = 4, likelihoodRatioDistribution = "maxlr",
    enforceMonotonicity = TRUE
  )
  # Class correct?
  expect_s4_class(design_interim, "TrialDesignOptimalConditionalError")

  # Fields correct?
  expect_equal(design_interim$alpha, 0.025)
  expect_equal(design_interim$alpha1, 0.001)
  expect_equal(design_interim$alpha0, 0.5)
  expect_equal(design_interim$conditionalPower, 0.9)
  expect_equal(design_interim$useInterimEstimate, TRUE)
  expect_equal(design_interim$firstStageInformation, 4)
  expect_equal(design_interim$likelihoodRatioDistribution, "maxlr")
  expect_equal(design_interim$enforceMonotonicity, TRUE)
  expect_equal(design_interim$delta1Min, 0.2)
  expect_equal(design_interim$delta1Max, 1)
  expect_equal(design_interim$ncp1Min, 0.2 * sqrt(4), tolerance = 1e-8)
  expect_equal(design_interim$ncp1Max, 1 * sqrt(4), tolerance = 1e-8)

  # Skip remaining tests on CRAN
  skip_on_cran()

  # Create a design object using a fixed effect for CP
  design_fixed <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5,
    conditionalPower = 0.9, delta1 = 0.5, useInterimEstimate = FALSE,
    firstStageInformation = 4, likelihoodRatioDistribution = "fixed",
    deltaLR = 1, enforceMonotonicity = FALSE
  )
  # Class correct?
  expect_s4_class(design_fixed, "TrialDesignOptimalConditionalError")

  # Fields correct?
  expect_equal(design_fixed$alpha, 0.025)
  expect_equal(design_fixed$alpha1, 0.001)
  expect_equal(design_fixed$alpha0, 0.5)
  expect_equal(design_fixed$conditionalPower, 0.9)
  expect_equal(design_fixed$delta1, 0.5)
  expect_equal(design_fixed$useInterimEstimate, FALSE)
  expect_equal(design_fixed$firstStageInformation, 4)
  expect_equal(design_fixed$likelihoodRatioDistribution, "fixed")
  expect_equal(design_fixed$deltaLR, 1)
  expect_equal(design_fixed$enforceMonotonicity, FALSE)
  expect_equal(design_fixed$ncp1, 0.5 * sqrt(4), tolerance = 1e-8)
})
