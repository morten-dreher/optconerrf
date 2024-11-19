test_that("Design objects correctly created", {
  design_interim <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5,
    conditionalPower = 0.9, delta1 = 0.5, useInterimEstimate = TRUE,
    firstStageInformation = 4, likelihoodRatioDistribution = "maxlr",
    enforceMonotonicity = TRUE
  )
  expect_s4_class(design_interim, "TrialDesignOptimalConditionalError")

  design_fixed <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5,
    conditionalPower = 0.9, delta1 = 0.5, useInterimEstimate = FALSE,
    firstStageInformation = 4, likelihoodRatioDistribution = "fixed",
    deltaLR = 1, enforceMonotonicity = FALSE
  )
  expect_s4_class(design_interim, "TrialDesignOptimalConditionalError")
})
