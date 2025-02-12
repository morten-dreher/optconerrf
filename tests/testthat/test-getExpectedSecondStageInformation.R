test_that("Correct results for expected second stage information", {

  design_fixed_delta <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                               likelihoodRatioDistribution = "fixed", ncp1=sqrt(170/2)*0.25, deltaLR=0.25,
                                                               firstStageInformation = 170/2, useInterimEstimate = FALSE)

  res0 <- getExpectedSecondStageInformation(design = design_fixed_delta, distDelta = "fixed", deltaLR=0)
  res1_15 <- getExpectedSecondStageInformation(design = design_fixed_delta, distDelta = "fixed", deltaLR=0.125)
  res2_3 <- getExpectedSecondStageInformation(design = design_fixed_delta, distDelta = "fixed", deltaLR=0.25)

  expect_equal(res0, 95.89078, tolerance = 1e-4)
  expect_equal(res1_15, 115.9472, tolerance = 1e-4)
  expect_equal(res2_3, 55.40431, tolerance = 1e-4)

  design_fixed_delta0 <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                                likelihoodRatioDistribution = "fixed", ncp1=sqrt(170/2)*0.25, deltaLR=0,
                                                                firstStageInformation = 170/2, useInterimEstimate = FALSE)

  res0 <- getExpectedSecondStageInformation(design = design_fixed_delta0, distDelta = "fixed", deltaLR=0)
  res1_15 <- getExpectedSecondStageInformation(design = design_fixed_delta0, distDelta = "fixed", deltaLR=0.125)
  res2_3 <- getExpectedSecondStageInformation(design = design_fixed_delta0, distDelta = "fixed", deltaLR=0.25)

  expect_equal(res0, 68.62581, tolerance = 1e-4)
  expect_equal(res1_15, 119.2098, tolerance = 1e-4)
  expect_equal(res2_3, 122.479, tolerance = 1e-4)

  design_uniform <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                           likelihoodRatioDistribution = "unif",
                                                           ncp1=sqrt(170/2)*0.25, deltaMaxLR = 2*sqrt(170/2)*0.25,
                                                           firstStageInformation = 170/2, useInterimEstimate = FALSE)

  res0 <- getExpectedSecondStageInformation(design = design_uniform, distDelta = "fixed", deltaLR=0)
  res1_15 <- getExpectedSecondStageInformation(design = design_uniform, distDelta = "fixed", deltaLR=0.125)
  res2_3 <- getExpectedSecondStageInformation(design = design_uniform, distDelta = "fixed", deltaLR=0.25)

  expect_equal(res0, 81.44857, tolerance = 1e-4)
  expect_equal(res1_15, 107.1427, tolerance = 1e-4)
  expect_equal(res2_3, 58.24021, tolerance = 1e-4)

  design_normal <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                          likelihoodRatioDistribution = "normal", deltaLR = 0.25, tauLR = 1.4/sqrt(170/2), #tauLR standard deviation
                                                          ncp1=sqrt(170/2)*0.25,
                                                          firstStageInformation = 170/2, useInterimEstimate = FALSE)

  res0 <- getExpectedSecondStageInformation(design = design_normal, distDelta = "fixed", deltaLR=0)
  res1_15 <- getExpectedSecondStageInformation(design = design_normal, distDelta = "fixed", deltaLR=0.125)
  res2_3 <- getExpectedSecondStageInformation(design = design_normal, distDelta = "fixed", deltaLR=0.25)

  expect_equal(res0, 83.1371, tolerance = 1e-4)
  expect_equal(res1_15, 107.9471, tolerance = 1e-4)
  expect_equal(res2_3, 57.40248, tolerance = 1e-4)

  design_exponential<-getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                               likelihoodRatioDistribution = "exp", kappaLR = (1/2.3)/sqrt(170/2),
                                                               ncp1=sqrt(170/2)*0.25,
                                                               firstStageInformation = 170/2, useInterimEstimate = FALSE)

  res0 <- getExpectedSecondStageInformation(design = design_exponential, distDelta = "fixed", deltaLR=0)
  res1_15 <- getExpectedSecondStageInformation(design = design_exponential, distDelta = "fixed", deltaLR=0.125)
  res2_3 <- getExpectedSecondStageInformation(design = design_exponential, distDelta = "fixed", deltaLR=0.25)

  expect_equal(res0, 77.32785, tolerance = 1e-4)
  expect_equal(res1_15, 105.2796, tolerance = 1e-4)
  expect_equal(res2_3, 61.74755, tolerance = 1e-4)

  design_maxlr <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                         likelihoodRatioDistribution = "maxlr", deltaLR = sqrt(170/2)*0.25,
                                                         ncp1=sqrt(170/2)*0.25,
                                                         firstStageInformation = 170/2, useInterimEstimate = FALSE)

  res0 <- getExpectedSecondStageInformation(design = design_maxlr, distDelta = "fixed", deltaLR=0)
  res1_15 <- getExpectedSecondStageInformation(design = design_maxlr, distDelta = "fixed", deltaLR=0.125)
  res2_3 <- getExpectedSecondStageInformation(design = design_maxlr, distDelta = "fixed", deltaLR=0.25)

  expect_equal(res0, 78.01145, tolerance = 1e-4)
  expect_equal(res1_15, 106.3943, tolerance = 1e-4)
  expect_equal(res2_3, 60.6867, tolerance = 1e-4)

})
