test_that("Correct results for expected second stage information", {

  design_fixed_delta <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                               likelihoodRatioDistribution = "fixed", ncp1=sqrt(170/2)*0.25, deltaLR=0.25,
                                                               firstStageInformation = 170/2, useInterimEstimate = FALSE)
  res0 <- getExpectedSecondStageInformation(design = design_fixed_delta, distDelta = "fixed", deltaLR=0)
  #res0_125 <- getExpectedSecondStageInformation(design = design_fixed_delta, distDelta = "fixed", deltaLR=1.15/sqrt(170/2))
  expect_equal(res0, 95.89078, tolerance = 1e-4)
  #expect_equal(res0_125, 95.89078, tolerance = 1e-4)

  design_fixed_delta0 <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                                likelihoodRatioDistribution = "fixed", ncp1=sqrt(170/2)*0.25, deltaLR=0,
                                                                firstStageInformation = 170/2, useInterimEstimate = FALSE)
  res0 <- getExpectedSecondStageInformation(design = design_fixed_delta0, distDelta = "fixed", deltaLR=0)
  expect_equal(res0, 68.62581, tolerance = 1e-4)

  design_uniform <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                           likelihoodRatioDistribution = "unif",
                                                           ncp1=sqrt(170/2)*0.25, deltaMaxLR = 2*sqrt(170/2)*0.25,
                                                           firstStageInformation = 170/2, useInterimEstimate = FALSE)
  res0 <- getExpectedSecondStageInformation(design = design_uniform, distDelta = "fixed", deltaLR=0)
  expect_equal(res0, 81.44857, tolerance = 1e-4)

  design_normal <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                          likelihoodRatioDistribution = "normal", deltaLR = 0.25, tauLR = 1.4/sqrt(170/2), #tauLR standard deviation
                                                          ncp1=sqrt(170/2)*0.25,
                                                          firstStageInformation = 170/2, useInterimEstimate = FALSE)
  res0 <- getExpectedSecondStageInformation(design = design_normal, distDelta = "fixed", deltaLR=0)
  expect_equal(res0, 83.1371, tolerance = 1e-4)

  design_maxlr <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
                                                         likelihoodRatioDistribution = "maxlr", deltaLR = sqrt(170/2)*0.25,
                                                         ncp1=sqrt(170/2)*0.25,
                                                         firstStageInformation = 170/2, useInterimEstimate = FALSE)
  res0 <- getExpectedSecondStageInformation(design = design_maxlr, distDelta = "fixed", deltaLR=0)
  expect_equal(res0, 78.01145, tolerance = 1e-4)

  #design_exponential<-getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.9,
  #                                                             likelihoodRatioDistribution = "exp", kappaLR = 1/(sqrt(170/2)*0.25),
  #                                                             ncp1=sqrt(170/2)*0.25,
  #                                                             firstStageInformation = 170/2, useInterimEstimate = FALSE)
  #res0 <- getExpectedSecondStageInformation(design = design_exponential, distDelta = "fixed", deltaLR=0)
  #expect_equal(res0, , tolerance = 1e-4)

})
