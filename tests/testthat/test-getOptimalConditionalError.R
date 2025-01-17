test_that("Correct result for optimal conditional error function", {

  setting_1  <- read.csv(test_path("testdata", "ocef_setting1.csv"))

  #Comparison conditional error functions - fixed delta
  design_fixed_delta_1 <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.90,
                                                                   likelihoodRatioDistribution = "fixed", ncp1=1,  deltaLR=1/sqrt(170/2),
                                                                   firstStageInformation = 170/2, useInterimEstimate = FALSE)
  cond_error <- getOptimalConditionalError(setting_1$p1[-1], design_fixed_delta_1)
  expect_equal(cond_error, setting_1$fixed[-1], tolerance = 1e-6)

  #Comparison conditional error functions - maxlr

  design_maxlr_1 <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.90,
                                                             likelihoodRatioDistribution = "maxlr", ncp1=1,  deltaLR=1/sqrt(170/2),
                                                             firstStageInformation = 170/2, useInterimEstimate = FALSE)
  cond_error <- getOptimalConditionalError(setting_1$p1[-1], design_maxlr_1)
  expect_equal(cond_error, setting_1$maxlr[-1], tolerance = 1e-6)


  #Comparison conditional error functions - normal

  design_normal_1 <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.90,
                                                              likelihoodRatioDistribution = "normal", ncp1=1,  deltaLR=1/sqrt(170/2),
                                                              tauLR=0.6/sqrt(170/2),
                                                              firstStageInformation = 170/2, useInterimEstimate = FALSE)
  cond_error <- getOptimalConditionalError(setting_1$p1[-1], design_normal_1)
  expect_equal(cond_error, setting_1$normal[-1], tolerance = 1e-5) #With 1e-6 derivations in 5.5% of the cases


  #Comparison conditional error functions - exponential

  design_exp_1 <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.90,
                                                           likelihoodRatioDistribution = "exp", ncp1=1,  deltaLR=1/sqrt(170/2),
                                                           kappaLR = 1/sqrt(170/2),
                                                           firstStageInformation = 170/2, useInterimEstimate = FALSE)
  cond_error <- getOptimalConditionalError(setting_1$p1[-1], design_exp_1)
  expect_equal(cond_error, setting_1$exp[-1], tolerance = 1e-6)

  #Comparison conditional error functions - uniform

  design_unif_1 <- getDesignOptimalConditionalErrorFunction(alpha=0.025, alpha1= 0.000158, alpha0=0.5, conditionalPower = 0.90,
                                                            likelihoodRatioDistribution = "unif", ncp1=1,  deltaLR=1/sqrt(170/2),
                                                            deltaMaxLR = 2*1/sqrt(170/2),
                                                            firstStageInformation = 170/2, useInterimEstimate = FALSE)
  cond_error <- getOptimalConditionalError(setting_1$p1[-1], design_unif_1)
  expect_equal(cond_error, setting_1$unif[-1], tolerance = 1e-6)

})
