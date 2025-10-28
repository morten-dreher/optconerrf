testthat::test_that(desc = "integrateExpectedInformation works correctly", code = {
  # Get a design
  design <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.001,
    alpha0 = 0.5,
    conditionalPower = 0.9,
    delta1 = 0.25,
    likelihoodRatioDistribution = "fixed",
    deltaLR = 0.25,
    firstStageInformation = 80,
    useInterimEstimate = FALSE,
  )

  testthat::expect_equal(
    object = integrateExpectedInformation(
      firstStagePValue = c(0.1, 0.25, 0.3),
      design = design,
      likelihoodRatioDistribution = "fixed",
      deltaLR = 0.25
    ),
    expected = c(203.0228586001, 75.2106316639, 57.7096125375),
    tolerance = 1e-8
  )

  testthat::expect_equal(
    object = integrateExpectedInformation(
      firstStagePValue = c(0.1, 0.25, 0.3),
      design = design,
      likelihoodRatioDistribution = "normal",
      deltaLR = 0.25,
      tauLR = 0.1
    ),
    expected = c(185.2844593244, 96.3795515369, 82.4835033051),
    tolerance = 1e-8
  )

  testthat::expect_equal(
    object = integrateExpectedInformation(
      firstStagePValue = c(0.1, 0.25, 0.3),
      design = design,
      likelihoodRatioDistribution = "exp",
      kappaLR = 0.25
    ),
    expected = c(211.539307161, 227.708914650, 229.501879160),
    tolerance = 1e-8
  )

  testthat::expect_equal(
    object = integrateExpectedInformation(
      firstStagePValue = c(0.1, 0.25, 0.3),
      design = design,
      likelihoodRatioDistribution = "unif",
      deltaMaxLR = 0.5
    ),
    expected = c(161.3848893178, 107.0021378628, 97.9706170662),
    tolerance = 1e-8
  )

  testthat::expect_equal(
    object = integrateExpectedInformation(
      firstStagePValue = c(0.1, 0.25, 0.3),
      design = design,
      likelihoodRatioDistribution = "maxlr"
    ),
    expected = c(320.175526905, 254.564916843, 249.716558773),
    tolerance = 1e-8
  )

  # Get design with interim estimate
  designInterim <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025,
    alpha1 = 0.001,
    alpha0 = 0.5,
    conditionalPower = 0.9,
    likelihoodRatioDistribution = "fixed",
    deltaLR = 0.25,
    firstStageInformation = 80,
    useInterimEstimate = TRUE,
    delta1Min = 0.15,
  )

  testthat::expect_equal(
    object = integrateExpectedInformation(
      firstStagePValue = c(0.1, 0.25, 0.3),
      design = designInterim,
      likelihoodRatioDistribution = NULL
    ),
    expected = c(466.484282391, 185.267412616, 143.575823351),
    tolerance = 1e-8
  )
})
