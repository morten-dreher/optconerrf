testthat::test_that("Tests for getSimulationResults() work", {
  # Tests are always skipped on CRAN (as they are time intensive)
  testthat::skip_on_cran()

  # Create a design
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

  # Get simulation results under H0 (alternative = 0) and under H1 (alternative = 0.25)
  simulationResults <- getSimulationResults(
    design = design,
    alternative = c(0, 0.25),
    seed = 1234
  )

  # Test class
  testthat::expect_s4_class(
    object = simulationResults,
    class = "SimulationResultsOptimalConditionalError"
  )

  # Test alternative
  testthat::expect_equal(
    object = simulationResults$alternative,
    expected = c(0, 0.25)
  )

  # Test number of iterations
  testthat::expect_equal(
    object = simulationResults$maxNumberOfIterations,
    expected = 10000
  )

  # Test first-stage futility
  testthat::expect_equal(
    object = simulationResults$firstStageFutility,
    expected = c(0.4984, 0.0119),
    tolerance = 1e-2
  )

  # Test first-stage efficacy
  testthat::expect_equal(
    object = simulationResults$firstStageEfficacy,
    expected = c(0.0011, 0.1952),
    tolerance = 1e-2
  )

  # Test overall power
  testthat::expect_equal(
    object = simulationResults$overallPower,
    expected = c(0.0252, 0.9079),
    tolerance = 1e-2
  )
})
