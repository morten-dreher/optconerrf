testthat::test_that(desc = "getPsi works correctly", code = {

  #The design and the value of the first stage p-value,
  #only need to be provided, if the cp is not within the bounds
  #pnorm(2) and pnorm(-2).

  testthat::expect_equal(
    object = getPsi(
      nuPrime = getNuPrime(alpha = 0.05, conditionalPower = 0.9),
      conditionalPower = 0.9,
      design = NULL,
      firstStagePValue = NULL
    ),
    expected = 0.05
  )

  testthat::expect_equal(
    object = getPsi(
      nuPrime = getNuPrime(alpha = 0.01, conditionalPower = 0.8),
      conditionalPower = 0.8,
      design = NULL,
      firstStagePValue = NULL
    ),
    expected = 0.01
  )

  testthat::expect_equal(
    object = getPsi(
      nuPrime = c(-10, -11, -12, -13, -14),
      conditionalPower = 0.9,
      design = NULL,
      firstStagePValue = NULL
    ),
    expected = c(
      0.312539192762,
      0.282106799080,
      0.256949542063,
      0.235827887892,
      0.217855383703
    ),
    tolerance = 1e-8
  )

  #Create an example design
  design_example1 <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5,
    conditionalPower = 0.995, delta1 = 0.5, useInterimEstimate = FALSE,
    firstStageInformation = 4, likelihoodRatioDistribution = "fixed",
    deltaLR = 1, enforceMonotonicity = FALSE
  )

  testthat::expect_equal(
    object = getPsi(
      nuPrime = c(-10, -11, -12, -13, -14),
      conditionalPower = 0.995,
      design = design_example1,
      firstStagePValue = NULL
    ),
    expected = c(
      0.993518878813,
      0.993293975574,
      0.993044787376,
      0.992766201371,
      0.430717398170
    ),
    tolerance = 1e-8
  )

  design_example2 <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.0005, alpha1 = 0, alpha0 = 1,
    conditionalPower = 0.021, delta1 = 0.5, useInterimEstimate = FALSE,
    firstStageInformation = 4, likelihoodRatioDistribution = "fixed",
    deltaLR = 1, enforceMonotonicity = FALSE
  )

  testthat::expect_equal(
    object = getPsi(
      nuPrime = c(-10, -11, -12, -13, -14),
      conditionalPower = 0.021,
      design = design_example2,
      firstStagePValue = NULL
    ),
    expected = c(
      0.0136273452348,
      0.0132047207500,
      0.0128121326993,
      0.0124462618859,
      0.0121042796519
    ),
    tolerance = 1e-8
  )
})
