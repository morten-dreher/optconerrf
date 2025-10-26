testthat::test_that("Likelihood Ratio correctly calculated", {
  # Sequence of p-values
  pValues <- c(0.001, 0.002, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1)

  # Only specify required information to reduce total testing time

  # Fixed likelihood ratio
  design_fixed_list <- list(
    "likelihoodRatioDistribution" = "fixed",
    "deltaLR" = 0.25,
    "firstStageInformation" = 80
  )

  likelihoodRatio_fixed <- getLikelihoodRatio(
    firstStagePValue = pValues,
    design = design_fixed_list
  )

  testthat::expect_equal(
    object = likelihoodRatio_fixed,
    expected = c(
      82.2669546,
      51.2013193,
      3.2478866,
      1.4414311,
      0.5389795,
      0.2651645,
      0.1446413,
      0.0820850,
      0.0000000
    ),
    tolerance = 1e-5
  )

  # Normally distributed likelihood ratio
  design_normal_list <- list(
    "likelihoodRatioDistribution" = "normal",
    "deltaLR" = 0.25,
    "tauLR" = 0.5,
    "firstStageInformation" = 80
  )

  likelihoodRatio_normal <- getLikelihoodRatio(
    firstStagePValue = pValues,
    design = design_normal_list
  )

  testthat::expect_equal(
    object = likelihoodRatio_normal,
    expected = c(
      25.4098091,
      13.5967390,
      0.8371000,
      0.4854073,
      0.2968895,
      0.2335123,
      0.2052004,
      0.1937263,
      Inf
    ),
    tolerance = 1e-5
  )

  # Exponentially distributed likelihood ratio
  design_exp_list <- list(
    "likelihoodRatioDistribution" = "exp",
    "kappaLR" = 0.25,
    "firstStageInformation" = 80
  )

  likelihoodRatio_exp <- getLikelihoodRatio(
    firstStagePValue = pValues,
    design = design_exp_list
  )

  testthat::expect_equal(
    object = likelihoodRatio_exp,
    expected = c(
      6.4861496,
      5.0944188,
      1.8503353,
      1.5018965,
      1.2090883,
      1.0545168,
      0.9483223,
      0.8653926,
      NaN
    ),
    tolerance = 1e-5
  )

  # Uniformly distributed likelihood ratio
  design_uniform_list <- list(
    "likelihoodRatioDistribution" = "unif",
    "deltaMaxLR" = 0.5,
    "firstStageInformation" = 80
  )

  likelihoodRatio_uniform <- getLikelihoodRatio(
    firstStagePValue = pValues,
    design = design_uniform_list
  )

  testthat::expect_equal(
    object = likelihoodRatio_uniform,
    expected = c(
      60.7978625,
      33.2412443,
      2.0545914,
      1.1458079,
      0.6388505,
      0.4501561,
      0.3472601,
      0.2802474,
      NaN
    ),
    tolerance = 1e-5
  )

  # Maximum likelihood ratio
  design_maxlr_list <- list(
    "likelihoodRatioDistribution" = "maxlr",
    "firstStageInformation" = 80
  )

  likelihoodRatio_maxlr <- getLikelihoodRatio(
    firstStagePValue = pValues,
    design = design_maxlr_list
  )

  testthat::expect_equal(
    object = likelihoodRatio_maxlr,
    expected = c(
      118.482806,
      62.922732,
      3.868132,
      2.273197,
      1.424988,
      1.147399,
      1.032613,
      1.000000,
      1.000000
    ),
    tolerance = 1e-5
  )
})
