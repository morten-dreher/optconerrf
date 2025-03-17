test_that("Overall power correctly calculated", {

# Test if the calculated overall power matches with two special cases (fix cp, fixed LR, without use of Interim Estimate)
design_fix <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
    delta1 = 0.5, likelihoodRatioDistribution = "fixed", deltaLR = 1,
    firstStageInformation = 2, useInterimEstimate = FALSE)

ncp <- design_fix$delta1 * sqrt(design_fix$firstStageInformation)
power <- 1 - pnorm(qnorm(1 - design_fix$alpha1) - ncp) +
  design_fix$conditionalPower * (pnorm(qnorm(1 - design_fix$alpha1) - ncp) - pnorm(qnorm(1 - design_fix$alpha0) - ncp))

expect_equal(getOverallPower(design_fix, design_fix$delta1), power, tolerance = 1e-4)

# Skip remaining tests on CRAN
skip_on_cran()

expect_equal(getOverallPower(design_fix, 0), design_fix$alpha, tolerance = 1e-4)

# Test if the calculated overall power matches with two special cases (cp function, fixed LR, without use of Interim Estimate)
design_function <- getDesignOptimalConditionalErrorFunction(
  alpha = 0.05, alpha1 = 0.001, alpha0 = 0.5, conditionalPowerFunction = function(x) pnorm(1-x),
  delta1 = 0.7, likelihoodRatioDistribution = "fixed", deltaLR = 1,
  firstStageInformation = 10, useInterimEstimate = FALSE
)

ncp <- design_function$delta1 * sqrt(design_function$firstStageInformation)
secondStageRejection <- function(firstStagePValue) {
  design_function$conditionalPowerFunction(firstStagePValue) * exp(qnorm(1 - firstStagePValue) * ncp - ncp^2 / 2)
}
integral <- stats::integrate(f = secondStageRejection, lower = design_function$alpha1, upper = design_function$alpha0)$value
power <- 1 - pnorm(qnorm(1 - design_function$alpha1) - ncp) + integral

expect_equal(getOverallPower(design_function, design_function$delta1), power, tolerance = 1e-4)
expect_equal(getOverallPower(design_function, 0), design_function$alpha, tolerance = 1e-4)

})
