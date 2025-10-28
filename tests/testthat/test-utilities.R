# Fix design for all tests
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

# More elaborate design
design2 <- getDesignOptimalConditionalErrorFunction(
  alpha =
)


testthat::test_that(desc = "print.TrialDesignOptimalConditionalError works", code = {
  print_output <- capture.output(print(design))

  # Sporadic tests of output
  testthat::expect_true(
    object = all(
      c(
        grepl(
          pattern = "Optimal Conditional Error Function Design:",
          x = print_output[1]
        ),
        grepl(
          pattern = "General design parameters:",
          x = print_output[3]
        ),
        grepl(
          pattern = "Overall significance level: 0.025",
          x = print_output[4]
        ),
        grepl(
          pattern = "0.001",
          x = print_output[5]
        ),
        grepl(
          pattern = "0.5",
          x = print_output[6]
        ),
        grepl(
          pattern = "Alternative: 0.25",
          x = print_output[10]
        ),
        grepl(
          pattern = "First-stage information: 80",
          x = print_output[12]
        )
      )
    )
  )
})

testthat::test_that(desc = "print.SimulationResultsOptimalConditionalError works", code = {
  sim_results <- getSimulationResults(
    design = design,
    alternative = c(0, 0.5),
    seed = 12345,
    maxNumberOfIterations = 10
  )

  print_output <- capture.output(print(sim_results))

  # Sporadic tests of output
  testthat::expect_true(
    object = all(
      c(
        grepl(
          pattern = "Simulation results",
          x = print_output[1]
        ),
        grepl(
          pattern = "Alternative:",
          x = print_output[3]
        ),
        grepl(
          pattern = "First-stage futility:",
          x = print_output[4]
        ),
        grepl(
          pattern = "First-stage efficacy:",
          x = print_output[5]
        ),
        grepl(
          pattern = "Overall power:",
          x = print_output[6]
        )
      )
    )
  )
})

testthat::test_that(desc = "plot.TrialDesignOptimalConditionalError with type = 1 operational", code = {
  plot_output <- plot(
    design,
    type = 1
  )
  testthat::expect_s3_class(
    object = plot_output,
    class = "ggplot"
  )
})

testthat::test_that(desc = "plot.TrialDesignOptimalConditionalError with type = 2 operational", code = {
  plot_output <- plot(
    design,
    type = 2
  )
  testthat::expect_s3_class(
    object = plot_output,
    class = "ggplot"
  )
})

testthat::test_that(desc = "plot.TrialDesignOptimalConditionalError with type = 3 operational", code = {
  plot_output <- plot(
    design,
    type = 3
  )
  testthat::expect_s3_class(
    object = plot_output,
    class = "ggplot"
  )
})

testthat::test_that(desc = "plot.TrialDesignOptimalConditionalError with type = 4 operational", code = {
  plot_output <- plot(
    design,
    type = 4
  )
  testthat::expect_s3_class(
    object = plot_output,
    class = "ggplot"
  )
})

testthat::test_that(desc = "summary.TrialDesignOptimalConditionalError operational", code = {
  summary_output <- capture.output(summary(design))

  testthat::expect_true(
    all(c(
      grepl(
        pattern = "Summary of the Optimal Conditional Error Function Design",
        x = summary_output[1]
      ),
      grepl(
        pattern = "Overall significance level: 0.025",
        x = summary_output[4]
      ),
      grepl(pattern = "First-stage efficacy", x = summary_output[5]),
      grepl(pattern = "Binding first-stage futility", x = summary_output[6]),
      grepl(
        pattern = "Maximum second-stage information",
        x = summary_output[13]
      ),
      grepl(pattern = "Futility stopping probability", x = summary_output[19])
    ))
  )
})

testthat::test_that(desc = ".rangeCheck operational", code = {
  testvar <- c(0, 1, 2)
  testthat::expect_true(
    .rangeCheck(variable = testvar, range = c(0, 2), allowedEqual = TRUE)
  )
  testthat::expect_error(
    .rangeCheck(variable = testvar, range = c(0, 1), allowedEqual = FALSE)
  )
  testthat::expect_error(
    .rangeCheck(variable = testvar, range = c(1, 2), allowedEqual = FALSE)
  )
})
