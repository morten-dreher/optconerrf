testthat::test_that(desc = "getPsi works correctly", code = {
  testthat::expect_equal(
    object = getPsi(
      nuPrime = getNuPrime(alpha = 0.05, conditionalPower = 0.9),
      conditionalPower = 0.9
    ),
    expected = 0.05
  )

  testthat::expect_equal(
    object = getPsi(
      nuPrime = getNuPrime(alpha = 0.01, conditionalPower = 0.8),
      conditionalPower = 0.8
    ),
    expected = 0.01
  )

  testthat::expect_equal(
    object = getPsi(
      nuPrime = c(-10, -11, -12, -13, -14),
      conditionalPower = 0.9
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

  testthat::expect_equal(
    object = getPsi(
      nuPrime = c(-10, -11, -12, -13, -14),
      conditionalPower = 0.995
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

  testthat::expect_equal(
    object = getPsi(
      nuPrime = c(-10, -11, -12, -13, -14),
      conditionalPower = 0.021
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
