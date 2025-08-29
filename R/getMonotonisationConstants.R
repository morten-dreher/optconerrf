#' Calculate the Constants for Monotonisation
#' @name getMonotonisationConstants
#'
#' @description Computes the constants required to make a function non-increasing on the specified interval. The output of this function is necessary to calculate the monotone optimal conditional error function.
#' The output object is a list that contains the intervals on which constant values are required, specified by the minimum \code{dls} and maximum \code{dus} of the interval and the respective constants, \code{qs}.
#'
#' @template param_fun_mono
#' @template param_lower_mono
#' @template param_upper_mono
#' @template param_argument_mono
#' @template param_nSteps_mono
#' @template param_epsilon_mono
#' @template param_numberOfIterationsQ
#' @template param_design
#'
#' @return A list containing the monotonisation constants (element \code{$qs}) and the intervals on which they must be applied, specified via minimum (element \code{qls}) and maximum (element \code{qus}).
#'
#' @template reference_monotone

getMonotonisationConstants <- function(
  fun,
  lower = 0,
  upper = 1,
  argument,
  nSteps = 10^4,
  epsilon = 10^(-5),
  numberOfIterationsQ = 10^4,
  design
) {
  # Sequence of argument values
  argumentValues <- max(0, lower - 1 / (nSteps + 1)) +
    (upper - lower) * (1:nSteps) / (nSteps + 1)

  # Create a list of arguments that fun requires
  argumentList <- list(argumentValues, design)
  names(argumentList) <- c(argument, "design")

  # Call fun with the specified arguments
  functionValues <- do.call(what = fun, args = argumentList)

  # Get min and max function value
  minFunctionValue <- min(functionValues)
  maxFunctionValue <- max(functionValues)

  # Append max and min values
  functionValues <- c(maxFunctionValue, functionValues, minFunctionValue)

  # Helper variable that saves the number of function values
  m <- length(functionValues)

  # Calculate initial "integral"
  initialIntegral <- (sum(functionValues) -
    0.5 * (minFunctionValue + maxFunctionValue)) /
    m

  # Changes in function values
  derivativeFunctionValues <- functionValues[-1] - functionValues[-m]

  output <- NULL

  # Check if function is increasing anywhere
  if (max(derivativeFunctionValues) > 0) {
    # Vector for the constants
    qs <- NULL

    for (i in 1:m) {
      # Recalculate min and max value
      minFunctionValue <- min(functionValues)
      maxFunctionValue <- max(functionValues)

      # Determine derivatives of function values
      derivativeFunctionValues <- c(0, functionValues[-1] - functionValues[-m])

      # Additional check to potentially save runtime: function already non-increasing?
      if (max(derivativeFunctionValues) <= 0) {
        break
      }

      # Index of lower boundary of first interval
      dl1Position <- min(c(sum(cummin(derivativeFunctionValues <= 0)) + 1, m))

      # Index of upper boundary of first interval
      if (dl1Position < length(functionValues)) {
        duPosition <- dl1Position +
          sum(cummin(derivativeFunctionValues[dl1Position:(m - 2)] > 0))
      } else {
        duPosition <- dl1Position
      }

      # Index of lower boundary of second interval
      if (duPosition < length(functionValues)) {
        dl2Position <- duPosition +
          sum(cummin(derivativeFunctionValues[duPosition:(m - 1)] <= 0))
      } else {
        dl2Position <- duPosition
      }

      # Repeat until integrals are similar enough or maximum number of iterations reached
      for (j in 1:numberOfIterationsQ) {
        # Initial guess for q
        q <- (minFunctionValue + maxFunctionValue) / 2

        # Temporal modification of functionValues with the current guess for q
        modFunctionValues <- pmax(q, functionValues[1:dl1Position])
        modFunctionValues <- c(
          modFunctionValues,
          rep(q, duPosition + 1 - dl1Position)
        )

        if (duPosition < (m - 1)) {
          modFunctionValues <- c(
            modFunctionValues,
            pmin(q, functionValues[(duPosition + 2):dl2Position])
          )
        }
        if (dl2Position < m) {
          modFunctionValues <- c(
            modFunctionValues,
            functionValues[(dl2Position + 1):m]
          )
        }

        newIntegral <- (sum(modFunctionValues) -
          0.5 * (max(modFunctionValues) + min(modFunctionValues))) /
          m

        # If difference between integrals is small enough, stop current iteration
        if (abs(newIntegral - initialIntegral) < epsilon) {
          break
        }

        # If difference between integrals is not small enough yet, update the maximal or minimal values
        if (newIntegral > initialIntegral) {
          maxFunctionValue <- q
        } else {
          minFunctionValue <- q
        }
      }

      # No constants q so far: add
      if (is.null(qs)) {
        qs <- c(qs, q)
      } else {
        # If some constants already exist: if the new constant is larger than the last constant, it must replace the last constant.
        # This ensures that the constants are also non-increasing
        if (q >= qs[length(qs)]) {
          qs[length(qs)] <- q
        } else {
          qs <- c(qs, q)
        }
      }
      # No more increasing interval -> Finished
      if (dl2Position == m - 1) {
        break
      } else {
        functionValues <- modFunctionValues
      }
    }
    dls <- NULL
    dus <- NULL
    argumentValues <- c(lower, argumentValues, upper)

    # Find min and max of the intervals for each q
    for (q in qs) {
      dls <- c(
        dls,
        argumentValues[sum(cummin(modFunctionValues[2:(m - 1)] > q)) + 1]
      )
      dus <- c(
        dus,
        argumentValues[sum(cummin(modFunctionValues[2:(m - 1)] >= q)) + 1]
      )
    }

    output <- list(qs = qs, dls = dls, dus = dus, integral = newIntegral)
  }

  # Special case: function is increasing at the last element -> set last du to upper
  if (length(output$dus) > 0) {
    if (output$dus[length(output$dus)] >= argumentValues[m - 1]) {
      output$dus[length(output$dus)] <- upper
    }
  }

  # If there are no entries, no monotonisation is required
  if (is.null(unlist(output))) {
    output <- list()
  } else {
    if (!design$enforceMonotonicity) {
      warning(
        "Monotonisation is required. Set enforceMonotonicity to TRUE in design object for strict type I error control."
      )
    }
  }

  return(output)
}
