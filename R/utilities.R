#' @export
print.TrialDesignOptimalConditionalError <- function(x, ...) {
  cat("Optimal Conditional Error Function Design: \n \n")
  cat("General design parameters: \n")
  cat("  Overall significance level:", x$alpha, "\n")
  cat("  First-stage efficacy boundary (p-value scale):", x$alpha1, "\n")
  cat("  Binding first-stage futility boundary (p-value scale):", x$alpha0, "\n")
  cat(
    "  Specified constraints on optimal conditional error:",
    paste("[", x$minimumConditionalError, ", ", x$maximumConditionalError, "]", sep = ""), "\n"
  )

  cat("\n")

  cat("Conditional power specification: \n")
  if (is.na(x$conditionalPower)) {
    cat("  Data-dependent user-specified function \n")
  } else if (!is.na(x$conditionalPower)) {
    cat("  Target conditional power:", x$conditionalPower, "\n")
  }
  if (x$useInterimEstimate) {
    cat("  Alternative: interim estimate restricted to", paste("[", x$delta1Min, ", ", x$delta1Max, "]", sep = ""), "\n")
    cat("  First-stage non-centrality parameter restricted to", paste("[", x$ncp1Min, ", ", x$ncp1Max, "]", sep = ""), "\n")
  } else {
    cat("  Alternative:", x$delta1, "\n")
    cat("  First-stage non-centrality parameter:", x$ncp1, "\n")
  }
  cat("  First-stage information:", x$firstStageInformation, "\n")
  cat("\n")

  cat("Likelihood ratio specification: \n")
  switch(x$likelihoodRatioDistribution,
    fixed = {
      cat("  Fixed Parameter(s) in Likelihood Ratio: ", paste(format(x$deltaLR, trim = TRUE), collapse = ", "), "\n")
      cat("  Parameter weights: ", paste(format(x$weightsDeltaLR, trim = TRUE), collapse = ", "), "\n")
    },
    normal = {
      cat("  Normally distributed prior in Likelihood Ratio with mean ", x$deltaLR, " and standard deviation ", x$tauLR, "\n")
    },
    unif = {
      cat("  Uniformly distributed prior in Likelihood Ratio with maximum ", x$deltaMaxLR, "\n")
    },
    exp = {
      cat("  Exponentially distributed prior in Likelihood Ratio with mean ", x$kappaLR, "\n")
    },
    maxlr = {
      cat("  Maximum Likelihood Ratio \n")
    }
  )

  cat("\n")
  cat("Level constant: \n")
  cat("  Constant:", x$levelConstant, "\n")
  cat("  Searched on interval:", paste("[", x$levelConstantMinimum, ", ", x$levelConstantMaximum, "]", sep = ""), "\n")

  if (!is.null(unlist(x$monotonisationConstants))) {
    cat("\n")
    cat("Monotonisation constants: \n")
    cat("  Intervals (p-value scale):", paste("[", apply(X = cbind(x$monotonisationConstants$dls, x$monotonisationConstants$dus), FUN = paste, MARGIN = 1, collapse = ", "), "]", sep = ""), "\n")
    cat("  Constant(s) (Q scale):", x$monotonisationConstants$qs, "\n")
  }
  if (!x$enforceMonotonicity) {
    cat("\n")
    cat("Monotonicity was not enforced \n")
  }
}


#' Plot the optimal conditional error function
#' @name plot.TrialDesignOptimalConditionalError
#'
#' @param x Design object of class \code{TrialDesignOptimalConditionalError}.
#' @param range Numeric vector with two entries specifying the range of the plot.
#' @param type Type of plot to be created. Options are: \itemize{
#' \item \code{type = 1}: Plot the values of the optimal conditional error function against the first-stage p-value.
#' \item \code{type = 2}: Plot the second-stage information resulting from the optimal conditional error function against the first-stage p-value.
#' \item \code{type = 3}: Plot the likelihood ratio of the given specification of the optimal conditional error function against the first-stage p-value.
#' \item \code{type = 4}: Plot the function Q of the given specification of the optimal conditional error function against the first-stage p-value.
#' }
#' @param plotNonMonotoneFunction Logical. Should the non-monotone version of the plot be drawn? Not applicable for plot type 3. Default: \code{FALSE}.
#'
#'
#' @export
plot.TrialDesignOptimalConditionalError <- function(x, range = c(0, 1), type = 1, plotNonMonotoneFunction = FALSE, ...) {
  # Set a range of first-stage p-values
  firstStagePValues <- seq(from = range[1], to = range[2], length.out = 1e3)

  # Plot type 1: draw the optimal conditional error function
  if (type == 1) {
    # Calculate optimal conditional error for provided design
    optimalConditionalErrors <- getOptimalConditionalError(
      firstStagePValue = firstStagePValues, design = x
    )

    # Create base plot and save it for possible addition of the non-monotone function
    designPlot <- ggplot2::ggplot(data = NULL) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = firstStagePValues, y = optimalConditionalErrors), colour = "black", linetype = "solid", linewidth = 1.1) +
      ggplot2::labs(x = "First-stage p-value", y = "Optimal Conditional Error") +
      ggplot2::theme_bw() +
      ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dotted", col = "red", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dotted", col = "blue", linewidth = 0.8) +
      ggplot2::xlim(c(range[1], range[2]))

    # In addition to the monotone function, the non-monotone one should be drawn
    if (plotNonMonotoneFunction) {
      # There are no monotonisation constants -> monotonisation is not required
      if (is.null(unlist(x$monotonisationConstants))) {
        warning("No monotonisation required. Displaying monotone function only.")
        designPlot # Display base plot
      } else {
        # Plotting the non-monotone function only makes sense if the provided design is enforced to be monotone
        if (x$enforceMonotonicity) {
          # Here, a new design must be created, modifying the old one will not work as intended (Operation is not "copy-on-modify")
          # All fields apart from enforceMonotonicity are copied from the original design object.
          # (There may be a better way to implement this)
          # suppressWarnings() is used because this code is expected to always produce a warning
          secondDesign <- suppressWarnings(new(
            "TrialDesignOptimalConditionalError",
            alpha = x$alpha,
            alpha1 = x$alpha1,
            alpha0 = x$alpha0,
            conditionalPower = x$conditionalPower,
            conditionalPowerFunction = x$conditionalPowerFunction,
            delta1 = x$delta1,
            firstStageInformation = x$firstStageInformation,
            useInterimEstimate = x$useInterimEstimate,
            likelihoodRatioDistribution = x$likelihoodRatioDistribution,
            deltaLR = x$deltaLR,
            weightsDeltaLR = x$weightsDeltaLR,
            tauLR = x$tauLR,
            kappaLR = x$kappaLR,
            deltaMaxLR = x$deltaMaxLR,
            minimumConditionalError = x$minimumConditionalError,
            maximumConditionalError = x$maximumConditionalError,
            levelConstantMinimum = x$levelConstantMinimum,
            levelConstantMaximum = x$levelConstantMaximum,
            ncp1Min = x$ncp1Min,
            ncp1Max = x$ncp1Max,
            enforceMonotonicity = FALSE
          ))

          # Calculate optimal conditional error for the new design
          nonMonoOptimalConditionalErrors <- getOptimalConditionalError(
            firstStagePValue = firstStagePValues, design = secondDesign
          )

          # Add non-monotone optimal conditional error to base plot and display it
          designPlot +
            ggplot2::geom_line(mapping = ggplot2::aes(x = firstStagePValues, y = nonMonoOptimalConditionalErrors), colour = "gray", linetype = "dashed", linewidth = 1.1)
        } else {
          warning("When using plotNonMonotoneFunction=TRUE, x should provide a monotone function. Consider setting enforceMonotonicity=TRUE in design object.")
          designPlot # Display base plot
        }
      }
    } else {
      designPlot # Display base plot
    }
  }
  # Plot type 2: draw second-stage information
  else if (type == 2) {
    # Calculate second-stage information
    secondStageInformation <- getSecondStageInformation(
      firstStagePValue = firstStagePValues, design = x
    )

    # Create base plot and save it for possible addition of the non-monotone function
    informationPlot <- ggplot2::ggplot() +
      ggplot2::geom_line(mapping = ggplot2::aes(x = firstStagePValues, y = secondStageInformation), colour = "black", linetype = "solid", linewidth = 1.1) +
      ggplot2::labs(x = "First-stage p-value", y = "Second-stage information") +
      ggplot2::theme_bw() +
      ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dotted", col = "red", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dotted", col = "blue", linewidth = 0.8) +
      ggplot2::xlim(c(range[1], range[2]))


    if (plotNonMonotoneFunction) {
      # There are no monotonisation constants -> monotonisation is not required
      if (is.null(unlist(x$monotonisationConstants))) {
        warning("No monotonisation required. Displaying monotone function only.")
        informationPlot # Display base plot
      } else {
        # Plotting the non-monotone function only makes sense if the provided design is enforced to be monotone
        if (x$enforceMonotonicity) {
          # Here, a new design must be created, modifying the old one will not work as intended (Operation is not "copy-on-modify")
          # All fields apart from enforceMonotonicity are copied from the original design object.
          # (There may be a better way to implement this)
          # suppressWarnings() is used because this code is expected to always produce a warning
          secondDesign <- suppressWarnings(new(
            "TrialDesignOptimalConditionalError",
            alpha = x$alpha,
            alpha1 = x$alpha1,
            alpha0 = x$alpha0,
            conditionalPower = x$conditionalPower,
            conditionalPowerFunction = x$conditionalPowerFunction,
            delta1 = x$delta1,
            firstStageInformation = x$firstStageInformation,
            useInterimEstimate = x$useInterimEstimate,
            likelihoodRatioDistribution = x$likelihoodRatioDistribution,
            deltaLR = x$deltaLR,
            weightsDeltaLR = x$weightsDeltaLR,
            tauLR = x$tauLR,
            kappaLR = x$kappaLR,
            deltaMaxLR = x$deltaMaxLR,
            minimumConditionalError = x$minimumConditionalError,
            maximumConditionalError = x$maximumConditionalError,
            levelConstantMinimum = x$levelConstantMinimum,
            levelConstantMaximum = x$levelConstantMaximum,
            ncp1Min = x$ncp1Min,
            ncp1Max = x$ncp1Max,
            enforceMonotonicity = FALSE
          ))

          # Calculate optimal conditional error for the new design
          nonMonoSecondStageInformation <- getSecondStageInformation(
            firstStagePValue = firstStagePValues, design = secondDesign
          )

          # Add non-monotone optimal conditional error to base plot and display it
          informationPlot +
            ggplot2::geom_line(mapping = ggplot2::aes(x = firstStagePValues, y = nonMonoSecondStageInformation), colour = "gray", linetype = "dashed", linewidth = 1.1)
        } else {
          warning("When using plotNonMonotoneFunction=TRUE, x should provide a monotone function. Consider setting enforceMonotonicity=TRUE in design object.")
          informationPlot # Display base plot
        }
      }
    } else {
      informationPlot # Display base plot
    }
  }
  # Plot type 3: likelihood ratio
  else if(type == 3) {
    # Calculate likelihood ratio values
    likelihoodRatios <- getLikelihoodRatio(
      firstStagePValue = firstStagePValues, design = x)

    likelihoodRatioPlot <- ggplot2::ggplot() + ggplot2::geom_line(mapping = ggplot2::aes(x = firstStagePValues, y = likelihoodRatios), colour = "black", linetype = "solid", linewidth = 1.1) +
      ggplot2::labs(x = "First-stage p-value", y = "Likelihood ratio") +
      ggplot2::theme_bw() +
      ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dotted", col = "red", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dotted", col = "blue", linewidth = 0.8) +
      ggplot2::xlim(c(range[1], range[2]))

    likelihoodRatioPlot
  }
  # Plot type 4: Q
  else if(type == 4) {
    # Calculate Q values
    Q <- getMonotoneFunction(
      x = firstStagePValues, fun = getQ, lower = x$alpha1, upper = x$alpha0, design = x
    )
    QPlot <- ggplot2::ggplot() + ggplot2::geom_line(mapping = ggplot2::aes(x = firstStagePValues, y = Q), colour = "black", linetype = "solid", linewidth = 1.1) +
      ggplot2::labs(x = "First-stage p-value", y = "Q") +
      ggplot2::theme_bw() +
      ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dotted", col = "red", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dotted", col = "blue", linewidth = 0.8) +
      ggplot2::xlim(c(x$alpha1, x$alpha0))

    if(plotNonMonotoneFunction) {
      nonMonoQ <- getQ(
        firstStagePValue = firstStagePValues, design = x
      )
    }

    QPlot + ggplot2::geom_line(mapping = ggplot2::aes(
      x = firstStagePValues, y = nonMonoQ), colour = "gray", linetype = "dashed", linewidth = 1.1)

    }
}


#' Simple range check for numeric variables
#' @description
#' This function performs very basic range checks for numeric variables and throws
#' an error if the range is violated. A custom hint may be added to the message.
#'
#' @param variable The (named) variable to be checked.
#' @param range A vector of length 2 giving the minimum and maximum of the allowed range.
#' @param allowedEqual Logical. Are the borders of range valid values?
#' @param hint Additional message that may be printed after the error.
#'
rangeCheck <- function(variable, range, allowedEqual, hint = "") {
  if (allowedEqual) {
    if (any(variable < range[1]) || any(variable > range[2])) {
      variableName <- deparse(substitute(variable))
      stop(paste0(variableName, " must lie in [", range[1], ", ", range[2], "]. ", hint))
    }
  } else {
    if (any(variable <= range[1]) || any(variable >= range[2])) {
      variableName <- deparse(substitute(variable))
      stop(paste0(variableName, " must lie in ]", range[1], ", ", range[2], "[. ", hint))
    }
  }
}
