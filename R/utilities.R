#' @export
print.TrialDesignOptimalConditionalError <- function(x, ...) {

  cat("Optimal Conditional Error Function Design: \n \n")
  cat("General design parameters: \n")
  cat("  Overall significance level:", x$alpha, "\n")
  cat("  First-stage efficacy boundary (p-value scale):", x$alpha1, "\n")
  cat("  Binding first-stage futility boundary (p-value scale):", x$alpha0, "\n")
  cat("  Specified constraints on optimal conditional error:",
      paste("[",x$minimumConditionalError, ", ", x$maximumConditionalError, "]", sep=""), "\n")

  cat("\n")

  cat("Conditional power specification: \n")
  if(is.na(x$conditionalPower)) {
    cat("  Data-dependent user-specified function \n")
  }
  else if(!is.na(x$conditionalPower)){
    cat("  Target conditional power:", x$conditionalPower, "\n")
  }
  if(x$useInterimEstimate) {
    cat("  Alternative: interim estimate truncated for values <=", x$delta1, "\n")
  }
  else{
    cat("  Alternative:", x$delta1, "\n")
  }
  cat("  First-stage information:", x$firstStageInformation, "\n")
  cat("  First-stage non-centrality parameter:", x$ncp1, "\n")
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
         })

  cat("\n")
  cat("Level constant: \n")
  cat("  Constant:", x$levelConstant, "\n")
  cat("  Searched on interval:", paste("[",x$levelConstantMinimum, ", ", x$levelConstantMaximum, "]", sep=""), "\n")

  if(!is.null(unlist(x$monotonisationConstants))) {
    cat("\n")
    cat("Monotonisation constants: \n")
    cat("  Intervals (p-value scale):" , paste("[", apply(X = cbind(x$monotonisationConstants$dls, x$monotonisationConstants$dus), FUN = paste, MARGIN = 1, collapse = ", "), "]", sep = ""), "\n")
    cat("  Constant(s) (Q scale):", x$monotonisationConstants$qs, "\n")
  }
  if(!x$enforceMonotonicity) {
    cat("\n")
    cat("Monotonicity was not enforced \n")
  }

}


#'
#' @export
plot.TrialDesignOptimalConditionalError <- function(x, range = c(0, 1), plotNonMonotoneFunction = FALSE, ...) {

 # Calculate optimal conditional error for provided design
 firstStagePValues <- seq(from = range[1], to = range[2], length.out = 1e3)
 optimalConditionalErrors <- getOptimalConditionalError(
   firstStagePValue = firstStagePValues, design = x
 )

 # Create base plot and save it for possible addition of the non-monotone function
 designPlot <- ggplot2::ggplot(data = NULL) +
   ggplot2::geom_line(mapping=ggplot2::aes(x = firstStagePValues, y = optimalConditionalErrors), colour = "black", linetype = "solid", linewidth = 1.1) +
   ggplot2::labs(x = "First-stage p-value", y = "Optimal Conditional Error") +
   ggplot2::theme_bw() +
   ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dotted", col = "red", linewidth = 0.8) +
   ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dotted", col = "blue", linewidth = 0.8)

 # In addition to the monotone function, the non-monotone one should be drawn
 if(plotNonMonotoneFunction) {
   # There are no monotonisation constants -> monotonisation is not required
    if(is.null(unlist(x$monotonisationConstants))) {
      warning("No monotonisation required. Displaying monotone function only.")
      designPlot # Display base plot
    }
   else {

     # Plotting the non-monotone function only makes sense if the provided design is enforced to be monotone
      if(x$enforceMonotonicity) {

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
          ncp1 = x$ncp1,
          enforceMonotonicity = FALSE
          ))

        # Calculate optimal conditional error for the new design
        nonMonoOptimalConditionalErrors <- getOptimalConditionalError(
          firstStagePValue = firstStagePValues, design = secondDesign
        )

        # Add non-monotone optimal conditional error to base plot and display it
        designPlot +
          ggplot2::geom_line(mapping=ggplot2::aes(x = firstStagePValues, y = nonMonoOptimalConditionalErrors), colour = "gray", linetype = "dashed", linewidth = 1.1)

      }
      else {
        warning("When using plotNonMonotoneFunction=TRUE, x should provide a monotone function. Consider setting enforceMonotonicity=TRUE in design object.")
        designPlot  # Display base plot
      }
   }
 }
 else {
   designPlot  # Display base plot
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
  if(allowedEqual) {
    if(any(variable < range[1]) || any(variable > range[2])) {
      variableName <- deparse(substitute(variable))
      stop(paste0(variableName, " must lie in [", range[1], ", ", range[2], "]. ", hint))
    }
  }
  else {
    if(any(variable <= range[1]) || any(variable >= range[2])) {
      variableName <- deparse(substitute(variable))
      stop(paste0(variableName, " must lie in ]", range[1], ", ", range[2], "[. ", hint))
    }
  }
}
