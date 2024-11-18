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
            cat("  Uniformly distributed prior in Likelihood Ratio with maximum ", x$maxDeltaLR, "\n")
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

  if(length(x$monotonisationConstants) > 0) {
    cat("\n")
    cat("Monotonisation constants: \n")
    cat("  Intervals (p-value scale):" , paste("[", apply(X = cbind(x$monotonisationConstants$dls, x$monotonisationConstants$dus), FUN = paste, MARGIN = 1, collapse = ", "), "]", sep = ""), "\n")
    cat("  Constant(s) (Q scale):", x$monotonisationConstants$qs, "\n")
  }

}


#'
#' @export
plot.TrialDesignOptimalConditionalError <- function(x, range = c(0, 1), ...) {
 firstStagePValues <- seq(from = range[1], to = range[2], length.out = 1e3)
 optimalConditionalErrors <- getOptimalConditionalError(
   firstStagePValue = firstStagePValues, design = x
 )

 ggplot2::ggplot(data = NULL) +
   ggplot2::geom_line(mapping=ggplot2::aes(x = firstStagePValues, y = optimalConditionalErrors), linewidth = 1.1) +
   ggplot2::labs(x = "First-stage p-value", y = "Optimal Conditional Error") +
   ggplot2::theme_bw() +
   ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dashed", col = "red", linewidth = 0.8) +
   ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dashed", col = "blue", linewidth = 0.8)
}
