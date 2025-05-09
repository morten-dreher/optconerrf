% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSimulationResults.R
\name{getSimulationResults}
\alias{getSimulationResults}
\title{Simulate trials}
\usage{
getSimulationResults(
  design,
  maxNumberOfIterations = 10000,
  alternative,
  seed = NULL
)
}
\arguments{
\item{design}{An object of class \code{TrialDesignOptimalConditionalError} created by \code{getDesignOptimalConditionalErrorFunction()}. Contains all necessary arguments to calculate the optimal conditional error function for the specified case.}

\item{maxNumberOfIterations}{Number of trials to be simulated.}

\item{alternative}{Assumed relative effect size.}

\item{seed}{An optional seed for reproducibility.}
}
\value{
An object of class \code{SimulationResultsOptimalConditionalError} containing the simulation results.
}
\description{
Simulate the rejection probability for a given design and alternative.
}
\details{
Simulates the probabilities of overall rejection as well as early futility and early efficacy for the provided scenario and design.
This is done by generating random normally distributed test statistics and calculating their p-values.
}
\examples{
\dontrun{
design <- getDesignOptimalConditionalErrorFunction(
 alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, delta1 = 0.25,
 useInterimEstimate = FALSE,
 conditionalPower = 0.9, likelihoodRatioDistribution = "maxlr",
 firstStageInformation = 10
)

# Simulate under the null hypothesis and for a mean difference of 0.5
getSimulationResults(
 design = design, alternative = c(0, 0.5)
)

}
}
\seealso{
\code{\link[=getDesignOptimalConditionalErrorFunction]{getDesignOptimalConditionalErrorFunction()}}, \code{\link[=getOverallPower]{getOverallPower()}}
}
