% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_domain_functions.R
\name{computeCrossCorrelationsPerShakerCondition}
\alias{computeCrossCorrelationsPerShakerCondition}
\title{computeCrossCorrelationsPerShakerCondition}
\usage{
computeCrossCorrelationsPerShakerCondition(data)
}
\arguments{
\item{data}{An object of class "spec"}
}
\value{
List object containing the cross-correlations and lags for each shaker condition
}
\description{
'computeCrossCorrelationsPerShakerCondition' computes for each shaker condition the maximum cross-correlations between two signals and the corresponding lag per shaker condition using the function computeCrossCorrelations
}
