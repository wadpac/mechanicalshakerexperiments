% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency_domain_functions.R
\name{deriveBins}
\alias{deriveBins}
\title{deriveBins}
\usage{
deriveBins(peaks, mean.spec, XLIM = c(0, 5), plot = TRUE)
}
\arguments{
\item{peaks}{A vector containing the corresponding frequencies of where the peaks (local maxima) are located}

\item{mean.spec}{A list containing estimates of the spectral density (y) at corresponding frequencies (x)}

\item{XLIM}{Vector that limits the frequency content, default is c(0, 5) to limit the frequency content to 5 Hz (as 250 rpm / 60 = 4.1667 Hz is the expected max)}

\item{plot}{Boolean, default is TRUE the derived bins will be plotted over the mean frequency spectrum}
}
\value{
A vector indicating the frequencies at which the peaks occur
}
\description{
'deriveBins' derives frequency bins from the peaks detected in the mean frequency spectrum
}
