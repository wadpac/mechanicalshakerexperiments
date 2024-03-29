% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency_domain_functions.R
\name{derivePeaks}
\alias{derivePeaks}
\title{derivePeaks}
\usage{
derivePeaks(mean.spec, sampling_rate, XLIM = c(0, 5), plot = TRUE)
}
\arguments{
\item{mean.spec}{A list containing estimates of the spectral density (y) at corresponding frequencies (x)}

\item{sampling_rate}{An integer indicating whether peaks are derived for the low or high sampling rate experiment, one of c(25, 100)}

\item{XLIM}{Vector that limits the frequency content, default is c(0, 5) to limit the frequency content to 5 Hz (as 250 rpm / 60 = 4.1667 Hz is the expected max)}

\item{plot}{Boolean, default is TRUE the derived peaks will be plotted over the mean frequency spectrum}
}
\value{
A vector indicating the frequencies at which the peaks occur
}
\description{
'derivePeaks' derives peaks from the mean frequency spectrum
}
