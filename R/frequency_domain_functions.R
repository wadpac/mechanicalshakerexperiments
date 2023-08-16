#' deriveSpectrum
#'

#' @description 'deriveSpectrum' derives the frequency spectrum of a signal
#'
#' @param x Vector that contains the values of the signal (i.e., acceleration measured in the shaking direction)
#' @param sampling_rate Integer that indicates the sampling rate of the signal (i.e., the average number of samples obtained in one second in Hz)
#' @param file_name String to indicate the name of the plot (i.e., "spectrum" result in the following name for plot 1 "spectrum_1.jpeg").
#' @param plot Boolean, default is TRUE the derived spectrum will be plotted
#' @param datadir String that defines the path to save the plot
#' @param XLIM Vector that limits the frequency content of the plot, default is c(0, 5) to limit the frequency content to 5 Hz (as 250 rpm / 60 = 4.1667 Hz is the expected max)
#' @return An object of class "spec", which is a list containing at least the following components: 
#' \item{freq}{Vector of frequencies at which the spectral density is estimated.} 
#' \item{spec}{Vector (for univariate series) or matrix (for multivariate series) of estimates of the spectral density at frequencies corresponding to freq.}
#' \item{coh}{NULL for univariate series. For multivariate time series, a matrix containing the squared coherency between different series.}  
#' \item{phase}{NULL for univariate series. For multivariate time series a matrix containing the cross-spectrum phase between different series.} 
#' \item{series}{The name of the time series.}
#' \item{snames}{For multivariate input, the names of the component series.}
#' \item{method}{The method used to calculate the spectrum.} 
#' @export
deriveSpectrum <- function(x, sampling_rate, file_name, plot = TRUE, datadir, XLIM = c(0, 5)) {
  
  specd <- spectrum(x, log = "no", plot = FALSE) # Default calculate spectrum on log-scale, but we use raw data
  
  # Frequency axis of the spectrum, specd, is calculated in terms of cycles per sampling interval
  # We need to convert this to cyles per unit time (the same can be done using the MakePowerSpectralDensity function from the package psdr)
  delta <- 1/sampling_rate # Sampling interval
  specd$specx <- specd$freq/delta # Converts to cycles per unit time (divide by sampling interval)
  specd$specy <- 2*specd$spec # Multiply the spectral density by 2 so that the area under the periodogram actually equals the variance of the time series.
  
  #Save the frequency spectrum plot and zoom in on low-frequencies (at most 5 based on max rpm)
  if (plot == TRUE){
    jpeg(paste(datadir, paste0(paste(file_name, file, sep = "_"), ".jpeg"), sep = "/plots/"),
         width=600, height=500, res=120) # start export
    plot(specd$specx, specd$specy,
         main = "Signal measured in shaking direction", xlab = xlabel, ylab = ylabel, type = "l", xlim = XLIM, bty= "l") 
    dev.off()
  }
  return(specd)
}

#' deriveMeanSpectrum
#'
#' @description 'deriveMeanSpectrum' derives the mean frequency spectrum from the derived spectra
#'
#' @param spectra A list containing the raw spectra over which the mean will be derived
#' @return A list containing two following elements: 
#' \item{x}{Vector of frequencies at which the spectral density (y) is estimated} 
#' \item{y}{Vector of estimates of the spectral density at frequencies corresponding to frequencies (x)}
#' @export
deriveMeanSpectrum <- function(spectra) {
  df_specx <- data.frame()
  df_specy <- data.frame()
  for(spectrum in 1:length(spectra)){
    df_specx <- rbind(df_specx, spectra[[spectrum]]$specx)
    df_specy <- rbind(df_specy, spectra[[spectrum]]$specy)
  }
  x <- colMeans(df_specx)
  y <- colMeans(df_specy)
  spec <- list(x = x, y = y)
  return(spec)
}

#' derivePeaks
#'
#' @description 'derivePeaks' derives peaks from the mean frequency spectrum
#'
#' @param mean.spec A list containing estimates of the spectral density (y) at corresponding frequencies (x)
#' @param sampling_rate An integer indicating whether peaks are derived for the low or high sampling rate experiment, one of c(25, 100) 
#' @param XLIM Vector that limits the frequency content, default is c(0, 5) to limit the frequency content to 5 Hz (as 250 rpm / 60 = 4.1667 Hz is the expected max)
#' @param plot Boolean, default is TRUE the derived peaks will be plotted over the mean frequency spectrum
#' @return A vector indicating the frequencies at which the peaks occur 
#' @importClassesFrom pracma findpeaks
#' @export
derivePeaks <- function(mean.spec, sampling_rate, XLIM = c(0, 5), plot = TRUE) {
  peaks <- pracma::findpeaks(mean.spec$y[mean.spec$x <= XLIM[2]], 
                             minpeakheight = mean(mean.spec$y) + sd(mean.spec$y) , 
                             minpeakdistance = 4*sampling_rate)
  local.max <- mean.spec$x[sort(peaks[,2])]
  local.max <- local.max[local.max <= XLIM[2]]
  
  if (plot == TRUE){
    plot <- plot(mean.spec$x, mean.spec$y,  xlab = xlabel, ylab = ylabel, type = "l", xlim = XLIM, bty= "l", main = "Detected peaks")
    for(max in 1:length(local.max)){
      p <- plot + abline(v=local.max[max], col = "red")
    }
  }
  return(local.max)
}

#' deriveBins
#'
#' @description 'deriveBins' derives frequency bins from the peaks detected in the mean frequency spectrum
#'
#' @param peaks A vector containing the corresponding frequencies of where the peaks (local maxima) are located
#' @param mean.spec A list containing estimates of the spectral density (y) at corresponding frequencies (x)
#' @param XLIM Vector that limits the frequency content, default is c(0, 5) to limit the frequency content to 5 Hz (as 250 rpm / 60 = 4.1667 Hz is the expected max)
#' @param plot Boolean, default is TRUE the derived bins will be plotted over the mean frequency spectrum
#' @return A vector indicating the frequencies at which the peaks occur 
#' @importFrom graphics plot abline
#' @export
deriveBins <- function(peaks, mean.spec, XLIM = c(0, 5), plot = TRUE) {
  peaks <- c(XLIM[1], peaks)
  
  freq.bin <- c()
  for(bin in 2:length(peaks)){
    freq.bin <- c(freq.bin, (peaks[bin-1] + peaks[bin])/2)
  }
  if(plot == TRUE){
    plot <- plot(mean.spec$x, mean.spec$y,  xlab = xlabel, ylab = ylabel, type = "l", xlim = XLIM, bty= "l", main = "Derived frequency cut-offs")
    for(bin in 1:length(freq.bin)){
      p <- plot + abline(v=freq.bin[bin], col = "red")
    }  
  }
  freq.bin <- c(XLIM[1], freq.bin, XLIM[2])
  return(freq.bin)
}

#' deriveComparisonValues
#'
#' @description 'deriveComparisonValues' derives the dominant frequency and the mean power spectral density from a frequency spectrum
#'
#' @param spectrum An object of class "spec"
#' @param freqBins Vector containing frequency values that define the frequency bins (can be derived using the function 'deriveBins') 
#' @return Data.frame consisting of two columns: \item{domFreq}{The dominant frequency of the signal in each frequency bin, i.e. frequency associated with the maximum value} \item{meanPSD}{The mean power spectral density of the signal in each frequency bin}

#' @export
deriveComparisonValues <- function(spectrum, freqBins){
  df <- data.frame()
  for(i in 1:(length(freqBins)-1)){
    lower <- freqBins[i]
    upper <- freqBins[i+1]
    index <- which(spectrum$specx >= lower & spectrum$specx <= upper)
    df[i, 1] <- spectrum$specx[index[which.max(spectrum$specy[index])]] # dominant frequency
    df[i, 2] <- mean(spectrum$specy[index]) # mean power spectral density
  }
  colnames(df) <- c("domFreq", "meanPSD")
  return(df)
}


#' createBoxplot
#'
#' @description 'createBoxplot' creates a boxplot between or within devices for the dominant frequency or the mean power spectral density per frequency bin
#'
#' @param data A data set containing the comparison values for the experiment
#' @param freqBins A vector that indicates the cut-offs for the frequency bins
#' @param outcome String containing the type of the comparison value. One of c("domFreq", "meanPSD") 
#' @param group String to indicate the grouping variable. One of c("brand", "dynamic_range")
#' @param sampling_rate String to indicate the sampling rate during the experiment. One of c("low", "high", "bag")
#' @param orientation_analyses Boolean to indicate if the analysis was for the ms_bag experiment, default = FALSE
#' @return Boxplot between or within devices for the outcome (y-axis) per frequency bin (x-axis)
#' @import ggpubr
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom viridis viridis
#' @importFrom stats aggregate
#' @export
createBoxplot <- function(data, freqBins, outcome = c("domFreq", "meanPSD"), group = c("brand", "dynamic_range"), sampling_rate = c("low", "high", "bag"), orientation_analyses = FALSE){
  if (outcome == "domFreq"){
    label = "Dominant frequency (Hertz)"
  } else {
    label = expression(paste("Mean power spectral density (", italic("g"), "2/Hertz)"))
  } 
  bins = as.character(1:(length(freqBins) - 1))
  bin.labels <- c()
  for (bin in 2:length(freqBins)){
    bin.labels <- c(bin.labels, paste(round(freqBins[bin-1], digits = 2), 
                                      round(freqBins[bin], digits = 2), sep = "-"))
  }
  bin.means <- aggregate(data[, c("domFreq", "meanPSD")], list(data$bin), mean)
  if(outcome == "domFreq"){
    data <- transform(data, domFreq.binmean = ave(domFreq, bin.labels, FUN = mean))
  } else{
    data <- transform(data, meanPSD.binmean = ave(meanPSD, bin.labels, FUN = mean))
    #data <- transform(data, perc = ave(meanPSD, bin.labels, FUN = prop.table)*100)
  }
  
  if(group == "dynamic_range" | orientation_analyses == TRUE){
    palette = viridis::viridis(4)
    
  } else if(group == "brand"){
    if(sampling_rate == "low"){
      palette = c("#31688EFF", "#35B779FF", "#FDE725FF")
    } else if(sampling_rate == "high"){
      palette = c("#440154FF", "#31688EFF", "#35B779FF")
    } else if(sampling_rate == "bag"){
      palette = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")
    }
  }
  
  boxplot <- ggboxplot(data, x = "bin", y = outcome, color = group, palette = palette,
                               xlab = "Shaker frequency (Hertz)", ylab = label, 
                               order = bins,
                               font.label = list(size = 10, color = "black")) +
    font("xlab", size = 10, color = "black") +
    font("x.text", size = 8, color = "black") + 
    font("ylab", size = 10, color = "black") +
    font("y.text", size = 10, color = "black") + 
    font("legend.title", size = 10, color = "black") +
    font("legend.text", size = 10, color = "black") + 
    ggplot2::scale_x_discrete(breaks=bins,
                              labels= bin.labels)
  return(boxplot)
}