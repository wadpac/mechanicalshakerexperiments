rm(list=ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# TO DO: Check which GENEActiv was removed towards end of one of the experiments, and make sure data is not included

#====================================================================================
# Specify file paths
datadir = paste0(shaker_experiments_folder, "/analyses/")
if (!dir.exists(datadir)) {
  stop(paste0("Directory does not exist: ", datadir))
}
# Load mechanical shaker data for horizontal axis 
filename_flatHA = paste0(datadir, "ms_flat_HA.RData") 
if (!file.exists(filename_flatHA)) {
  stop(paste0("File does not exist: ", filename_flatHA))
}
load(filename_flatHA)

###FUNCTIONS

#' deriveSpectrum
#'
#' @description 'deriveSpectrum' derives the frequency spectrum of a signal (called from within derivePeakIntervals to derive the smoothed spectrum)
#'
#' @param x Vector that contains the values of the signal (i.e., acceleration)
#' @param sampling_frequency Integer that indicates the sampling frequency of the signal (i.e., the average number of samples obtained in one second in Hz)
#' @param raw Boolean, if TRUE the raw frequency spectrum will be derived, FALSE: smoothed spectrum will be derived
#' @return An object of class "spec", which is a list containing at least the following components: 
#' \item{freq}{Vector of frequencies at which the spectral density is estimated.} 
#' \item{spec}{Vector (for univariate series) or matrix (for multivariate series) of estimates of the spectral density at frequencies corresponding to freq.}
#' \item{coh}{NULL for univariate series. For multivariate time series, a matrix containing the squared coherency between different series.}  
#' \item{phase}{NULL for univariate series. For multivariate time series a matrix containing the cross-spectrum phase between different series.} 
#' \item{series}{The name of the time series.}
#' \item{snames}{For multivariate input, the names of the component series.}
#' \item{method}{The method used to calculate the spectrum.} 
#' @export
deriveSpectrum <- function(x, sampling_frequency, raw = TRUE) {
  if(raw == TRUE){
    specd <- spectrum(x, log = "no", plot = FALSE) # Default is to calculate the spectrum on a log-scale, but we use the raw data
  } else {
    specd <- spectrum(x, log = "no", span = 75, plot = FALSE) # Smooth the raw signal
  }
  # The frequency axis of the spectrum, specd, is calculated in terms of cycles per sampling interval
  delta <- 1/sampling_frequency
  specd$specx <- specd$freq/delta # Convert to cycles per unit time (so divide by the sampling interval)
  specd$specy <- 2*specd$spec # Multiply the spectrum by 2 to make it actually equal to variance
  return(specd)
}

# Function to derive peak intervals of a signal (x, a vector) ; if numberPeaks = TRUE the number of peaks is specified

#' derivePeakIntervals
#'
#' @description 'derivePeakIntervals' To derive global peak intervals of a raw signal
#'
#' @param x Vector that contains the values of the signal (i.e., acceleration)
#' @param sampling_frequency Integer that indicates the sampling frequency of the signal (i.e., the average number of samples obtained in one second in Hz)
#' @param freqContentLim Vector of length 2 that limits the search for peaks in the frequency content (i.e., c(0, 10) limits the search for peaks to 10 Hz)
#' @param plot Boolean, if TRUE plots will be saved as .jpeg for the smoothed signal with peak intervals, FALSE: the data will not be plotted.
#' @param numberPeaks Boolean, if TRUE the number of peaks that will be found is restricted to a number of 'npeaks', FALSE: there will be no restriction to the number of peaks.
#' @param npeaks Integer that indicates the maximum number of peaks that will be derived.
#' @param file_name String to indicate the name of the plot (i.e., "initial_intervals" result in the following name for plot 1 "initial_intervals_1.jpeg").
#' @return List of data.frames with the accelerometer time series where each list item represents 1 recording
#' @importFrom gdata read.xls
#' @export

derivePeakIntervals <- function(x, sampling_frequency, freqContentLim, plot = TRUE, numberPeaks, npeaks, file_name = " ") {
  spec.smooth <- deriveSpectrum(x, sampling_frequency, raw = FALSE)   # Use smoothed spectrum to find peaks
  # Find peaks in frequency spectrum -> limit frequency content to 10 Hz!!
  if(numberPeaks == FALSE){ # without number specified peaks
    peaks <- pracma::findpeaks(spec.smooth$specy[spec.smooth$specx <= freqContentLim[2]], 
                               minpeakheight = 2*mean(spec.smooth$specy), 
                               minpeakdistance = 2*sampling_frequency)
  } else {
    peaks <- pracma::findpeaks(spec.smooth$specy[spec.smooth$specx <= freqContentLim[2]], 
                               minpeakheight = 2*mean(spec.smooth$specy), 
                               minpeakdistance = 2*sampling_frequency, 
                               npeaks = npeaks)
  }
  #Find start and end x-values
  start <- spec.smooth$specx[peaks[, 3]] 
  end <- spec.smooth$specx[peaks[, 4]]
  
  #Save the plots of the frequency spectra and indicate the start and end values 
  if (plot == TRUE){
    jpeg(paste(datadir, paste0(paste(file_name, file, sep = "_"), ".jpeg"), sep = "/plots/"), width=600, height=500, res=120) # start export
    plot(spec.smooth$specx, spec.smooth$specy,
         main = "HA", xlab = xlabel, ylab = ylabel, type = "l", xlim = freqContentLim, bty= "l") # zoom in on low-frequencies, as at most 5 is expected based on max rpm)
    for(i in 1:length(start)){
      abline(v=start[i], col = "red")
      abline(v=end[i], col = "red")
    }  
    abline(h = 2* mean(spec.smooth$specy), col='red') #Indicate minimum peak height
    dev.off()
  }
  return(list(start = start, end = end))
}

# Function to derive frequency bins that ensure peaks fall within frequency bin using derived peak lower interval as input
defineFreqBins <- function(startIntervals, deviants, npeaks, freqContentLim) {
  startIntervals <- startIntervals[-deviants] #disregard deviant signals
  start <- data.frame()
  for (indice in 1:length(startIntervals)){
    if(length(startIntervals[[indice]]) < npeaks){
      next   #if less peaks than npeaks were found, diregard the intervals
    }
    start <- rbind(start,sort(startIntervals[[indice]]))
  }
  min_start <- c()  # Get minimum start for peaks to fall within the intervals 
  for (index in 1:ncol(start)) {
    min_start <- c(min_start, min(start[,index]))
  }
  return(c(freqContentLim[1], min_start, freqContentLim[2])) 
}  

# Function to derive comparison values (dominant frequency, mean power spectral density) from a frequency spectrum using vector of intervals
deriveComparisonValues <- function(spectrum, intervals){
  df <- data.frame()
  for(i in 1:(length(intervals)-1)){
    lower <- intervals[i]
    upper <- intervals[i+1]
    index <- which(spectrum$specx >= lower & spectrum$specx <= upper)
    df[i, 1] <- spectrum$specx[index[which.max(spectrum$specy[index])]] #dominant frequency
    df[i, 2] <- mean(spectrum$specy[index]) #mean power spectral density
    #add SD PSD?
  }
  colnames(df) <- c("domFreq", "meanPSD")
  return(df)
}

#SELECT DATA OF EXPERIMENT: HIGH and LOW
data <- ms_flat_HA$data[ms_flat_HA$specifications$experiment == "ms_hfcr" | ms_flat_HA$specifications$experiment == "ms_lfcr"]
specifications <- ms_flat_HA$specifications[ms_flat_HA$specifications$experiment == "ms_hfcr" | ms_flat_HA$specifications$experiment == "ms_lfcr",]

# DERIVE RAW FREQUENCY SPECTRA
raw.spectra_HA <- list() 
raw.spectra_normHA <- list() 

for (file in 1:length(data)) {# derive frequency spectra for all accelerometer files
  sampling_frequency <- as.numeric(specifications$sampling_frequency[file])  
  #for horizontal axis
  HA <- data[[file]]$HA
  rawSpectrum_HA <- deriveSpectrum(HA, sampling_frequency, raw = TRUE)
  raw.spectra_HA[[file]] <- rawSpectrum_HA
  #for normalized horizontal axis
  normHA <- data[[file]]$normHA
  rawSpectrum_normHA <- deriveSpectrum(normHA, sampling_frequency, raw = TRUE)
  raw.spectra_normHA[[file]] <- rawSpectrum_normHA
}
# Indicate the accelerometer serial numbers
names(raw.spectra_HA) <- specifications$serial_number  
names(raw.spectra_normHA) <- specifications$serial_number 
# Save derived raw spectra
raw_spectra <- list(HA = raw.spectra_HA, normHA = raw.spectra_normHA, specifications = ms_flat_HA$specifications)
cat("\nSaving data...")
save(raw_spectra, file = paste0(datadir, "raw_frequencySpectra_high-low.RData"))
rm(raw.spectra_HA, raw.spectra_normHA, HA, normHA, sampling_frequency, rawSpectrum_HA, rawSpectrum_normHA, file)

###DATA ANALYSIS (first for HA only)

# Plot parameters
plot = TRUE
xlabel = "Frequency (Hz)"
ylabel = "Spectral Density (g2/Hz)"
XLIM = c(0, 10) #Limit frequency content to 10 Hz

#Find peaks and start peak intervals for all files
intervalStarts_HA <- list()
for (file in 1:length(data)) {
  cat(paste0(file, "/",  length(data), " "))
  HA <- data[[file]]$HA
  sampling_frequency <- as.numeric(specifications$sampling_frequency[[file]])
  intervals <- derivePeakIntervals(HA, sampling_frequency, file_name = "initial_intervals", XLIM, numberPeaks=FALSE) #
  intervalStarts_HA[[file]] <- intervals$start
}
names(intervalStarts_HA) <- names(data)
rm(HA, intervals, file)

#Frequency content plots of 42 (AP672490), 51 (AP473254), 55 (AP672490), 64 (AP473254), 78 (6011406), 93 (6011406) deviate a lot from the other signals
specifications$serial_number[c(42,51,55,64,78,93)]

#See how many peaks were found for each accelerometer file
nPeaks <- c()
for (element in 1:length(intervalStarts_HA)){ 
  nPeaks <- c(nPeaks, length(intervalStarts_HA[[element]]))
}
summary(nPeaks)
numberPeaksFound <- table(nPeaks) 
numberPeaksFound
max(numberPeaksFound) # Set number of peaks to 15 as this is the most frequently occuring number of peaks
rm(element, numberPeaksFound)
deviants <- c(42,51,55,64,78,93)
nPeaks[deviants]

# Derive start intervals for 15 peaks
npeaks = 15
startIntervals15_HA <- list()
for (file in 1:length(data)) {
  cat(paste0(file, "/",  length(data), " "))
  HA <- data[[file]]$HA
  sampling_frequency <- as.numeric(specifications$sampling_frequency[[file]])
  intervals <- derivePeakIntervals(HA, sampling_frequency, XLIM, plot = TRUE, numberPeaks = TRUE, npeaks, file_name = "15peaks") 
  startIntervals15_HA[[file]] <- intervals$start
}
names(startIntervals15_HA) <- names(data)
rm(HA, intervals, file)

# Define general intervals that can be used to compare the signals
finalintervals_HA <- defineFreqBins(startIntervals15_HA, deviants, npeaks, XLIM)

freqBandDefinitionsHA <- list(intervalsVaryingPeaks = intervalStarts_HA, npeaks = nPeaks, 
                               intervalsPeaks = startIntervals15_HA, 
                               final = finalintervals_HA)
rm(intervalStarts_HA, startIntervals15_HA, finalintervals_HA, data, nPeaks)
cat("\nSaving data...")
save(freqBandDefinitionsHA, file = paste0(datadir, "frequencyBandDefinitions_HA.RData"))

# Derive values for comparison of spectra 
spectra_HA <- raw_spectra$HA
comparisonValues_HA <- list()

for(spectrum in 1:length(spectra_HA)){ 
  cat(paste0(spectrum, "/",  length(spectra_HA), " "))
  spectrum.raw <- spectra_HA[[spectrum]]
  values <- deriveComparisonValues(spectrum.raw, freqBandDefinitionsHA$final)
  comparisonValues_HA[[spectrum]] <- values
  
  jpeg(paste(datadir, paste0(paste("rawSpec_HA_bands", spectrum, sep = "_"), ".jpeg"), sep = "/plots/"), width=600, height=500, res=120) # start export
  plot(spectrum.raw$specx, spectrum.raw$specy,
       main = "HA", xlab = xlabel, ylab = ylabel, type = "l", xlim = XLIM, bty= "l") # zoom in on low-frequencies, as at most 5 is expected based on max rpm)
  for(i in 1:length(freqBandDefinitionsHA$final)){
    abline(v=freqBandDefinitionsHA$final[i], col = "red")
  }
  dev.off()
  
}
rm(spectrum, values, spectrum.raw)
names(comparisonValues_HA) <- names(spectra_HA)
freqspecValues <- list(raw_spectra = spectra_HA, comparison_values = comparisonValues_HA)
rm(spectra_HA, comparisonValues_HA)

cat("\nSaving data...")
save(freqspecValues, file = paste0(datadir, "freqspecValues_HA.RData"))


# STATISTICAL COMPARISON: ANOVA repeated measures (grouping variable = brand, repeated measure domFrequency)
# Prepare dataset for 2-way repeated measures ANOVA: https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/#data-preparation-1

# Wide format; make data frame with id, brand and columns for each measurement of dominant frequency
df_wide <- data.frame()
id <- names(freqspecValues$comparison_values)
df_wide <- cbind(id, specifications$brand, specifications$experiment, specifications$sampling_frequency, specifications$dynamic_range)

domFrequency <- data.frame()
meanPSD <- data.frame()

for (accelerometer in 1:length(freqspecValues$comparison_values)) {
  domFrequency <- rbind(domFrequency, freqspecValues$comparison_values[[accelerometer]]$domFreq)
  meanPSD <- rbind(meanPSD, freqspecValues$comparison_values[[accelerometer]]$meanPSD)
}
df_wide_domFreq <- cbind(df_wide, domFrequency)
colnames(df_wide_domFreq) <- c("id", "brand", "experiment", "sampling_frequency", "dynamic_range", 
                       "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", 
                       "t10", "t11", "t12", "t13", "t14", "t15")
df_wide_meanPSD <- cbind(df_wide, meanPSD)
colnames(df_wide_meanPSD) <- c("id", "brand", "experiment", "sampling_frequency", "dynamic_range", 
                               "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", 
                               "t10", "t11", "t12", "t13", "t14", "t15")

# Gather the columns for dominant frequency measures into long format
library(tidyverse)
# Convert id, brand, and time into factor variables
df_long_domFreq <- df_wide_domFreq %>%
  gather(key = "freqBin", value = "domFreq", t0, t1, t2, t3, t4, t5, t6, t7, t8, 
         t9, t10, t11, t12, t13, t14, t15) %>%
  rstatix::convert_as_factor(id, freqBin, brand, experiment, sampling_frequency, dynamic_range)

df_long_meanPSD <- df_wide_meanPSD %>%
  gather(key = "freqBin", value = "meanPSD", t0, t1, t2, t3, t4, t5, t6, t7, t8, 
         t9, t10, t11, t12, t13, t14, t15) %>%
  rstatix::convert_as_factor(id, freqBin, brand, experiment, sampling_frequency, dynamic_range)

df_long <- join(df_long_domFreq, df_long_meanPSD)

cat("\nSaving data...")
save(df_long, file = paste0(datadir, "datasetComparison-high-low-brand.RData"))

###Find out which test to apply..
#Summary statistics
df_long %>%
  group_by(brand, freqBin) %>%
  get_summary_stats(domFreq, type = "mean_sd")
df_long %>%
  group_by(brand, freqBin) %>%
  get_summary_stats(meanPSD, type = "mean_sd")


bxp <- ggpubr::ggboxplot(
  df_long, x = "measurement", y = "domFreq",
  color = "brand", palette = "jco"
)
bxp

#identify outliers
outliers <- df_long %>% group_by(measurement) %>% identify_outliers(domFreq)
#which ouliers are extreme?
sum(outliers$is.outlier == TRUE & outliers$is.extreme == TRUE)

#CHECK ASSUMPTIONS
#normality assumption
ggqqplot(df_long, "domFreq", facet.by = "freqBin")
ggqqplot(df_long, "meanPSD", facet.by = "freqBin")


#ANOVA
res.aov <- df_long %>% anova_test(domFreq ~ brand*freqBin + experiment*freqBin)
get_anova_table(res.aov)

res.aov <- df_long %>% anova_test(measPSD ~ brand*freqBin + experiment*freqBin)
get_anova_table(res.aov)