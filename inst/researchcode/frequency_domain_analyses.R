rm(list=ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# Checked: GENEActiv (058029) was removed towards end of experiment ms_hfcr and data not included

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

# Select data of high and low sampling frequency experiments only
data <- ms_flat_HA$data[ms_flat_HA$specifications$experiment == "ms_hfcr" | ms_flat_HA$specifications$experiment == "ms_lfcr"]
specifications <- ms_flat_HA$specifications[ms_flat_HA$specifications$experiment == "ms_hfcr" | ms_flat_HA$specifications$experiment == "ms_lfcr",]
rm(ms_flat_HA)
#====================================================================================
###FUNCTIONS

#' deriveSpectrum
#'
#' @description 'deriveSpectrum' derives the frequency spectrum of a signal (called from within derivePeakIntervals to derive smoothed spectrum)
#'
#' @param x Vector that contains the values of the signal (i.e., acceleration)
#' @param sampling_frequency Integer that indicates the sampling frequency of the signal (i.e., the average number of samples obtained in one second in Hz)
#' @param raw Boolean, if TRUE the raw frequency spectrum will be derived, FALSE: smoothed spectrum will be derived
#' @param file_name String to indicate the name of the plot (i.e., "spectrum" result in the following name for plot 1 "spectrum_1.jpeg").
#' @return An object of class "spec", which is a list containing at least the following components: 
#' \item{freq}{Vector of frequencies at which the spectral density is estimated.} 
#' \item{spec}{Vector (for univariate series) or matrix (for multivariate series) of estimates of the spectral density at frequencies corresponding to freq.}
#' \item{coh}{NULL for univariate series. For multivariate time series, a matrix containing the squared coherency between different series.}  
#' \item{phase}{NULL for univariate series. For multivariate time series a matrix containing the cross-spectrum phase between different series.} 
#' \item{series}{The name of the time series.}
#' \item{snames}{For multivariate input, the names of the component series.}
#' \item{method}{The method used to calculate the spectrum.} 
#' @export
deriveSpectrum <- function(x, sampling_frequency, raw = TRUE, file_name) {
  if(raw == TRUE){
    specd <- spectrum(x, log = "no", plot = FALSE) # Default is to calculate the spectrum on a log-scale, but we use the raw data
  } else {
    specd <- spectrum(x, log = "no", span = 75, plot = FALSE) # Smooth the raw signal
  }
  # The frequency axis of the spectrum, specd, is calculated in terms of cycles per sampling interval
  delta <- 1/sampling_frequency
  specd$specx <- specd$freq/delta # Convert to cycles per unit time (so divide by the sampling interval)
  specd$specy <- 2*specd$spec # Multiply the spectrum by 2 to make it actually equal to variance
  
  #Save the plots of the frequency spectra
  if (plot == TRUE){
    jpeg(paste(datadir, paste0(paste(file_name, file, sep = "_"), ".jpeg"), sep = "/plots/"), width=600, height=500, res=120) # start export
    plot(specd$specx, specd$specy,
         main = "HA", xlab = xlabel, ylab = ylabel, type = "l", xlim = XLIM, bty= "l") # Zoom in on low-frequencies (at most 5 is expected based on max rpm)
    dev.off()
  }
  return(specd)
}

#' #' derivePeakIntervals
#' #'
#' #' @description 'derivePeakIntervals' To derive global peak intervals of a raw signal
#' #'
#' #' @param x Vector that contains the values of the signal (i.e., acceleration)
#' #' @param sampling_frequency Integer that indicates the sampling frequency of the signal (i.e., the average number of samples obtained in one second in Hz)
#' #' @param freqContentLim Vector of length 2 that limits the search for peaks in the frequency content (i.e., c(0, 10) limits the search for peaks to 10 Hz)
#' #' @param plot Boolean, if TRUE plots will be saved as .jpeg for the smoothed signal with peak intervals, FALSE: the data will not be plotted.
#' #' @param numberPeaks Boolean, if TRUE the number of peaks that will be found is restricted to a number of 'npeaks', FALSE: there will be no restriction to the number of peaks.
#' #' @param npeaks Integer that indicates the maximum number of peaks that will be derived.
#' #' @param file_name String to indicate the name of the plot (i.e., "initial_intervals" result in the following name for plot 1 "initial_intervals_1.jpeg").
#' #' @return List of vectors indicating the interval of frequency values of the peaks, containing the following elements: \item{start}{Vector of the start values of the derived peaks} \item{end}{Vector of the end values of the derived peaks}
#' #' @importFrom pracma findpeaks
#' #' @export
#' derivePeakIntervals <- function(x, sampling_frequency, freqContentLim, plot = TRUE, numberPeaks, npeaks, file_name = " ") {
#'   spec.smooth <- deriveSpectrum(x, sampling_frequency, raw = FALSE)   # Use smoothed spectrum to find peaks
#'   # Find peaks in frequency spectrum -> limit frequency content to 5 Hz!!
#'   if(numberPeaks == FALSE){ # Without maximum number peaks
#'     peaks <- pracma::findpeaks(spec.smooth$specy[spec.smooth$specx <= freqContentLim[2]], 
#'                                minpeakheight = 2*mean(spec.smooth$specy), 
#'                                minpeakdistance = 2*sampling_frequency)
#'   } else {
#'     peaks <- pracma::findpeaks(spec.smooth$specy[spec.smooth$specx <= freqContentLim[2]], 
#'                                minpeakheight = 2*mean(spec.smooth$specy), 
#'                                minpeakdistance = 2*sampling_frequency, 
#'                                npeaks = npeaks)
#'   }
#'   #Find start and end x-values
#'   start <- spec.smooth$specx[peaks[, 3]] 
#'   end <- spec.smooth$specx[peaks[, 4]]
#'   
#'   #Save the plots of the frequency spectra and indicate the start and end values 
#'   if (plot == TRUE){
#'     jpeg(paste(datadir, paste0(paste(file_name, file, sep = "_"), ".jpeg"), sep = "/plots/"), width=600, height=500, res=120) # start export
#'     plot(spec.smooth$specx, spec.smooth$specy,
#'          main = "HA", xlab = xlabel, ylab = ylabel, type = "l", xlim = freqContentLim, bty= "l") # Zoom in on low-frequencies (at most 5 is expected based on max rpm)
#'     for(i in 1:length(start)){
#'       abline(v=start[i], col = "red")
#'       abline(v=end[i], col = "red")
#'     }  
#'     abline(h = 2* mean(spec.smooth$specy), col='red') #Indicate minimum peak height
#'     dev.off()
#'   }
#'   return(list(start = start, end = end))
#' }
#' 
#' #' defineFreqBins
#' #'
#' #' @description 'defineFreqBins' frequency bins that ensure the derived peaks fall within this bin, using the lower intervals of the derived peaks as input
#' #'
#' #' @param startIntervals Vector that contains the start interval of frequency values of the derived peaks
#' #' @param deviants Vector that contains a list of integers indicating the position of deviant signals. If this is a non-empty vector these intervals will not be considered for defining the bins.
#' #' @param npeaks Integer that indicates the maximum number of peaks that will be derived.
#' #' @param freqContentLim Vector of length 2 that limited the search for peaks in the frequency content (i.e., c(0, 10) limits the search for peaks to 10 Hz). These values will be considered the start and end values of the bins
#' #' @return Vector containing frequency values that define the frequency bins
#' #' @export
#' defineFreqBins <- function(startIntervals, deviants = c(), npeaks, freqContentLim) {
#'   startIntervals <- startIntervals[-deviants] #Disregard deviant signals
#'   start <- data.frame() #Construct a data frame of all start frequency values of the derived peaks
#'   for (indice in 1:length(startIntervals)){
#'     if(length(startIntervals[[indice]]) < npeaks){
#'       next   #If less peaks than npeaks were found, disregard the corresponding start values
#'     }
#'     start <- rbind(start,sort(startIntervals[[indice]]))
#'   }
#'   min_start <- c()  # Get minimum start to ensure peaks will fall within the intervals 
#'   for (index in 1:ncol(start)) {
#'     min_start <- c(min_start, min(start[,index]))
#'   }
#'   return(c(freqContentLim[1], min_start, freqContentLim[2])) 
#' }  

#' deriveComparisonValues
#'
#' @description 'deriveComparisonValues' derives the dominant frequency and the mean power spectral density from a frequency spectrum
#'
#' @param spectrum An object of class "spec"
#' @param freqBins Vector containing frequency values that define the frequency bins (can be derived using the function 'defineFreqBins') 
#' @return Data.frame consisting of two columns: \item{domFreq}{The dominant frequency of the signal in each frequency bin} \item{meanPSD}{The mean power spectral density of the signal in each frequency bin}
#' @export
deriveComparisonValues <- function(spectrum, freqBins){
  df <- data.frame()
  for(i in 1:(length(freqBins)-1)){
    lower <- freqBins[i]
    upper <- freqBins[i+1]
    index <- which(spectrum$specx >= lower & spectrum$specx <= upper)
    df[i, 1] <- spectrum$specx[index[which.max(spectrum$specy[index])]] #dominant frequency
    df[i, 2] <- mean(spectrum$specy[index]) #mean power spectral density
  }
  colnames(df) <- c("domFreq", "meanPSD")
  return(df)
}

#' removeOutliersIQR
#'
#' @description 'removeOutliersIQR' returns a subset of the data excluding the outliers
#' 
#' @param data Data.frame object that contains the complete data set
#' @param yvariable Vector that contains a vector representing the variable of interest (i.e. on which the outliers are detected)
#' @return Data.frame object that contains the data without outliers
#' @export
removeOutliersIQR <- function(data, yvariable){
  Q1 <- quantile(yvariable, .25)
  Q3 <- quantile(yvariable, .75)
  IQR <- IQR(yvariable)
  no_outliers <- subset(data, yvariable > (Q1 - 1.5*IQR) & yvariable < (Q3 + 1.5*IQR))
  return(no_outliers)
}

#====================================================================================
###DATA ANALYSES
# Note: Activpal was set to a sampling frequency of 20 Hz (in both experiments), however, these data were considered both experiments (ms_hfcr and ms_lfcr) as the recorded amplitude was expected to be the same.

# Plot parameters
plot = TRUE
xlabel = "Frequency (Hz)"
ylabel = "Spectral Density (g2/Hz)"
XLIM = c(0, 5) #Limit frequency content to 5 Hz (as 250 rpm / 60 = 4.1667 Hz is the expected max)

# DERIVE RAW FREQUENCY SPECTRA (for visual inspection and save data for further analysis)
raw.spectra_HA <- list() 
raw.spectra_normHA <- list() 

## for all accelerometer files
for (file in 1:length(data)) {
  sampling_frequency <- as.numeric(specifications$sampling_frequency[file])  
  #for horizontal axis
  HA <- data[[file]]$HA
  rawSpectrum_HA <- deriveSpectrum(HA, sampling_frequency, raw = TRUE, file_name = "raw_spectrum_HA")
  raw.spectra_HA[[file]] <- rawSpectrum_HA
  #for normalized horizontal axis
  normHA <- data[[file]]$normHA
  rawSpectrum_normHA <- deriveSpectrum(normHA, sampling_frequency, raw = TRUE, file_name = "raw_spectrum_normHA")
  raw.spectra_normHA[[file]] <- rawSpectrum_normHA
}
rm(rawSpectrum_HA, rawSpectrum_normHA)
names(raw.spectra_HA) <- specifications$serial_number  
names(raw.spectra_normHA) <- specifications$serial_number 

# Save derived raw spectra for HA and normalized HA in one list
raw_spectra <- list(HA = raw.spectra_HA, normHA = raw.spectra_normHA, specifications = data$specifications)
cat("\nSaving data...")
save(raw_spectra, file = paste0(datadir, "frequency_spectra_HA.RData"))
rm(raw.spectra_HA, raw.spectra_normHA, HA, normHA, sampling_frequency, file)

# Visual inspection of the following spectra showed clear deviations for element:
#42 (AP672490), 51 (AP473254), 55 (AP672490), 64 (AP473254), 78 (6011406), and 93 (6011406)
deviations <- c(42, 51, 55, 64, 78, 93)
specifications$serial_number[deviations]
# Raw data inspection of these devices
#AP672490: in both experiments raw values around 7.3 (M = 7.332, SD = 0.01083037) was measured during all shaking frequencies
#AP672490: in both experiments values around 0.3 (M = 0.32503, SD = 0.00628132) was measured during all shaking frequencies
#6011406: in both experiments value around 0 (M = -0.0148, SD = 0.2275247) with min of -14 and max of -22 during shaker frequency 0
# Remove data of these devices for now
data[deviations] <- NULL
specifications <- specifications[-deviations,]
raw_spectra$HA[deviations] <- NULL
raw_spectra$normHA[deviations] <- NULL

# EXPLORE CUTOFFS: Mechanical shaker machine
rpm <- c(30, 50, 75, 100, 125, 150, 175, 200, 225, 250)
shaking_frequencies <- rpm / 60

# for horizontal axis
filename <- "HAspectrum_MS_cutoff"
for (spectrum in 1:length(raw_spectra$HA)) {
  jpeg(paste(datadir, paste0(paste(filename, spectrum, sep = "_"), ".jpeg"), sep = "/plots/"), width=600, height=500, res=120) # start export
    plot(raw_spectra$HA[[spectrum]]$specx, raw_spectra$HA[[spectrum]]$specy,
       main = "HA with MS cutoffs", xlab = xlabel, ylab = ylabel, type = "l", xlim = XLIM, bty= "l") # Zoom in on low-frequencies (at most 5 is expected based on max rpm)
  for(i in 1:length(shaking_frequencies)){
    abline(v=shaking_frequencies[i], col = "red")
    abline(v=shaking_frequencies[i], col = "red")
  }  
  dev.off()
}

# for normalized horizontal axis
filename <- "normHAspectrum_MS_cutoff"
for (spectrum in 1:length(raw_spectra$normHA)) {
  jpeg(paste(datadir, paste0(paste(filename, spectrum, sep = "_"), ".jpeg"), sep = "/plots/"), width=600, height=500, res=120) # start export
  plot(raw_spectra$normHA[[spectrum]]$specx, raw_spectra$normHA[[spectrum]]$specy,
       main = "normalized HA with MS cutoffs", xlab = xlabel, ylab = ylabel, type = "l", xlim = XLIM, bty= "l") # Zoom in on low-frequencies (at most 5 is expected based on max rpm)
  for(i in 1:length(shaking_frequencies)){
    abline(v=shaking_frequencies[i], col = "red")
    abline(v=shaking_frequencies[i], col = "red")
  }  
  dev.off()
}

# COMPUTE: dominant frequency and mean power spectral density for both HA and normalized HA
spectra_HA <- raw_spectra$HA
spectra_normHA <- raw_spectra$normHA

comparison_values_HA <- list()
comparison_values_normHA <- list()

freqBins <- round(c(XLIM[1], shaking_frequencies, XLIM[2]), digits = 2)
#for horizontal axis
for(spectrum in 1:length(raw_spectra$HA)){ 
  cat(paste0(spectrum, "/",  length(spectra_HA), " "))
  spectrum.raw <- spectra_HA[[spectrum]]
  values <- deriveComparisonValues(spectrum.raw, freqBins)
  comparison_values_HA[[spectrum]] <- values
}
#for normalized horizontal axis
for(spectrum in 1:length(raw_spectra$normHA)){ 
  cat(paste0(spectrum, "/",  length(spectra_normHA), " "))
  spectrum.raw <- spectra_normHA[[spectrum]]
  values <- deriveComparisonValues(spectrum.raw, freqBins)
  comparison_values_normHA[[spectrum]] <- values
}
names(comparison_values$HA) <- names(spectra_HA)
names(comparison_values$normHA) <- names(spectra_normHA)

# Save computed comparison values for HA and normalized HA in one list
comparison_values <- list(HA = comparison_values_HA, normHA = comparison_values_normHA)
rm(spectrum, values, spectrum.raw, comparison_values_HA, comparison_values_normHA)
cat("\nSaving data...")
save(comparison_values, file = paste0(datadir, "domfreq_meanPSD_HA_freqbins.RData"))
rm(raw_spectra)

## PREPARE: data set for mixed model analyses
# Wide format: for both domFreq and meanPSD
df_wide <- data.frame()
id <- names(comparison_values$HA)
df_wide <- cbind(id, specifications$brand, specifications$experiment, specifications$sampling_frequency, specifications$dynamic_range)

domFreq_wide_HA <- data.frame() 
domFreq_wide_normHA <- data.frame() 
meanPSD_wide_HA <- data.frame()
meanPSD_wide_normHA <- data.frame()

for (accelerometer in 1:length(comparison_values$HA)) {
  domFreq_wide_HA <- rbind(domFreq_wide_HA, comparison_values$HA[[accelerometer]]$domFreq)
  domFreq_wide_normHA <- rbind(domFreq_wide_normHA, comparison_values$normHA[[accelerometer]]$domFreq)
  meanPSD_wide_HA <- rbind(meanPSD_wide_HA, comparison_values$HA[[accelerometer]]$meanPSD)
  meanPSD_wide_normHA <- rbind(meanPSD_wide_normHA, comparison_values$normHA[[accelerometer]]$meanPSD)
}

#Dominant frequency
domFreq_ms_wide_HA <- cbind(df_wide, domFreq_wide_HA)
colnames(domFreq_ms_wide_HA) <- c("id", "brand", "experiment", "sampling_frequency", "dynamic_range", 
                               "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")
domFreq_ms_wide_normHA <- cbind(df_wide, domFreq_wide_normHA)
colnames(domFreq_ms_wide_normHA) <- c("id", "brand", "experiment", "sampling_frequency", "dynamic_range", 
                               "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")
#Mean PSD
meanPSD_ms_wide_HA <- cbind(df_wide, meanPSD_wide_HA)
colnames(meanPSD_ms_wide_HA) <- c("id", "brand", "experiment", "sampling_frequency", "dynamic_range", 
                               "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")
meanPSD_ms_wide_normHA <- cbind(df_wide, meanPSD_wide_normHA)
colnames(meanPSD_ms_wide_normHA) <- c("id", "brand", "experiment", "sampling_frequency", "dynamic_range", 
                               "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")

# Long format: combining domFreq and meanPSD (gather columns for outcome variables)
library(tidyverse)
domFreq_long_HA <- domFreq_ms_wide_HA %>%
  gather(key = "freqBin", value = "domFreq", t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) %>%
  rstatix::convert_as_factor(id, freqBin, brand, experiment, sampling_frequency, dynamic_range)
domFreq_long_normHA <- domFreq_ms_wide_normHA %>%
  gather(key = "freqBin", value = "domFreq", t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) %>%
  rstatix::convert_as_factor(id, freqBin, brand, experiment, sampling_frequency, dynamic_range)
domFreq_long <- cbind(domFreq_long_HA, domFreq_long_normHA$domFreq)

meanPSD_long_HA <- meanPSD_ms_wide_HA %>%
  gather(key = "freqBin", value = "meanPSD", t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) %>%
  rstatix::convert_as_factor(id, freqBin, brand, experiment, sampling_frequency, dynamic_range)
meanPSD_long_normHA <- meanPSD_ms_wide_normHA %>%
  gather(key = "freqBin", value = "meanPSD", t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) %>%
  rstatix::convert_as_factor(id, freqBin, brand, experiment, sampling_frequency, dynamic_range)
meanPSD_long <- cbind(meanPSD_long_HA, meanPSD_long_normHA$meanPSD)

# Join both data.frame objects to derive a single dataset
df_long <- merge(domFreq_long, meanPSD_long)
colnames(df_long) <- c("id", "brand", "experiment", "sampling_frequency", "dynamic_range", "freqBin", "domFreq_HA", "domFreq_normHA", "meanPSD_HA", "meanPSD_normHA")
cat("\nSaving data...")
save(df_long, file = paste0(datadir, "domfreq_meanPSD_ms_HA_dataset_long.RData"))
rm(comparison_values_HA, accelerometer, df_wide, domFreq_wide, domFreq_wide_HA, domFreq_wide_HA, domFreq_ms_wide_HA, domFreq_ms_wide_normHA, meanPSD_wide, meanPSD_wide_HA, meanPSD_wide_normHA, meanPSD_ms_wide_HA, meanPSD_ms_wide_normHA, domFreq_ms_wide, meanPSD_ms_wide, domFreq_long_HA, domFreq_long_normHA, meanPSD_long_HA, meanPSD_long_normHA)

# Subset: data for high and low frequency experiments
df_high <- df_long[df_long$experiment == "ms_hfcr",] 
df_high <- df_high[order(df_high$id),]
df_low <- df_long[df_long$experiment == "ms_lfcr",] 
df_low <- df_low[order(df_low$id),]

cat("\nSaving data...")
save(df_high, file = paste0(datadir, "domfreq_meanPSD_ms_HA_dataset_long_hf.RData"))
save(df_low, file = paste0(datadir, "domfreq_meanPSD_HA_ms_dataset_long_lf.RData"))

#====================================================================================
# STATISTICAL COMPARISON

############# LOW FREQUENCY EXPERIMENT #############
load(paste(shaker_experiments_folder, "analyses/domfreq_meanPSD_HA_ms_dataset_long_lf.RData", sep = "/"))

## Boxplots
# Dominant frequency - HA
df_low$freqBin <- factor(df_low$freqBin, levels=c("t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10"))
bxp_HA_domfreq_low <- ggpubr::ggboxplot(df_low, x = "freqBin", y = "domFreq_HA",
                                        color = "brand", palette = c("gray", "blue", "red", "green", "orange")) 
bxp_HA_domfreq_low
# Dominant frequency - norm HA
df_low$freqBin_low <- factor(df_low$freqBin, levels=c("t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10"))
bxp_normHA_domfreq <- ggpubr::ggboxplot(df_low, x = "freqBin", y = "domFreq_normHA",
                                        color = "brand", palette = c("gray", "blue", "red", "green", "orange")) 
bxp_normHA_domfreq_low
# Mean PSD - HA
df_low$freqBin <- factor(df_low$freqBin, levels=c("t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10"))
bxp_HA_psd_low <- ggpubr::ggboxplot(df_low, x = "freqBin", y = "meanPSD_HA",
                                    color = "brand", palette = c("gray", "blue", "red", "green", "orange")) 
bxp_HA_psd_low
# Mean PSD - norm HA
df_low$freqBin <- factor(df_low$freqBin, levels=c("t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10"))
bxp_normHA_psd_low <- ggpubr::ggboxplot(df_low, x = "freqBin", y = "meanPSD_normHA",
                                        color = "brand", palette = c("gray", "blue", "red", "green", "orange")) 
bxp_normHA_psd_low

## ASSUMPTIONS
# Levene's tests are significant, so the variances are not equal
# Use the normalized HA for further analyses? Also significant
# Dominant frequency - HA
homogeneity_domFreq_HA_lf <- car::leveneTest(domFreq_HA ~ freqBin, data = df_low)
# Dominant frequency - norm HA
homogeneity_domFreq_normHA_lf <- car::leveneTest(domFreq_normHA ~ freqBin, data = df_low)
# Mean PSD - HA
homogeneity_meanPSD_HA_lf <- car::leveneTest(meanPSD_HA ~ freqBin, data = df_low)
# Mean PSD - norm HA
homogeneity_meanPSD_normHA_lf <- car::leveneTest(meanPSD_normHA ~ freqBin, data = df_low)

# Normal distribution of residuals
# Dominant frequency - HA
ggpubr::ggqqplot(df_low, "domFreq_HA", facet.by = "freqBin")
# Dominant frequency - norm HA
ggpubr::ggqqplot(df_low, "domFreq_normHA", facet.by = "freqBin")
# Mean PSD - HA
ggpubr::ggqqplot(df_low, "meanPSD_HA", facet.by = "freqBin")
# Mean PSD - norm HA
ggpubr::ggqqplot(df_low, "meanPSD_normHA", facet.by = "freqBin")

## MIXED MODEL ANALYSIS
df_low <- df_low[order(df_low$id),] # sort on id
df_low$brand <- as.factor(df_low$brand)
str(df_low$brand) # 1 = "Actigraph", 2 = "Activpal", 3 = "Axivity", 4 = "GENEActiv", 5 = "MOX"      
df_low$brand <- plyr::revalue(df_low$brand, c("1"="0", "2"="1", "3" = "2", "4" = "3", "5" = "4")) # 0 = "Actigraph", 1 = "Activpal", 2 = "Axivity", 3 = "GENEActiv", 4 = "MOX"     

## Starting model ## with random intercept for accelerometer id (to adjust for the repeated measurements of the accelerometer devices)
## Mean PSD - HA
meanPSD_intercept_id_low_HA <- nlme::lme(meanPSD_HA ~ brand, random = ~ 1|id, method="ML", data = df_low)
## Mean PSD - norm HA 
meanPSD_intercept_id_low_normHA <- nlme::lme(meanPSD_normHA ~ brand, random = ~ 1|id, method="ML", data = df_low)
## Dominant frequency - HA
domFreq_intercept_id_low_HA <- nlme::lme(domFreq_HA ~ brand, random = ~ 1|id, method="ML", data = df_low)
## Dominant frequency - norm HA
domFreq_intercept_id_low_normHA <- nlme::lme(domFreq_normHA ~ brand, random = ~ 1|id, method="ML", data = df_low)

## Add random intercept ## for freqBin (to adjust for the frequencies)
## Mean PSD - HA
meanPSD_intercept_freqbin_low_HA <- nlme::lme(meanPSD_HA ~ brand, random = list(~ 1|id, ~ 1|freqBin), method="ML", data = df_low)
## Mean PSD - norm HA
meanPSD_intercept_freqbin_low_normHA <- nlme::lme(meanPSD_normHA ~ brand, random = list(~ 1|id, ~ 1|freqBin), method="ML", data = df_low)
## Dominant frequency - HA
domFreq_intercept_freqbin_low_HA <- nlme::lme(domFreq_HA ~ brand, random = list(~ 1|id, ~ 1|freqBin), method="ML", data = df_low)
## Dominant frequency - norm HA
domFreq_intercept_freqbin_low_normHA <- nlme::lme(domFreq_normHA ~ brand, random = list(~ 1|id, ~ 1|freqBin), method="ML", data = df_low)

## Evaluate ## if adding a random intercept for freqBin is necessary
## Mean PSD - HA
diff_ll_meanPSD_low_HA_id_freqbin <- (-2*(logLik(meanPSD_intercept_id_low_HA))) - (-2*logLik(meanPSD_intercept_freqbin_low_HA))
(diff_ll_meanPSD_low_HA_id_freqbin > 3.84) #FALSE adding a random intercept for frequency bin to the model is not necessary
## Mean PSD - normHA
diff_ll_meanPSD_low_normHA_id_freqbin <- (-2*(logLik(meanPSD_intercept_id_low_normHA))) - (-2*logLik(meanPSD_intercept_freqbin_low_normHA))
(diff_ll_meanPSD_low_normHA_id_freqbin > 3.84) #FALSE adding a random intercept for frequency bin to the model is not necessary
## Dominant frequency - HA
diff_ll_domFreq_low_HA_id_freqbin <- (-2*(logLik(domFreq_intercept_id_low_HA))) - (-2*logLik(domFreq_intercept_freqbin_low_HA))
(diff_ll_domFreq_low_HA_id_freqbin > 3.84) #FALSE adding a random intercept for frequency bin to the model is not necessary
## Dominant frequency - norm HA
diff_ll_domFreq_low_normHA_id_freqbin <- (-2*(logLik(domFreq_intercept_id_low_normHA))) - (-2*logLik(domFreq_intercept_freqbin_low_normHA))
(diff_ll_domFreq_low_normHA_id_freqbin > 3.84) #FALSE adding a random intercept for frequency bin to the model is not necessary
#Adding of a random intercept for freqBin is not necessary for the low frequency experiment
rm(meanPSD_intercept_freqbin_low_HA, meanPSD_intercept_freqbin_low_normHA, domFreq_intercept_freqbin_low_HA, domFreq_intercept_freqbin_low_normHA)

## Report ## model with the random intercept for id and rerun re-leveled model
df_low$brand <- relevel(df_low$brand, ref = 5) 
levels(df_low$brand) #check brand levels
## Mean PSD - HA
meanPSD_intercept_id_low_HA <- nlme::lme(meanPSD_HA ~ brand, random = ~ 1|id, method="ML", data = df_low)
summary(meanPSD_intercept_id_low_HA) #fixed effect estimates
lme4::VarCorr(meanPSD_intercept_id_low_HA) #random effect estimates
## Mean PSD - norm HA
meanPSD_intercept_id_low_normHA <- nlme::lme(meanPSD_normHA ~ brand, random = ~ 1|id, method="ML", data = df_low)
summary(meanPSD_intercept_id_low_normHA) #fixed effect estimates
lme4::VarCorr(meanPSD_intercept_id_low_normHA) #random effect estimates
## Dominant frequency - HA
domFreq_intercept_id_low_HA <- nlme::lme(domFreq_HA ~ brand, random = ~ 1|id, method="ML", data = df_low)
summary(domFreq_intercept_id_low_HA) #fixed effect estimates
lme4::VarCorr(domFreq_intercept_id_low_HA) #random effect estimates
## Dominant frequency - norm HA
domFreq_intercept_id_low_normHA <- nlme::lme(domFreq_normHA ~ brand, random = ~ 1|id, method="ML", data = df_low)
summary(domFreq_intercept_id_low_normHA) #fixed effect estimates
lme4::VarCorr(domFreq_intercept_id_low_normHA) #random effect estimates

############# HIGH FREQUENCY EXPERIMENT #############
load(paste(shaker_experiments_folder, "analyses/domfreq_meanPSD_ms_HA_dataset_long_hf.RData", sep = "/"))

## Boxplots
# Dominant frequency - HA
df_high$freqBin <- factor(df_high$freqBin, levels=c("t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10"))
bxp_HA_domfreq_high <- ggpubr::ggboxplot(df_high, x = "freqBin", y = "domFreq_HA",
                                         color = "brand", palette = c("gray", "blue", "red", "green", "orange")) 
bxp_HA_domfreq_high
# Dominant frequency - norm HA
df_high$freqBin <- factor(df_high$freqBin, levels=c("t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10"))
bxp_normHA_domfreq_high <- ggpubr::ggboxplot(df_high, x = "freqBin", y = "domFreq_normHA",
                                             color = "brand", palette = c("gray", "blue", "red", "green", "orange")) 
bxp_normHA_domfreq_high
# Mean PSD - HA
df_high$freqBin <- factor(df_high$freqBin, levels=c("t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10"))
bxp_HA_psd_high <- ggpubr::ggboxplot(df_high, x = "freqBin", y = "meanPSD_HA",
                                     color = "brand", palette = c("gray", "blue", "red", "green", "orange")) 
bxp_HA_psd_high
# Mean PSD - norm HA
df_high$freqBin <- factor(df_high$freqBin, levels=c("t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10"))
bxp_normHA_psd_high <- ggpubr::ggboxplot(df_high, x = "freqBin", y = "meanPSD_normHA",
                                         color = "brand", palette = c("gray", "blue", "red", "green", "orange")) 
bxp_normHA_psd_high

## ASSUMPTIONS
# Levene's tests are significant, so the variances are not equal
# Use the normalized HA for further analyses? Also significant
# Dominant frequency - HA
homogeneity_domFreq_HA_hf <- car::leveneTest(domFreq_HA ~ freqBin, data = df_high)
# Dominant frequency - norm HA
homogeneity_domFreq_normHA_hf <- car::leveneTest(domFreq_normHA ~ freqBin, data = df_high)
# Mean PSD - HA
homogeneity_meanPSD_HA_hf <- car::leveneTest(meanPSD_HA ~ freqBin, data = df_high)
# Mean PSD - norm HA
homogeneity_meanPSD__norm_HA_hf <- car::leveneTest(meanPSD_normHA ~ freqBin, data = df_high)

# Normal distribution of residuals
# Dominant frequency - HA
ggpubr::ggqqplot(df_high, "domFreq_HA", facet.by = "freqBin")
# Dominant frequency - norm HA
ggpubr::ggqqplot(df_high, "domFreq_normHA", facet.by = "freqBin")
# Mean PSD - HA
ggpubr::ggqqplot(df_high, "meanPSD_HA", facet.by = "freqBin")
# Mean PSD - norm HA
ggpubr::ggqqplot(df_high, "meanPSD_normHA", facet.by = "freqBin")

## MIXED MODEL ANALYSIS
df_high <- df_high[order(df_high$id),] # sort on id
df_high$brand <- as.factor(df_high$brand)
str(df_high$brand) # 1 = "Actigraph", 2 = "Activpal", 3 = "Axivity", 4 = "GENEActiv", 5 = "MOX"      
df_high$brand <- plyr::revalue(df_high$brand, c("1"="0", "2"="1", "3" = "2", "4" = "3", "5" = "4")) # 0 = "Actigraph", 1 = "Activpal", 2 = "Axivity", 3 = "GENEActiv", 4 = "MOX"     

## Starting model ## with random intercept for accelerometer id (to adjust for the repeated measurements of the accelerometer devices)
## Mean PSD - HA
meanPSD_intercept_id_high_HA <- nlme::lme(meanPSD_HA ~ brand, random = ~ 1|id, method="ML", data = df_high)
## Mean PSD - norm HA 
meanPSD_intercept_id_high_normHA <- nlme::lme(meanPSD_normHA ~ brand, random = ~ 1|id, method="ML", data = df_high)
## Dominant frequency - HA
domFreq_intercept_id_high_HA <- nlme::lme(domFreq_HA ~ brand, random = ~ 1|id, method="ML", data = df_high)
## Dominant frequency - norm HA
domFreq_intercept_id_high_normHA <- nlme::lme(domFreq_normHA ~ brand, random = ~ 1|id, method="ML", data = df_high)

## Add random intercept ## for freqBin (to adjust for the frequencies)
## Mean PSD - HA
meanPSD_intercept_freqbin_high_HA <- nlme::lme(meanPSD_HA ~ brand, random = list(~ 1|id, ~ 1|freqBin), method="ML", data = df_high)
## Mean PSD - norm HA
meanPSD_intercept_freqbin_high_normHA <- nlme::lme(meanPSD_normHA ~ brand, random = list(~ 1|id, ~ 1|freqBin), method="ML", data = df_high)
## Dominant frequency - HA
domFreq_intercept_freqbin_high_HA <- nlme::lme(domFreq_HA ~ brand, random = list(~ 1|id, ~ 1|freqBin), method="ML", data = df_high)
## Dominant frequency - norm HA
domFreq_intercept_freqbin_high_normHA <- nlme::lme(domFreq_normHA ~ brand, random = list(~ 1|id, ~ 1|freqBin), method="ML", data = df_high)

## Evaluate ## if adding a random intercept for freqBin is necessary
## Mean PSD - HA
diff_ll_meanPSD_high_HA_id_freqbin <- (-2*(logLik(meanPSD_intercept_id_high_HA))) - (-2*logLik(meanPSD_intercept_freqbin_high_HA))
(diff_ll_meanPSD_high_HA_id_freqbin > 3.84) #TRUE adding a random intercept for frequency bin to the model is necessary
## Mean PSD - normHA
diff_ll_meanPSD_high_normHA_id_freqbin <- (-2*(logLik(meanPSD_intercept_id_high_normHA))) - (-2*logLik(meanPSD_intercept_freqbin_high_normHA))
(diff_ll_meanPSD_high_normHA_id_freqbin > 3.84) #FALSE adding a random intercept for frequency bin to the model is not necessary
## Dominant frequency - HA
diff_ll_domFreq_high_HA_id_freqbin <- (-2*(logLik(domFreq_intercept_id_high_HA))) - (-2*logLik(domFreq_intercept_freqbin_high_HA))
(diff_ll_domFreq_high_HA_id_freqbin > 3.84) #FALSE adding a random intercept for frequency bin to the model is not necessary
## Dominant frequency - norm HA
diff_ll_domFreq_high_normHA_id_freqbin <- (-2*(logLik(domFreq_intercept_id_high_normHA))) - (-2*logLik(domFreq_intercept_freqbin_high_normHA))
(diff_ll_domFreq_high_normHA_id_freqbin > 3.84) #FALSE adding a random intercept for frequency bin to the model is not necessary
#Adding of a random intercept for freqBin is not necessary for the low frequency experiment
rm(meanPSD_intercept_freqbin_high_HA, meanPSD_intercept_freqbin_high_normHA, domFreq_intercept_freqbin_high_HA, domFreq_intercept_freqbin_high_normHA)

## Report ## model with the random intercept for id and rerun re-leveled model
df_high$brand <- relevel(df_high$brand, ref = 5) 
levels(df_high$brand) #check brand levels
## Mean PSD - HA
meanPSD_intercept_id_high_HA <- nlme::lme(meanPSD_HA ~ brand, random = ~ 1|id, method="ML", data = df_high)
summary(meanPSD_intercept_id_high_HA) #fixed effect estimates
lme4::VarCorr(meanPSD_intercept_id_high_HA) #random effect estimates
## Mean PSD - norm HA
meanPSD_intercept_id_high_normHA <- nlme::lme(meanPSD_normHA ~ brand, random = ~ 1|id, method="ML", data = df_high)
summary(meanPSD_intercept_id_high_normHA) #fixed effect estimates
lme4::VarCorr(meanPSD_intercept_id_high_normHA) #random effect estimates
## Dominant frequency - HA
domFreq_intercept_id_high_HA <- nlme::lme(domFreq_HA ~ brand, random = ~ 1|id, method="ML", data = df_high)
summary(domFreq_intercept_id_high_HA) #fixed effect estimates
lme4::VarCorr(domFreq_intercept_id_high_HA) #random effect estimates
## Dominant frequency - norm HA
domFreq_intercept_id_high_normHA <- nlme::lme(domFreq_normHA ~ brand, random = ~ 1|id, method="ML", data = df_high)
summary(domFreq_intercept_id_high_normHA) #fixed effect estimates
lme4::VarCorr(domFreq_intercept_id_high_normHA) #random effect estimates



# ## EXPLORE: peaks and intervals for all accelerometer files
# intervalStarts_HA <- list()
# for (file in 1:length(data)) {
#   cat(paste0(file, "/",  length(data), " "))
#   HA <- data[[file]]$HA
#   sampling_frequency <- as.numeric(specifications$sampling_frequency[[file]])
#   intervals <- derivePeakIntervals(HA, sampling_frequency, file_name = "initial_intervals", XLIM, numberPeaks=FALSE) # Find initial intervals to decide on the number of peaks
#   intervalStarts_HA[[file]] <- intervals$start
# }
# names(intervalStarts_HA) <- names(data)
# rm(HA, intervals, file)
# 
# # How many peaks were found for each accelerometer file
# nPeaks <- c()
# for (element in 1:length(intervalStarts_HA)){ 
#   nPeaks <- c(nPeaks, length(intervalStarts_HA[[element]]))
# }
# summary(nPeaks)
# numberPeaksFound <- table(nPeaks) 
# numberPeaksFound
# numberPeaksFound[which.max(numberPeaksFound)]
# rm(element, numberPeaksFound, intervalStarts_HA)
# nPeaks[deviations]
# 
# ## DERIVE PEAK INTERVALS: start intervals for most frequently occurring number of peaks
# npeaks = 15
# startIntervals15_HA <- list()
# for (file in 1:length(data)) {
#   cat(paste0(file, "/",  length(data), " "))
#   HA <- data[[file]]$HA
#   sampling_frequency <- as.numeric(specifications$sampling_frequency[[file]])
#   intervals <- derivePeakIntervals(HA, sampling_frequency, XLIM, plot = TRUE, numberPeaks = TRUE, npeaks, file_name = "15peaks") 
#   startIntervals15_HA[[file]] <- intervals$start
# }
# names(startIntervals15_HA) <- names(data)
# rm(HA, intervals, file, nPeaks)
# 
# ## DEFINE: Global intervals to be used as frequency bins for spectrum comparison
# freq_bins_HA <- defineFreqBins(startIntervals15_HA, deviations, npeaks, XLIM)
# cat("\nSaving data...")
# save(freq_bins_HA, file = paste0(datadir, "frequency_bins_HA.RData"))
# rm(startIntervals15_HA, npeaks)

#====================================================================================
