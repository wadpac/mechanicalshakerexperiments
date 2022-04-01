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
# Function to derive frequency spectrum of a signal (x, a vector); if raw = TRUE the raw spectrum is derived
deriveSpectrum <- function(x, sampling_frequency, raw = TRUE) {
  if(raw == TRUE){
    specd <- spectrum(x, log = "no", plot = FALSE) # the default for spectrum is to calculate the spectrum on a log-scale, use the raw data
  } else {
    specd <- spectrum(x, log = "no", span = 75, plot = FALSE)   #Use the smoothed spectrum
  }
  delta <- 1/sampling_frequency
  specd$specx <- specd$freq/delta # spectrum calculates the frequency axis in terms of cycles per sampling interval -> convert to cycles per unit time (so divide by the sampling interval)
  specd$specy <- 2*specd$spec # spectrum needs to be multiplied by 2 to make it actually equal to variance
  #dom.freq.raw <- specx.raw[which.max(specy.raw)] # extract the dominant frequency
  return(specd)
}

# Function to derive peak intervals of a signal (x, a vector); if npeaks = TRUE the number of peaks is specified
derivePeakIntervals <- function(x, sampling_frequency, plot = TRUE, npeaks = FALSE, numberPeaks = 0) {
  # Use smoothed spectrum to find peaks
  spec.smooth <- deriveSpectrum(x, sampling_frequency, raw = FALSE)
  
  # Find peaks in frequency spectrum
  if(npeaks == TRUE){ # with number specified peaks
    peaks <- pracma::findpeaks(spec.smooth$specy, 
                               minpeakheight = 5*mean(spec.smooth$specy), 
                               minpeakdistance = 2*sampling_frequency, 
                               npeaks = numberPeaks)
  } else {
    peaks <- pracma::findpeaks(spec.smooth$specy, 
                               minpeakheight = 5*mean(spec.smooth$specy), 
                               minpeakdistance = 2*sampling_frequency)
  }
  #Find start and end x-values
  start <- spec.smooth$specx[peaks[, 3]] 
  end <- spec.smooth$specx[peaks[, 4]]
  
  if (plot == TRUE){
    plot(spec.smooth$specx, spec.smooth$specy,
         main = "HA", xlab = xlabel, ylab = ylabel, type = "l", xlim = XLIM, bty= "l") # zoom in on low-frequencies, as at most 5 is expected based on max rpm)
    for(i in 1:length(start)){
      abline(v=start[i], col = "red")
      abline(v=end[i], col = "red")
    }
  }
  return(list(start = start, end = end))
}

# Function to get the minimum start index and the max index from a list of intervals, to ensure peaks are within intervals
defineFreqBands <- function(intervals) {
  start.indices <- sort(intervals[[1]]$start)
  end.indices <- sort(intervals[[1]]$end)  
  for (observation in 2:length(intervals)) {
    start <- sort(intervals[[observation]]$start)
    end <- sort(intervals[[observation]]$end) 
    for(i in 1:length(start)){
      start.indices[i] <- min(start.indices[i], start[i])
      end.indices[i] <- max(end.indices[i], end[i])
    }
  }
  return(list(start = start, end = end)) 
  #mean indices? also an option for this function instead of min max
}  

# Function to derive comparison values (dominant frequency, avg frequency) from a frequency spectrum using intervals
deriveComparisonValues <- function(spectrum, intervals){
  df <- data.frame()
  #start values
  lower <- 0
  upper <- intervals$start[1]
  index <- which(spectrum$specx <= upper)
  df[1, 1] <- spectrum$specx[which.max(spectrum$specy[index])] #dominant frequency
  df[1, 2] <- max(spectrum$specy[index]) #max power spectral density
  
  for(i in 1:length(intervals$start)){
    lower <- intervals$start[i]
    upper <- intervals$end[i]
    index <- which(spectrum$specx >= lower & spectrum$specx <= upper)
    df[1+i, 1] <- spectrum$specx[index[which.max(spectrum$specy[index])]]
    df[1+i, 2] <- max(spectrum$specy[index])
  }
  
  index <- which(spectrum$specx >= upper)
  df[2+i, 1] <- spectrum$specx[index[which.max(spectrum$specy[index])]] #dominant frequency
  df[2+i, 2] <- max(spectrum$specy[index]) #maximum power spectral density
  
  colnames(df) <- c("domFreq", "maxPSD")
  
  return(df)
}


# DERIVE RAW FREQUENCY SPECTRA
raw.spectra_HA <- list() 
raw.spectra_normHA <- list() 

for (file in 1:length(ms_flat_HA$data)) {# derive frequency spectra for all accelerometer files
  sampling_frequency <- as.numeric(ms_flat_HA$specifications$sampling_frequency[file])  
  #for horizontal axis
  HA <- ms_flat_HA$data[[file]]$HA
  rawSpectrum_HA <- deriveSpectrum(HA, sampling_frequency, raw = TRUE)
  raw.spectra_HA[[file]] <- rawSpectrum_HA
  #for normalized horizontal axis
  normHA <- ms_flat_HA$data[[file]]$normHA
  rawSpectrum_normHA <- deriveSpectrum(normHA, sampling_frequency, raw = TRUE)
  raw.spectra_normHA[[file]] <- rawSpectrum_normHA
}
# Indicate the accelerometer serial numbers
names(raw.spectra_HA) <- ms_flat_HA$specifications$serial_number  
names(raw.spectra_normHA) <- ms_flat_HA$specifications$serial_number 
# Save derived raw spectra
raw_spectra <- list(HA = raw.spectra_HA, normHA = raw.spectra_normHA, specifications = ms_flat_HA$specifications)
cat("\nSaving data...")
save(raw_spectra, file = paste0(datadir, "raw_frequency_spectra.RData"))
rm(raw.spectra_HA, raw.spectra_normHA, HA, normHA, sampling_frequency, rawSpectrum_HA, rawSpectrum_normHA, file)

###DATA ANALYSIS (first for HA only)

##SAMPLING FREQUENCY = 100
sampling_frequency <- 100
data <- ms_flat_HA$data[ms_flat_HA$specifications$sampling_frequency == sampling_frequency]

# Plot parameters
plot = TRUE
xlabel = "Frequency (Hz)"
ylabel = "Spectral Density (g2/Hz)"
XLIM = c(0, 5)

#Find peaks and peak intervals for all files with sampling frequency of 100
intervals_HA100 <- list()
for (file in 1:length(data)) {
  cat(paste0(file, "/",  length(data), " "))
  HA <- data[[file]]$HA 
  intervals <- derivePeakIntervals(HA, sampling_frequency) #
  intervals_HA100[[file]] <- intervals
}
rm(HA, intervals, file)
names(intervals_HA100) <- names(data)
# Plots of recording 35 and 36 seem deviant!
#See how many peaks were found for each accelerometer file
nPeaks <- c()
for (element in 1:length(intervals_HA100)){ 
  nPeaks <- c(nPeaks, length(intervals_HA100[[element]]$start))
}
nPeaks #The number of peaks indeed show deviant patterns
nPeaks[35] #Axivity 6011406
nPeaks[36] #Axivity 6011834
numberPeaksFound <- table(nPeaks) 
nPeaks[max(numberPeaksFound)] # Set number of peaks to 13 as this is the most frequent number of peaks
rm(element, numberPeaksFound)

# Derive all intervals for 13 peaks
intervals13_HA100 <- list()
for (record in 1:length(data)) {
  cat(paste0(record, "/",  length(data), " "))
  HA <- data[[record]]$HA
  intervals <- derivePeakIntervals(HA, sampling_frequency, plot = TRUE, npeaks = FALSE, numberPeaks = 13) 
  intervals13_HA100[[record]] <- intervals
}
rm(HA, intervals, record, plot, xlabel, ylabel, XLIM)
names(intervals13_HA100) <- names(data)

# Exclude recording 35 (Axivity 6011406) and 36 (Axivity 6011834) to define the frequency bands for analyses as these were deviant
intervals13_HA100[[35]] <- NULL
intervals13_HA100[[36]] <- NULL

# Define general intervals that can be used to compare the signals
finalintervals_HA100 <- defineFreqBands(intervals13_HA100)

freqBandDefinitionsHA100 <- list(intervalsVaryingPeaks = intervals_HA100, npeaks = nPeaks, 
                               intervals13Peaks = intervals13_HA100, 
                               final = finalintervals_HA100)
rm(intervals_HA100, intervals13_HA100, finalintervals_HA100, data, nPeaks, numberPeaksFound)
cat("\nSaving data...")
save(freqBandDefinitionsHA100, file = paste0(datadir, "frequencyBandDefinitions_HAsf100.RData"))

# Derive values for comparison of spectra (sampling frequency = 100)
spectra100HA <- raw_spectra$HA[ms_flat_HA$specifications$sampling_frequency == sampling_frequency]
comparisonValues_HAsf100 <- list()

for(spectrum in 1:length(spectra100HA)){ 
  cat(paste0(spectrum, "/",  length(spectra100HA), " "))
  spectrum.raw <- spectra100HA[[spectrum]]
  values <- deriveComparisonValues(spectrum.raw, freqBandDefinitionsHA100$final)
  comparisonValues_HAsf100[[spectrum]] <- values
}
rm(spectrum, values, spectrum.raw)
names(comparisonValues_HAsf100) <- names(spectra100HA)
freqspecValues_sf100 <- list(raw_spectra = spectra100HA, comparison_values = comparisonValues_HAsf100)
rm(spectra100HA, comparisonValues_HAsf100)

cat("\nSaving data...")
save(freqspecValues_sf100, file = paste0(datadir, "freqspecValues_HAsf100.RData"))


# STATISTICAL COMPARISON: ANOVA repeated measures (grouping variable = brand, repeated measure domFrequency)
# Prepare dataset for 2-way repeated measures ANOVA: https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/#data-preparation-1

# Wide format; make data frame with id, brand and columns for each measurement of dominant frequency
df_wide <- data.frame()
id <- names(freqspecValues_sf100$comparison_values)
brand <- ms_flat_HA$specifications$brand[ms_flat_HA$specifications$sampling_frequency == 100]
df_wide <- cbind(id, brand)
domFrequency <- data.frame()
for (accelerometer in 1:length(freqspecValues_sf100$comparison_values)) {
  domFrequency <- rbind(domFrequency, freqspecValues_sf100$comparison_values[[accelerometer]]$domFreq)
}
df_wide <- cbind(df_wide, domFrequency)
colnames(df_wide) <- c("id", "brand", "t0", "t1", "t2", "t3", "t4", "t5", "t6", 
                       "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14")

# Gather the columns for dominant frequency measures into long format
library(tidyverse)
# Convert id, brand, and time into factor variables
df_long <- df_wide %>%
  gather(key = "measurement", value = "domFreq", t0, t1, t2, t3, t4, t5, t6, t7, t8, 
         t9, t10, t11, t12, t13, t14) %>%
  rstatix::convert_as_factor(id, measurement, brand)

#Summary statistics
df_long %>%
  group_by(brand, measurement) %>%
  get_summary_stats(domFreq, type = "mean_sd")

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
ggqqplot(df_long, "domFreq", facet.by = "measurement")


#ANOVA
res.aov <- df_long %>% anova_test(domFreq ~ measurement*brand)
get_anova_table(res.aov)

