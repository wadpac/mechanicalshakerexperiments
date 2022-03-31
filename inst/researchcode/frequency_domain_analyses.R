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
  df[2+i, 2] <- max(spectrum$specy[index]) #average power spectral density
  
  colnames(df) <- c("domFreq", "maxPSD")
  
  return(df)
}


# DERIVE RAW FREQUENCY SPECTRA
raw.spectra_HA <- list()
raw.spectra_normHA <- list()
for (record in 1:length(ms_flat_HA$data)) {
  HA <- ms_flat_HA$data[[record]]$HA
  normHA <- ms_flat_HA$data[[record]]$normHA
  sampling_frequency <- as.numeric(ms_flat_HA$specifications$sampling_frequency[record])  
  rawSpectrum_HA <- deriveSpectrum(HA, sampling_frequency, raw = TRUE)
  rawSpectrum_normHA <- deriveSpectrum(normHA, sampling_frequency, raw = TRUE)
  raw.spectra_HA[[record]] <- rawSpectrum_HA
  raw.spectra_normHA[[record]] <- rawSpectrum_normHA
}
rm(HA, normHA, sampling_frequency, rawSpectrum_HA, rawSpectrum_normHA, record)
names(raw.spectra_HA) <- ms_flat_HA$specifications$serial_number  
names(raw.spectra_normHA) <- ms_flat_HA$specifications$serial_number  
raw_spectra <- list(HA = raw.spectra_HA, normHA = raw.spectra_normHA, specifications = ms_flat_HA$specifications)
rm(raw.spectra_HA, raw.spectra_normHA)
cat("\nSaving data...")
save(raw_spectra, file = paste0(datadir, "raw_frequency_spectra.RData"))

### DATA ANALYSIS (first for HA only)

# For sampling frequency of 100; ####
sampling_frequency <- 100
plot = TRUE
xlabel = "Frequency (Hz)"
ylabel = "Spectral Density (g2/Hz)"
XLIM = c(0, 5)
data <- ms_flat_HA$data[ms_flat_HA$specifications$sampling_frequency == sampling_frequency]

#Find peaks and intervals for all recordings with sampling frequency of 100
intervals_HA100 <- list()
for (record in 1:length(data)) {
  cat(paste0(record, "/",  length(data), " "))
  HA <- data[[record]]$HA
  intervals <- derivePeakIntervals(HA, sampling_frequency) 
  intervals_HA100[[record]] <- intervals
}
rm(HA, intervals, record)
names(intervals_HA100) <- names(data)
# Plots of recording 35 and 36 seem deviant!
lengte <- c()
for (element in 1:length(intervals_HA100)){ 
  lengte <- c(lengte, length(intervals_HA100[[element]]$start))
}
lengte 
lengte[35] #The number of peaks indeed show deviant patterns
lengte[36]
numberPeaksFound <- table(lengte) 
lengte[max(numberPeaksFound)] # Set number of peaks to 13 as this is the most frequent number of peaks
rm(element)

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

# Exclude recording 35 and 36 to define the frequency bands for analyses as these were deviant
intervals13_HA100[[35]] <- NULL
intervals13_HA100[[36]] <- NULL

# Define general intervals that can be used to compare the signals
finalintervals_HA100 <- defineFreqBands(intervals13_HA100)

freqBandDefinitionsHA100 <- list(intervalsVaryingPeaks = intervals_HA100, peakLengths = lengte, 
                               intervals13Peaks = intervals13_HA100, 
                               final = finalintervals_HA100)
rm(intervals_HA100, intervals13_HA100, finalintervals_HA100, data, lengte, numberPeaksFound)
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


# Statistical comparison: ANOVA repeated measures; domFreq and brand

