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

deriveSpectrum <- function(x, sampling_frequency) {
  specd.raw <- spectrum(x, log = "no", plot = FALSE) # the default for spectrum is to calculate the spectrum on a log-scale
  #or smooth the spectrum/data using a filter -> Butterworth band-pass filter?
  #spec.smooth <- spectrum(x, log = "no", span = 10) # use the argument span (specifies the parameter(s) for the what is known as the modified Daniell kernel) to smooth data
  delta <- 1/sampling_frequency
  specd.raw$specx <- specd.raw$freq/delta # spectrum calculates the frequency axis in terms of cycles per sampling interval -> convert to cycles per unit time (so divide by the sampling interval)
  specd.raw$specy <- 2*specd.raw$spec # spectrum needs to be multiplied by 2 to make it actually equal to variance
  #dom.freq.raw <- specx.raw[which.max(specy.raw)] # extract the dominant frequency
  return(specd.raw)
}

freq_spec <- list()
Nobs = length(ms_flat_HA$data)
# plot parameters
plot = TRUE
xlabel = "Frequency (Hz)"
ylabel = "Spectral Density (g2/Hz)"
XLIM = c(0, 5)
for(observation in 1:Nobs){ 
  cat(paste0(observation, "/",  Nobs, " "))
  HA <- ms_flat_HA$data[[observation]]$x
  normHA <- ms_flat_HA$data[[observation]]$normHA
  sampling_frequency <- as.numeric(ms_flat_HA$specifications$sampling_frequency[observation])
  # spec.raw.HA <- deriveSpectrum(HA, sampling_frequency)
  # spec.raw.normHA <- deriveSpectrum(normHA, sampling_frequency)
  freq_spec$periodigram_HA[[observation]] <- deriveSpectrum(HA, sampling_frequency)
  freq_spec$periodigram_normHA[[observation]] <- deriveSpectrum(normHA, sampling_frequency)  
  if (plot == TRUE) {
    par(mfrow = c(1,2))
    plot(freq_spec$periodigram_HA[[observation]]$specx, freq_spec$periodigram_HA[[observation]]$specy,
         main = "HA", xlab = xlabel, ylab = ylabel, type = "l", xlim = XLIM, bty= "l") # zoom in on low-frequencies, as at most 5 is expected based on max rpm)
    plot(freq_spec$periodigram_normHA[[observation]]$specx, freq_spec$periodigram_normHA[[observation]]$specy,
         main = "normHA", xlab = xlabel, ylab =  ylabel, type = "l", xlim = XLIM, bty= "l") # zoom in on low-frequencies, as at most 5 is expected based on max rpm)
  }

}
names(freq_spec$periodigram_HA) <- ms_flat_HA$specifications$serial_number  
names(freq_spec$periodigram_normHA) <- ms_flat_HA$specifications$serial_number  
freq_spec$specifications <- ms_flat_HA$specifications
cat("\nSaving data...")
# setwd(datadir)
save(freq_spec, file = paste0(datadir, "frequency_spectra.RData"))