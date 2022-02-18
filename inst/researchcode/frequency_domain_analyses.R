rm(list=ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
#shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# TO DO: Check which GENEActiv was removed towards end of one of the experiments, and make sure data is not included

#====================================================================================
# Specify file paths
datadir = paste0(shaker_experiments_folder, "/analyses/")

# Load mechanical shaker data for horizontal axis 
setwd(datadir)
load(paste0(datadir, "ms_flat_HA.RData"))

deriveSpectrum <- function(x, sampling_frequency) {
  specd.raw <- spectrum(x, log = "no", plot = FALSE) # the default for spectrum is to calculate the spectrum on a log-scale
  #or smooth the spectrum/data using a filter -> Butterworth band-pass filter?
  #spec.smooth <- spectrum(x, log = "no", span = 10) # use the argument span (specifies the parameter(s) for the what is known as the modified Daniell kernel) to smooth data
  delta <- 1/sampling_frequency
  specd.raw$specx <- specd.raw$freq/delta # spectrum calculates the frequency axis in terms of cycles per sampling interval -> convert to cycles per unit time (so divide by the sampling interval)
  specd.raw$specy <- 2*specd.raw$spec # spectrum needs to be multiplied by 2 to make it actually equal to variance
  #dom.freq.raw <- specx.raw[which.max(specy.raw)] # extract the dominant frequency
  plot(specd.raw$specx, specd.raw$specy, xlab="Frequency (Hz)", ylab="Spectral Density (g2/Hz)", type="l", xlim = c(0,5)) # zoom in on low-frequencies, as at most 5 is expected based on max rpm)
  return(specd.raw)
}

freq_spec <- list()

for(observation in 1:length(ms_flat_HA$data)){ 
  HA <- ms_flat_HA$data[[observation]]$x
  normHA <- ms_flat_HA$data[[observation]]$normHA
  sampling_frequency <- as.numeric(ms_flat_HA$specifications$sampling_frequency[observation])
  spec.raw.HA <- deriveSpectrum(HA, sampling_frequency)
  spec.raw.normHA <- deriveSpectrum(normHA, sampling_frequency)
  freq_spec$periodigram_HA[[observation]] <- deriveSpectrum(HA, sampling_frequency)
  freq_spec$periodigram_normHA[[observation]] <- deriveSpectrum(normHA, sampling_frequency)   
}
names(freq_spec$periodigram_HA) <- ms_flat_HA$specifications$serial_number  
names(freq_spec$periodigram_normHA) <- ms_flat_HA$specifications$serial_number  
freq_spec$specifications <- ms_flat_HA$specifications

setwd(datadir)
save(freq_spec, file = "frequency_spectra.RData")