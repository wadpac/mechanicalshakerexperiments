# Script to subset ms_flat_HA from structured data (required for freq. spec and cross-correlation analyses with the experimental data)

rm(list=ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# TO DO: Check which GENEActiv was removed towards end of one of the experiments, and make sure data is not included

#====================================================================================
# Specify file paths
structured_data_dir = paste0(shaker_experiments_folder, "/structured_raw_data")
outputdir = paste0(shaker_experiments_folder, "/analyses")

## Subset data for the analyses into one data.frame for shaker experiment - flat
# Required data: HA (horizontal axis; x-axis), and normalised HA = (HA - M) / SD

brands_to_load = c("Actigraph", "Activpal", "Axivity", "GENEActiv")
experiments_to_load = c("ms_hfcr", "ms_lfcr", "ms_mfcr", "ms_hfmr", "ms_lfmr")

ms_flat_HA <- list()
specifications <- data.frame()
sampling_frequency <- c()
dynamic_range <- c()
serial_numbers <- c()
brands <- c()
experiments <- c()
counter = 1
for (brand in 1:length(brands_to_load)) {
  for (experiment in 1:length(experiments_to_load)){
    if (brands_to_load[brand] != "Axivity" & endsWith(experiments_to_load[experiment], "mr")) { #to avoid loading mixed dynamic range experiments for other devices
      cat(paste0("\nThis device was not included in experiment:"), experiments_to_load[experiment])
      next
    } else{
      
      load(paste0(structured_data_dir, "/", brands_to_load[brand], "_", experiments_to_load[experiment]))
      
      for (file in 1:length(extractedata$data)) {
        tmp <- extractedata$data[[file]] #tmp is the structured data now
        if(length(tmp > 0)) {
          if(brands_to_load[brand] %in% c("Actigraph", "Activpal")) {
            names(tmp) <- tolower(names(tmp))
          }
          tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
          tmp = tmp[, c("time", "x", "shaking_frequency")] # select data for the HA (x-axis), time and shaking_frequency
          tmp$normHA <- (tmp$x - mean(tmp$x)) / sd(tmp$x) # normalize HA
          ms_flat_HA$data[[counter]] <- tmp
          counter = counter + 1
          serial_numbers <- c(serial_numbers, extractedata$specifications[file,"serial_number"])
          brands <- c(brands, brands_to_load[brand])
          experiments <- c(experiments, experiments_to_load[experiment])
          sampling_frequency <- c(sampling_frequency, extractedata$specifications[file,"sampling_frequency"])
          dynamic_range <- c(dynamic_range, extractedata$specifications[file,"dynamic_range"])
        }
      }
    }
  }
}
names(ms_flat_HA$data) <- serial_numbers
specifications <- cbind(serial_numbers, brands, experiments, sampling_frequency, dynamic_range)
colnames(specifications) <- c("serial_number", "brand", "experiment", "sampling_frequency", "dynamic_range")
ms_flat_HA$specifications <- specifications

save(ms_flat_HA, file = paste0(outputdir, "ms_flat_HA.RData"))