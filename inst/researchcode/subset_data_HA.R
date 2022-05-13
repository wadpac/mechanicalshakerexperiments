# Script to subset ms_flat_HA from structured data (required for freq. spec and cross-correlation analyses with the experimental data)

rm(list=ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# Check: GENEActiv data (lfcr and hfcr) not included, only the data of this device (058029) for the ms_mfcr experiment is taken into account
#====================================================================================
# Specify file paths
structured_data_dir = paste0(shaker_experiments_folder, "/structured_raw_data")
outputdir = paste0(shaker_experiments_folder, "/analyses")
if (!dir.exists(outputdir)) dir.create(outputdir)

## Subset data for the analyses into one data.frame for shaker experiment - flat
# Required data: HA (horizontal axis; x-axis), and normalized HA = (HA - M) / SD

brands_to_load = c("Actigraph", "Activpal", "Axivity", "GENEActiv")
experiments_to_load = c("ms_hfcr", "ms_lfcr", "ms_mfcr", "ms_hfmr", "ms_lfmr")

ms_flat_HA <- list()
specifications <- data.frame()
serial_numbers <- c()

counter = 1
for (brand in 1:length(brands_to_load)) {
  for (experiment in 1:length(experiments_to_load)){
    if (brands_to_load[brand] != "Axivity" & endsWith(experiments_to_load[experiment], "mr")) { #to avoid loading mixed dynamic range experiments for other devices
      cat(paste0("\nThis device was not included in experiment:"), experiments_to_load[experiment])
      next
    } else{
      load(paste0(structured_data_dir, "/", brands_to_load[brand], "_", experiments_to_load[experiment], ".RData"))
      for (file in 1:length(extracteddata$data)) {
        tmp <- extracteddata$data[[file]] #tmp is the structured data now
        if(length(tmp > 0)) {
          if(brands_to_load[brand] %in% c("Actigraph", "Activpal")) {
            names(tmp) <- tolower(names(tmp))
          }
          tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
          # Check if the x-axis was the axis aligned with the shaker direction 
          maxAxes <- c(sd(tmp$x), sd(tmp$y), sd(tmp$z)) # calculate the standard deviation of the axes
          HA <- unlist(tmp[which.max(maxAxes) + 1]) # select the axis with the highest SD as this will be the shaking direction
          
          tmp = tmp[, c("shaking_frequency", "time")] # select data for the HA (x-axis), time and shaking_frequency
          tmp$HA <- HA
          tmp$normHA <- (tmp$HA - mean(tmp$HA)) / sd(tmp$HA) # normalize HA
          ms_flat_HA$data[[counter]] <- tmp
          counter = counter + 1
          specs <- c(extracteddata$specifications[file,"serial_number"], brands_to_load[brand], experiments_to_load[experiment], 
                     extracteddata$specifications[file,"sampling_frequency"], extracteddata$specifications[file,"dynamic_range"])
          specifications <- rbind(specifications, unname(specs))
        }
      }
    }
  }
}
colnames(specifications) <- c("serial_number", "brand", "experiment", "sampling_frequency", "dynamic_range")
names(ms_flat_HA$data) <- specifications$serial_number
ms_flat_HA$specifications <- specifications
as.factor(ms_flat_HA$specifications$brand)
as.factor(ms_flat_HA$specifications$experiment)

save(ms_flat_HA, file = paste0(outputdir, "/ms_flat_HA.RData"))