## Script to load the required acceleration data during no-movement segments for the noise analyses
rm(list=ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

#===============================

# Specify file paths
structured_data_dir = paste0(shaker_experiments_folder, "/structured_raw_data")
outputdir = paste0(shaker_experiments_folder, "/analyses")
if (!dir.exists(outputdir)) dir.create(outputdir)

brands_to_load = c("Actigraph", "Activpal", "Axivity", "GENEActiv", "MOX")
experiments_to_load = c("ms_hfcr", "ms_lfcr", "ms_hfmr", "ms_lfmr", "ms_bag") # for all five experiments

no_movement <- list()
specifications <- data.frame()
counter = 1
for (brand in 1:length(brands_to_load)) {
  for (experiment in 1:length(experiments_to_load)){
    cat(paste0("\nBrand: ", brands_to_load[brand], ", experiment: ", experiments_to_load[experiment]))
    if (brands_to_load[brand] != "Axivity" & endsWith(experiments_to_load[experiment], "mr")) { # to avoid loading mixed dynamic range experiments for other devices
      cat(paste0("\nThis device was not included in experiment:"), experiments_to_load[experiment])
      next
    } else{
      load(paste0(structured_data_dir, "/", brands_to_load[brand], "_", experiments_to_load[experiment], ".RData"))
      for (file in 1:length(extracteddata$data)) {
        tmp <- extracteddata$data[[file]] #tmp is the structured data now
        if(length(which(tmp > 0))) {
          tmp <- tmp[tmp$shaking_frequency == 0,]
          tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
          no_movement$data[[counter]] <- tmp
          counter = counter + 1
          specs <- c(extracteddata$specifications[file,"label"], extracteddata$specifications[file,"serial_number"], brands_to_load[brand], experiments_to_load[experiment],
                     extracteddata$specifications[file,"sampling_frequency"], extracteddata$specifications[file,"dynamic_range"])
          specifications <- rbind(specifications, unname(specs))
        }
      }
    }
  }
}

colnames(specifications) <- c("label", "serial_number", "brand", "experiment", "sampling_frequency", "dynamic_range")
names(no_movement$data) <- specifications$label
no_movement$specifications <- specifications
as.factor(no_movement$specifications$brand)
as.factor(no_movement$specifications$experiment)

save(no_movement, file = paste0(outputdir, "/no_movement.RData"))

