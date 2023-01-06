## Script to load the structured acceleration data for the five experiments: ms_hfcr, ms_lfcr, ms_hfmr, ms_lfmr, ms_bag
## and to subset these data for the analyses of:
# 1) visual_inspection: load all data for plotting
# 2) noise: no-movement segments (shaking_frequency == 0) 

rm(list=ls())
graphics.off()

# User input
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine" # Update to be your local directory
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# Specify file paths
structured_data_dir = paste0(shaker_experiments_folder, "/structured_raw_data")
outputdir = paste0(shaker_experiments_folder, "/analyses")
if (!dir.exists(outputdir)) dir.create(outputdir)

# Specify the analyses
analyse <- "noise" # one of: visual_inspection, noise, ..

#===============================

brands_to_load = c("Actigraph", "Activpal", "Axivity", "GENEActiv", "MOX")

if(analyse == "visual_inspection" | analyse == "noise"){
 experiments_to_load = c("ms_hfcr", "ms_lfcr", "ms_hfmr", "ms_lfmr", "ms_bag") # for all five experiments
} 

tz = "Europe/Amsterdam"

data <- list()
specifications <- data.frame()
counter = 1
for (brand in 1:length(brands_to_load)){
  for (experiment in 1:length(experiments_to_load)){
    cat(paste0("\nBrand: ", brands_to_load[brand], ", experiment: ", experiments_to_load[experiment]))
    if (brands_to_load[brand] != "Axivity" & endsWith(experiments_to_load[experiment], "mr")) { # to avoid loading mixed dynamic range experiments for other devices
      cat(paste0("\nThis device was not included in experiment:"), experiments_to_load[experiment])
      next
    } else if (brands_to_load[brand] == "Activpal" & endsWith(experiments_to_load[experiment], "hfcr")) {
      next # Do not load Activpal data for the ms_hfcr experiment 
    } else{
      load(paste0(structured_data_dir, "/", brands_to_load[brand], "_", experiments_to_load[experiment], ".RData"))
      for (file in 1:length(extracteddata$data)) {
        tmp <- extracteddata$data[[file]] #tmp is the structured data now
        if(length(which(tmp > 0))) {
          tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz = tz)
          if (analyse == "visual_inspection") {
            tmp <- tmp
          } else if (analyse == "noise"){
            tmp <- tmp[tmp$shaking_frequency == 0, ] #select no movement segments
          data$data[[counter]] <- tmp
          counter = counter + 1
          specs <- c(extracteddata$specifications[file, "label"], extracteddata$specifications[file, "serial_number"], brands_to_load[brand], experiments_to_load[experiment],
                     extracteddata$specifications[file, "sampling_frequency"], extracteddata$specifications[file, "dynamic_range"])
          specifications <- rbind(specifications, unname(specs))
          }
        }
      }
    }
  }
}

colnames(specifications) <- c("label", "serial_number", "brand", "experiment", "sampling_frequency", "dynamic_range")
names(data$data) <- specifications$label
data$specifications <- specifications
as.factor(data$specifications$brand)
as.factor(data$specifications$experiment)

if (analyse == "visual_inspection") {
  filename <- "/complete_data.RData"
} else if (analyse == "noise"){
  filename <- "/no_movement.RData"
}

save(data, file = paste0(outputdir, filename))

