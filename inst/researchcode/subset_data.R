## Script to load the structured acceleration data and to subset these data for the analyses of:
# 1) visual_inspection: load all data for plotting
# 2) noise: select the no-movement segments (shaking_frequency == 0), but omit data for the ActiGraph devices in which idle sleep mode was enabled
# 4) E1: analyse differences between brands with high sampling rate (hrcr)
# 3) E2: analyse differences between brands with low sampling rate (lrcr)
# 5) E3: analyse differences between dynamic ranges with low sampling rate (hrmr)
# 6) E4: analyse differences between dynamic ranges with high sampling rate (lrmr)
# 7) E5: analyse influence of sampling rate (mrcr)
# 8) bag: bag experiment, influence of orientation

rm(list=ls())
graphics.off()

# User input
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine" # Update to be your local directory
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# Specify file paths
structured_data_dir = paste0(shaker_experiments_folder, "/structured_raw_data")
outputdir = paste0(shaker_experiments_folder, "/analyses")
if (!dir.exists(outputdir)) dir.create(outputdir)

# Specify the analysis and the brands to include
analysis <- "E5" # one of: c("visual_inspection", "noise", "E1", "E2", "E3", "E4", "E5", "bag")
brands_to_load = c("ActiGraph", "activPAL", "Axivity", "GENEActiv", "MOX")

#===============================

if(analysis == "visual_inspection"){
 experiments_to_load = c("ms_hrcr", "ms_lrcr", "ms_hrmr", "ms_lrmr", "ms_bag") # for all five experiments
} else if(analysis == "noise") {
  experiments_to_load = c("ms_hrcr", "ms_lrcr", "ms_hrmr", "ms_lrmr") # not for ms_bag because axes were oriented randomly
} else if(analysis == "E1") {
  experiments_to_load = c("ms_hrcr") 
} else if(analysis == "E2") {
  experiments_to_load = c("ms_lrcr") 
} else if(analysis == "E3") {
  experiments_to_load = c("ms_hrmr") 
} else if(analysis == "E4") {
  experiments_to_load = c("ms_lrmr") 
} else if(analysis == "E5") {
  experiments_to_load = c("ms_mrcr") 
} else if(analysis == "bag") {
  experiments_to_load = c("ms_bag") 
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
    } else if (brands_to_load[brand] == "activPAL" & endsWith(experiments_to_load[experiment], "hrcr")) {
      next # Do not load activPAL data for the ms_hrcr experiment (in noise analyses to avoid double observations with same settings)
    } else{
      load(paste0(structured_data_dir, "/", brands_to_load[brand], "_", experiments_to_load[experiment], ".RData"))
      
      if (analysis == "noise" & brands_to_load[brand] == "ActiGraph" & experiments_to_load[experiment] == "ms_hrcr"){ # Omit data of devices in which idle.sleep.mode was enabled during ms_hrcr
          sleep.mode.devices <- c("AG_CLE_039", "AG_CLE_077", "AG_CLE_091", "AG_CLE_132", "AG_MOS_028", "AG_MOS_192", "AG_MOS_352", "AG_MOS_527", "AG_MOS_008")
          index.sleep.mode <- which(extracteddata$specifications[,c("label")] %in% sleep.mode.devices)
          extracteddata$data[index.sleep.mode] <- NULL
          extracteddata$specifications <- extracteddata$specifications[-index.sleep.mode,]
      }
      for (file in 1:length(extracteddata$data)) {
        tmp <- extracteddata$data[[file]] #tmp is the structured data now
        tmp = tmp[, c("time", "x", "y", "z","shaking_frequency", "condition")] # ensure the data format is equal for all devices
        if(length(which(tmp > 0))) {
          tmp <- tmp[which(tmp$shaking_frequency != "-1"),] # Remove data outside experiments (shaking frequency -1)
          tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz = tz)
          if (analysis == "visual_inspection") {
            tmp <- tmp
          } else if (analysis == "noise"){
            tmp <- tmp[tmp$shaking_frequency == 0, ] #select no movement segments
          } else if (analysis == "E1" | analysis == "E2" | analysis == "E3"| analysis == "E4" | analysis == "E5") {
            if(experiments_to_load[experiment] == "ms_hrcr"){ # Remove the data due to repetition of the experiment
              # These are the start and end times from the description file where accelerometers_used = all_except_one_GENEActiv
              start1 <- as.POSIXlt("2020-11-24 9:42:00", tz = "Europe/Amsterdam")
              end1 <- as.POSIXlt("2020-11-24 10:13:00", tz = "Europe/Amsterdam")
              start2 <- as.POSIXlt("2020-11-24 10:35:05", tz = "Europe/Amsterdam")
              end2 <- as.POSIXlt("2020-11-24 10:47:00", tz = "Europe/Amsterdam")
              cleaned_signala <- tmp[which(tmp$time >= start1 & tmp$time <= end1),]
              cleaned_signalb <- tmp[which(tmp$time >= start2 & tmp$time <= end2),]
              tmp <- rbind(cleaned_signala, cleaned_signalb)
            }
            # Select the axis that measures the acceleration signal in the shaking direction
            maxAxes <- c(sd(tmp$x), sd(tmp$y), sd(tmp$z)) # calculate the standard deviation of the axes
            SD <- unlist(tmp[which.max(maxAxes) + 1]) # select the axis with the highest SD as this will be the dominant axis in the shaking direction
            if(analysis == "bag") {
              VM <- sqrt(tmp$x^2 + tmp$y^2 + tmp$z^2) # calculate the vector magnitude
            }
            tmp = tmp[, c("shaking_frequency", "time")] # select data for the correct axis, time, and shaking_frequency
            tmp$SD <- SD
            if(analysis == "bag") {
              tmp$VM <- VM # add VM to the data
            }
          }
          }
          data$data[[counter]] <- tmp
          counter = counter + 1
          specs <- c(extracteddata$specifications[file, "label"], extracteddata$specifications[file, "serial_number"], brands_to_load[brand], experiments_to_load[experiment],
                     extracteddata$specifications[file, "sampling_rate"], extracteddata$specifications[file, "dynamic_range"])
          specifications <- rbind(specifications, unname(specs))
        }
      }
    }
  }
}

colnames(specifications) <- c("label", "serial_number", "brand", "experiment", "sampling_rate", "dynamic_range")
names(data$data) <- specifications$label
data$specifications <- specifications
as.factor(data$specifications$brand)
as.factor(data$specifications$experiment)

if (analysis == "visual_inspection") {
  filename <- "/complete_data.RData"
} else if (analysis == "noise"){
  filename <- "/no_movement.RData"
} else if (analysis == "E1"){
  filename <- "/E1_hrcr.RData"
} else if (analysis == "E2"){
  filename <- "/E2_lrcr.RData"
} else if (analysis == "E3"){
  filename <- "/E3_hrmr.RData"
} else if (analysis == "E4"){
  filename <- "/E4_lrmr.RData"
} else if (analysis == "E5"){
  filename <- "/E5_mrcr.RData"
} else if (analysis == "bag"){
  filename <- "/ms_bag.RData"
}

save(data, file = paste0(outputdir, filename))

