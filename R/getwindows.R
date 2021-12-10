# Read in data description and select relevant information based on experiment
getwindows <- function(brand, experiment, path, data, experimentfile) {
  library(gdata)
  description <- read.xls(experimentfile, header = TRUE)
  description <- description[which(description$experiment == experiment),]
  # Calculate indices for the windows to select
  start_time <- c()
  end_time <- c()
  tz = "Europe/Amsterdam"
  if (experiment == "box") {
    start <- strftime(toString(paste(description$date[1], description$start_time[1]), sep = " "), format = "%Y-%m-%d %H:%M:%OS2", tz = tz)
    start_time <- start
    end <- strftime(toString(paste(description$date[1], description$end_time[1]), sep = " "), format = "%Y-%m-%d %H:%M:%OS2", tz = tz)
    end_time <- end
  }
  if (experiment == "timer_check") {
    if (brand == "Actigraph" | brand == "Activpal") {
      selection <- description[description$accelerometers_used == "activpal_actigraph",]
    }
    if (brand == "Axivity" | brand == "GENEActiv" | brand == "Acttrust" | brand == "Shimmer" | brand == "MOX"){
      selection <- description[description$accelerometers_used == "axivity_geneactiv_acttrust_shimmer_mox",]
    }
    start <- strftime(paste0(selection$date[1], selection$start_time[1]), format = "%Y-%m-%d %H:%M:%OS2", tz = tz)
    start_time <- start
    end <- strftime(paste0(selection$date[nrow(selection)], selection$start_time[nrow(selection)]), format = "%Y-%m-%d %H:%M:%OS2", tz = tz)
    end_time <- end
  }
  if (startsWith(experiment, "ms")) { #does not work yet for extracting data for door experiment
    for (r in 1:nrow(description)) {
      if(startsWith(description$accelerometers_used[r], "all") || (brand == "Axivity" & description$accelerometers_used[r] == "axivity")){
        start <- strftime(toString(paste(description$date[r], description$start_time[r]), sep = " "), format = "%Y-%m-%d %H:%M:%OS2", tz = tz)
        start_time <- c(start_time, start)
        end <- strftime(toString(paste(description$date[r], description$end_time[r]), sep = " "), format = "%Y-%m-%d %H:%M:%OS2", tz = tz)
        end_time <- c(end_time, end)
      }
    }
  }
  selected_data_list <- list()
  selected_data <- data.frame()
  #Select the windows
  cat("file ")
  tz = "Europe/Amsterdam"
  for(pp in 1:length(data)) { # pp is file number?
    cat(paste0(" ",pp))
    if (brand %in% c("Axivity", "GENEActiv")) {
      # GENEActiv and Axivity come with separate file header object
      # which we do not need anymore, and by moving the data up we
      # standardise the object structure:
      if (brand == "GENEActiv") {
        data[[pp]] = data[[pp]]$data.out
      } else if (brand == "Axivity") {
        data[[pp]] = data[[pp]]$data
      }
    }
    if (nrow(data[[pp]]) > 0) {
      selected_data = data[[pp]]
      selected_data$shaking_frequency = -1 # default is -1 frequency
      selected_data$condition = ""
      for(w in 1:length(start_time)) { # w is condition within the experiment (e.g. shaker frequency)
        if(brand == "Actigraph") {
          stime = as.POSIXct(start_time[w], tz = tz)
          etime = as.POSIXct(end_time[w], tz = tz)
        } else if (brand == "Activpal") {
          stime = as.POSIXlt(start_time[w], tz = tz)
          etime = as.POSIXlt(end_time[w], tz = tz)
        } else if (brand == "Acttrust") {
          stime = as.POSIXlt(start_time[w], tz = tz)
          etime = as.POSIXlt(end_time[w], tz = tz)
        } else if(brand == "Axivity") {
          stime = as.POSIXct(start_time[w], tz = tz)
          etime = as.POSIXct(end_time[w], tz = tz)
        } else if(brand == "GENEActiv") {
          stime = as.POSIXct(start_time[w], tz = tz)
          etime = as.POSIXct(end_time[w], tz = tz)
        } else if(brand == "MOX") {
          stime = as.POSIXct(start_time[w], tz = tz)
          etime = as.POSIXct(end_time[w], tz = tz)
        }
        segment = which(selected_data$time >= stime & selected_data$time < etime)
        if(length(segment) > 0) {
          selected_data$shaking_frequency[segment] = as.numeric(description$mechanical_shaker_setting[w])
          selected_data$condition[segment] = as.character(description$condition[w])
        }
      }
      # DO NOT DELETE ALL TIME SEGMENTS WITHOUT SHAKING FREQUENCY
      # BECAUSE THIS WILL CAUSE ARTIFACT IN SIGNAL DURING TRANSITIONS
      # ONLY DELETE IIME BEFORE FIRST AND AFTER LAST CONDITION
      # MissingFreqs = which(selected_data$shaking_frequency == -1)
      # if (length(MissingFreqs) > 0) {
      #   selected_data = selected_data[-MissingFreqs,]
      # }
      if(experiment == "timer_check" | experiment == "door" | experiment == "box") {
        validdata = which(is.na(selected_data$shaking_frequency))
      } else {
        validdata = which(selected_data$shaking_frequency != -1) #Doesn't work for experiment timer_check, door as shaking frequency is NA
      }      
      if (validdata[1] != 1 & validdata[length(validdata)] != nrow(selected_data)) {
        MissingFreqs = c(1:(validdata[1]-1),
                         (validdata[length(validdata)]+1):nrow(selected_data))
        if (length(MissingFreqs) > 0) {
          selected_data = selected_data[-MissingFreqs,]
        }
      }
      selected_data_list[[pp]] <- selected_data
    } else {
      selected_data_list[[pp]] <- NULL
    }
  }
  rm(selected_data)
  return(selected_data_list)
}


