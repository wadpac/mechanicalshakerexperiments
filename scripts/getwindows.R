# Read in data description and select relevant information based on protocol and session
getwindows <- function(brand, protocol, session, path, data, protocolfile) {
  library(gdata)
  description <- read.xls(protocolfile, header = TRUE)
  description <- description[which(description$protocol == protocol & description$session == session),]
  # Calculate indices for the windows to select
  start_time <- c()
  end_time <- c()
  tz = "Europe/Amsterdam"
  if (protocol == 1) {
    if (brand == "Actigraph" || brand == "Activpal") {
      selection <- description[description$accelerometers_used == "activpal_actigraph",]
    }
    if (brand == "Axivity" || brand == "GENEactiv" || brand == "Acttrust" || brand == "Shimmer" || brand == "MOX"){
      selection <- description[description$accelerometers_used == "axivity_geneactiv_acttrust_shimmer_mox",]
    }
    start <- strftime(paste0(selection$date[1], selection$start_time[1]), format = "%Y-%m-%d %H:%M:%OS2", tz = tz)
    start_time <- start
    end <- strftime(paste0(selection$date[nrow(selection)], selection$start_time[nrow(selection)]), format = "%Y-%m-%d %H:%M:%OS2", tz = tz)
    end_time <- end
  }
  if (protocol == 2 || protocol == 3) {
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
    d <- data[[pp]]
    if (nrow(d) > 0) {
      selected_data = d
      selected_data$shaking_freqency = -1 # default is -1 frequency
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
          etime = as.POSIXct(end_time[w], tz = tz)
        } else if(brand == "Axivity") {
          stime = as.POSIXct(start_time[w], tz = tz)
          etime = as.POSIXct(end_time[w], tz = tz)
        }
        segment = which(selected_data$time >= stime & selected_data$time < etime)
        if(length(segment) > 0) {
          selected_data$shaking_freqency[segment] = as.numeric(description$mechanical_shaker_setting[w])
          selected_data$condition[segment] = as.character(description$condition[w])
        }
      }
      MissingFreqs = which(selected_data$shaking_freqency == -1)
      if (length(MissingFreqs) > 0) selected_data = selected_data[-MissingFreqs,]
      selected_data_list[[pp]] <- selected_data
    } else {
      selected_data_list[[pp]] <- NULL
    }
  }
  rm(selected_data)
  return(selected_data_list)
  
}


