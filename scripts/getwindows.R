# Read in data description and select relevant information based on protocol and session
getwindows <- function(brand, protocol, session, path, data) {
  library(gdata)
  description <- read.xls(paste0(path, "/data_description_V1.xlsx"), header = TRUE)
  description_pro <- description[description$protocol == protocol,]
  description_pro_ses <- description_pro[description_pro$session == session,]
  # Calculate indices for the windows to select
  start_time <- c()
  end_time <- c()
  if (protocol == 1) {
    if (brand == "Actigraph" || brand == "Activpal") {
      selection <- description_pro_ses[description_pro_ses$accelerometers_used == "activpal_actigraph",]
    }
    if (brand == "Axivity" || brand == "GENEactiv" || brand == "Acttrust" || brand == "Shimmer" || brand == "MOX"){
      selection <- description_pro_ses[description_pro_ses$accelerometers_used == "axivity_geneactiv_acttrust_shimmer_mox",]
    }
    start <- strftime(paste0(selection$date[1], selection$start_time[1]), format = "%Y-%m-%d %H:%M:%OS2", tz = "GMT")
    start_time <- start
    end <- strftime(paste0(selection$date[nrow(selection)], selection$start_time[nrow(selection)]), format = "%Y-%m-%d %H:%M:%OS2", tz = "GMT")
    end_time <- end
  }
  if (protocol == 2 || protocol == 3) {
    for (r in 1:nrow(description_pro_ses)) {
      if(startsWith(description_pro_ses$accelerometers_used[r], "all") || (brand == "Axivity" & description_pro_ses$accelerometers_used[r] == "axivity")){
        start <- strftime(toString(paste(description_pro_ses$date[r], description_pro_ses$start_time[r]), sep = " "), format = "%Y-%m-%d %H:%M:%OS2", tz = "GMT")
        start_time <- c(start_time, start)
        end <- strftime(toString(paste(description_pro_ses$date[r], description_pro_ses$end_time[r]), sep = " "), format = "%Y-%m-%d %H:%M:%OS2", tz = "GMT")
        end_time <- c(end_time, end)
      }
    }
  }
  
  selected_data_list <- list()
  selected_data <- data.frame()
  #Select the windows
  cat("file ")
  for(pp in 1:length(data)) { # pp is file number?
    cat(paste0(" ",pp))
    d <- data[[pp]]
    selected_data = d
    selected_data$shaking_freqency = 0
    selected_data$condition = ""
    for(w in 1:length(start_time)) { # w is condition within the experiment (e.g. shaker frequency)
      if(brand == "Actigraph") {
        stime = as.POSIXct(start_time[w], tz = "GMT")
        etime = as.POSIXct(end_time[w], tz = "GMT")
      } else if (brand == "Activpal") {
        stime = as.POSIXlt(start_time[w])
        etime = as.POSIXlt(end_time[w])
        
      } else if (brand == "Acttrust") {
        stime = as.POSIXlt(start_time[w], tz = "GMT")
        etime = as.POSIXct(end_time[w], tz = "GMT")
      } else if(brand == "Axivity") {
        stime = as.POSIXct(start_time[w], tz = "GMT") &
          etime = as.POSIXct(end_time[w], tz = "GMT")
      }
      segment = which(selected_data$time >= stime & selected_data$time < etime)
      if(length(segment) > 0) {
        selected_data$shaking_freqency[segment] = as.numeric(description_pro_ses$mechanical_shaker_setting[w])
        selected_data$condition[segment] = as.character(description_pro_ses$condition[w])
      }
    }
    
    selected_data_list[[pp]] <- selected_data
  }
  rm(selected_data)
  return(selected_data_list)
  
}
