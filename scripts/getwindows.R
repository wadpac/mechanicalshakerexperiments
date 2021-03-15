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
      if(startsWith(description_pro_ses$accelerometers_used[r], "all") || (brand == "Axivity" & description_pro_ses$accelerometers_used[r] == "Axivity")){
        start <- strftime(toString(paste(description_pro_ses$date[r], description_pro_ses$start_time[r]), sep = " "), format = "%Y-%m-%d %H:%M:%OS2", tz = "GMT")
        start_time <- c(start_time, start)
        end <- strftime(toString(paste(description_pro_ses$date[r], description_pro_ses$end_time[r]), sep = " "), format = "%Y-%m-%d %H:%M:%OS2", tz = "GMT")
        end_time <- c(end_time, end)
      }
    }
  }
  
  selected_data_list <- list()
  selected_data <- data.frame()
  condition <- c()
  shaking_freqency <- c()
  #event <- c()
  
  #Select the windows
  for(pp in 1:length(data)) {
    d <- data[[pp]]
    for(w in 1:length(start_time)) {
      if(! is.na(start_time[w])) {
        if(brand == "Actigraph") {
          select_window <- subset(d, d$time >= as.POSIXct(start_time[w], tz = "GMT") & d$time <= as.POSIXct(end_time[w], tz = "GMT"), c("time", "X", "Y", "Z"))
        }
        if(brand == "Activpal") {
          select_window <- subset(d, d$time >= as.POSIXlt(start_time[w]) & d$time <= as.POSIXlt(end_time[w]), c("time", "X", "Y", "Z"))
        }
          if(length(select_window$time) > 0) {
            condition <- rep(description_pro_ses$condition[w], nrow(select_window))
            shaking_freqency <- rep(description_pro_ses$mechanical_shaker_setting[w], nrow(select_window))
            #event <- rep(description_pro_ses$event[w], nrow(select_window))
            select_window <- cbind(select_window, condition, shaking_freqency)
          }
      }
      if(length(select_window) > 0) {
        selected_data <- rbind(selected_data, select_window)
      }
    }
    selected_data_list[[pp]] <- selected_data
  }
  
  return(selected_data_list)
  
}