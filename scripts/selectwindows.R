selectwindows <- function(brand, data, data_description, protocol, session) {
  # Read in data description and select relevant information based on protocol and session
  library(gdata)
  description <- read.xls(data_description, header = TRUE)
  description_pro <- description[description$protocol == protocol,]
  description_pro_ses <- description_pro[description_pro$session == session,]
  
  # Calculate indices for the windows to select
  start_index <- c()
  end_index <- c()
  
  if (protocol == 1) {
    
    if (brand == "Actigraph" || brand == "Activpal") {
      selection <- description_pro_ses[description_pro_ses$accelerometers_used == "activpal_actigraph",]
      start <- strftime(paste0(selection$date[1], selection$start_time[1]), format = "%Y-%m-%d %H:%M:%OS2", tz = "Europe/Amsterdam")
      end <- strftime(paste0(selection$date[nrow(selection)], selection$start_time[nrow(selection)]), format = "%Y-%m-%d %H:%M:%OS2", tz = "Europe/Amsterdam")
      start_index <- which(data$time == as.POSIXct(start, tz = "GMT"))
      end_index <- which(data$time == as.POSIXct(end, tz = "GMT"))
    }
  }
  
  else {
    for (r in 1:nrow(description_pro_ses)) {
      start <- strftime(toString(paste(description_pro_ses$date[r], description_pro_ses$start_time[r]), sep = " "), format = "%Y-%m-%d %H:%M:%OS2", tz = "GMT")
      end <- strftime(toString(paste(description_pro_ses$date[r], description_pro_ses$end_time[r]), sep = " "), format = "%Y-%m-%d %H:%M:%OS2", tz = "GMT")
      
      if(brand == "Actigraph") {
        # Calculate indices
        start_i <- which(data$time == as.POSIXct(start, tz = "GMT"))
        if(length(start_i) == 0) {start_index <- c(start_index, NA)}
        else {start_index <- c(start_index, start_i)}
        end_i <- which(data$time == as.POSIXct(end, tz = "GMT"))
        if(length(end_i) == 0) {end_index <- c(end_index, NA)}
        else {end_index <- c(end_index, end_i)}
      }
    }
  }
  
  #Select the frames
  data_selected_windows <- data.frame()
  
  for (i in 1:length(start_index)) {
    if(!is.na(start_index[i])) {
      select_window <- data[start_index[i]:end_index[i],]
      select_window$protocol <- rep(description_pro_ses$protocol[i], nrow(select_window))
      select_window$session <- rep(description_pro_ses$session[i], nrow(select_window))
      select_window$condition <- rep(description_pro_ses$condition[i], nrow(select_window))
      select_window$shaker_setting <- rep(description_pro_ses$mechanical_shaker_setting[i], nrow(select_window))
      select_window$brand <- rep(brand, nrow(select_window))
      data_selected_windows <- rbind(data_selected_windows, select_window)
    }
  }
  return(data_selected_windows)
}