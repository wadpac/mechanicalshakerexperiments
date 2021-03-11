# Function to select relevant information based on the timestamps relevant for the protocol and session, depending on brand

selectwindows <- function(data, brand, window) {
  # Select the windows from the data
  selected_data <- data.frame()
  
  for(w in 1:length(window$start_ts)) {
    if(! is.na(window$start_ts[w])) {
      if(brand == "Actigraph") {
      select_window <- subset(data, data$time >= as.POSIXct(window$start_ts[w], tz = "GMT") & data$time <= as.POSIXct(window$end_ts[w], tz = "GMT")) 
      }
      condition <- rep(window$condition[w], nrow(select_window))
      shaker_condition <- rep(window$shaker_setting[w], nrow(select_window))
      select_window <- cbind(select_window, condition, shaker_condition)
      
      selected_data <- rbind(selected_data, select_window)
      
    }
  }
  
  return(selected_data)
}