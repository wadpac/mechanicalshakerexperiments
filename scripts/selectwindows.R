# Function to select relevant information based on the timestamps relevant for the protocol and session, depending on brand

selectwindows <- function(data, brand, window) {
  # Calculate indices for the windows to select
  start_index <- c()
  end_index <- c()
  selected_data <- data.frame()
  
  for(w in 1:length(window$start_ts)) {
    if(brand == "Actigraph") {
      start_i <- which(data$time == as.POSIXct(window$start_ts[w], tz = "GMT"))
      start_index <- c(start_index, start_i)
      end_i <- which(data$time == as.POSIXct(window$end_ts[w], tz = "GMT"))
      end_index <- c(end_index, end_i)
    }
  }
  
  # Select the windows from the data
  for(i in 1:length(start_index)){
    if(!is.na(start_index[i])) {
      select_window <- data[start_index[i]:end_index[i], ]
      select_window$condition <- rep(description_pro_ses$condition[i], nrow(select_window))
      select_window$shaker_setting <- rep(description_pro_ses$mechanical_shaker_setting[i], nrow(select_window))
      #select_window$event <- rep(description_pro_ses$event[i], nrow(select_window))
      selected_data <- rbind(selected_data, select_window)
    }
  }
  
  return(selected_data)
}