getspecs <- function(brand, data) {
  #serial number
  #sampling frequency 
  #dynamic range
  specifications <- data.frame()
  serial_number <- c()
  sampling_frequency <- c()
  dynamic_range <- c()
  
  if(brand == "Actigraph") {
    for (data_file in 1:length(data)) {
      head <- attributes(data[[data_file]])[setdiff(names(attributes(data[[data_file]])), c("dim", "dimnames", "time_index"))]
      serial_number <- c(serial_number, head$header$`Serial Number`)
      sampling_frequency <- c(sampling_frequency, head$header$`Sample Rate`)
      dynamic_range <- c(dynamic_range, head$header$`Acceleration Max`)
    }
  }
  
  specifications <- cbind(serial_number, sampling_frequency, dynamic_range)
  
  return(specifications)
}