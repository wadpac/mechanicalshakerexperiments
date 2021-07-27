getspecs <- function(brand, data) {
  #serial number
  #sampling frequency 
  #dynamic range
  specifications <- data.frame()
  serial_number <- c()
  sampling_frequency <- c()
  dynamic_range <- c()
  for (data_file in 1:length(data)) {
    if(brand == "Actigraph") {
      head <- attributes(data[[data_file]])[setdiff(names(attributes(data[[data_file]])), c("dim", "dimnames", "time_index"))]
      serial_number <- c(serial_number, head$header$`Serial Number`)
      sampling_frequency <- c(sampling_frequency, head$header$`Sample Rate`)
      dynamic_range <- c(dynamic_range, head$header$`Acceleration Max`)
    }
    if(brand == "Activpal") {
      number <- strsplit(strsplit(names(data)[data_file], " ")[[1]][1], "-")[[1]]
      for(s in 1:length(number)) {
        if (startsWith(number[[s]], "AP")) {sn <- number[[s]]}
      }
      serial_number <- c(serial_number, sn)
      sampling_frequency <- c(sampling_frequency, 20)
      dynamic_range <- c(dynamic_range, 2)
    }
    if(brand == "Axivity"){
      head <- data[[data_file]]$header
      serial_number <- c(serial_number, data[[data_file]]$header$uniqueSerialCode)
      sampling_frequency <- c(sampling_frequency, data[[data_file]]$header$frequency)
      dynamic_range <- c(dynamic_range, data[[data_file]]$header$accrange)
    }
    if(brand == "GENEActiv"){
      head <- data[[data_file]]$header
      serial_number <- c(serial_number, head[which(rownames(head) == "Device_Unique_Serial_Code"),])
      sampling_frequency <- c(sampling_frequency, head[which(rownames(head) == "Measurement_Frequency"),])
      dynamic_range <- c(dynamic_range, 8)
    }
    if(brand == "MOX"){ #No information available in the data itself, but in the configuration sheet of data description file
      configurations <- read.xls(protocolfile, header = TRUE, sheet = 2)
      serial_number <- c(serial_number, configurations$serial_number[which(configurations$protocol == protocol & configurations$session == session & configurations$number == data_file)])
      sampling_frequency <- c(sampling_frequency, configurations$sample_rate[which(configurations$protocol == protocol & configurations$session == session & configurations$number == data_file)])
      dynamic_range <- c(dynamic_range, 8)
    }
    if (brand == "Shimmer") {#No information available in the data
      serial_number <- c(serial_number, 1)
      sampling_frequency <- c(sampling_frequency, 20)
      dynamic_range <- c(dynamic_range, 2)
    }
  }
  specifications <- cbind(serial_number, sampling_frequency, dynamic_range)
  
  return(specifications)
}