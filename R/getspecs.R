#' getspecs
#'
#' @description 'getspecs' Called from within loaddata to extract recording specifications
#'
#' @param brand Sensor brand: "Actigraph", "Activpal", "Acttrust", "Axivity", "GENEActiv", or "MOX".
#' @param data Data object
#' @param experimentfile xlsx file with protocol description, defaults to file stored inside the code
#' @param experiment Experiment to load: "timer_check", "ms_hfcr", "ms_lfcr", "ms_hfmr", "ms_lfmr", or "box".
#' @return specifications, a data.frame with serial_number, sampling_frequency, and dynamic_range
#' @importFrom gdata read.xls
#' @export

getspecs <- function(brand, data, experimentfile = c(), experiment) {
  specifications <- data.frame()
  serial_number <- c()
  sampling_frequency <- c()
  dynamic_range <- c()
  label <- c()
  
  if (length(experimentfile) == 0) {
    experimentfile = system.file("datadescription/data_description.xlsx", package = "mechanicalshakerexperiments")[1]
  }
  for (data_file in 1:length(data)) {
    if(brand == "Actigraph") {
      head <- attributes(data[[data_file]])[setdiff(names(attributes(data[[data_file]])), c("dim", "dimnames", "time_index"))]
      serial_number <- c(serial_number, head$header$`Serial Number`)
      sampling_frequency <- c(sampling_frequency, head$header$`Sample Rate`)
      dynamic_range <- c(dynamic_range, head$header$`Acceleration Max`)
      lab <- paste(paste0("AG_", substr(head$header$`Serial Number`,1,3)),
                   substr(head$header$`Serial Number`,nchar(head$header$`Serial Number`)-3, nchar(head$header$`Serial Number`)), sep = "_")
    }
    if(brand == "Activpal") {
      number <- strsplit(strsplit(names(data)[data_file], " ")[[1]][1], "-")[[1]]
      for(s in 1:length(number)) {
        if (startsWith(number[[s]], "AP")) {sn <- number[[s]]}
      }
      serial_number <- c(serial_number, sn)
      sampling_frequency <- c(sampling_frequency, 20)
      dynamic_range <- c(dynamic_range, 2)
      lab <- paste0("aP_", substr(serial_number, nchar(serial_number)-3, nchar(serial_number)))
    }
    if(brand == "Acttrust") {#no information available in the data 
      serial_number <- c(serial_number, strsplit(strsplit(names(data[data_file]), "_")[[1]][2], ".txt")[[1]][1])
      sampling_frequency <- c(sampling_frequency, 30)
      dynamic_range <- c(dynamic_range, NA) #What is the dynamic range?
      lab <- paste0("Ac_", substr(serial_number, nchar(serial_number)-3, nchar(serial_number)))
    }
    if(brand == "Axivity"){
      head <- data[[data_file]]$header
      serial_number <- c(serial_number, data[[data_file]]$header$uniqueSerialCode)
      sampling_frequency <- c(sampling_frequency, data[[data_file]]$header$frequency)
      dynamic_range <- c(dynamic_range, data[[data_file]]$header$accrange)
      lab <- paste0("Ax_", substr(serial_number, nchar(serial_number)-3, nchar(serial_number)))
    }
    if(brand == "GENEActiv"){
      head <- data[[data_file]]$header
      serial_number <- c(serial_number, head[which(rownames(head) == "Device_Unique_Serial_Code"),])
      sampling_frequency <- c(sampling_frequency, head[which(rownames(head) == "Measurement_Frequency"),])
      dynamic_range <- c(dynamic_range, 8)
      lab <- paste0("GA_", substr(serial_number, nchar(serial_number)-3, nchar(serial_number)))
    }
    if(brand == "MOX") {#No information available in the data itself, but in the configuration sheet of data description file
      configurations <- gdata::read.xls(experimentfile, header = TRUE, sheet = 2)
      if(experiment == "box") {
        serial_number <- c(serial_number, configurations$serial_number[which(configurations$experiment == "ms_mfcr" & configurations$number == data_file)])
        sampling_frequency <- c(sampling_frequency, configurations$sample_rate[which(configurations$experiment == "ms_mfcr" & configurations$number == data_file)])
      } else {
        serial_number <- c(serial_number, configurations$serial_number[which(configurations$experiment == experiment & configurations$number == data_file)])
        sampling_frequency <- c(sampling_frequency, configurations$sample_rate[which(configurations$experiment == experiment & configurations$number == data_file)])
      }
      dynamic_range <- c(dynamic_range, NA) #What is the dynamic range?
      lab <- paste0("MOX_", substr(serial_number, (nchar(serial_number)-3), nchar(serial_number)))
    }
    label <- c(label, lab)
  }
  specifications <- cbind(label, serial_number, sampling_frequency, dynamic_range)
  
  return(specifications)
}