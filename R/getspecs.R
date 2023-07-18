#' getspecs
#'
#' @description 'getspecs' Called from within loaddata to extract recording specifications
#'
#' @param brand Sensor brand: "ActiGraph", "activPAL", "Acttrust", "Axivity", "GENEActiv", or "MOX".
#' @param data Data object
#' @param experimentfile .xlsx file with protocol description, defaults to file stored inside the code
#' @param experiment Experiment to load: "timer_check", "ms_hrcr", "ms_lrcr", "ms_mrcr", ms_hrmr", "ms_lrmr", "ms_bag" or "box".
#' @return specifications, a data.frame with serial_number, sampling_rate, and dynamic_range
#' @importFrom gdata read.xls
#' @export

getspecs <- function(brand, data, experimentfile = c(), experiment) {
  specifications <- data.frame()
  serial_number <- c()
  sampling_frequency <- c()
  dynamic_range <- c()
  label <- c()
  
  if (length(experimentfile) == 0) {
    experimentfile = system.file("datadescription/data_description_new.xlsx", package = "mechanicalshakerexperiments")[1]
  }
  for (data_file in 1:length(data)) {
    if(brand == "ActiGraph") {
      head <- attributes(data[[data_file]])[setdiff(names(attributes(data[[data_file]])), c("dim", "dimnames", "time_index"))]
      sn <- head$header$`Serial Number`
      sf <- head$header$`Sample Rate`
      dr <- head$header$`Acceleration Max`
      lab <- paste(paste0("AG_", substr(sn,1, 3)), substr(sn,nchar(sn)-2, nchar(sn)), sep = "_")
    }
    if(brand == "activPAL") {
      number <- strsplit(strsplit(names(data)[data_file], " ")[[1]][1], "-")[[1]]
      for(s in 1:length(number)) {
        if (startsWith(number[[s]], "AP")) {sn <- number[[s]]}
      }
      sf <- 20
      dr <- 2
      lab <- paste0("aP_", substr(sn, nchar(sn)-2, nchar(sn)))
    }
    if(brand == "Acttrust") {#no information available in the data 
      sn <- strsplit(strsplit(names(data[data_file]), "_")[[1]][2], ".txt")[[1]][1]
      sf <- 30
      dr <- NA #Does not have a dynamic range
      lab <- paste0("Ac_", substr(sn, nchar(sn)-2, nchar(sn)))
    }
    if(brand == "Axivity"){
      head <- data[[data_file]]$header
      sn <- data[[data_file]]$header$uniqueSerialCode
      sf <- data[[data_file]]$header$frequency
      dr <- data[[data_file]]$header$accrange
      lab <- paste0("Ax_", substr(sn, nchar(sn)-2, nchar(sn)))
    }
    if(brand == "GENEActiv"){
      head <- data[[data_file]]$header
      sn <- head[which(rownames(head) == "Device_Unique_Serial_Code"),]
      sf <- head[which(rownames(head) == "Measurement_Frequency"),]
      dr <- 8
      lab <- paste0("GA_", substr(sn, nchar(sn)-2, nchar(sn)))
    }
    if(brand == "MOX") {#No information available in the data itself, but in the configuration sheet of data description file
      configurations <- gdata::read.xls(experimentfile, header = TRUE, sheet = 2)
      if(experiment == "box") {
        sn <- configurations$serial_number[which(configurations$experiment == "ms_mrcr" & configurations$number == data_file)]
        sf <- configurations$sample_rate[which(configurations$experiment == "ms_mrcr" & configurations$number == data_file)]
      } else {
        sn <- configurations$serial_number[which(configurations$experiment == experiment & configurations$number == data_file)]
        sf <- configurations$sample_rate[which(configurations$experiment == experiment & configurations$number == data_file)]
      }
      dr <- 8 #as specified on the manufacturers website
      lab <- paste0("MOX_", substr(sn, (nchar(sn)-2), nchar(sn)))
    }
    serial_number <- c(serial_number, sn)
    sampling_frequency <- c(sampling_frequency, sf)
    dynamic_range <- c(dynamic_range, dr)
    label <- c(label, lab)
  }
  specifications <- cbind(label, serial_number, sampling_frequency, dynamic_range)
  
  return(specifications)
}