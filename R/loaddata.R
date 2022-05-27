#' loaddata
#'
#' @description 'loaddata' Loads data
#'
#' @param path Path to the root of the experimental data (rawdatadir)
#' @param brand Sensor brand: "Actigraph", "Activpal", "Acttrust", "Axivity", "GENEActiv", or "MOX".
#' @param experiment Experiment to load: "timer_check", "ms_hfcr", "ms_lfcr", "ms_hfmr", "ms_lfmr", or "box".
#' @param windows Boolean, if TRUE then windows from the data_description file will be selected, FALSE: complete data file will be loaded;
#' @param experimentfile xlsx file with protocol description
#' @param actigraph_preprocessing Boolean,if  TRUE then sleepmode segments that jump by >1 sec are filled up using the function fill_sleep
#' @return A list of: \item{data}{List of data.frames with the accelerometer time series where each list item represents 1 recording} \item{specifications}{Specifications for each recording}
#' @importFrom utils read.csv
#' @importFrom lubridate ymd
#' @importFrom GGIR g.cwaread
#' @importFrom GENEAread read.bin
#' @importFrom read.gt3x read.gt3x
#' @import foreach
#' @export

loaddata <- function(path, brand, experiment, windows = TRUE, experimentfile, actigraph_preprocessing = TRUE) {
  cat(paste0("\nAttempting to load ",brand, " | experiment: ", experiment, ":\n"))
  options(digits.secs = 5)
  options(scipen = 999)
  #------------------------------------------------------------
  # Get folder name of the data files and set path to files
  if(brand == "MOX") { # temporarily change name of brand to enable getting the folder path right
    brand <- "MOX_exportedCSV/exportedCSV/MOX" 
  }
  if (brand %in% c("Acttrust", "Fitbit") == FALSE) {
    if (experiment == "box" & brand != "Activpal"){
      folder <- paste0(brand, paste0("_", "ms_mfcr"))
    } else if (brand == "Activpal" & (endsWith(experiment, "cr") | experiment == "box")){
      folder <- paste0(brand, paste0("_", "ms_cr")) 
    } else if((brand == "Actigraph" | brand == "Activpal" | brand == "GENEActiv" | brand == "Shimmer") 
              && (endsWith(experiment, "door") | endsWith(experiment, "bag"))){
      folder <- paste0(brand, paste0("_", "ms_bag"))
    } else {
      folder <- paste0(brand, paste0("_", experiment)) 
    }
  }
  if (brand == "Acttrust") {
    folder <- "Acttrust_Condor"
  } else if (brand == "Fitbit") {
    folder <- brand
  } else if (brand == "MOX_exportedCSV/exportedCSV/MOX") {
    brand <- "MOX"
  }
  #------------------------------------------------------------
  # Get brand specific file extensions
  if (brand == "Actigraph") {
    pattern ="*.gt3x"
  } else if (brand == "Activpal" | brand == "MOX") {
    pattern ="*.csv"
  } else if (brand == "Acttrust") {
    pattern ="*.txt"
  } else if (brand == "Axivity") {
    pattern ="*.cwa"
  } else if (brand == "GENEActiv") {
    pattern ="*.bin"
  }
  #else {} #fitbit
  #------------------------------------------------------------
  # Get file list
  # if(exists("folders")){ # commmented out because unclear where object folders should come from
  #   file_path <- c()
  #   file_path[1] <- paste(path, folders[1], sep = "/")
  #   file_path[2] <- paste(path, folders[2], sep = "/")
  # } else {
  file_path <- paste(path, folder, sep = "/")
  # }
  
  if (length(file_path) == 0) {
    warning("\nNo recordings identified. Check folder path.")
  } 
  
  file_list <- list.files(file_path, pattern = pattern, all.files = FALSE)
  cat(paste0("\nLooking inside: ",file_path," for ", pattern," files"))
  cat(paste0("\n...", length(file_list), " files identified for loading:"))
  
  # Load data in parallel
  closeAllConnections() # in case there is a still something running from last time, kill it.
  cores = parallel::detectCores()
  Ncores = cores[1]
  Ncores = Ncores - 2
  if (Ncores < 1) Ncores = 1
  cl <- parallel::makeCluster(Ncores) #not to overload your computer
  doParallel::registerDoParallel(cl)
  i = NULL
  `%myinfix%` = foreach::`%dopar%`
  parallelLoad <- function(file_path, file_list, brand, experiment, windows, path) {
    foreach::foreach(i = 1:length(file_list), .packages = c("read.gt3x", "GGIR", "GENEAread"),
                     .export = "read.activpal") %myinfix% {
                       # Load in the data
                       tz = "Europe/Amsterdam"
                       if (brand == "Actigraph") {
                         rawdata <- as.data.frame(GGIR::read.gt3x_ggir(paste(file_path, file_list[i], sep = "/"), asDataFrame = TRUE))
                         options(digits.secs = 5)
                         options(scipen = 999)
                         rawdata$time = as.POSIXct(rawdata$time, origin = "1970-01-01", tz = tz)
                         rawdata$time = as.POSIXlt(as.character(rawdata$time), tz = tz, origin = "1970-01-01")
                       } else if (brand == "Activpal") {
                         rawdata <- read.activpal(paste(file_path, file_list[i], sep = "/"))
                         rawdata[,c("X","Y","Z")] = ((rawdata[,c("X","Y","Z")] / (2^8)) - 0.5) * 2 * 2
                         rawdata = rawdata[,c("time", "X", "Y", "Z")]
                       } else if (brand == "Acttrust") {
                         rawdata <- read.csv(paste(file_path, file_list[i], sep = "/"), sep = ";", skip = 25)
                         rawdata$time <- strptime(rawdata$DATE.TIME, format = "%d/%m/%Y %H:%M:%OS", tz = tz)
                         rawdata = rawdata[,c("time", "PIM", "PIMn", "TAT", "TATn", "ZCM", "ZCMn")] #Select variables
                       } else if (brand == "Axivity") {
                         rawdata <- GGIR::g.cwaread(paste(file_path, file_list[i], sep = "/"),
                                                    start = 1, end = 1000000, desiredtz = tz, interpolationType = 2)
                         # rawdata = rawdata$data # ignore header already at this stage
                       } else if (brand == "GENEActiv") {
                         rawdata <- GENEAread::read.bin(paste(file_path, file_list[i], sep = "/"))
                         rawdata$data.out = as.data.frame(rawdata$data.out)
                         colnames(rawdata$data.out)[1] = "time"
                         # We may have configured device relative to UTC, which is 1 hour earlier, therefore subtract 3600
                         rawdata$data.out$time = as.POSIXlt(rawdata$data.out$time - 3600, desiredtz = tz, origin = "1970-01-01")
                       } else if (brand == "MOX") {
                         rawdata <- read.csv(paste(file_path, file_list[i], sep = "/"))
                         # We may have configured device relative to UTC, which is 1 hour earlier, therefore subtract 3600; 86400 = number of s in 1 day to convert Time Number
                         timestamps <- format(as.POSIXct((rawdata$DateTimeNumber * 86400) - 3600, origin = "1970-01-01", tz = tz), "%H:%M:%OS")
                         # Date cannot be converted correctly, but this can be derived from the filenames
                         date <- lubridate::ymd(strsplit(strsplit(file_list[i], "_")[[1]][2], ".csv")[[1]])
                         rawdata$time <- as.POSIXct(paste(rep(date, length(timestamps)), timestamps), format = "%Y-%m-%d %H:%M:%OS")
                         rawdata <- rawdata[, c("time", "AccX", "AccY", "AccZ")]
                       }# brand = Fitbit
                       
                       # make selection of the data with 2 minutes before and after each experiment
                       # this to reduce amount data in memory
                       if (experiment == "timer_check") {
                         start = as.POSIXlt("2020-11-26 9:12:00", tz = tz)
                         end = as.POSIXlt("2020-11-26 19:30:00", tz = tz)
                       }
                       if (experiment == "ms_hfcr") {
                         start = as.POSIXlt("2020-11-24 9:42:00", tz = tz)
                         end = as.POSIXlt("2020-11-24 10:47:00", tz = tz)
                       }
                       if (experiment == "ms_lfcr") {
                         start = as.POSIXlt("2020-11-24 12:37:00", tz = tz)
                         end = as.POSIXlt("2020-11-24 13:20:00", tz = tz)
                       }
                       if (experiment == "ms_mfcr") {
                         start = as.POSIXlt("2020-11-24 15:07:00", tz = tz)
                         end = as.POSIXlt("2020-11-24 15:50:00", tz = tz)
                       }
                       if (experiment == "box") {
                         start = as.POSIXlt("2020-11-24 15:54:00", tz = tz)
                         end = as.POSIXlt("2020-11-24 16:55:00", tz = tz)
                       }
                       if (experiment == "ms_hfmr") {
                         start = as.POSIXlt("2020-11-27 8:37:00", tz = tz)
                         end = as.POSIXlt("2020-11-27 9:20:00", tz = tz)
                       }
                       if (experiment == "ms_lfmr") {
                         start = as.POSIXlt("2020-11-27 10:02:00", tz = tz)
                         end = as.POSIXlt("2020-11-27 10:45:00", tz = tz)
                       }
                       if (experiment == "ms_bag") {
                         start = as.POSIXlt("2020-11-27 11:32:00", tz = tz)
                         end = as.POSIXlt("2020-11-27 12:15:00", tz = tz)
                       }
                       if (experiment == "door") {
                         start = as.POSIXlt("2020-11-27 13:35:00", tz = tz)
                         end = as.POSIXlt("2020-11-27 13:44:00", tz = tz)
                       }
                       if (brand %in% c("Axivity")) {
                         start = as.numeric(start)
                         end = as.numeric(end)
                       }
                       if (brand %in% c("Axivity", "GENEActiv") == FALSE) {
                         rawdata = rawdata[which(rawdata$time >= start & rawdata$time <= end),]
                       }else {
                         if (brand == "GENEActiv") {
                           rawdata$data.out = rawdata$data.out[which(rawdata$data.out$time >= start & rawdata$data.out$time <= end),]
                         } else if (brand == "Axivity") {
                           rawdata$data = rawdata$data[which(rawdata$data$time >= start & rawdata$data$time <= end),]
                         }
                       }
                       #if(actigraph_preprocessing = TRUE) {
                       #data <- fill_sleepmode(rawdata, start, end)
                       #}
                       return(rawdata)
                     }
  }
  data <- tryCatch(parallelLoad(file_path, file_list, brand, experiment, windows, path), error = function(e) print(e))
  parallel::stopCluster(cl)
  #Get data specifications
  
  names(data) <- file_list
  specifications <- getspecs(brand, data, experimentfile, experiment)
  
  #Get and select windows
  if (windows == TRUE) {
    data <- getwindows(brand, experiment, path, data, experimentfile)
  }
  return(list(data = data, specifications = specifications))
}
