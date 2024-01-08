#' loaddata
#'
#' @description 'loaddata' Loads data
#'
#' @param path Path to the root of the experimental data (rawdatadir)
#' @param brand Sensor brand: "ActiGraph", "activPAL", "Acttrust", "Axivity", "GENEActiv", or "MOX".
#' @param experiment Experiment to load: "timer_check", "ms_hrcr", "ms_lrcr", "ms_mrcr", "ms_hrmr", "ms_lrmr", "ms_bag", or "box".
#' @param windows Boolean, if TRUE then windows from the data_description file will be selected, FALSE: complete data file will be loaded;
#' @param experimentfile .xlsx file with protocol description
#' @param actigraph_preprocessing Boolean,if  TRUE then sleepmode segments that jump by >1 sec are filled up using the function fill_sleep
#' @return A list of: \item{data}{List of data.frames with the accelerometer time series where each list item represents 1 recording} \item{specifications}{Specifications for each recording}
#' @importFrom utils read.csv
#' @importFrom lubridate ymd
#' @importFrom GGIRread readAxivity
#' @importFrom GGIRread resample
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
    if (experiment == "box" & brand != "activPAL"){
      folder <- paste0(brand, paste0("_", "ms_mrcr"))
    } else if (brand == "activPAL" & (endsWith(experiment, "cr") | experiment == "box")){
      folder <- paste0(brand, paste0("_", "ms_cr")) 
    } else if((brand == "ActiGraph" | brand == "activPAL" | brand == "GENEActiv" | brand == "Shimmer") 
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
  if (brand == "ActiGraph") {
    pattern = "*.gt3x"
  } else if (brand == "activPAL" | brand == "MOX") {
    pattern = "*.csv"
  } else if (brand == "Acttrust") {
    pattern = "*.txt"
  } else if (brand == "Axivity") {
    pattern = "*.cwa"
  } else if (brand == "GENEActiv") {
    pattern = "*.bin"
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
    foreach::foreach(i = 1:length(file_list), .packages = c("read.gt3x", "GGIRread", "GENEAread"),
                     .export = "read.activpal") %myinfix% {
                       # for (i in 1:length(file_list)) {
                       # Load in the data
                       tz = "Europe/Amsterdam"
                       if (brand == "ActiGraph") {
                         rawdata <- as.data.frame(read.gt3x::read.gt3x(paste(file_path, file_list[i], sep = "/"), asDataFrame = TRUE))
                         options(digits.secs = 5)
                         options(scipen = 999)
                         options(digits = 20)
                         rawdata$time = as.POSIXct(rawdata$time, origin = "1970-01-01", tz = tz)
                         rawdata$time = as.POSIXlt(as.character(rawdata$time), tz = tz, origin = "1970-01-01")
                         rawdata_backup = rawdata
                         #======================================
                         # resampling
                         # extract sampling rate to aid resampling:
                         head <- attributes(rawdata)[setdiff(names(attributes(rawdata)), c("dim", "dimnames", "time_index"))]
                         sampling_rate <- head$header$`Sample Rate`
                         rm(head)
                         # prepare data for resampling
                         raw = as.matrix(rawdata[, c("X", "Y", "Z")])
                         rawTime = as.numeric(rawdata$time)
                         time = seq(ceiling(min(rawTime)), floor(max(rawTime)), by = 1/sampling_rate)
                         # resample
                         rawdata2 = as.data.frame(GGIRread::resample(raw = raw, rawTime = rawTime, time = time, nrow(raw), 2))
                         # put data back into expected format
                         rawdata2$time = NA
                         colnames(rawdata2) = c("X", "Y", "Z", "time")
                         rawdata2 = rawdata2[,c("time", "X", "Y", "Z")]
                         rawdata2$time = time
                         rawdata2$time = as.POSIXct(rawdata2$time, origin = "1970-01-01", tz = tz)
                         rawdata2$time = as.POSIXlt(as.character(rawdata2$time), tz = tz, origin = "1970-01-01")
                         # try to preserve rawdata object attributes copy them to new object
                         rawdata = rawdata2
                         attributes(rawdata) = attributes(rawdata_backup)
                         rm(rawdata_backup, rawdata2)
                         #======================================
                       } else if (brand == "activPAL") {
                         rawdata <- read.activpal(paste(file_path, file_list[i], sep = "/"))
                         rawdata[,c("X","Y","Z")] = ((rawdata[,c("X","Y","Z")] / (2^8)) - 0.5) * 2 * 2
                         rawdata = rawdata[,c("time", "X", "Y", "Z")]
                       } else if (brand == "Acttrust") {
                         rawdata <- read.csv(paste(file_path, file_list[i], sep = "/"), sep = ";", skip = 25)
                         rawdata$time <- strptime(rawdata$DATE.TIME, format = "%d/%m/%Y %H:%M:%OS", tz = tz)
                         rawdata = rawdata[,c("time", "PIM", "PIMn", "TAT", "TATn", "ZCM", "ZCMn")] #Select variables
                       } else if (brand == "Axivity") {
                         rawdata <- GGIRread::readAxivity(paste(file_path, file_list[i], sep = "/"),
                                                    start = 1, end = 1000000, desiredtz = tz, interpolationType = 2)
                         # rawdata = rawdata$data # ignore header already at this stage
                       } else if (brand == "GENEActiv") {
                         rawdata <- GENEAread::read.bin(paste(file_path, file_list[i], sep = "/"))
                         rawdata$data.out = as.data.frame(rawdata$data.out)
                         colnames(rawdata$data.out)[1] = "time"
                         # We may have configured device relative to UTC, which is 1 hour earlier, therefore subtract 3600
                         rawdata$data.out$time = as.POSIXlt(rawdata$data.out$time - 3600, desiredtz = tz, origin = "1970-01-01")
                       } else if (brand == "MOX") {
                         options(digits = 20)
                         options(digits.secs = 5)
                         options(scipen = 999)
                         rawdata <- read.csv(paste(file_path, file_list[i], sep = "/"))
                         # We may have configured device relative to UTC, which is 1 hour earlier, therefore subtract 3600; 86400 = number of s in 1 day to convert Time Number
                         timestamps <- format(as.POSIXct((rawdata$DateTimeNumber * 86400) - 3600, origin = "1970-01-01", tz = tz), "%H:%M:%OS")
                         # Date cannot be converted correctly, but this can be derived from the filenames
                         date <- lubridate::ymd(strsplit(strsplit(file_list[i], "_")[[1]][2], ".csv")[[1]])
                         timestamps2 = paste(rep(date, length(timestamps)), timestamps)
                         timestamps3 = as.POSIXct(timestamps2, format = "%Y-%m-%d %H:%M:%OS")
                         # following lines needed to preserve decimal places in POSIXct
                         rawdata$time = as.character(timestamps3) 
                         rawdata$time = as.POSIXct(rawdata$time, format = "%Y-%m-%d %H:%M:%OS")
                         rawdata <- rawdata[, c("time", "AccX", "AccY", "AccZ")]
                       }# brand = Fitbit
                       
                       # rename the acceleration data column names for consistency
                       if(brand %in% c("ActiGraph", "activPAL")) {
                         names(rawdata) <- tolower(names(rawdata))
                       }
                       if(brand == "MOX") {
                         names(rawdata) <- c("time", "x", "y", "z")
                       }
                       
                       # make selection of the data with 2 minutes before and after each experiment
                       # this to reduce amount data in memory
                       if (experiment == "timer_check") {
                         start = as.POSIXlt("2020-11-26 9:12:00", tz = tz)
                         end = as.POSIXlt("2020-11-26 19:30:00", tz = tz)
                       }
                       if (experiment == "ms_hrcr") {
                         start = as.POSIXlt("2020-11-24 9:42:00", tz = tz)
                         end = as.POSIXlt("2020-11-24 10:47:00", tz = tz)
                       }
                       if (experiment == "ms_lrcr") {
                         start = as.POSIXlt("2020-11-24 12:37:00", tz = tz)
                         end = as.POSIXlt("2020-11-24 13:20:00", tz = tz)
                       }
                       if (experiment == "ms_mrcr") {
                         start = as.POSIXlt("2020-11-24 15:07:00", tz = tz)
                         end = as.POSIXlt("2020-11-24 15:50:00", tz = tz)
                       }
                       if (experiment == "box") {
                         start = as.POSIXlt("2020-11-24 15:54:00", tz = tz)
                         end = as.POSIXlt("2020-11-24 16:55:00", tz = tz)
                       }
                       if (experiment == "ms_hrmr") {
                         start = as.POSIXlt("2020-11-27 8:37:00", tz = tz)
                         end = as.POSIXlt("2020-11-27 9:20:00", tz = tz)
                       }
                       if (experiment == "ms_lrmr") {
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
                       } else {
                         if (brand == "GENEActiv") {
                           rawdata$data.out = rawdata$data.out[which(rawdata$data.out$time >= start & rawdata$data.out$time <= end),]
                         } else if (brand == "Axivity") {
                           rawdata$data = rawdata$data[which(rawdata$data$time >= start & rawdata$data$time <= end),]
                         }
                       }
                       return(rawdata)
                     }
  }
  # } # for loop
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
