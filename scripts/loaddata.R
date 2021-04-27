## Function to read in the mechanical shaker data using:
# path;
# brand: one of c("Actigraph", "Activpal", "Acttrust", "Axivity", "GENEActiv", "MOX", "Shimmer", "Fitbit), 
# protocol number: 1, 2 or 3;
# session number: 1, 2 or 3;
# windows: TRUE then windows from the data_description file will be selected, FALSE: complete data file will be loaded.

loaddata <- function(path, brand, protocol, session, windows = TRUE, protocolfile) {
  # Load required libraries
  library(doParallel)
  library(GGIR)
  library(GENEAread)
  library(read.gt3x)
  cat(paste0("\nAttempting to load ",brand, " | protocol: ", protocol, " | session: ", session, ":\n"))
  # Get folder name of the data files and set path to files
  if(brand == "MOX")
    brand <- "MOX_exportedCSV/exportedCSV/MOX" 
  
  if((protocol == 1 || protocol == 3) && (brand != "Acttrust" || brand != "Fitbit")) {
    folder <- paste0(brand, paste0("_pro", protocol))
    if(brand == "Axivity") {
      folder <- paste0(brand, paste0(paste0("_pro", protocol), paste0("_ses", session))) 
    }
  }
  if(protocol == 2 && (brand != "Acttrust" || brand != "Fitbit")) {
    if(brand == "Activpal") {
      folder <- paste0(brand, paste0("_pro", protocol)) 
    }
    else {
      folder <- paste0(brand, paste0(paste0("_pro", protocol), paste0("_ses", session))) 
    }
  }
  if(brand == "Acttrust") { folder <- "Acttrust_Condor" }
  if(brand == "Fitbit") { folder <- brand }
  if(brand == "MOX_exportedCSV/exportedCSV/MOX") { brand <- "MOX" }
  
  file_path <- paste(path, folder, sep = "/")
  
  # Get file list
  if(brand == "Actigraph") { pattern ="*.gt3x"}
  if(brand == "Activpal" || brand == "MOX" || brand == "Shimmer") { pattern ="*.csv" }
  if(brand == "Acttrust") { pattern ="*.txt" }
  if(brand == "Axivity") { pattern ="*.cwa" }
  if(brand == "GENEActiv") { pattern ="*.bin" }
  #else {} #fitbit
  
  file_list <- list.files(file_path, pattern = pattern, all.files = FALSE)
  
  print(paste0(length(file_list), " files identified for loading:"))
  
  # Load data in parallel
  closeAllConnections() # in case there is a still something running from last time, kill it.
  cores = parallel::detectCores()
  Ncores = cores[1]
  cl <- parallel::makeCluster(Ncores-1) #not to overload your computer
  doParallel::registerDoParallel(cl)
  
  parallelLoad <- function(file_path, file_list, brand, protocol, windows, session, path) {
    foreach(i = 1:length(file_list), .packages = c("read.gt3x", "GGIR"), .export = "read.activpal") %dopar% {
      # i = 1
      # Load in the data
      
      tz = "Europe/Amsterdam"
      if(brand == "Actigraph") {
        rawdata <- as.data.frame(read.gt3x(paste(file_path, file_list[i], sep = "/"), asDataFrame = TRUE))
        rawdata$time = as.POSIXlt(rawdata$time, tz = tz)
      } else if (brand == "Activpal") {
        rawdata <- read.activpal(paste(file_path, file_list[i], sep = "/"))
      } else if(brand == "Acttrust") {
        rawdata <- read.csv(paste(file_path, file_list[i], sep = "/"), sep = ";", skip = 25)
      } else if(brand == "Axivity") {
        rawdata <- g.cwaread(paste(file_path, file_list[i], sep = "/"), start = 1, end = 1000000, desiredtz = tz)
        rawdata = rawdata$data # ignore header already at this stage
      } else if (brand == "GENEActiv") {
        rawdata <- read.bin(paste(file_path, file_list[i], sep = "/")) 
      } else if (brand == "MOX") {
        rawdata <- read.csv(paste(file_path, file_list[i], sep = "/")) 
      } else if (brand == "Shimmer") {
        rawdata <- read.csv(paste(file_path, file_list[i], sep = "/"), nrow = 10, skip = 1, sep = '\t')
      } else {
      }# brand = Fitbit
      
      # make selection of the data with 2 minutes before and after each experiment
      # this to reduce amount data in memory
      if (protocol == 1 & session == 1) {
        start = as.POSIXlt("2020-11-26 9:12:00", tz = tz)
        end = as.POSIXlt("2020-11-26 19:30:00", tz = tz)
      }
      if (protocol == 2 & session == 1) {
        start = as.POSIXlt("2020-11-24 9:42:00", tz = tz)
        end = as.POSIXlt("2020-11-24 10:47:00", tz = tz)
      }
      if (protocol == 2 & session == 2) {
        start = as.POSIXlt("2020-11-24 12:37:00", tz = tz)
        end = as.POSIXlt("2020-11-24 13:20:00", tz = tz)
      }
      if (protocol == 2 & session == 3) {
        start = as.POSIXlt("2020-11-24 15:07:00", tz = tz)
        end = as.POSIXlt("2020-11-24 15:50:00", tz = tz)
      }
      if (protocol == 2 & session == 4) {
        start = as.POSIXlt("2020-11-24 15:54:00", tz = tz)
        end = as.POSIXlt("2020-11-24 16:55:00", tz = tz)
      }
      if (protocol == 3 & session == 1) {
        start = as.POSIXlt("2020-11-27 8:37:00", tz = tz)
        end = as.POSIXlt("2020-11-27 9:20:00", tz = tz)
      }
      if (protocol == 3 & session == 2) {
        start = as.POSIXlt("2020-11-27 10:02:00", tz = tz)
        end = as.POSIXlt("2020-11-27 10:45:00", tz = tz)
      }
      if (protocol == 3 & session == 3) {
        start = as.POSIXlt("2020-11-27 11:32:00", tz = tz)
        end = as.POSIXlt("2020-11-27 12:15:00", tz = tz)
      }
      if (protocol == 3 & session == 4) {
        start = as.POSIXlt("2020-11-27 13:35:00", tz = tz)
        end = as.POSIXlt("2020-11-27 13:44:00", tz = tz)
      }
      if (brand %in% c("Axivity")) {
        start = as.numeric(start)
        end = as.numeric(end)
      }
      rawdata = rawdata[which(rawdata$time >= start & rawdata$time <= end),] 
      return(rawdata)
    }
  }
  
  data <- tryCatch(parallelLoad(file_path, file_list, brand, protocol, windows, session, path), error = function(e) print(e))
  parallel::stopCluster(cl)
  #Get data specifications
  names(data) <- file_list
  specifications <- getspecs(brand, data)
  
  print("get windows...")
  #Get and select windows
  if (windows == TRUE) {
    window_data <- getwindows(brand, protocol, session, path, data, protocolfile)
    data <- window_data
  }
  return(list(data = data, specifications = specifications))
}
