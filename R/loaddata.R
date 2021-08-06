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
  #------------------------------------------------------------
  # Get folder name of the data files and set path to files
  if(brand == "MOX") { # temporarily change name of brand to enable getting the folder path right
    brand <- "MOX_exportedCSV/exportedCSV/MOX" 
  }
  if (brand %in% c("Acttrust", "Fitbit") == FALSE) {
    if (protocol == 1 | brand == "Activpal" | (protocol == 3 & brand != "Axivity")) {
      folder <- paste0(brand, paste0("_pro", protocol))
    } else {
      folder <- paste0(brand, paste0(paste0("_pro", protocol), paste0("_ses", session))) 
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
  } else if (brand == "Activpal" | brand == "MOX" | brand == "Shimmer") {
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
  file_path <- paste(path, folder, sep = "/")
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
  cl <- parallel::makeCluster(Ncores-2) #not to overload your computer
  doParallel::registerDoParallel(cl)
  parallelLoad <- function(file_path, file_list, brand, protocol, windows, session, path) {
    foreach(i = 1:length(file_list), .packages = c("read.gt3x", "GGIR", "GENEAread"), .export = "read.activpal") %dopar% {
      # for (i in 1:length(file_list)) {
      # Load in the data
      options(digits.secs=7)
      tz = "Europe/Amsterdam"
      if(brand == "Actigraph") {
        rawdata <- as.data.frame(read.gt3x(paste(file_path, file_list[i], sep = "/"), asDataFrame = TRUE))
        rawdata$time = as.POSIXlt(as.character(rawdata$time), tz = tz, origin = "1970-01-01")
      } else if (brand == "Activpal") {
        rawdata <- read.activpal(paste(file_path, file_list[i], sep = "/"))
        rawdata[,c("X","Y","Z")] = ((rawdata[,c("X","Y","Z")] / (2^8)) - 0.5) * 2 * 2
        rawdata = rawdata[,c("time", "X", "Y", "Z")]
      } else if(brand == "Acttrust") {
        rawdata <- read.csv(paste(file_path, file_list[i], sep = "/"), sep = ";", skip = 25)
      } else if(brand == "Axivity") {
        rawdata <- g.cwaread(paste(file_path, file_list[i], sep = "/"), start = 1, end = 1000000, desiredtz = tz, interpolationType=2)
        # rawdata = rawdata$data # ignore header already at this stage
      } else if (brand == "GENEActiv") {
        rawdata <- read.bin(paste(file_path, file_list[i], sep = "/")) 
        rawdata$data.out = as.data.frame(rawdata$data.out)
        colnames(rawdata$data.out)[1] = "time"
        # We may have configured device relative to UTC, which is 1 hour earlier, therefore subtract 3600
        rawdata$data.out$time = as.POSIXlt(rawdata$data.out$time-3600, desiredtz = tz, origin = "1970-01-01")
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
      if (brand %in% c("Axivity", "GENEActiv") == FALSE) {
        rawdata = rawdata[which(rawdata$time >= start & rawdata$time <= end),] 
      } else {
        if (brand == "GENEActiv") {
          rawdata$data.out = rawdata$data.out[which(rawdata$data.out$time >= start & rawdata$data.out$time <= end),]
        } else if (brand == "Axivity") {
          rawdata$data = rawdata$data[which(rawdata$data$time >= start & rawdata$data$time <= end),]
        } 
      }
      # } # for loop
      return(rawdata)
    }
  }
  options(digits.secs=7) # critical step, because otherwishe decimal places are lost
  data <- tryCatch(parallelLoad(file_path, file_list, brand, protocol, windows, session, path), error = function(e) print(e))
  parallel::stopCluster(cl)
  #Get data specifications
  names(data) <- file_list
  specifications <- getspecs(brand, data)
  #Get and select windows
  if (windows == TRUE) {
    data <- getwindows(brand, protocol, session, path, data, protocolfile)
  }
  return(list(data = data, specifications = specifications))
}
