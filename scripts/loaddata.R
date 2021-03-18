## Function to read in the mechanical shaker data using:
# path;
# brand: one of c("Actigraph", "Activpal", "Acttrust", "Axivity", "GENEActiv", "MOX", "Shimmer", "Fitbit), 
# protocol number: 1, 2 or 3;
# session number: 1, 2 or 3;
# windows: TRUE then windows from the data_description file will be selected, FALSE: complete data file will be loaded.

loaddata <- function(path, brand, protocol, session, windows = TRUE) {
  # Load required libraries
  library(doParallel)
  library(GGIR)
  library(GENEAread)

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
  
  # Load data in parallel
  closeAllConnections() # in case there is a still something running from last time, kill it.
  cores = parallel::detectCores()
  Ncores = cores[1]
  cl <- parallel::makeCluster(Ncores-1) #not to overload your computer
  doParallel::registerDoParallel(cl)
  
  parallelLoad <- function(file_path, file_list, brand, protocol, windows, session, path) {
    foreach(i = 1:length(file_list), .packages = c("read.gt3x", "GGIR"), .export = "read.activpal") %dopar% {
      # Load in the data
      if(brand == "Actigraph"){ d <- read.gt3x(paste(file_path, file_list[i], sep = "/"), asDataFrame = TRUE) }
      else if(brand == "Activpal") { d <- read.activpal(paste(file_path, file_list[i], sep = "/")) }
      else if(brand == "Acttrust") { d <- read.csv(paste(file_path, file_list[i], sep = "/"), sep = ";", skip = 25) }
      else if(brand == "Axivity") { d <- g.cwaread(paste(file_path, file_list[i], sep = "/"), start = 1, end = 1000000) }
      else if(brand == "GENEActiv") { d <- read.bin(paste(file_path, file_list[i], sep = "/")) }
      else if(brand == "MOX") { d <- read.csv(paste(file_path, file_list[i], sep = "/")) }
      else if(brand == "Shimmer") { d <- read.csv(paste(file_path, file_list[i], sep = "/"), nrow = 10, skip = 1, sep = '\t')}
      else {}# brand = Fitbit
      }
  } 

  data <- tryCatch(parallelLoad(file_path, file_list, brand, protocol, windows, session, path), error = function(e) print(e))
  parallel::stopCluster(cl)
  
  #Get data specifications
  names(data) <- file_list
  specifications <- getspecs(brand, data)
  
  #Remove header
  d <- list()
  if(brand == "Axivity") {
    for (f in 1:length(data)) {
      d[[f]] <- data[[f]]$data
    }
    data <- d
  }

  #Get and select windows
  if (windows == TRUE) {
    window_data <- getwindows(brand, protocol, session, path, data)
    data <- window_data
  }

  return(list(data = data, specifications = specifications))
}
