#Function to read in the data using the folder name, the brand name, windows (if true then windows in data_description file will be selected)
loaddata <- function(path, folder, brand, windows = TRUE, data_description, protocol, session) {
  #Load required libraries
  #library(read.gt3x)
  library(GGIR)
  library(GENEAread)
  
  file_path <- paste(path, folder, sep = "/")
  data <- list()

  if (brand == "Actigraph") {
    file_list <- list.files(file_path, pattern ="*.gt3x", all.files = FALSE)
    for(f in 1:length(file_list)) {
      d <- read.gt3x(paste(file_path, file_list[f], sep = "/"), asDataFrame = TRUE)
      if (windows == TRUE) {
        d <- selectwindows(brand, data = d, data_description, protocol, session)
      }
      data[[f]] <- d
    }
  }
  
  if (brand == "Activpal") {
    file_list <- list.files(file_path, pattern ="*.csv", all.files = FALSE)
    for(f in 1:length(file_list)) {
      d <- read.activpal(paste(file_path, file_list[f], sep = "/"))
      if (windows == TRUE) {
        d <- selectwindows(brand, data = d, data_description, protocol, session)
      }
      data[[f]] <- as.data.frame(d)
    }
  }
  
  if (brand == "Acttrust") {
    file_list <- list.files(file_path, pattern ="*.txt", all.files = FALSE)
    for(f in 1:length(file_list)) {
      d <- read.csv(paste(file_path, file_list[f], sep = "/"), sep = ";", skip = 25)
      #d$DATE.TIME <- as.POSIXlt(strptime(d$DATE.TIME,format="%d/%m/%Y %H:%M:%OS"), tz = "GMT")
      data[[f]] <- as.data.frame(d)
    }
  }
  
  if (brand == "Axivity") {
    file_list <- list.files(file_path, pattern ="*.cwa", all.files = FALSE)
    for(f in 1:length(file_list)) {
      d <- g.cwaread(paste(file_path, file_list[f], sep = "/"), start = 1, end = 1000000)
      data[[f]] <- as.data.frame(d)
    }
  }
  
  if (brand == "GENEActiv") {
    file_list <- list.files(file_path, pattern ="*.bin", all.files = FALSE)
    for(f in 1:length(file_list)) {
      d <- read.bin(paste(file_path, file_list[f], sep = "/"))
      data[[f]] <- d
    }
  }
  
  if (brand == "MOX") {
    file_list <- list.files(file_path, pattern ="*.csv", all.files = FALSE)
    for(f in 1:length(file_list)) {
      d <- read.csv(paste(file_path, file_list[f], sep = "/"))
      data[[f]] <- as.data.frame(d)
    }
  }
  
  if (brand == "Shimmer") {
    file_list <- list.files(file_path, pattern ="*.csv", all.files = FALSE)
    for(f in 1:length(file_list)) {
      d <- read.csv(paste(file_path, file_list[f], sep = "/"), nrow = 10, skip = 1, sep = '\t')
      data[[f]] <- as.data.frame(d)
    }
  }
  rm(f, d)
  return(data)
}

