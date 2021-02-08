### Load raw data
# Set path fo main folder
path <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine/shakerexperiments/data"
setwd(path)

#Function to read in the data using the folder name, the brand name, windows (if true then windows in data_description file will be selected)
loaddata <- function(path, folder, brand, windows = FALSE, data_description) {
  #Load required libraries
  library(read.gt3x)
  library(GGIR)
  library(GENEAread)
  
  file_path <- paste(path, folder, sep = "/")
  data <- list()

  if (brand == "Actigraph") {
    file_list <- list.files(file_path, pattern ="*.gt3x", all.files = FALSE)
    for(f in 1:length(file_list)) {
      d <- read.gt3x(paste(file_path, file_list[f], sep = "/"))
      data[[f]] <- as.data.frame(d)
    }
  }
  
  if (brand == "Activpal") {
    file_list <- list.files(file_path, pattern ="*.csv", all.files = FALSE)
    for(f in 1:length(file_list)) {
      d <- read.activpal.raw(paste(file_path, file_list[f], sep = "/"))
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

read.activpal.raw <- function(file.name.and.path) 
  #function adapted from package (activalProcessing), as the original function (activpal.file.reader) is not suitable for the raw data
{
  data <- read.csv(file.name.and.path, stringsAsFactors = FALSE, skip = 1, sep = ";")
  data <- data[,(1:5)]
  names(data) <- c("time","sample_index","x","y","z")
  
  data$time <- sub("#","",data$time)
  data$time <- sub("#","",data$time)
  data[,2] <- as.numeric(as.character(data[,2]))
  data[,3] <- as.numeric(as.character(data[,3]))
  data[,4] <- as.numeric(as.character(data[,4]))
  data[,5] <- as.numeric(as.character(data[,5]))
  
  t <- dim(data)[1]
  data <- data[!(data[,"time"] == "1899-12-30"),]
  data <- data[!(data[,"time"] == "0"),]
  n <- dim(data)[1]		
  
  if(is.character(data$time)==TRUE&t==n)
  {
    data$time <- as.numeric(data$time)
    data$time <- as.POSIXct(as.Date(data$time,origin="1899-12-30"))
    data$time <- as.POSIXlt(data$time,tz="GMT")
    data$time <- strptime(data$time,format="%Y-%m-%d %H:%M:%S")
  }
  return(data)
}

## Actigraph
# Protocol 1
data_time_xyz_actigraph_pro1 <- loaddata(path = path, folder = "Actigraph_pro1", brand = "Actigraph")
# Protocol 2
data_time_xyz_actigraph_pro2_ses1 <- loaddata(path = path, folder = "Actigraph_pro2_ses1", brand = "Actigraph") # session 1
data_time_xyz_actigraph_pro2_ses2 <- loaddata(path = path, folder = "Actigraph_pro2_ses2", brand = "Actigraph") # session 2
data_time_xyz_actigraph_pro2_ses3 <- loaddata(path = path, folder = "Actigraph_pro2_ses3", brand = "Actigraph") # session 3
# Protocol 3
data_time_xyz_actigraph_pro3 <- loaddata(path = path, folder = "Actigraph_pro3", brand = "Actigraph")

## Activpal
# Protocol 1
data_time_xyz_activpal_pro1 <- loaddata(path = path, folder = "Activpal_pro1", brand = "Activpal")
# Protocol 2
data_time_xyz_activpal_pro2 <- loaddata(path = path, folder = "Activpal_pro2", brand = "Activpal")
# Protocol 3
data_time_xyz_activpal_pro3 <- loaddata(path = path, folder = "Activpal_pro3", brand = "Activpal")

## Acttrust
# Protocol 1, 2, 3
data_time_xyz_acttrust_pro123 <- loaddata(path = path, folder = "Acttrust_Condor", brand = "Acttrust")

## Axivity
# Protocol 1
data_time_xyz_axivity_pro1 <- loaddata(path = path, folder = "Axivity_pro1", brand = "Axivity")
# Protocol 2
data_time_xyz_axivity_pro2_ses1 <- loaddata(path = path, folder = "Axivity_pro2_ses1", brand = "Axivity") #session 1
data_time_xyz_axivity_pro2_ses2 <- loaddata(path = path, folder = "Axivity_pro2_ses2", brand = "Axivity") #session 2
data_time_xyz_axivity_pro2_ses3 <- loaddata(path = path, folder = "Axivity_pro2_ses3", brand = "Axivity") #session 3
# Protocol 3
data_time_xyz_axivity_pro3_ses1 <- loaddata(path = path, folder = "Axivity_pro3_ses1", brand = "Axivity") #session 1
data_time_xyz_axivity_pro3_ses2 <- loaddata(path = path, folder = "Axivity_pro3_ses2", brand = "Axivity") #session 2
data_time_xyz_axivity_pro3_ses3 <- loaddata(path = path, folder = "Axivity_pro3_ses3", brand = "Axivity") #session 3

## GENEActiv
# Protocol 1
data_time_xyz_geneactiv_pro1 <- loaddata(path = path, folder = "GENEActiv_pro1", brand = "GENEActiv")
# Protocol 2
data_time_xyz_geneactiv_pro2_ses1 <- loaddata(path = path, folder = "GENEActiv_pro2_ses1", brand = "GENEActiv") #session 1
data_time_xyz_geneactiv_pro2_ses2 <- loaddata(path = path, folder = "GENEActiv_pro2_ses2", brand = "GENEActiv") #session 2
data_time_xyz_geneactiv_pro2_ses3 <- loaddata(path = path, folder = "GENEActiv_pro2_ses3", brand = "GENEActiv") #session 3
# Protocol 3
data_time_xyz_geneactiv_pro3 <- loaddata(path = path, folder = "GENEActiv_pro3", brand = "GENEActiv")

## MOX
# Protocol 1
data_time_xyz_mox_pro1 <- loaddata(path = path, folder = "MOX_exportedCSV/exportedCSV/MOX_pro1", brand = "MOX")
# Protocol2
data_time_xyz_mox_pro2_ses1 <- loaddata(path = path, folder = "MOX_exportedCSV/exportedCSV/MOX_pro2_ses1", brand = "MOX") #session 1
data_time_xyz_mox_pro2_ses2 <- loaddata(path = path, folder = "MOX_exportedCSV/exportedCSV/MOX_pro2_ses2", brand = "MOX") #session 2
data_time_xyz_mox_pro2_ses3 <- loaddata(path = path, folder = "MOX_exportedCSV/exportedCSV/MOX_pro2_ses3", brand = "MOX") #session 3
##Protocol 3
data_time_xyz_mox_pro3 <- loaddata(path = path, folder = "MOX_exportedCSV/exportedCSV/MOX_pro3", brand = "MOX")

## Shimmer
# Protocol 1
data_time_xyz_shimmer_pro1 <- loaddata(path = path, folder = "Shimmer_pro1", brand = "Shimmer")
# Procotol 2
data_time_xyz_shimmer_pro2_ses1 <- loaddata(path = path, folder = "Shimmer_pro2_ses1", brand = "Shimmer") #session 1
data_time_xyz_shimmer_pro2_ses2 <- loaddata(path = path, folder = "Shimmer_pro2_ses2", brand = "Shimmer") #session 2
data_time_xyz_shimmer_pro2_ses3a <- loaddata(path = path, folder = "Shimmer_pro2_ses3A", brand = "Shimmer") #session 3a
data_time_xyz_shimmer_pro2_ses3b <- loaddata(path = path, folder = "Shimmer_pro2_ses3B", brand = "Shimmer") #session 3b
# Protocol 3
data_time_xyz_shimmer_pro3 <- loaddata(path = path, folder = "Shimmer_pro3", brand = "Shimmer")
