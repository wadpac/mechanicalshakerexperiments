## Function to read in the mechanical shaker data using:
# path;
# folder name;
# brand: one of c("Actigraph", "Activpal", "Acttrust", "Axivity", "GENEActiv", "MOX", "Shimmer"), 
# windows: TRUE then windows from data_description file will be selected, FALSE: complete data file will be loaded
# data_description: file name (only when windows = TRUE);
# protocol;
# session.

loaddata <- function(path, folder, brand, windows = TRUE, data_description, protocol, session) {
  #Load required libraries
  library(read.gt3x)
  library(GGIR)
  library(GENEAread)

  file_path <- paste(path, folder, sep = "/")
  data <- list()
  specifications <- data.frame()
  
  if(brand == "Actigraph") { pattern ="*.gt3x" }
  else if(brand == "Activpal" || brand == "MOX" || brand == "Shimmer") { pattern ="*.csv" }
  else if(brand == "Acttrust") { pattern ="*.txt" }
  else if(brand == "Axivity") { pattern ="*.cwa" }
  else if(brand == "GENEActiv") { pattern ="*.bin" }
  
  file_list <- list.files(file_path, pattern = pattern, all.files = FALSE)
  
  for(f in 1:length(file_list)) {
    # Load in the data
    if(brand == "Actigraph"){ d <- read.gt3x(paste(file_path, file_list[f], sep = "/"), asDataFrame = TRUE) }
    else if(brand == "Activpal") { d <- read.activpal(paste(file_path, file_list[f], sep = "/")) }
    else if(brand == "Acttrust") { d <- read.csv(paste(file_path, file_list[f], sep = "/"), sep = ";", skip = 25) }
    else if(brand == "Axivity") { d <- g.cwaread(paste(file_path, file_list[f], sep = "/"), start = 1, end = 1000000) }
    else if(brand == "GENEActiv") { d <- read.bin(paste(file_path, file_list[f], sep = "/")) }
    else if(brand == "MOX") { d <- read.csv(paste(file_path, file_list[f], sep = "/")) }
    else if(brand == "Shimmer") { d <- read.csv(paste(file_path, file_list[f], sep = "/"), nrow = 10, skip = 1, sep = '\t') }
    
    specs <- getspecs()
    
    if (windows == TRUE) {
      window <- getwindows(brand, data_description, protocol, session)
      select <- selectwindows(d, brand, window)
      d <- select
    }
    data[[f]] <- as.data.frame(d)
    
  }
  
  return(list(data, specifications))
}

