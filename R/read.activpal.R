#' read.activpal
#'
#' @description 'read.activpal' function adapted from package (activalProcessing), as the original function (activpal.file.reader) is not suitable for the raw data
#'
#' @param file.name.and.path Full file path to Activpal file (.csv format)
#' @return Data.frame with acceleration timeseries and timestamps
#' @export

read.activpal <- function(file.name.and.path) {
  #function adapted from package (activpalProcessing), as the original function (activpal.file.reader) is not suitable for the raw data
  data <- as.data.frame(data.table::fread(file.name.and.path, stringsAsFactors = FALSE, skip = 1, sep = ";"))
  #data <- data[,(1:5)]
  names(data) <- c("time","sample_index","X","Y","Z")
  
  data$time <- sub("#","",data$time)
  data$time <- sub("#","",data$time)
  for (j in 2:5) {
    data[,j] <- as.numeric(as.character(data[,j]))
  }
  t <- dim(data)[1]
  data <- data[!(data[,"time"] == "1899-12-30"),]
  data <- data[!(data[,"time"] == "0"),]
  n <- dim(data)[1]
  if(is.character(data$time) == TRUE & t == n) {
    data$time <- as.numeric(data$time)
    data$time <- as.POSIXct(as.Date(data$time,origin="1899-12-30"))
    data$time <- as.POSIXlt(data$time,tz="GMT")
    data$time <- strptime(data$time,format="%Y-%m-%d %H:%M:%OS")
  }
  return(data)
}