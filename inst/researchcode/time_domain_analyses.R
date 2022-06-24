rm(list=ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# TO DO: Check which GENEActiv was removed towards end of one of the experiments, and make sure data is not included

#====================================================================================
# Specify file paths
datadir = paste0(shaker_experiments_folder, "/analyses/")
if (!dir.exists(datadir)) {
  stop(paste0("Directory does not exist: ", datadir))
}
# Load mechanical shaker data for horizontal axis 
filename_flatHA = paste0(datadir, "ms_flat_HA.RData") 
if (!file.exists(filename_flatHA)) {
  stop(paste0("File does not exist: ", filename_flatHA))
}
load(filename_flatHA)

#SELECT DATA OF EXPERIMENT: HIGH
data <- ms_flat_HA$data[ms_flat_HA$specifications$experiment == "ms_hfcr" &
                          ms_flat_HA$specifications$sampling_frequency == "100"]
specifications <- ms_flat_HA$specifications[ms_flat_HA$specifications$experiment == "ms_hfcr" &
                          ms_flat_HA$specifications$sampling_frequency == "100",]

#DATA ANALYSIS

# Calculate cross-correlations for HA (at lag=0, and determine max correlation and corresponding lag)
correlationMatrix <- matrix(data=NA,nrow=length(data),ncol=length(data)) #cross-correlations at lag = 0
lagCorrelationMatrix <- matrix(data=NA,nrow=length(data),ncol=length(data)) #max cross-correlations
lagMatrix <- matrix(data=NA,nrow=length(data),ncol=length(data)) #Lag where max cross-correlations occur

for (x in 1:length(data)) {
  for (y in 1:length(data)) {
    cat(paste0(y, "/",  x, "/", length(data), " "))
    if(x==y) {
      correlationMatrix[x,y] <- 1
      lagCorrelationMatrix[x,y] <- 1
      x = x + 1
    }
    else{
      #df <- matchSignals(data[[x]], data[[y]]) 
      #if(nrow(df) > 0){
      #MAAK PLOTJES!!!
        crossCorr <- ccf(data[[x]]$HA, data[[y]]$HA, type = "correlation", plot = FALSE)
        correlationMatrix[x,y] <- crossCorr$acf[crossCorr$lag==0]
        lagCorrelationMatrix[x,y] <- max(crossCorr$acf)
        lagMatrix[x,y] <- crossCorr$lag[crossCorr$acf==max(crossCorr$acf)]
      #} 
    }
  }
}

colnames(correlationMatrix) <- names(data)
colnames(lagCorrelationMatrix) <- names(data)
colnames(lagMatrix) <- names(data)
correlations <- list(correlationMatrix = correlationMatrix, lagCorrelationMatrix = lagCorrelationMatrix, lagMatrix = lagMatrix)

save(correlations, file = paste0(datadir, "crossCorrelations_HA.RData"))

#irr::icc(df[1:200,2:3], model = "twoway", type = "agreement", unit = "average") #geen icc maar cross-correlatie.

# Function to match two signals (data.frame object consisting of timestamp and value), based on their timestamps
matchSignals <- function(signal1, signal2, sf_equal = TRUE){
  # Values of the first signal
  df1 <- data.frame(timestamp = as.POSIXct(signal1$time,origin = "1970-01-01", tz = "Europe/Amsterdam"), 
                    HA = signal1$HA)
  df2 <- data.frame(timestamp = as.POSIXct(signal2$time,origin = "1970-01-01", tz = "Europe/Amsterdam"), 
                    HA = signal2$HA)
  df <- dplyr::full_join(df1, df2, by = "timestamp")
  df <- na.omit(df)
  return(df)
}

