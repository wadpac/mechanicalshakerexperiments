rm(list=ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

#====================================================================================
# Specify file paths
datadir = paste0(shaker_experiments_folder, "/analyses/")
if (!dir.exists(datadir)) {
  stop(paste0("Directory does not exist: ", datadir))
}
# Load mechanical shaker data for horizontal axis 
filename_flatHA = paste0(datadir, "ms_flat_HA_deviates_removed.RData") 
if (!file.exists(filename_flatHA)) {
  stop(paste0("File does not exist: ", filename_flatHA))
}
load(filename_flatHA)

# Select data with same sampling frequency for high and low experiments only
# Cross-correlations are only calculated for the selected data as otherwise interpolation is required (this is not longer raw data and outside the scope of this paper)
data_high_100 <- ms_flat_HA_new$data[ms_flat_HA_new$specifications$experiment == "ms_hfcr" 
                                     & round(as.numeric(ms_flat_HA_new$specifications$sampling_frequency)) == "100"]
data_low_25 <- ms_flat_HA_new$data[ms_flat_HA_new$specifications$experiment == "ms_lfcr" 
                                   & round(as.numeric(ms_flat_HA_new$specifications$sampling_frequency)) == "25"]

specifications_high <- ms_flat_HA_new$specifications[ms_flat_HA_new$specifications$experiment == "ms_hfcr" 
                                                     & round(as.numeric(ms_flat_HA_new$specifications$sampling_frequency)) == "100",]
specifications_low <- ms_flat_HA_new$specifications[ms_flat_HA_new$specifications$experiment == "ms_lfcr" 
                                                    & round(as.numeric(ms_flat_HA_new$specifications$sampling_frequency)) == "25",]

rm(shaker_experiments_folder, filename_flatHA, ms_flat_HA_new)

#====================================================================================
##FUNCTIONS
#' computeCrossCorrelations
#'
#' @description 'computeCrossCorrelations' computes the maximum cross-correlations between two signals and the corresponding lag
#' @param data An object of class "spec"
#' @return List object consisting of two elements: \item{correlationMatrix}{A correlation matrix representing the maximum cross-correlations between the signals} \item{lags}{A matrix that presents the lags corresponding to the lag in which the maximum cross-correlation occurred}
#' @export
computeCrossCorrelations <- function(data, plot = TRUE) {
  correlationMatrix <- matrix(data=NA,nrow=length(data),ncol=length(data)) # create empty matrix for max cross-correlations
  lagMatrix <- matrix(data=NA,nrow=length(data),ncol=length(data)) # create empty matrix to save lags corresponding to max cross-correlations
  for (x in 1:length(data)) {
    for (y in 1:length(data)) {
      cat(paste0(y, "/",  x, "/", length(data), " "))
      if (x==y) {
        correlationMatrix[x,y] <- 1
        x = x + 1
      }
      else {
        crossCorr <- ccf(data[[x]]$HA, data[[y]]$HA, type = "correlation", plot = FALSE)
        correlationMatrix[x,y] <- max(crossCorr$acf)
        lagMatrix[x,y] <- crossCorr$lag[crossCorr$acf==max(crossCorr$acf)]
        #create pair-wise plots
        if (plot == TRUE) {
          ggplot2::ggplot()
          + ggplot2::geom_point(data = data[[x]], ggplot2::aes(time, HA), alpha = 0.1, size = 0.25) 
          + ggplot2::geom_point(data = data[[y]], ggplot::aes(time, HA), color = "red", alpha = 0.1, size = 0.25)
        }
      }
    }
  }
  colnames(correlationMatrix) <- names(data)
  rownames(correlationMatrix) <- names(data)
  colnames(lagMatrix) <- names(data)
  rownames(lagMatrix) <- names(data)
  return(list(correlationMatrix = correlationMatrix, lags = lagMatrix))
}

#' pairwisePlots
#'
#' @description 'pairwisePlots' plots two timeseries
#' @param correlationMatrix A matrix that indicates for which pairs plots need to be derived
#' @param data A list that includes the timeseries data
#' @return Plot including two time series the timeseries of the device first in the title is presented in black
#' @export
pairwisePlots <- function(correlationMatrix, data) {
  plot_list <- list()
  counter = 0
  for (x in 1:nrow(correlationMatrix)){
    for (y in 1:ncol(correlationMatrix)){
      if (correlationMatrix[x,y] < 1 & !is.na(correlationMatrix[x,y])) {
        counter = counter + 1
        file_name <- paste(names(data)[[x]], names(data)[[y]], sep = "_vs_")
        jpeg(paste(datadir, file_name, ".jpeg", sep = "/plots/"), width=600, height=500, res=120) 
        p <- ggplot2::ggplot() +
          ggplot2::geom_point(data = data[[x]], ggplot2::aes(time, HA), alpha = 0.1, size = 0.25)+
          ggplot2::geom_point(data = data[[y]], ggplot2::aes(time, HA), color = "red", 
                              alpha =  0.1, size = 0.25) +
          ggplot2::ggtitle(file_name)
        dev.off()
        plot_list[[counter]] <- p
      }
    }
  }
  return(plot_list)
}
pairwisePlots <- function(correlationMatrix, data) {
  plot_list <- list()
  list_name <- c()
  counter = 0
  for (x in 1:nrow(correlationMatrix)){
    for (y in 1:ncol(correlationMatrix)){
      if (correlationMatrix[x,y] < 1 | !is.na(correlationMatrix[x,y])) {
        counter = counter + 1
        file_name <- paste(names(data)[[x]], names(data)[[y]], sep = "_vs_")
        list_name <- c(list_name, file_name)
        jpeg(paste(datadir, file_name, ".jpeg", sep = "/plots/"), width=600, height=500, res=120) 
        p <- ggplot2::ggplot() +
          ggplot2::geom_point(data = data[[x]], ggplot2::aes(time, HA), alpha = 0.1, size = 0.25)+
          ggplot2::geom_point(data = data[[y]], ggplot2::aes(time, HA), color = "red", 
                              alpha =  0.1, size = 0.25)
        dev.off()
        plot_list[[counter]] <- p
      }
    }
  }
  names(plot_list) <- list_name
  return(plot_list)
}

#' dataframeShapeComparisonBeween
#'
#' @description 'dataframeShapeComparisonBeweenWithin' derives a data.frame object required for between brand comparison of the cross-correlations
#'
#' @param crossCorrelations A list consisting of two elements: \item{correlationMatrix}{A correlation matrix representing the maximum cross-correlations between the signals} \item{lags}{A matrix that presents the lags corresponding to the lag in which the maximum cross-correlation occurred}
#' @param specifications A data.frame object that represents specifications of the signal, including label, serial_number, and brand
#' @return A data.frame with the following variables: \item{correlation}{Representing the cross-correlation between the two signals}, \item{lag}{Representing the lag corresonding to the maximum correlation between the two signals} 
#' \item{brand1}{Representing the brand of the first signal}, \item{brand2}{Representing the brand of the second signal}, 
#' \item{device1}{Representing the label of the device that recorded the first signal}, \item{device2}{Representing the label of the device that recorded the second signal},
#' \item{same_brand}{An indicator of correlations that can be selected for the within (1) or between(0) brand comparison}
#' 
#' @export
#' 
dataframeShapeComparisonBeween <- function(crossCorrelations, specifications){
  correlation <- c()
  lag  <- c()
  device1 <- c()
  device2 <- c()
  brand1 <- c()
  brand2 <- c()
  for(row in 1:nrow(crossCorrelations$correlationMatrix)) {
    for(col in 1:ncol(crossCorrelations$correlationMatrix)) {
      if(!is.na(crossCorrelations$correlationMatrix[row, col]) & !(row == col)){
        correlation <- c(correlation, as.double(crossCorrelations$correlationMatrix[row, col]))
        lag  <- c(lag, crossCorrelations$lags[row, col])
        device1 <- c(device1, specifications$label[row])
        device2 <- c(device2, specifications$label[col])
        brand1 <- c(brand1, specifications$brand[row])
        brand2 <- c(brand2, specifications$brand[col])
      }
    }
  }
  df_correlations <- as.data.frame(cbind(correlation, lag, brand1, device1, brand2, device2))
  brands <- as.factor(paste0(brand1, brand2))
  
  same_brand <- c()
  for(i in 1:nrow(df_correlations)){
    if(df_correlations$brand1[i] == df_correlations$brand2[i]){
      same_brand <- c(same_brand, 1)
    } else{
      same_brand <- c(same_brand, 0)}
  }
  df_correlations <- cbind(df_correlations, same_brand, brands)
  return(as.data.frame(df_correlations))
}


#====================================================================================

### COMPUTE: cross-correlation matrices ###
correlations_low <- computeCrossCorrelations(data_low_25, plot = TRUE) 
correlations_high <- computeCrossCorrelations(data_high_100, plot = TRUE)
cat("\nSaving data...")
correlation_matrices <- list(low = correlations_low, high = correlations_high)
save(correlation_matrices, file = paste0(datadir, "cross_correlations.RData"))

### DATA ANALYSES ###
#load(paste0(datadir, "cross_correlations.RData"))


## LOW FREQUENCY EXPERIMENT 
# Summary statistics
# cross-correlations
mean(correlation_matrices$low$correlationMatrix, na.rm = TRUE)
min(correlation_matrices$low$correlationMatrix, na.rm = TRUE)
max(correlation_matrices$low$correlationMatrix, na.rm = TRUE)
sd(correlation_matrices$low$correlationMatrix, na.rm = TRUE)
# lags
mean(correlation_matrices$low$lags, na.rm = TRUE)
min(correlation_matrices$low$lags, na.rm = TRUE)
max(correlation_matrices$low$lags, na.rm = TRUE)
sd(correlation_matrices$low$lags, na.rm = TRUE)

# Heatmap
fullmatrix_low <- correlation_matrices$low$correlationMatrix
heatmap_low <- corrplot::corrplot(pmax(fullmatrix_low, t(fullmatrix_low), na.rm = TRUE), type = 'lower', method = 'color', order = "alphabet",
                                  col.lim = c(0,1), addCoef.col = 'black', tl.srt=45, 
                                  number.cex = 0.75, tl.cex = 0.5, tl.col = "black")
heatmap_low

## Pairwise plots
pairwisePlots(correlation_matrices$low$correlationMatrix, data_low_25) 

## HIGH FREQUENCY EXPERIMENT 
# Summary statistics
# cross-correlations
mean(correlation_matrices$high$correlationMatrix, na.rm = TRUE)
min(correlation_matrices$high$correlationMatrix, na.rm = TRUE)
max(correlation_matrices$high$correlationMatrix, na.rm = TRUE)
sd(correlation_matrices$high$correlationMatrix, na.rm = TRUE)
# lags
mean(correlation_matrices$high$lags, na.rm = TRUE)
min(correlation_matrices$high$lags, na.rm = TRUE)
max(correlation_matrices$high$lags, na.rm = TRUE)
sd(correlation_matrices$high$lags, na.rm = TRUE)

# Heatmap
fullmatrix_high <- correlation_matrices$high$correlationMatrix
heatmap_high <- corrplot::corrplot(pmax(fullmatrix_high, t(fullmatrix_high), na.rm = TRUE), type = 'lower', method = 'color', order = "alphabet",
                                   col.lim = c(0,1), addCoef.col = 'black', tl.srt=45, diag = TRUE, 
                                   number.cex = 0.5, tl.cex = 0.5, tl.col = "black")
heatmap_high

## Pairwise plots
pairwisePlots(correlation_matrices$high$correlationMatrix, data_high_100) 

## DERIVE: data.frames required for comparison of the signal shape between brands
## Between brand comparison
df_correlations_low <- dataframeShapeComparisonBeween(correlation_matrices$low, specifications_low) 
df_correlations_high <- dataframeShapeComparisonBeween(correlation_matrices$high, specifications_high) 
cat("\nSaving data...")
correlation_df <- list(low = df_correlations_low, high = df_correlations_high)
save(correlation_df, file = paste0(datadir, "cross_correlation_df.RData"))

# Subset the data
#load(paste0(datadir, "cross_correlation_df.RData"))
correlations_between_low <- correlation_df$low[correlation_df$low$same_brand =="0", ]
correlations_between_high <- correlation_df$high[correlation_df$high$same_brand =="0", ]

## Low frequency experiment
# Descriptives
library(dplyr)
correlations_between_low$correlation <- as.numeric(correlations_between_low$correlation)
# table_between_low <- correlations_between_low %>% group_by(brands)
tapply(correlations_between_low$correlation, correlations_between_low$brands, summary)
tapply(correlations_between_low$correlation, correlations_between_low$brands, sd)

## High frequency experiment
# Descriptives
library(dplyr)
correlations_between_high$correlation <- as.numeric(correlations_between_high$correlation)
# table_between_high <- correlations_between_high %>% group_by(brands)
tapply(correlations_between_high$correlation, correlations_between_high$brands, summary)
tapply(correlations_between_high$correlation, correlations_between_high$brands, sd)