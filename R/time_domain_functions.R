#' computeCrossCorrelations
#'
#' @description 'computeCrossCorrelations' computes the maximum cross-correlations between two signals and the corresponding lag
#' @param data An object of class "spec"
#' @param plot A boolean: if TRUE the cross-correlation will be plotted
#' @return List object consisting of two elements: \item{correlationMatrix}{A correlation matrix representing the maximum cross-correlations between the signals} \item{lags}{A matrix that presents the lags corresponding to the lag in which the maximum cross-correlation occurred}
#' @importFrom stats sd ccf
#' @export
computeCrossCorrelations <- function(data, plot) {
  correlationMatrix <- matrix(data=NA,nrow=length(data),ncol=length(data)) # create empty matrix for max cross-correlations
  lagMatrix <- matrix(data=NA,nrow=length(data),ncol=length(data)) # create empty matrix to save lags corresponding to max cross-correlations
  for (x in 1:length(data)) {
    for (y in 1:length(data)) {
      #cat(paste0(y, "/",  x, "/", length(data), " "))
      if (x==y) {
        correlationMatrix[x,y] <- 1
        x = x + 1
      }
      else {
        #If one of the time series during the shaker condition(s) has a standard deviation of 0, cross-correlation coefficient = 0
        if(sd(data[[x]]$SD) == 0 | sd(data[[y]]$SD) == 0){
          correlationMatrix[x,y] <- 0
          lagMatrix[x,y] <- NA
        }
        #If both time series during the shaker condition(s) have a standard deviation of 0, cross-correlation coefficient = 1
        else if(sd(data[[x]]$SD) == 0 & sd(data[[y]]$SD) == 0){
          correlationMatrix[x,y] <- 1
        }
        else{
          crossCorr <- ccf(data[[x]]$SD, data[[y]]$SD, type = "correlation", plot = plot)
          correlationMatrix[x,y] <- max(crossCorr$acf)
          lagMatrix[x,y] <- crossCorr$lag[crossCorr$acf==max(crossCorr$acf)]
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

#' computeCrossCorrelationsPerShakerCondition
#'
#' @description 'computeCrossCorrelationsPerShakerCondition' computes for each shaker condition the maximum cross-correlations between two signals and the corresponding lag per shaker condition using the function computeCrossCorrelations
#' @param data An object of class "spec"
#' @return List object containing the cross-correlations and lags for each shaker condition
#' @export
computeCrossCorrelationsPerShakerCondition <- function(data) {
  correlationList <- list()
  shakerCondition <- c(30, 40, 50, 62, 75, 87, 100, 112, 125, 137, 150, 160, 175, 187, 200, 212, 225, 237, 250) # add 0 if no-movement condition is to be included
  for(condition in 1:length(shakerCondition)){
    #Select data associated with the shaker condition
    selectData <- list()
    for(device in 1:length(data)){
      tmp <- subset(data[[device]], data[[device]]$shaking_frequency == shakerCondition[condition])
      selectData[[device]] <- tmp
    }
    names(selectData) <- names(data)
    correlationList[[condition]] <- computeCrossCorrelations(selectData, plot = TRUE)
  }
  names(correlationList) <- shakerCondition
  return(correlationList)
}

#' pairwisePlots
#'
#' @description 'pairwisePlots' generates a plot including two timeseries
#' @param correlationMatrix A matrix that indicates for which pairs plots need to be derived
#' @param data A list that includes the timeseries data
#' @param threshold A double number indicating the threshold cross-correlation for which pairwise plots will be generated
#' @return Plot including two time series the timeseries of the device first in the title is presented in black
#' @import ggplot2
#' @export
pairwisePlots <- function(correlationMatrix, data, threshold) {
  plot_list <- list()
  list_name <- c()
  counter = 0
  for (x in 1:nrow(correlationMatrix)){
    for (y in 1:ncol(correlationMatrix)){
      if (correlationMatrix[x,y] < 1 & !is.na(correlationMatrix[x,y])) {
        if(correlationMatrix[x,y] <= threshold){ #only create pairwise plots if the cross-correlation is equal/lower to the threshold
          counter = counter + 1
          device1 <- names(data)[[x]]
          device2 <- names(data)[[y]]
          file_name <- paste(paste(device1, device2, sep = "_vs_"), correlationMatrix[x,y], sep = " r = ")
          jpeg(paste(datadir, file_name, ".jpeg", sep = "/plots/"), width=600, height=500, res=120) 
          p <- ggplot() +
            geom_point(data = data[[x]], aes(x = time, y = SD, colour = "black"), 
                                alpha = 0.2, size = 0.25) +
            geom_point(data = data[[y]], aes(x = time, y = SD, colour = "red"), 
                                alpha =  0.2, size = 0.25) +
            theme_bw() +
            labs(title = paste0("Cross-correlation = ",  correlationMatrix[x,y]), y = "acceleration (g)") +
            #Legend
            scale_color_manual(values = c("black", "red"), labels=c(device1, device2)) +
            scale_fill_manual(name ="signal") + theme(legend.position = "right") 
          dev.off()
          plot_list[[counter]] <- p
        }
      }
    }
  }
  return(plot_list)
}

#' dataframeShapeComparisonBeween
#'
#' @description 'dataframeShapeComparisonBeween' derives a data.frame object required for between and within brand comparison of the cross-correlations
#'
#' @param crossCorrelations A list consisting of two elements: \itemize{\item{correlationMatrix}{A correlation matrix representing the maximum cross-correlations between the signals} \item{lags}{A matrix that presents the lags corresponding to the lag in which the maximum cross-correlation occurred}}
#' @param specifications A data.frame object that represents specifications of the signal, including serial_number, brand, and dynamic range
#' @param between A string one of c("brands", dynamic_ranges")
##' @return A data.frame with the following variables: \item{correlation}{Representing the cross-correlations between the two signals} \item{lag}{Representing the lag corresonding to the maximum correlation between the two signals} \item{between1}{Representing the brand/dynamic range of the first signal} \item{between2}{Representing the brand/dynamic range of the second signal} \item{device1}{Representing the serial number of the device that recorded the first signal} \item{device2}{Representing the serial number of the device that recorded the second signal} \item{same_brand}{An indicator of correlations that can be selected for the within (1) or between(0) brand/dynamic range comparison}
#' 
#' @export
#' 
dataframeShapeComparisonBeween <- function(crossCorrelations, specifications, between){
  correlation <- c()
  lag  <- c()
  device1 <- c()
  device2 <- c()
  between1 <- c()
  between2 <- c()
  for(row in 1:nrow(crossCorrelations$correlationMatrix)) {
    for(col in 1:ncol(crossCorrelations$correlationMatrix)) {
      if(!is.na(crossCorrelations$correlationMatrix[row, col]) & !(row == col)){
        correlation <- c(correlation, as.double(crossCorrelations$correlationMatrix[row, col]))
        lag  <- c(lag, crossCorrelations$lags[row, col])
        device1 <- c(device1, specifications$label[row])
        device2 <- c(device2, specifications$label[col])
        if (between == "brand"){
          between1 <- c(between1, specifications$brand[row])
          between2 <- c(between2, specifications$brand[col])
        } else if (between == "dynamic_range"){
          between1 <- c(between1, specifications$dynamic_range[row])
          between2 <- c(between2, specifications$dynamic_range[col])
        }
      }
    }
  }
  df_correlations <- as.data.frame(cbind(correlation, lag, between1, device1, between2, device2))
  between <- as.factor(paste0(between1, between2))
  
  same_group <- c()
  for(i in 1:nrow(df_correlations)){
    if(df_correlations$between1[i] == df_correlations$between2[i]){
      same_group <- c(same_group, 1)
    } else{
      same_group <- c(same_group, 0)}
  }
  df_correlations <- cbind(df_correlations, same_group, between)
  return(as.data.frame(df_correlations))
}