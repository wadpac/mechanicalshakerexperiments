rm(list=ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# Checked: GENEActiv (058029) was removed towards end of experiment ms_hfcr and data not included

#====================================================================================
# Specify file paths
datadir = paste0(shaker_experiments_folder, "/analyses/")
if (!dir.exists(datadir)) {
  stop(paste0("Directory does not exist: ", datadir))
}
# Load mechanical shaker data for horizontal axis 
filename_flatHA = paste0(datadir, "ms_flat_HA_VM.RData") 
if (!file.exists(filename_flatHA)) {
  stop(paste0("File does not exist: ", filename_flatHA))
}
load(filename_flatHA)

#AP672490: in both experiments raw values around 7.3 (M = 7.332, SD = 0.01083037) was measured during all shaking frequencies
#AP672490: in both experiments values around 0.3 (M = 0.32503, SD = 0.00628132) was measured during all shaking frequencies
#6011406: in both experiments value around 0 (M = -0.0148, SD = 0.2275247) with min of -14 and max of -22 during shaker frequency 0
# Remove data of these devices for now
deviations <- c("AP672490", "AP672490", "6011406")
ms_flat_HA$specifications <- ms_flat_HA$specifications[!(ms_flat_HA$specifications$serial_number==deviations),]
ms_flat_HA$data[names(ms_flat_HA$data)==deviations] <- NULL

# Select data with same sampling frequency for high and low experiments only
# Cross-correlations are only calculated for the selected data as otherwise interpolation is required (this is not longer raw data and outside the scope of this paper)
data_high_100 <- ms_flat_HA$data[ms_flat_HA$specifications$experiment == "ms_hfcr" 
                                 & round(as.numeric(ms_flat_HA$specifications$sampling_frequency)) == "100"]
data_low_25 <- ms_flat_HA$data[ms_flat_HA$specifications$experiment == "ms_lfcr" 
                            & round(as.numeric(ms_flat_HA$specifications$sampling_frequency)) == "25"]

specifications_high <- ms_flat_HA$specifications[ms_flat_HA$specifications$experiment == "ms_hfcr" 
                                                 & round(as.numeric(ms_flat_HA$specifications$sampling_frequency)) == "100",]
specifications_low <- ms_flat_HA$specifications[ms_flat_HA$specifications$experiment == "ms_lfcr" 
                                      & round(as.numeric(ms_flat_HA$specifications$sampling_frequency)) == "25",]

##FUNCTIONS
#' computeCrossCorrelations
#'
#' @description 'computeCrossCorrelations' computes the maximum cross-correlations between two signals and the corresponding lag
#'
#' @param data An object of class "spec"
#' @param axis String that represents the axis of the signal, one of: c("HA", "VM")
#' @return List object consisting of two elements: \item{correlationMatrix}{A correlation matrix representing the maximum cross-correlations between the signals} \item{lags}{A matrix that presents the lags corresponding to the lag in which the maximum cross-correlation occurred}
#' @export
computeCrossCorrelations <- function(data, axis) {
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
        if (axis == "HA") {
          crossCorr <- ccf(data[[x]]$HA, data[[y]]$HA, type = "correlation", plot = FALSE)
        } else if (axis == "VM"){
          crossCorr <- ccf(data[[x]]$VM, data[[y]]$VM, type = "correlation", plot = FALSE)
        }
        correlationMatrix[x,y] <- max(crossCorr$acf)
        lagMatrix[x,y] <- crossCorr$lag[crossCorr$acf==max(crossCorr$acf)]
      }
    }
  }
  colnames(correlationMatrix) <- names(data)
  rownames(correlationMatrix) <- names(data)
  colnames(lagMatrix) <- names(data)
  rownames(lagMatrix) <- names(data)
  return(list(correlationMatrix = correlationMatrix, lags = lagMatrix))
}

#' dataframeShapeComparisonBeweenWithin
#'
#' @description 'dataframeShapeComparisonBeweenWithin' derives a data.frame object required for between and within brand comparison of the cross-correlations
#'
#' @param crossCorrelations A list consisting of two elements: \item{correlationMatrix}{A correlation matrix representing the maximum cross-correlations between the signals} \item{lags}{A matrix that presents the lags corresponding to the lag in which the maximum cross-correlation occurred}
#' @param specifications A data.frame object that represents specifications of the signal, including serial_number and brand
#' @return A data.frame with the following variables: \item{correlations}{Representing the cross-correlations between the two signals}, \item{lags}{Representing the lag corresonding to the maximum correlation between the two signals} 
#' \item{brand1}{Representing the brand of the first signal}, \item{brand2}{Representing the brand of the second signal}, 
#' \item{device1}{Representing the serial number of the device that recorded the first signal}, \item{device2}{Representing the serial number of the device that recorded the second signal},
#' \item{same_brand}{An indicator of correlations that can be selected for the within (1) or between(0) brand comparison}
#' 
#' @export
#' 
dataframeShapeComparisonBeweenWithin <- function(crossCorrelations, specifications){
  correlations <- c()
  lags  <- c()
  device1 <- c()
  device2 <- c()
  brand1 <- c()
  brand2 <- c()
  for(row in 1:nrow(crossCorrelations$correlationMatrix)) {
    for(col in 1:ncol(crossCorrelations$correlationMatrix)) {
      if(!is.na(crossCorrelations$correlationMatrix[row, col]) & !(row == col)){
        correlations <- c(correlations, as.double(crossCorrelations$correlationMatrix[row, col]))
        lags  <- c(lags, crossCorrelations$lags[row, col])
        device1 <- c(device1, specifications$serial_number[row])
        device2 <- c(device2, specifications$serial_number[col])
        brand1 <- c(brand1, specifications$brand[row])
        brand2 <- c(brand2, specifications$brand[col])
      }
    }
  }
  df_correlations <- as.data.frame(cbind(correlations, lags, brand1, device1, brand2, device2))
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

#' plotHeatmapBetween
#'
#' @description 'plotHeatmapBetween' plot a heatmap for the between brand comparison
#'
#' @param df_correlations A data.frame with the following variables: \item{correlations}{Representing the cross-correlations between the two signals}, \item{lags}{Representing the lag corresonding to the maximum correlation between the two signals} 
#' \item{brand1}{Representing the brand of the first signal}, \item{brand2}{Representing the brand of the second signal}, 
#' \item{device1}{Representing the serial number of the device that recorded the first signal}, \item{device2}{Representing the serial number of the device that recorded the second signal},
#' \item{same_brand}{An indicator of correlations selected for the between(0) brand comparison}
#' @return A heatmap representing the cross-correlations between brands
#' 
#' @export
#' 
plotHeatmapBetween <- function(df_correlations) {
  heatmap_between <- ggplot(data = df_correlations, aes(x=brand1, y=brand2, fill=correlations)) + 
    geom_tile(color = "black") +
    coord_fixed() +
    
    scale_fill_continuous(
      limits = c(0,1), breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
      guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)
    )
  
  return(heatmap_between)
}

#' plotHeatmapWithin
#'
#' @description 'plotHeatmapWithin' plot a heatmap for the within brand comparison
#'
#' @param df_correlations A data.frame where there is selected for one brand with the following variables: \item{correlations}{Representing the cross-correlations between the two signals}, \item{lags}{Representing the lag corresonding to the maximum correlation between the two signals} 
#' \item{brand1}{Representing the brand of the first signal}, \item{brand2}{Representing the brand of the second signal}, 
#' \item{device1}{Representing the serial number of the device that recorded the first signal}, \item{device2}{Representing the serial number of the device that recorded the second signal},
#' \item{same_brand}{An indicator of correlations selected for the within(1) brand comparison}
#' @return A heatmap representing the cross-correlations within brand
#' 
#' @export
#' 
plotHeatmapWithin <- function(df_correlations) {
  heatmap_within <- ggplot(data = df_correlations, aes(x=device1, y=device2, fill=correlations)) + 
    geom_tile(color = "black") +
    coord_fixed() +
    scale_fill_continuous(
      limits = c(0,1), breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
      guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)
    )
   theme(legend.title = element_blank(),
        axis.text.x = element_text(angle=30,hjust=1,vjust=1.0))
  return(heatmap_within)
}

###DATA ANALYSES: for HA (norm HA has the same shape) and VM
## COMPUTE: cross-correlation matrices
# Low frequency experiments - HA
correlations_low_HA <- computeCrossCorrelations(data_low_25, "HA") 
# Low frequency experiments - VM
correlations_low_VM <- computeCrossCorrelations(data_low_25, "VM")
# High frequency experiments - HA
correlations_high_HA <- computeCrossCorrelations(data_high_100, "HA")
# High frequency experiments - VM
correlations_high_VM <- computeCrossCorrelations(data_high_100, "VM")

## DERIVE: data.frames required for comparison of the signal shape between and within brands
# Low frequency experiments - HA
df_correlations_low_HA <- dataframeShapeComparisonBeweenWithin(correlations_low_HA, specifications_low) 
# Low frequency experiments - VM
df_correlations_low_VM <- dataframeShapeComparisonBeweenWithin(correlations_low_VM, specifications_low) 
# High frequency experiments - HA
df_correlations_high_HA <- dataframeShapeComparisonBeweenWithin(correlations_high_HA, specifications_high) 
# High frequency experiments - VM
df_correlations_high_VM <- dataframeShapeComparisonBeweenWithin(correlations_high_VM, specifications_high) 

## COMPARISON
## LOW FREQUENCY EXPERIMENT - HA
#All devices
heatmap_low_HA <- corrplot::corrplot(correlations_low_HA$correlationMatrix, type = 'lower', method = 'color', 
                                     col.lim = c(0,1), addCoef.col = 'grey', tl.srt=45, diag = FALSE, 
                                     number.cex = 0.5, tl.cex = 0.5, tl.col = "black")
heatmap_low_HA

#Between brands
df_correlations_low_HA_between <- df_correlations_low_HA[df_correlations_low_HA$same_brand =="0", ]
df_correlations_low_HA_between$correlations <- as.numeric(df_correlations_low_HA_between$correlations)
heatmap_low_HA_between <- plotHeatmapBetween(df_correlations_low_HA_between)
heatmap_low_HA_between
table_low_HA_between <- df_correlations_low_HA_between %>% group_by(brands)
tapply(df_correlations_low_HA_between$correlations, df_correlations_low_HA_between$brands, summary)
tapply(df_correlations_low_HA_between$correlations, df_correlations_low_HA_between$brands, sd)

#Within Axivity
df_correlations_low_HA_within_Ax <- df_correlations_low_HA[df_correlations_low_HA$same_brand =="1" & df_correlations_low_HA$brand1 == "Axivity", ]
df_correlations_low_HA_within_Ax$correlations <- as.numeric(df_correlations_low_HA_within_Ax$correlations)
heatmap_low_HA_Ax <- plotHeatmapWithin(df_correlations_low_HA_within_Ax)
heatmap_low_HA_Ax
summarize(df_correlations_low_HA_within_Ax)
summary(df_correlations_low_HA_within_Ax$correlations)
sd(df_correlations_low_HA_within_Ax$correlations)

#Within GENEActiv
df_correlations_low_HA_within_GA <- df_correlations_low_HA[df_correlations_low_HA$same_brand =="1" & df_correlations_low_HA$brand1 == "GENEActiv", ]
df_correlations_low_HA_within_GA$correlations <- as.numeric(df_correlations_low_HA_within_GA$correlations)
heatmap_low_HA_GA <- plotHeatmapWithin(df_correlations_low_HA_within_GA)
heatmap_low_HA_GA
summarize(df_correlations_low_HA_within_GA)
summary(df_correlations_low_HA_within_GA$correlations)
sd(df_correlations_low_HA_within_GA$correlations)

#Within MOX
df_correlations_low_HA_within_MOX <- df_correlations_low_HA[df_correlations_low_HA$same_brand =="1" & df_correlations_low_HA$brand1 == "MOX", ]
df_correlations_low_HA_within_MOX$correlations <- as.numeric(df_correlations_low_HA_within_MOX$correlations)
heatmap_low_HA_MOX <- plotHeatmapWithin(df_correlations_low_HA_within_MOX)
heatmap_low_HA_MOX
summarize(df_correlations_low_HA_within_MOX)
summary(df_correlations_low_HA_within_MOX$correlations)
sd(df_correlations_low_HA_within_MOX$correlations)

## LOW FREQUENCY EXPERIMENT - VM
#All devices
heatmap_low_VM <- corrplot::corrplot(correlations_low_VM$correlationMatrix, type = 'lower', method = 'color', 
                                     col.lim = c(0,1), addCoef.col = 'grey', tl.srt=45, diag = FALSE, 
                                     number.cex = 0.5, tl.cex = 0.5, tl.col = "black")
heatmap_low_VM

#Between brands
df_correlations_low_VM_between <- df_correlations_low_VM[df_correlations_low_VM$same_brand =="0", ]
df_correlations_low_VM_between$correlations <- as.numeric(df_correlations_low_VM_between$correlations)
heatmap_low_VM_between <- plotHeatmapBetween(df_correlations_low_VM_between)
heatmap_low_VM_between
table_low_HA_between <- df_correlations_low_VM_between %>% group_by(brands)
tapply(df_correlations_low_VM_between$correlations, df_correlations_low_VM_between$brands, summary)
tapply(df_correlations_low_VM_between$correlations, df_correlations_low_VM_between$brands, sd)

#Within Axivity
df_correlations_low_VM_within_Ax <- df_correlations_low_VM[df_correlations_low_VM$same_brand =="1" & df_correlations_low_VM$brand1 == "Axivity", ]
df_correlations_low_VM_within_Ax$correlations <- as.numeric(df_correlations_low_VM_within_Ax$correlations)
heatmap_low_VM_Ax <- plotHeatmapWithin(df_correlations_low_VM_within_Ax)
heatmap_low_VM_Ax
summarize(df_correlations_low_VM_within_Ax)
summary(df_correlations_low_VM_within_Ax$correlations)
sd(df_correlations_low_VM_within_Ax$correlations)

#Within GENEActiv
df_correlations_low_VM_within_GA <- df_correlations_low_VM[df_correlations_low_VM$same_brand =="1" & df_correlations_low_VM$brand1 == "GENEActiv", ]
df_correlations_low_VM_within_GA$correlations <- as.numeric(df_correlations_low_VM_within_GA$correlations)
heatmap_low_VM_GA <- plotHeatmapWithin(df_correlations_low_VM_within_GA)
heatmap_low_VM_GA
summarize(df_correlations_low_VM_within_GA)
summary(df_correlations_low_VM_within_GA$correlations)
sd(df_correlations_low_VM_within_GA$correlations)

#Within MOX
df_correlations_low_VM_within_MOX <- df_correlations_low_VM[df_correlations_low_VM$same_brand =="1" & df_correlations_low_VM$brand1 == "MOX", ]
df_correlations_low_VM_within_MOX$correlations <- as.numeric(df_correlations_low_VM_within_MOX$correlations)
heatmap_low_VM_MOX <- plotHeatmapWithin(df_correlations_low_VM_within_MOX)
heatmap_low_VM_MOX
summarize(df_correlations_low_VM_within_MOX)
summary(df_correlations_low_VM_within_MOX$correlations)
sd(df_correlations_low_VM_within_MOX$correlations)

## HIGH FREQUENCY EXPERIMENT - HA
#All devices
heatmap_high_HA <- corrplot::corrplot(correlations_high_HA$correlationMatrix, type = 'lower', method = 'color', 
                                     col.lim = c(0,1), addCoef.col = 'grey', tl.srt=45, diag = FALSE, 
                                     number.cex = 0.25, tl.cex = 0.5, tl.col = "black")
heatmap_high_HA

#Between brands
df_correlations_high_HA_between <- df_correlations_high_HA[df_correlations_high_HA$same_brand =="0", ]
df_correlations_high_HA_between$correlations <- as.numeric(df_correlations_high_HA_between$correlations)
heatmap_high_HA_between <- plotHeatmapBetween(df_correlations_high_HA_between)
heatmap_high_HA_between
table_high_HA_between <- df_correlations_high_HA_between %>% group_by(brands)
tapply(df_correlations_high_HA_between$correlations, df_correlations_high_HA_between$brands, summary)
tapply(df_correlations_high_HA_between$correlations, df_correlations_high_HA_between$brands, sd)

#Within Axivity
df_correlations_high_HA_within_Ax <- df_correlations_high_HA[df_correlations_high_HA$same_brand =="1" & df_correlations_high_HA$brand1 == "Axivity", ]
df_correlations_high_HA_within_Ax$correlations <- as.numeric(df_correlations_high_HA_within_Ax$correlations)
heatmap_high_HA_Ax <- plotHeatmapWithin(df_correlations_high_HA_within_Ax)
heatmap_high_HA_Ax
summarize(df_correlations_high_HA_within_Ax)
summary(df_correlations_high_HA_within_Ax$correlations)
sd(df_correlations_high_HA_within_Ax$correlations)

#Within Actigraph
df_correlations_high_HA_within_AG <- df_correlations_high_HA[df_correlations_high_HA$same_brand =="1" & df_correlations_high_HA$brand1 == "Actigraph", ]
df_correlations_high_HA_within_AG$correlations <- as.numeric(df_correlations_high_HA_within_AG$correlations)
heatmap_high_HA_AG <- plotHeatmapWithin(df_correlations_high_HA_within_AG)
heatmap_high_HA_AG
summarize(df_correlations_high_HA_within_AG)
summary(df_correlations_high_HA_within_AG$correlations)
sd(df_correlations_high_HA_within_AG$correlations)

#Within GENEActiv
df_correlations_high_HA_within_GA <- df_correlations_high_HA[df_correlations_high_HA$same_brand =="1" & df_correlations_high_HA$brand1 == "GENEActiv", ]
df_correlations_high_HA_within_GA$correlations <- as.numeric(df_correlations_high_HA_within_GA$correlations)
heatmap_high_HA_GA <- plotHeatmapWithin(df_correlations_high_HA_within_GA)
heatmap_high_HA_GA
summarize(df_correlations_high_HA_within_GA)
summary(df_correlations_high_HA_within_GA$correlations)
sd(df_correlations_high_HA_within_GA$correlations)

#Within MOX
df_correlations_high_HA_within_MOX <- df_correlations_high_HA[df_correlations_high_HA$same_brand =="1" & df_correlations_high_HA$brand1 == "MOX", ]
df_correlations_high_HA_within_MOX$correlations <- as.numeric(df_correlations_high_HA_within_MOX$correlations)
heatmap_high_HA_MOX <- plotHeatmapWithin(df_correlations_high_HA_within_MOX)
heatmap_high_HA_MOX
summarize(df_correlations_high_HA_within_MOX)
summary(df_correlations_high_HA_within_MOX$correlations)
sd(df_correlations_high_HA_within_MOX$correlations)

## HIGH FREQUENCY EXPERIMENT - VM
#All devices
heatmap_high_VM <- corrplot::corrplot(correlations_high_VM$correlationMatrix, type = 'lower', method = 'color', 
                                     col.lim = c(0,1), addCoef.col = 'grey', tl.srt=45, diag = FALSE, 
                                     number.cex = 0.25, tl.cex = 0.5, tl.col = "black")
heatmap_high_VM

#Between brands
df_correlations_high_VM_between <- df_correlations_high_VM[df_correlations_high_VM$same_brand =="0", ]
df_correlations_high_VM_between$correlations <- as.numeric(df_correlations_high_VM_between$correlations)
heatmap_high_VM_between <- plotHeatmapBetween(df_correlations_high_VM_between)
heatmap_high_VM_between
table_high_VM_between <- df_correlations_high_VM_between %>% group_by(brands)
tapply(df_correlations_high_VM_between$correlations, df_correlations_high_VM_between$brands, summary)
tapply(df_correlations_high_VM_between$correlations, df_correlations_high_VM_between$brands, sd)

#Within Axivity
df_correlations_high_VM_within_Ax <- df_correlations_high_VM[df_correlations_high_VM$same_brand =="1" & df_correlations_high_VM$brand1 == "Axivity", ]
df_correlations_high_VM_within_Ax$correlations <- as.numeric(df_correlations_high_VM_within_Ax$correlations)
heatmap_high_VM_Ax <- plotHeatmapWithin(df_correlations_high_VM_within_Ax)
heatmap_high_VM_Ax
summarize(df_correlations_high_VM_within_Ax)
summary(df_correlations_high_VM_within_Ax$correlations)
sd(df_correlations_high_VM_within_Ax$correlations)

#Within Actigraph
df_correlations_high_VM_within_AG <- df_correlations_high_VM[df_correlations_high_VM$same_brand =="1" & df_correlations_high_VM$brand1 == "Actigraph", ]
df_correlations_high_VM_within_AG$correlations <- as.numeric(df_correlations_high_VM_within_AG$correlations)
heatmap_high_VM_AG <- plotHeatmapWithin(df_correlations_high_VM_within_AG)
heatmap_high_VM_AG
summarize(df_correlations_high_VM_within_AG)
summary(df_correlations_high_VM_within_AG$correlations)
sd(df_correlations_high_VM_within_AG$correlations)

#Within GENEActiv
df_correlations_high_VM_within_GA <- df_correlations_high_VM[df_correlations_high_VM$same_brand =="1" & df_correlations_high_VM$brand1 == "GENEActiv", ]
df_correlations_high_VM_within_GA$correlations <- as.numeric(df_correlations_high_VM_within_GA$correlations)
heatmap_high_VM_GA <- plotHeatmapWithin(df_correlations_high_VM_within_GA)
heatmap_high_VM_GA
summarize(df_correlations_high_VM_within_GA)
summary(df_correlations_high_VM_within_GA$correlations)
sd(df_correlations_high_VM_within_GA$correlations)

#Within MOX
df_correlations_high_VM_within_MOX <- df_correlations_high_VM[df_correlations_high_VM$same_brand =="1" & df_correlations_high_VM$brand1 == "MOX", ]
df_correlations_high_VM_within_MOX$correlations <- as.numeric(df_correlations_high_VM_within_MOX$correlations)
heatmap_high_VM_MOX <- plotHeatmapWithin(df_correlations_high_VM_within_MOX)
heatmap_high_VM_MOX
summarize(df_correlations_high_VM_within_MOX)
summary(df_correlations_high_VM_within_MOX$correlations)
sd(df_correlations_high_VM_within_MOX$correlations)
