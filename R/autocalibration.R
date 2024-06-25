#' autocalibration
#'
#' @description 'autocalibration' simplified version of auto-calibration from R package GGIR without temperature and without loading the data in blocks
#'
#' @param data data.frame with accelerometer time series
#' @param sr Sampling rate in Hertz
#' @param printsummary Boolean to indicate whether calibration summary should be printed
#' @param brand Character of sensor brand: "ActiGraph", "activPAL", "Acttrust", "Axivity", "GENEActiv", or "MOX".
#' @return List of objects identical to g.calibrate function R package GGIR
#' @importFrom stats sd lm.wfit
#' @importFrom GENEAread g.downsample
#' @export

autocalibration = function(data, sr, printsummary= TRUE, brand) {
  # simplified version of auto-calibration from R package GGIR
  # without temperature and without loading the data in blocks

  spherecrit=0.3
  minloadcrit=72
  # printsummary=TRUE
  windowsizes=c(5,600,600)

  use.temp = FALSE
  # filename = unlist(strsplit(as.character(datafile),"/"))
  # filename = filename[length(filename)]
  # set parameters
  filequality = data.frame(filetooshort=FALSE,filecorrupt=FALSE,
                           filedoesnotholdday = FALSE, stringsAsFactors = TRUE)
  ws4 = 10 #epoch for recalibration, don't change
  ws2 = windowsizes[2] #dummy variable
  ws =  windowsizes[3] # window size for assessing non-wear time (seconds)
  i = 1 #counter to keep track of which binary block is being read
  count = 1 #counter to keep track of the number of seconds that have been read
  LD = 2 #dummy variable used to identify end of file and to make the process stop
  cal.error.start=cal.error.end=c()
  spheredata=c()
  tempoffset=c()
  npoints=c()
  PreviousEndPage = c() # needed for g.readaccfile
  scale = c(1,1,1)
  offset = c(0,0,0)
  bsc_qc = data.frame(time=c(),size=c(),stringsAsFactors = FALSE)
  #inspect file
  op <- options(stringsAsFactors = FALSE)
  on.exit(options(op))
  #creating matrixes for storing output
  S = matrix(0,0,4) #dummy variable needed to cope with head-tailing succeeding blocks of data
  NR = ceiling((90*10^6) / (sr*ws4)) + 1000 #NR = number of 'ws4' second rows (this is for 10 days at 80 Hz)
  meta = matrix(99999,NR,7)
  LD = nrow(data)
  #store data that could not be used for this block, but will be added to next block
  use = (floor(LD / (ws*sr))) * (ws*sr) #number of datapoint to use
  if (length(use) > 0) {
    if (use > 0) {
      if (use != LD) {
        S = as.matrix(data[(use+1):LD,]) #store left over # as.matrix removed on 22May2019 because redundant
        #S = data[(use+1):LD,] #store left over
      }
      data = as.matrix(data[1:use,])
      LD = nrow(data) #redefine LD because there is less data
      ##==================================================
      dur = nrow(data)	#duration of experiment in data points
      durexp = nrow(data) / (sr*ws)	#duration of experiment in hrs
      # Initialization of variables
      # if (dformat != 5) {

      # }
      data = as.data.frame(data)
      data$X = as.numeric(data$X)
      data$Y = as.numeric(data$Y)
      data$Z = as.numeric(data$Z)
      Gx = data[,2]; Gy = data[,3]; Gz = data[,4]
      #=============================================
      # non-integer sampling rate is a pain for deriving epoch based sd
      # however, with an epoch of 10 seconds it is an integer number of samples per epoch
      EN = sqrt(Gx^2 + Gy^2 + Gz^2)
      D1 = GGIR::g.downsample(EN,sr,ws4,ws2)
      EN2 = D1$var2
      #mean acceleration
      D1 = GGIR::g.downsample(Gx,sr,ws4,ws2); 	GxM2 = D1$var2
      D1 = GGIR::g.downsample(Gy,sr,ws4,ws2); 	GyM2 = D1$var2
      D1 = GGIR::g.downsample(Gz,sr,ws4,ws2); 	GzM2 = D1$var2
      #sd acceleration
      dim(Gx) = c(sr*ws4,ceiling(length(Gx)/(sr*ws4))); 	GxSD2 = apply(Gx,2,sd)
      dim(Gy) = c(sr*ws4,ceiling(length(Gy)/(sr*ws4))); 	GySD2 = apply(Gy,2,sd)
      dim(Gz) = c(sr*ws4,ceiling(length(Gz)/(sr*ws4))); 	GzSD2 = apply(Gz,2,sd)
      #-----------------------------------------------------
      #expand 'out' if it is expected to be too short
      if (count > (nrow(meta) - (2.5*(3600/ws4) *24))) {
        extension = matrix(99999,((3600/ws4) *24),ncol(meta))
        meta = rbind(meta,extension)
        # cat("\nVariable meta extended\n")
      }
      #storing in output matrix
      meta[count:(count-1+length(EN2)),1] = EN2
      meta[count:(count-1+length(EN2)),2] = GxM2
      meta[count:(count-1+length(EN2)),3] = GyM2
      meta[count:(count-1+length(EN2)),4] = GzM2
      meta[count:(count-1+length(EN2)),5] = GxSD2
      meta[count:(count-1+length(EN2)),6] = GySD2
      meta[count:(count-1+length(EN2)),7] = GzSD2
      count = count + length(EN2) #increasing "count": the indicator of how many seconds have been read
      rm(Gx); rm(Gy); rm(Gz)
    }
    #--------------------------------------------
  }
  spherepopulated = 0
  meta_temp = data.frame(V = meta, stringsAsFactors = FALSE)
  cut = which(meta_temp[,1] == 99999)
  if (length(cut) > 0) {
    meta_temp = meta_temp[-cut,]
  }
  nhoursused = (nrow(meta_temp) * 10)/3600
  if (nrow(meta_temp) > minloadcrit) {  # enough data for the sphere?
    meta_temp = meta_temp[-1,]
    #select parts with no movement
    if (brand == "MOX") {
      sdcriter = 0.03 # MOX seems to have too much variation in X-axis
    } else {
      sdcriter = 0.013
    }
    nomovement = which(meta_temp[,5] < sdcriter & meta_temp[,6] < sdcriter & meta_temp[,7] < sdcriter &
                         abs(as.numeric(meta_temp[,2])) < 2 & abs(as.numeric(meta_temp[,3])) < 2 &
                         abs(as.numeric(meta_temp[,4])) < 2) #the latter three are to reduce chance of including clipping periods
    meta_temp = meta_temp[nomovement,]
    rm(nomovement)
    if (min(dim(meta_temp)) > 1) {
      meta_temp = meta_temp[(is.na(meta_temp[,4]) == F & is.na(meta_temp[,1]) == F),]
      npoints = nrow(meta_temp)
      cal.error.start = sqrt(as.numeric(meta_temp[,2])^2 + as.numeric(meta_temp[,3])^2 + as.numeric(meta_temp[,4])^2)
      cal.error.start = round(mean(abs(cal.error.start - 1)), digits = 5)
      #check whether sphere is well populated
      tel = 0
      for (axis in 2:4) {
        if ( min(meta_temp[,axis]) < -spherecrit & max(meta_temp[,axis]) > spherecrit) {
          tel = tel + 1
        }
      }
      if (tel == 3) {
        spherepopulated = 1
      } else {
        spherepopulated = 0
        QC = "recalibration not done because not enough points on all sides of the sphere"
      }
    } else {
      cat(" No non-movement found\n")
      QC = "recalibration not done because no non-movement data available"
      meta_temp = c()
    }
  } else {
    QC = "recalibration not done because not enough data in the file or because file is corrupt"
  }
  if (spherepopulated == 1) { #only try to improve calibration if there are enough datapoints around the sphere
    #---------------------------------------------------------------------------
    # START of Zhou Fang's code (slightly edited by vtv21 to use matrix meta_temp from above
    # instead the similar matrix generated by Zhou Fang's original code. This to allow for
    # more data to be used as meta_temp can now be based on 10 or more days of raw data
    input = meta_temp[,2:4] #as.matrix()
    inputtemp = matrix(0, nrow(input), ncol(input)) #temperature, here used as a dummy variable
    meantemp = mean(as.numeric(inputtemp[,1]),na.rm=TRUE)
    inputtemp = inputtemp - meantemp
    offset = rep(0, ncol(input))
    scale = rep(1, ncol(input))
    tempoffset = rep(0, ncol(input))
    weights = rep(1, nrow(input))
    res = Inf
    maxiter = 1000
    tol = 1e-10
    for (iter in 1:maxiter) {
      curr = c()
      try(expr={curr = scale(input, center = -offset, scale = 1/scale) +
        scale(inputtemp, center = F, scale = 1/tempoffset)},silent=TRUE)
      if (length(curr) == 0) {
        # set coefficients to default, because it did not work.
        cat("\nObject curr has length zero.")
        break
      }
      closestpoint = curr/ sqrt(rowSums(curr^2))
      k = 1
      offsetch = rep(0, ncol(input))
      scalech = rep(1,ncol(input))
      toffch = rep(0, ncol(inputtemp))
      for (k in 1:ncol(input)){
        invi = which(is.na(closestpoint[,k, drop = F]) == TRUE)
        if (length(invi) > 0) {
          closestpoint = closestpoint[-invi,]
          curr = curr[-invi,]
          inputtemp = inputtemp[-invi,]
          input = input[-invi,]
          weights = weights[-invi]
        }
        fobj = lm.wfit(cbind(1, curr[,k],inputtemp[,k]) , closestpoint[,k, drop = F], w = weights)
        offsetch[k] = fobj$coef[1]
        scalech[k] = fobj$coef[2]
        if (use.temp == TRUE) {
          toffch[k] = fobj$coeff[3]
        }
        curr[,k] = fobj$fitted.values
      }
      offset = offset + offsetch / (scale  * scalech)
      if (use.temp == TRUE) {
        tempoffset = tempoffset * scalech + toffch
      }
      scale = scale * scalech
      res = c(res,  3 * mean(weights*(curr-closestpoint)^2/ sum(weights)))
      weights = pmin(1/ sqrt(rowSums((curr - closestpoint)^2)), 1/0.01)
      if (abs(res[iter+1] - res[iter]) < tol)  break
    }
    if (use.temp == FALSE) {
      meta_temp2 = scale(as.matrix(meta_temp[,2:4]),center = -offset, scale = 1/scale)
    } else {
      yy = as.matrix(cbind(as.numeric(meta_temp[,8]),as.numeric(meta_temp[,8]),as.numeric(meta_temp[,8])))
      meta_temp2 = scale(as.matrix(meta_temp[,2:4]),center = -offset, scale = 1/scale) +
        scale(yy, center = rep(meantemp,3), scale = 1/tempoffset)
    }     #equals: D2[,axis] = (D[,axis] + offset[axis]) / (1/scale[axis])
    # END of Zhou Fang's code
    #-------------------------------------------
    cal.error.end = sqrt(meta_temp2[,1]^2 + meta_temp2[,2]^2 + meta_temp2[,3]^2)
    rm(meta_temp2)
    cal.error.end = round(mean(abs(cal.error.end-1)), digits = 5)
    # assess whether calibration error has sufficiently been improved
    if (cal.error.end < cal.error.start & cal.error.end < 0.01 & nhoursused > minloadcrit) { #do not change scaling if there is no evidence that calibration improves
      LD = 0 #stop loading
    } else { #continue loading data
      if (nhoursused > minloadcrit) {
        print(paste("new calibration error: ",cal.error.end," g",sep=""))
        print(paste("npoints around sphere: ", npoints,sep=""))
      }
      QC = "recalibration attempted with all available data, but possibly not good enough: Check calibration error variable to varify this"
    }
  }
  if (length(cal.error.end) > 0) {
    if (cal.error.end > cal.error.start) {
      QC = "recalibration not done because recalibration does not decrease error"
    }
  }
  if (length(ncol(meta_temp)) != 0) {
    spheredata = data.frame(A = meta_temp, stringsAsFactors = TRUE)
    if (use.temp == TRUE) {
      names(spheredata) = c("Euclidean Norm","meanx","meany","meanz","sdx","sdy","sdz","temperature")
    } else {
      names(spheredata) = c("Euclidean Norm","meanx","meany","meanz","sdx","sdy","sdz")
    }
  } else {
    spheredata = c()
  }
  rm(meta_temp)
  QCmessage = QC
  if (printsummary == TRUE) {
    # cat(paste0(rep('_ ',options()$width),collapse=''))
    cat("\nSummary of autocalibration procedure:")
    cat("\n")
    cat(paste0("\nStatus: ",QCmessage))
    cat(paste0("\nCalibration error (g) before: ",cal.error.start))
    cat(paste0("\nCallibration error (g) after: ",cal.error.end))
    cat(paste0("\nOffset correction ",c("x","y","z"),": ",offset))
    cat(paste0("\nScale correction ",c("x","y","z"),": ",scale))
    cat(paste0("\nNumber of hours used: ",nhoursused))
    cat(paste0("\nNumber of 10 second windows around the sphere: ",npoints))
    cat(paste0("\nTemperature used (if available): ",use.temp))
    cat(paste0("\nTemperature offset (if temperature is available) ",c("x","y","z"),": ",tempoffset))
    cat("\n")
    # cat(paste0(rep('_',options()$width),collapse=''))
  }
  invisible(list(scale=scale, offset=offset, tempoffset=tempoffset,
                 cal.error.start=cal.error.start, cal.error.end=cal.error.end,
                 spheredata=spheredata, npoints=npoints, nhoursused=nhoursused,
                 QCmessage=QCmessage, use.temp=use.temp, bsc_qc=bsc_qc))
}
