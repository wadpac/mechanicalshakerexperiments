rm(list = ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "/media/vincent/DATA/VUMC/shaker_experiments"

# TO DO: Check which GENEActiv was removed towards end of one of the experiments, and make sure data is not included

source("~/projects/mechanicalshakerexperiments/R/applymetrics.R")

# #========================================================================================
# # Most of these libraries are needed for running SummarizedActigraph and/or MIMSunit
library(tidyverse)
library(lubridate)
# remotes::install_github("muschellij2/SummarizedActigraphy")
library(SummarizedActigraphy)
options(digits.secs = 7)
options(scipen = 999)


#====================================================================================
# Specify file paths
extracted_data_path = paste0(shaker_experiments_folder, "/structured_raw_data")
# calib_files = paste0(shaker_experiments_folder, "/autocalibration_results")
# if (!dir.exists(calib_files)) {
#   stop("\nNot able to find the autocalibeation_results folder.")
# }
#======================================================================================
# Load relevant files, apply calibration correction coefficients, and apply metrics
fns = dir(extracted_data_path, full.names = TRUE)
outputfile = paste0(shaker_experiments_folder, "/analyses/explore_metrics.RData")
sessionames = c("ms_hfcr", "ms_lfcr", "ms_mfcr") 
overwrite = FALSE
epochsize = 5
averageperws3 = function(x,sf,epochsize) {
  x2 = cumsum(c(0, x))
  select = seq(1,length(x2), by = sf * epochsize)
  x3 = diff(x2[round(select)]) / abs(diff(round(select)))
}
if (!file.exists(outputfile) | overwrite == TRUE) {
  print("reprocess data")
  combineddata <- list()
  sn_ignored = c()
  cnt  = 1
  for (ses_name in sessionames) { #
    ses1 = grep(basename(fns), pattern = ses_name)
    for (fn in fns[ses1]) {
      print(fn)
      load(fn) # TO DO: This loads object extracteddata => maybe rename this "structured_data"???
      if (length(grep(fn, pattern = "Actigraph")) > 0) {
        brand = "Actigraph"
      } else if (length(grep(fn, pattern = "Axivity")) > 0) {
        brand = "Axivity"
      } else if (length(grep(fn, pattern = "GENEActiv")) > 0) {
        brand = "GENEActiv"
      } else if (length(grep(fn, pattern = "Activpal")) > 0) {
        brand = "Activpal"
      }
      for (i in 1:length(extracteddata$data)) {
        tmp = extracteddata$data[[i]] # tmp is the raw data now
        if (length(tmp) > 0) {
          DR = as.numeric(extracteddata$specifications[i, "dynamic_range"])
          sf = as.numeric(extracteddata$specifications[i, "sampling_frequency"])
          sn = as.character(extracteddata$specifications[i, "serial_number"])
          if (brand == "Actigraph" | brand == "Activpal") {
            tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz = "Europe/Amsterdam")
            tmp = tmp[, c("time","x","y","z","shaking_frequency")] #,"shaking_frequency"
            colnames(tmp)[2:4] = c("X", "Y", "Z")
          } else if (brand == "Axivity" | brand == "GENEActiv" ) {
            tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz = "Europe/Amsterdam")
            tmp = tmp[, c("time","x","y","z","shaking_frequency")] #
            colnames(tmp)[2:4] = c("X", "Y", "Z")
          }
          row.names(tmp) = 1:nrow(tmp)
          colnames(tmp)[1] = "HEADER_TIME_STAMP"
          if (brand == "Actigraph") { # following lines should only be needed for Actigraph
            duplicated_timestamps = duplicated(tmp$HEADER_TIME_STAMP)
            if (any(duplicated_timestamps)) {
              # if there are duplicated timestamps then
              # make tiny change to avoid issues with running mims_unit package, which cannot handle duplicated timestamps
              tmp$HEADER_TIME_STAMP[duplicated_timestamps] = tmp$HEADER_TIME_STAMP[duplicated_timestamps] + 0.0001 
            }
          }
          #-----------------------------------------
          # fill time gaps, which is needed for Actigraph, which can go into sleep.mode
          time_gaps = 1
          while (length(time_gaps) > 0) {
            dt = diff(as.numeric(tmp$HEADER_TIME_STAMP))
            time_gaps = which(dt > 1)
            
            if (length(time_gaps) > 0) {
              cnt2 = 1
              tmp2 = tmp
              for (j in 1:length(time_gaps)) {
                inpute = as.data.frame(matrix(0, dt[time_gaps[j]]*sf, ncol(tmp)))
                colnames(inpute) = colnames(tmp)
                newtime = seq(tmp$HEADER_TIME_STAMP[time_gaps[j]], 
                              tmp$HEADER_TIME_STAMP[time_gaps[j] + 1], by = 1/sf)
                inpute$HEADER_TIME_STAMP = newtime[1:nrow(inpute)]
                if (j == 1) {
                  cnt2 = time_gaps[j] - 1
                } else {
                  cnt2 = nrow(tmp2)
                }
                cols_of_interest = c("X", "Y", "Z", "shaking_frequency")
                inpute[, cols_of_interest] = tmp[time_gaps[j], cols_of_interest]
                if (j != length(time_gaps)) {
                  tmp2 = rbind(tmp2[1:cnt2,], inpute)
                } else {
                  tmp2 = rbind(tmp2[1:cnt2,], inpute, tmp[(time_gaps[j] + 1):nrow(tmp),])
                }
              }
              tmp = tmp2
            }
          }
          negativ_shakef = which(tmp$shaking_frequency == -1)
          if (length(negativ_shakef) > 0) {
            tmp$shaking_frequency[negativ_shakef] = -100000
          }
          shakefreq = averageperws3(x = tmp$shaking_frequency, sf, epochsize = 5)
          shakefreq[which(shakefreq < 0)] = -1
          tmp = tmp[,-which(colnames(tmp) == "shaking_frequency")]
          #-----------------------------------------------------------
          # load here previously derived calibration factors
          # if autocalibration was successful apply coefficients
          # if not skip file
          # calib_fn = dir(calib_files, pattern = sn, full.names = T)
          # if (length(calib_fn) > 0) {
          #   load(calib_fn)
          #   if (all(calib$scale != c(1,1,1)) & all(calib$scale != c(0,0,0))) {
          #     if (calib$C$cal.error.end < 0.01 & calib$C$npoints > 50) {
          # tmp[,c("X","Y","Z")] = scale(tmp[,c("X","Y","Z")],
          #                              center = -calib$offset,
          #                              scale = 1/calib$scale)
          
          #===================================================================
          # Calculate metrics
          EN_raw = sqrt(tmp$X^2 + tmp$Y^2 + tmp$Z^2)
          EN_raw = EN_raw[1:(floor(nrow(tmp)/sf)*sf)]
          EN = averageperws3(EN_raw,sf,epochsize=5)
          ENMOraw = pmax(EN_raw - 1, 0)
          ENMO = averageperws3(ENMOraw,sf,epochsize=5)
          MEANS = rep(EN, each = sf*5)
          MAD = abs(EN_raw[1:length(MEANS)] - MEANS)
          MAD = averageperws3(x=MAD,sf,epochsize=5)
          # For additional metrics use separate function:
          metrics = applymetrics(data = tmp[,c("X","Y","Z")], sf = sf, epochsize=5)
          HFEN = metrics$HFEN
          HFENplus = metrics$HFENplus
          BFEN = metrics$BFEN
          
          # #=================
          # # Try apply code from:
          # # https://martakarass.github.io/post/2021-06-29-pa_measures_and_summarizedactigraphy/#dataset-labeled-raw-accelerometry-data
          tmp_tible = as_tibble(tmp)
          out_i = SummarizedActigraphy::calculate_measures(
            df = tmp_tible,
            unit = "5 sec",
            dynamic_range = c(-8, 8),  # dynamic range
            fix_zeros = TRUE,         # fixes zeros from idle sleep mode -- not needed in our case
            calculate_mims = FALSE,     # uses algorithm from MIMSunit package
            calculate_ac = FALSE,       # uses algorithm from activityCounts package
            flag_data = TRUE,         # runs raw data quality control flags algorithm -- not used in our case
            verbose = FALSE)
          # # Turned off because too slow to compute
          # S_MIMSunit = MIMSunit::mims_unit(df = tmp, epoch = '5 sec', dynamic_range = c(-DR, DR), output_mims_per_axis = TRUE)
          TIMESTAMPS = tmp$HEADER_TIME_STAMP[seq(1, nrow(tmp), by = sf * 5)]
          AI = out_i$AI
          
          #========================================================
          # implement simplified and faster mims:
          MIMSlight_fun = function(df, epochsize = 5, acc_columns = 2:4, range = 8) {
            averageperepoch = function(x, sf, epochsize) { 
              # I am aware that this is numerically unstable, but in our use-case
              # expected drop in precision is far beyond required precision
              # and by that acceptable.
              x2 = cumsum(c(0, x))
              select = seq(1, length(x2), by = sf * epochsize)
              x3 = diff(x2[round(select)]) / abs(diff(round(select)))
            }
            bf_filter = function(lb, hb, n, sf) {
              hb = ifelse(test = sf <= (hb * 2), yes = (sf / 2) - 0.1, no = hb)
              Wc = matrix(0,2,1)
              Wc[1,1] = lb / (sf/2) # lb: lower boundary
              Wc[2,1] = hb / (sf/2) # hb: higher boundary
              return(signal::butter(n, Wc, type = c("pass"))) 
            }
            mims_per_axis = matrix(NA, floor(nrow(df) / (sf*epochsize)), 3)
            # df = MIMSunit::extrapolate(df, range=range)
            for (i in acc_columns) {
              coef = bf_filter(lb = 0.2, hb = 5, n = 4, sf = sf)
              filtered_signal = signal::filter(coef, df[,i])
              # using this way of aggregating is a lot faster, but comes at the price of
              # new value differences with the original approach, if I insert the orignal approach
              # value differences are zero for the full range, expect close to zero,
              # which relates to interpolation step we are not doing
              BFSM_per_axis = averageperepoch(abs(filtered_signal), sf, epochsize)
              noise = which(BFSM_per_axis < 0.01) # MIMSunit gives zeros when sensor not moving, so we do same
              if (length(noise) > 0) BFSM_per_axis[noise] = 0
              mims_per_axis[, i - 1] = BFSM_per_axis * epochsize
            }
            MIMSlight = rowSums(mims_per_axis)
            return(MIMSlight)
          }
          MIMSlight = MIMSlight_fun(df = tmp, epochsize = 5, acc_columns = 2:4, range = 8)
          
          #===================================================================
          # Standardise size of objects and put in single data.frame
          checklen = function(x, y, RR=0) {
            # x is acc metric vector, y is timestamp vector
            if (length(x) > length(y)) {
              x = x[1:length(y)]
            } else if (length(x) < length(y)) {
              x = c(x, rep(RR, length(y) - length(x)))
            }
            return(x)
          }
          EN = checklen(EN, TIMESTAMPS, RR = 1)
          AI = checklen(AI, TIMESTAMPS)
          MAD  = checklen(MAD, TIMESTAMPS)
          ENMO  = checklen(ENMO, TIMESTAMPS)
          MIMSlight = checklen(MIMSlight, TIMESTAMPS)
          HFEN = checklen(HFEN, TIMESTAMPS)
          HFENplus = checklen(HFENplus, TIMESTAMPS)
          BFEN = checklen(BFEN, TIMESTAMPS)
          # MIMS_UNIT= checklen(MIMS_UNIT, TIMESTAMPS) 
          shakefreq = checklen(shakefreq, TIMESTAMPS)
          
          S = data.frame(HEADER_TIME_STAMP = TIMESTAMPS,
                         brand = brand, sf = sf,
                         EN = EN, ENMO = ENMO, MAD = MAD,
                         AI = AI, MIMSlight = MIMSlight,
                         HFEN = HFEN, HFENplus = HFENplus,
                         BFEN = BFEN, ses_name = ses_name,
                         sn = sn, shakefreq = shakefreq)
          negativ_shakef = which(S$shakefreq < 0)
          S = S[-negativ_shakef,]
          combineddata[[cnt]] = S # store result for later use
          cnt = cnt + 1
        } else {
          print(paste0(sn, " ignored")) 
          sn_ignored = c(sn_ignored, paste0(sn,"_", brand,"_", ses_name))
        }
      }
    }
  }
  print("serial numbers ignored")
  print(sn_ignored)
  
  save(combineddata, file = outputfile)
} else {
  load(file = outputfile)
}

#====================================================================
# Combine into data.frame
DATA = do.call("rbind", combineddata)
DATA = DATA[-which(DATA$sn %in% c(21950, 37727) == TRUE & DATA$sf == 800),] # remove extreme configuration that caused artifacts
# jjjj
DATA = DATA[-which(DATA$sn %in% c("AP672490", "AP473254") == TRUE & DATA$ses_name == "ms_lfcr"),] # remove extreme configuration that caused artifacts
#====================================================================
# Stratify Actigraph by version:
MOS = grep(pattern = "MOS", x = DATA$sn)
CLE = grep(pattern = "CLE", x = DATA$sn)
DATA$brand[MOS] = "ActigraphMOS"
DATA$brand[CLE] = "ActigraphCLE"

#========================================================================================
# Aggregate epochs to one value per shaker frequency x serial number combination.
D = aggregate(x = DATA[,c("EN", "MAD", "AI", "ENMO", 
                          "MIMSlight", "HFENplus", "HFEN", "BFEN", "shakefreq")],
              by = list(DATA$HEADER_TIME_STAMP, DATA$brand, DATA$ses_name, DATA$sn), FUN = mean)
colnames(D)[1:4] = c("time", "brand", "ses_name", "sn")
# Remove epochs with rare frequencies, these are the frequencies
# that result from the frequency transitions
Dnew = c()
for (ses_name in sessionames) { #"pro3_ses3" ,
  Dtmp = D[which(D$ses_name == ses_name),]
  FT = table(Dtmp$shakefreq)
  conditions_to_exclude = names(FT[which(as.numeric(FT) < 12)])
  if (length(conditions_to_exclude) > 0) {
    Dtmp = Dtmp[-which(Dtmp$shakefreq %in% conditions_to_exclude == TRUE),]
  }
  if (length(Dnew) == 0) {
    Dnew = Dtmp
  } else {
    Dnew = rbind(Dnew, Dtmp)
  }
}
D = Dnew

#=============================================================================
# Assess whether metrics differ across brands with repeated measures ANOVA
brands_of_interest = c("GENEActiv", "Axivity", "ActigraphCLE", "ActigraphMOS", "Activpal")
selection = which(D$ses_name == "pro2_ses2" & D$brand %in% brands_of_interest)
# fitEN = aov(EN ~ brand + Error(shakefreq), data = D[selection,])
# fitENMO = aov(ENMO ~ brand + Error(shakefreq), data = D[selection,])
# fitMAD = aov(ENMO ~ brand + Error(shakefreq), data = D[selection,])
# fitMIMSlight = aov(MIMSlight ~ brand + Error(shakefreq), data = D[selection,])
# fitAI = aov(AI ~ brand + Error(shakefreq), data = D[selection,])
# fitBFEN = aov(BFEN ~ brand + Error(shakefreq), data = D[selection,])
# fitHFEN = aov(HFEN ~ brand + Error(shakefreq), data = D[selection,])
# fitHFENplus = aov(HFENplus ~ brand + Error(shakefreq), data = D[selection,])

# print("EN, ENMO, MAD")
# print(summary(fitEN))
# print(summary(fitENMO))
# print(summary(fitMAD))
# print("MIMS")
# print(summary(fitMIMSunit))
# print(summary(fitMIMSlight))
# print("AI")
# print(summary(fitAI))

#=============================================================================
# Create plot per protocol session
# to ease visualisation we only look at average per shaker frequency per device (so, no epoch values): 
D$time = as.numeric(D$time) # convert to numeric to ease aggregation
D = aggregate(D, list(D$ses_name, D$shakefreq, D$brand, D$sn), FUN = mean)
colnames(D)[1:4] = c("ses_name", "shakefreq", "brand", "sn")
D = D[,c("brand","ses_name", "sn", "time", "EN", "MAD",
         "AI", "ENMO", "MIMSlight", "HFENplus", "HFEN", "BFEN", "shakefreq")]
D$time = as.POSIXlt(D$time, tz = "Europe/Amsterdam", origin = "1970-01-01")
# D = D[-which(D$brand == "Activpal"),]

CX = 0.7
pdf(file = paste0(shaker_experiments_folder, "/analyses/inspect_metrics.pdf"))
for (metric in c("MAD", "AI", "MIMSlight",  "HFEN", "BFEN")) { # "EN",  "ENMO",  "HFENplus", 
  par(mfrow = c(1,3))  
  YLIM = range(D[,metric], na.rm = TRUE)
  YLIM[2] = min(YLIM[2], 4)
  for (ses_name in sessionames) {
    GA = which(D$brand == "GENEActiv" & D$ses_name == ses_name)
    AX = which(D$brand == "Axivity" & D$ses_name == ses_name)
    AGC = which(D$brand == "ActigraphCLE" & D$ses_name == ses_name)
    AGM = which(D$brand == "ActigraphMOS" & D$ses_name == ses_name)
    AP = which(D$brand == "Activpal" & D$ses_name == ses_name)
    plot(D$time[GA], D[GA, metric], type = "p", pch = 20, cex = CX, main = ses_name, ylab = metric, 
         xlab = "time", ylim = YLIM)
    lines(D$time[AX], D[AX, metric], type = "p", pch = 20, col = "blue", cex = CX)
    lines(D$time[AGC], D[AGC, metric], type = "p", pch = 20, col = "red", cex = CX)
    lines(D$time[AGM], D[AGM, metric], type = "p",pch = 20, col = "green", cex = CX)
    lines(D$time[AP], D[AP, metric], type = "p", pch = 20, col = "purple", cex = CX)
    legend("topleft",legend = c("GENEActiv","Axivity","Actigraph_CLE","Actigraph_MOS", "Activpal"),
           col = c("black","blue","red", "green", "purple"), lty = 1) #, 
  }
}
dev.off()
