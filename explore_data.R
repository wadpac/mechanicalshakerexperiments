rm(list=ls())
graphics.off()

#-----------------------------------
# libraries needed for running SummarizedActigraph)
# library(SummarizedActigraphy)
library(tidyverse)
library(lubridate)
library(lme4)
library(knitr)
remotes::install_github("muschellij2/SummarizedActigraphy")
library(SummarizedActigraphy)
library(MIMSunit)
library(activityCounts)
options(digits.secs = 3)
options(scipen=999)
#-----------------------------------

extracted_data_path = "~/data/VUMC/shaker_experiments/extracteddata"

# Exploration of MIMSunit and how it compares across accelerometer brands
library("MIMSunit")
# identify subset of files relevant to experimental session
fns = dir(extracted_data_path, full.names = TRUE)
outputfile = "~/data/VUMC/shaker_experiments/explore_MIMS.RData"
sessionames = c("pro2_ses1", "pro2_ses2", "pro2_ses3") #"pro3_ses3" #<= ignore protocol session 3 for now as this did not have flat orientation
if (!file.exists(outputfile)) {
  combineddata <- list()
  cnt  = 1
  for (ses_name in sessionames) { #
    ses1 = grep(basename(fns), pattern = ses_name)
    pdf(file = paste0("~/data/VUMC/shaker_experiments/explore_MIMS",ses_name,".pdf"))
    for (fn in fns[ses1]) {
      print(fn)
      load(fn)
      if (length(grep(fn, pattern = "Actigraph")) > 0) {
        brand = "Actigraph"
      } else if (length(grep(fn, pattern = "Axivity")) > 0) {
        brand = "Axivity"
      } else if (length(grep(fn, pattern = "GENEActiv")) > 0) {
        brand = "GENEActiv"
      }
      for (i in 1:length(extractedata$data)) {
        tmp = extractedata$data[[i]]
        if (length(tmp) > 0) {
          # apply aggregation function
          # check that this goes well for Axivity AX6
          DR = as.numeric(extractedata$specifications[i, "dynamic_range"])
          sf = as.numeric(extractedata$specifications[i, "sampling_frequency"])
          sn = as.character(extractedata$specifications[i, "serial_number"])
          tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
          if (brand == "Actigraph") {
            tmp$time = as.POSIXct(tmp$time)
            tmp = tmp[, c("time","X","Y","Z")]
          } else if (brand == "Axivity" | brand == "GENEActiv") {
            tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
            tmp = tmp[, c("time","x","y","z")]
            colnames(tmp)[2:4] = c("X", "Y", "Z")
          }
          row.names(tmp) = 1:nrow(tmp)
          colnames(tmp)[1] = "HEADER_TIME_STAMP"
          #-----------------------------------------
          # fill time gaps for Actigraph
          time_gaps= 1
          while(length(time_gaps) > 0) {
            dt = diff(as.numeric(tmp$HEADER_TIME_STAMP))
            time_gaps = which(dt > 1)
            if (length(time_gaps) > 0) {
              cnt = 1
              tmp2 = tmp
              for (j in 1:length(time_gaps)) {
                inpute = as.data.frame(matrix(0, dt[time_gaps[j]]*sf, ncol(tmp)))
                colnames(inpute) = colnames(tmp)
                newtime = seq(tmp$HEADER_TIME_STAMP[time_gaps[j]], 
                              tmp$HEADER_TIME_STAMP[time_gaps[j]+1], by = 1/sf)
                inpute$HEADER_TIME_STAMP = newtime[1:nrow(inpute)]
                if (j == 1) {
                  cnt = time_gaps[j]-1
                } else {
                  cnt = nrow(tmp2)
                }
                inpute[, c("X", "Y", "Z")] = tmp[time_gaps[j], c("X", "Y", "Z")]
                if (j != length(time_gaps)) {
                  tmp2 = rbind(tmp2[1:cnt,], inpute)
                } else {
                  tmp2 = rbind(tmp2[1:cnt,], inpute, tmp[(time_gaps[j]+1):nrow(tmp),])
                }
              }
              tmp = tmp2
            }
          }
          #-----------------------------------------------------------
          # offset calibration, based on first 90 seconds
          MX = mean(tmp$X[1:(90*sf)])
          MY = mean(tmp$Y[1:(90*sf)])
          MZ = mean(tmp$Z[1:(90*sf)])
          tmp$X = (tmp$X - MX) + round(MX)
          tmp$Y = (tmp$Y - MY) + round(MY)
          tmp$Z = (tmp$Z - MZ) + round(MZ)
         
          
          
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
          #
          # 
          # kkkkk
          #=======
          x0 = Sys.time()
          S = MIMSunit::mims_unit(df = tmp, epoch = '5 sec', dynamic_range = c(-DR, DR), output_mims_per_axis = TRUE)
          x1 = Sys.time()
          
          # implement simplified and faster mims:
          epochsize = 5
          averageperepoch = function(x,sf,epochsize) {
            x2 =cumsum(c(0,x))
            select = seq(1,length(x2),by=sf*epochsize)
            x3 = diff(x2[round(select)]) / abs(diff(round(select)))
          }
          bf_filter = function(lb, hb, n, sf) {
            Wc = matrix(0,2,1)
            Wc[1,1] = lb / (sf/2)
            Wc[2,1] = hb / (sf/2)
            return(signal::butter(n,Wc,type=c("pass")))
          }
          mymims = matrix(NA, floor(nrow(tmp) / (sf*5)), 3)
          for (ki in 2:4) {
            coef = bf_filter(lb=0.2, hb=5, n=4, sf=sf)
            filtered_signal = abs(signal::filter(coef, tmp[,ki]))
            mymims[,ki-1] = averageperepoch(filtered_signal, sf, epochsize) * epochsize
          }
          x2 = Sys.time()
          
          MIMS_BFEN = rowSums(mymims)
          
          if (nrow(S) > nrow(mymims)) {
            S = S[1:nrow(mymims),]
          }
          # 
          # dx = mymims[,1]-S$MIMS_UNIT_X
          # dy = mymims[,2]-S$MIMS_UNIT_Y
          # dz = mymims[,3]-S$MIMS_UNIT_Z
          # x11()
          # par(mfrow=c(2,1))
          # plot(S$MIMS_UNIT, MIMS-S$MIMS_UNIT, type="p", pch=20)
          # plot(S$MIMS_UNIT, ((MIMS-S$MIMS_UNIT)/S$MIMS_UNIT)*100, type="p", pch=20)
          # 
          # x11()
          # plot(S$MIMS_UNIT, MIMS, type="p", pch=20)
          # 
          # x11()
          # par(mfrow=c(4,1))
          # plot(mymims[,1], type="l", ylim=range(mymims), main="MIMS based on BFEN metric in GGIR with setting: lb=0.2, hb=5, n=5, multiplied by epoch length")
          # lines(mymims[,2], type="l", col="red")
          # lines(mymims[,3], type="l", col="green")
          # plot(S$MIMS_UNIT_X, type="l", ylim=range(S[,3:5]), main= "MIMS from MIMSunit package")
          # lines(S$MIMS_UNIT_Y, type="l", col="red")
          # lines(S$MIMS_UNIT_Z, type="l", col="green")
          # plot(dx, type="l", ylim=c(-0.1, 0.1), main="Difference")
          # lines(dy, type="l", col="red")
          # lines(dz, type="l", col="green")
          # plot((dx / S$MIMS_UNIT_X)*100, type="l", ylim=c(-5, 5), main="Difference")
          # 
          # print(difftime(x1, x0))
          # print(difftime(x2, x1))
          # 
          # kkkk
          # calculate EN to check data alignment
          averageperws3 = function(x,sf,ws3) {
            x2 =cumsum(c(0,x))
            select = seq(1,length(x2),by=sf*epochsize)
            x3 = diff(x2[round(select)]) / abs(diff(round(select)))
          }
          EN_raw = sqrt(tmp$X^2 + tmp$Y^2 + tmp$Z^2)
          EN_raw = EN_raw[1:(floor(nrow(tmp)/sf)*sf)]
          EN = averageperws3(EN_raw,sf,ws3=5)
          ENMOraw = pmax(EN_raw - 1, 0)
          ENMO = averageperws3(ENMOraw,sf,ws3=5)
          MEANS = rep(EN, each = sf*5)
          MAD = abs(EN_raw - MEANS)
          MAD = averageperws3(x=MAD,sf,ws3=5)
          AI = out_i$AI
          
          checklen = function(x, y, RR=0) {
            if (length(x) > nrow(y)) {
              x = x[1:nrow(y)]
            } else if (length(x) < nrow(y)) {
              if ((nrow(y) - length(x)) == 1) {
                x = c(x, rep(RR, nrow(y) - length(x)))
              } else {
                stop()
              }
            }
            return(x)
          }
          EN  = checklen(EN, S, RR= 1)
          AI  = checklen(AI, S)
          MAD  = checklen(MAD, S)
          ENMO  = checklen(ENMO, S)
          MIMS_BFEN = checklen(MIMS_BFEN, S)
          S$brand = brand
          S$sf = sf
          S$EN = EN
          S$ENMO = ENMO
          S$MAD = MAD
          S$AI = AI
          S$MIMS_BFEN = MIMS_BFEN
          S$ses_name = ses_name
          S$sn = sn
          combineddata[[cnt]] = S # store result for later use
          # visually compare values across brands
          par(mfrow=c(2,1))
          plot(S$HEADER_TIME_STAMP, S$MIMS_UNIT, type="l", main=paste0(brand," ",sf," ",DR)) #, ylim=c(0,6))
          plot(S$HEADER_TIME_STAMP, S$EN, type="l", col="black")
          cnt = cnt + 1
        }
      }
    }
    dev.off()
  }
  save(combineddata, file = outputfile)
} else {
  load(file = outputfile)
}

# combine into data.frame
DATA = do.call("rbind", combineddata)

# stratify Actigraph by version:


MOS = grep(pattern = "MOS", x = DATA$sn)
CLE = grep(pattern = "CLE", x = DATA$sn)
DATA$brand[MOS] = "ActigraphMOS"
DATA$brand[CLE] = "ActigraphCLE"


# aggregate
D2 = aggregate(x = DATA[,c("MIMS_UNIT", "EN", "MAD", "AI", "ENMO", "MIMS_BFEN")],by = list(DATA$HEADER_TIME_STAMP, DATA$brand, DATA$ses_name), FUN = median)
myq1 = function(x) as.numeric(quantile(x = x, probs=0.25, na.rm= TRUE))
myq3 = function(x) as.numeric(quantile(x, probs=0.75, na.rm= TRUE))
D1 = aggregate(x = DATA[,c("MIMS_UNIT", "EN", "MAD", "AI", "ENMO", "MIMS_BFEN")],by = list(DATA$HEADER_TIME_STAMP, DATA$brand, DATA$ses_name), FUN = myq1)
D3 = aggregate(x = DATA[,c("MIMS_UNIT", "EN", "MAD", "AI", "ENMO", "MIMS_BFEN")],by = list(DATA$HEADER_TIME_STAMP, DATA$brand, DATA$ses_name), FUN = myq3)
tidyupname = function(x) {
  # x = x[,-which(colnames(x) == "brand")]
  colnames(x)[1:8] = c("time", "brand", "ses_name", "MIMS_UNIT", "EN", "MAD", "AI", "ENMO", "MIMS_BFEN")
  return(x)
}
D1 = tidyupname(D1)
D2 = tidyupname(D2)
D3 = tidyupname(D3)
D = merge(D1, D3, by = c("time", "brand", "ses_name"), suffixes = c("q1","q3"))
D = merge(D, D2, by = c("time", "brand", "ses_name"))

# create plot per session
pdf(file = "~/data/VUMC/shaker_experiments/inspect_metrics.pdf")

for (metric in c("MIMS_UNIT", "EN", "MAD", "AI", "ENMO", "MIMS_BFEN")) { #
  par(mfrow=c(1,3))  
  for (ses_name in c("pro2_ses1", "pro2_ses2", "pro2_ses3")) { #"pro3_ses3" ,
    GA = which(D$brand == "GENEActiv" & D$ses_name == ses_name)
    AX = which(D$brand == "Axivity" & D$ses_name == ses_name)
    AGC = which(D$brand == "ActigraphCLE" & D$ses_name == ses_name)
    AGM = which(D$brand == "ActigraphMOS" & D$ses_name == ses_name)
    plot(D$time[GA], D[GA, metric], type="l", lwd=1.5, main= ses_name, ylab=metric, 
         xlab="time", ylim=range(D[,metric], na.rm=TRUE))
    # lines(D$time[GA], D[GA, paste0(metric,"q1")], type="l", lty=2)
    # lines(D$time[GA], D[GA, paste0(metric,"q3")], type="l", lty=2)
    lines(D$time[AX], D[AX, metric], type="l", col="blue", lwd=1.5)
    # lines(D$time[AX], D[AX, paste0(metric,"q1")], type="l", lty=2, col="blue")
    # lines(D$time[AX], D[AX, paste0(metric,"q3")], type="l", lty=2, col="blue")
    lines(D$time[AGC], D[AGC, metric], type="l", col="red", lwd=1.5)
    # lines(D$time[AGC], D[AGC, paste0(metric,"q1")], type="l", lty=2, col="red")
    # lines(D$time[AGC], D[AGC, paste0(metric,"q3")], type="l", lty=2, col="red")
    lines(D$time[AGM], D[AGM, metric], type="l", col="green", lwd=1.5)
    # lines(D$time[AGM], D[AGM, paste0(metric,"q1")], type="l", lty=2, col="green")
    # lines(D$time[AGM], D[AGM, paste0(metric,"q3")], type="l", lty=2, col="green")
    legend("topleft",legend = c("GENEActiv","Axivity","Actigraph_CLE","Actigraph_MOS"), col = c("black","blue","red", "green"), lty=1)
  }
}