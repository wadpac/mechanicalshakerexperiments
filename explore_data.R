rm(list=ls())
graphics.off()

shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

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
options(digits.secs = 7)
options(scipen=999)
# Exploration of MIMSunit and how it compares across accelerometer brands
library("MIMSunit")

#-----------------------------------

extracted_data_path = paste0(shaker_experiments_folder, "/labelled_data")

# identify subset of files relevant to experimental session
fns = dir(extracted_data_path, full.names = TRUE)
outputfile = paste0(shaker_experiments_folder, "/analyses/explore_MIMS.RData")
sessionames = c("pro2_ses1", "pro2_ses2", "pro2_ses3") # "pro3_ses3" #<= ignore protocol session 3 for now as this did not have flat orientation
overwrite= TRUE
epochsize = 5
averageperws3 = function(x,sf,epochsize) {
  x2 =cumsum(c(0,x))
  select = seq(1,length(x2),by=sf*epochsize)
  x3 = diff(x2[round(select)]) / abs(diff(round(select)))
}
if (!file.exists(outputfile) | overwrite == TRUE) {
  combineddata <- list()
  cnt  = 1
  for (ses_name in sessionames) { #
    ses1 = grep(basename(fns), pattern = ses_name)
    pdf(file = paste0(shaker_experiments_folder, "/analyses/explore_MIMS",ses_name,".pdf"))
    for (fn in fns[ses1]) {
      print(fn)
      load(fn)
      if (length(grep(fn, pattern = "Actigraph")) > 0) {
        brand = "Actigraph"
      } else if (length(grep(fn, pattern = "Axivity")) > 0) {
        brand = "Axivity"
      } else if (length(grep(fn, pattern = "GENEActiv")) > 0) {
        brand = "GENEActiv"
      } else if (length(grep(fn, pattern = "Activpal")) > 0) {
        brand = "Activpal"
      }
      for (i in 1:length(extractedata$data)) {
        tmp = extractedata$data[[i]]
        if (length(tmp) > 0) {
          # apply aggregation function
          # check that this goes well for Axivity AX6
          DR = as.numeric(extractedata$specifications[i, "dynamic_range"])
          sf = as.numeric(extractedata$specifications[i, "sampling_frequency"])
          sn = as.character(extractedata$specifications[i, "serial_number"])
          if (brand == "Actigraph" | brand == "Activpal") {
            tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
            tmp = tmp[, c("time","X","Y","Z","shaking_frequency")] #,"shaking_frequency"
          } else if (brand == "Axivity" | brand == "GENEActiv" ) {
            tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
            tmp = tmp[, c("time","x","y","z","shaking_frequency")] #
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
                cols_of_interest = c("X", "Y", "Z", "shaking_frequency")
                inpute[, cols_of_interest] = tmp[time_gaps[j], cols_of_interest]
                if (j != length(time_gaps)) {
                  tmp2 = rbind(tmp2[1:cnt,], inpute)
                } else {
                  tmp2 = rbind(tmp2[1:cnt,], inpute, tmp[(time_gaps[j]+1):nrow(tmp),])
                }
              }
              tmp = tmp2
            }
          }
          negativ_shakef = which(tmp$shaking_frequency == -1)
          if (length(negativ_shakef) > 0) {
            tmp$shaking_frequency[negativ_shakef] = -100000
          }
          shakefreq = averageperws3(x= tmp$shaking_frequency,sf,epochsize=5)
          shakefreq[which(shakefreq < 0)] = -1
          tmp = tmp[,-which(colnames(tmp) == "shaking_frequency")]
          #-----------------------------------------------------------
          # load here previously derived calibration factors
          # if autocalibrationw as succesful apply coefficients
          # if not skip file
          
          
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
          
          x0 = Sys.time()
          S = MIMSunit::mims_unit(df = tmp, epoch = '5 sec', dynamic_range = c(-DR, DR), output_mims_per_axis = TRUE)
          x1 = Sys.time()

          #=======
          # implement simplified and faster mims:
          MIMSlight_fun = function(df, epochsize=5, acc_columns = 2:4, range=8) {
            averageperepoch = function(x,sf,epochsize) { 
              # I am aware that this is numerically unstable, but in our use-case
              # expected drop in precision is far beyond required precision
              # and by that acceptable.
              x2 =cumsum(c(0,x))
              select = seq(1,length(x2),by=sf*epochsize)
              x3 = diff(x2[round(select)]) / abs(diff(round(select)))
            }
            bf_filter = function(lb, hb, n, sf) {
              hb = ifelse(test = sf <= (hb*2), yes = (sf / 2) - 0.1, no = hb)
              Wc = matrix(0,2,1)
              Wc[1,1] = lb / (sf/2) # lb: lower boundary
              Wc[2,1] = hb / (sf/2) # hb: higher boundary
              return(signal::butter(n,Wc,type=c("pass"))) 
            }
            mims_per_axis = matrix(NA, floor(nrow(df) / (sf*epochsize)), 3)
            # df = MIMSunit::extrapolate(df, range=range)
            for (i in acc_columns) {
              coef = bf_filter(lb=0.2, hb=5, n=4, sf=sf)
              filtered_signal = signal::filter(coef, df[,i])
              # using this way of aggregating is a lot faster, but comes at the price of
              # new value differences with the original approach, if I insert the orignal approach
              # value differences are zero for the full range, expect close to zero,
              # which relates to interpolation step we are not doing
              BFSM_per_axis = averageperepoch(abs(filtered_signal), sf, epochsize)
              noise = which(BFSM_per_axis < 0.01) # MIMSunit gives zeros when sensor not moving, so we do same
              if (length(noise) > 0) BFSM_per_axis[noise] = 0
              mims_per_axis[,i-1] = BFSM_per_axis * epochsize
            }
            MIMSlight = rowSums(mims_per_axis)
            return(MIMSlight)
          }
          MIMSlight = MIMSlight_fun(df=tmp, epochsize=5, acc_columns = 2:4, range = 8)
          
          if (nrow(S) > length(MIMSlight)) {
            S = S[1:length(MIMSlight),]
          }
          
          # calculate EN to check data alignment
          
          EN_raw = sqrt(tmp$X^2 + tmp$Y^2 + tmp$Z^2)
          EN_raw = EN_raw[1:(floor(nrow(tmp)/sf)*sf)]
          EN = averageperws3(EN_raw,sf,epochsize=5)
          ENMOraw = pmax(EN_raw - 1, 0)
          ENMO = averageperws3(ENMOraw,sf,epochsize=5)
          MEANS = rep(EN, each = sf*5)
          MAD = abs(EN_raw[1:length(MEANS)] - MEANS)
          MAD = averageperws3(x=MAD,sf,epochsize=5)
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
          MIMSlight = checklen(MIMSlight, S)
          shakefreq = checklen(shakefreq, S)
          S$brand = brand
          S$sf = sf
          S$EN = EN
          S$ENMO = ENMO
          S$MAD = MAD
          S$AI = AI
          S$MIMSlight = MIMSlight
          S$ses_name = ses_name
          S$sn = sn
          S$shakefreq = shakefreq
          negativ_shakef = which(S$shakefreq < 0)
          S = S[-negativ_shakef,]
          
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
D2 = aggregate(x = DATA[,c("MIMS_UNIT", "EN", "MAD", "AI", "ENMO", "MIMSlight", "shakefreq")],
               by = list(DATA$HEADER_TIME_STAMP, DATA$brand, DATA$ses_name, DATA$sn), FUN = mean)
tidyupname = function(x) {
  colnames(x)[1:11] = c("time", "brand", "ses_name", "sn", "MIMS_UNIT",
                       "EN", "MAD", "AI", "ENMO", "MIMSlight", "shakefreq")
  return(x)
}
D = tidyupname(D2)


Dnew = c()
# remove rare frequencies (bordering with transition period)
for (ses_name in c("pro2_ses1", "pro2_ses2", "pro2_ses3")) { #"pro3_ses3" ,
  Dtmp = D[which(D$ses_name == ses_name),]
  FT = table(Dtmp$shakefreq)
  conditions_to_exclude = names(FT[which(as.numeric(FT)< 12)])
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
# aggregate per shaker frequency
D$time=as.numeric(D$time)
D = aggregate(D, list(D$ses_name, D$shakefreq, D$brand, D$sn), FUN = mean)
colnames(D)[1:4] = c("ses_name", "shakefreq", "brand", "sn")
D = D[,c("brand","ses_name", "sn", "time", "MIMS_UNIT", "EN", "MAD",
         "AI", "ENMO", "MIMSlight", "shakefreq",  "brand")]

D$time=as.POSIXlt(D$time, tz = "Europe/Amsterdam", origin ="1970-01-01")

D = D[-which(D$brand == "Activpal"),]

# repeated measures ANOVA
fitEN = summary(aov(EN ~ brand + Error(shakefreq), data=D[which(D$ses_name=="pro2_ses2"),]))
fitENMO = summary(aov(ENMO ~ brand + Error(shakefreq), data=D[which(D$ses_name=="pro2_ses2"),]))
fitMIMSunit = summary(aov(MIMS_UNIT ~ brand + Error(shakefreq), data=D[which(D$ses_name=="pro2_ses2"),]))
fitMIMSlight = summary(aov(MIMSlight ~ brand + Error(shakefreq), data=D[which(D$ses_name=="pro2_ses2"),]))
fitAI = summary(aov(AI ~ brand + Error(shakefreq), data=D[which(D$ses_name=="pro2_ses2"),]))

# create plot per session
pdf(file = "~/data/VUMC/shaker_experiments/inspect_metrics.pdf")

for (metric in c("MIMS_UNIT", "EN", "MAD", "AI", "ENMO", "MIMSlight")) { #
  par(mfrow=c(1,3))  
  YLIM = range(D[,metric], na.rm=TRUE)
  YLIM[2] = min(YLIM[2], 4)
  for (ses_name in c("pro2_ses1", "pro2_ses2", "pro2_ses3")) { #"pro3_ses3" ,
    GA = which(D$brand == "GENEActiv" & D$ses_name == ses_name)
    AX = which(D$brand == "Axivity" & D$ses_name == ses_name)
    AGC = which(D$brand == "ActigraphCLE" & D$ses_name == ses_name)
    AGM = which(D$brand == "ActigraphMOS" & D$ses_name == ses_name)
    # AP = which(D$brand == "Activpal" & D$ses_name == ses_name)
    plot(D$time[GA], D[GA, metric], type="p",pch=20, lwd=1.5, main= ses_name, ylab=metric, 
         xlab="time", ylim=YLIM)
    lines(D$time[AX], D[AX, metric], type="p",pch=20, col="blue", lwd=1.5)
    lines(D$time[AGC], D[AGC, metric], type="p",pch=20, col="red", lwd=1.5)
    lines(D$time[AGM], D[AGM, metric], type="p",pch=20, col="green", lwd=1.5)
    # lines(D$time[AP], D[AP, metric], type="l", col="purple", lwd=1.5)
    legend("topleft",legend = c("GENEActiv","Axivity","Actigraph_CLE","Actigraph_MOS"), #, "Activpal"),
           col = c("black","blue","red", "green"), lty=1) #, "purple"
  }
}
dev.off()