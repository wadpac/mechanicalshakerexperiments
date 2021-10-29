rm(list=ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# TO DO: Check which GENEActiv was removed towards end of one of the experiments, and make sure data is not included
epochsize=15

#====================================================================================
# Specify file paths
extracted_data_path = paste0(shaker_experiments_folder, "/structured_raw_data")
calib_files = paste0(shaker_experiments_folder, "/autocalibration_results")
non_wear_analyses_output = paste0(shaker_experiments_folder, "/non_wear_analyses")
if (!dir.exists(non_wear_analyses_output)) {
  dir.create(non_wear_analyses_output)
}
if (!dir.exists(calib_files)) {
  stop("\nNot able to find the autocalibeation_results folder.")
}
#======================================================================================
# Load relevant files, apply calibration correction coefficients, and apply metrics
fns = dir(extracted_data_path, full.names = TRUE)
outputfile = paste0(non_wear_analyses_output, "/explore_non_wear_metrics.RData")
sessionames =  c("pro2_ses1", "pro2_ses2", "pro2_ses3", "pro3_ses1", "pro3_ses2") # "pro3_ses3" #<= ignore protocol session 3 for now as this did not have flat orientation
overwrite= FALSE
averageperws3 = function(x,sf,epochsize) {
  x2 =cumsum(c(0,x))
  select = seq(1,length(x2),by=sf*epochsize)
  x3 = diff(x2[round(select)]) / abs(diff(round(select)))
}
if (!file.exists(outputfile) | overwrite == TRUE) {
  combineddata <- list()
  sn_ignored= c()
  cnt  = 1
  for (ses_name in sessionames) { #
    ses1 = grep(basename(fns), pattern = ses_name)
    for (fn in fns[ses1]) {
      cat("\n")
      print(fn)
      load(fn) # TO DO: This loads object extractedata => maybe rename this "structured_data"???
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
        tmp = extractedata$data[[i]] # tmp is the raw data now
        if (length(tmp) > 0) {
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
          time_gaps= 1
          while(length(time_gaps) > 0) {
            dt = diff(as.numeric(tmp$HEADER_TIME_STAMP))
            time_gaps = which(dt > 1)
            
            if (length(time_gaps) > 0) {
              cnt2 = 1
              tmp2 = tmp
              for (j in 1:length(time_gaps)) {
                inpute = as.data.frame(matrix(0, dt[time_gaps[j]]*sf, ncol(tmp)))
                colnames(inpute) = colnames(tmp)
                newtime = seq(tmp$HEADER_TIME_STAMP[time_gaps[j]], 
                              tmp$HEADER_TIME_STAMP[time_gaps[j]+1], by = 1/sf)
                inpute$HEADER_TIME_STAMP = newtime[1:nrow(inpute)]
                if (j == 1) {
                  cnt2 = time_gaps[j]-1
                } else {
                  cnt2 = nrow(tmp2)
                }
                cols_of_interest = c("X", "Y", "Z", "shaking_frequency")
                inpute[, cols_of_interest] = tmp[time_gaps[j], cols_of_interest]
                if (j != length(time_gaps)) {
                  tmp2 = rbind(tmp2[1:cnt2,], inpute)
                } else {
                  tmp2 = rbind(tmp2[1:cnt2,], inpute, tmp[(time_gaps[j]+1):nrow(tmp),])
                }
              }
              tmp = tmp2
            }
          }
          negativ_shakef = which(tmp$shaking_frequency == -1)
          if (length(negativ_shakef) > 0) {
            tmp$shaking_frequency[negativ_shakef] = -100000
          }
          shakefreq = averageperws3(x= tmp$shaking_frequency,sf,epochsize=epochsize)
          shakefreq[which(shakefreq < 0)] = -1
          tmp = tmp[,-which(colnames(tmp) == "shaking_frequency")]
          #-----------------------------------------------------------
          # load here previously derived calibration factors
          # if autocalibration was successful apply coefficients
          # if not skip file
          calib_fn = dir(calib_files, pattern = sn, full.names = T)
          sn_skip = FALSE
          if (length(calib_fn) > 0) {
            load(calib_fn)
            if (all(calib$scale != c(1,1,1)) & all(calib$scale != c(0,0,0))) {
              if (calib$C$cal.error.end < 0.01 & calib$C$npoints > 50) {
                tmp[,c("X","Y","Z")] = scale(tmp[,c("X","Y","Z")],
                                             center = -calib$offset,
                                             scale = 1/calib$scale)
                
                #===================================================================
                # Calculate metrics
                cat(". ")
                EN_raw = sqrt(tmp$X^2 + tmp$Y^2 + tmp$Z^2)
                EN_raw = EN_raw[1:(floor(nrow(tmp)/sf)*sf)]
                EN = averageperws3(EN_raw,sf,epochsize=epochsize)
                
                SDX = zoo::rollapply(data = tmp$X, width=sf*epochsize, by= sf*epochsize, FUN = sd, na.rm = TRUE)
                SDY = zoo::rollapply(data = tmp$Y, width=sf*epochsize, by= sf*epochsize,  FUN = sd, na.rm = TRUE)
                SDZ = zoo::rollapply(data = tmp$Z, width=sf*epochsize, by= sf*epochsize,  FUN = sd, na.rm = TRUE)
                
                TIMESTAMPS = tmp$HEADER_TIME_STAMP[seq(1,nrow(tmp), by = sf*epochsize)]
                
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
                EN  = checklen(EN, TIMESTAMPS, RR= 1)
                SDX  = checklen(SDX, TIMESTAMPS)
                SDY  = checklen(SDY, TIMESTAMPS)
                SDZ  = checklen(SDZ, TIMESTAMPS)
                shakefreq = checklen(shakefreq, TIMESTAMPS)
                
                S = data.frame(HEADER_TIME_STAMP = TIMESTAMPS,
                               brand = brand, sf = sf,
                               EN = EN, SDX = SDX, SDY = SDY, 
                               SDZ = SDZ, ses_name = ses_name,
                               sn = sn, DR = DR,shakefreq = shakefreq)
                negativ_shakef = which(S$shakefreq < 0)
                S = S[-negativ_shakef,]
                combineddata[[cnt]] = S # store result for later use
                cnt = cnt + 1
              } else {
               sn_skip = TRUE
              }
            } else {
              sn_skip = TRUE
            }
            
          } else {
            sn_skip = TRUE
          }
          if (sn_skip == TRUE) {
            cat(paste0(sn, " ignored ")) 
            sn_ignored = c(sn_ignored, paste0(sn,"_", brand,"_", ses_name))
          }
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
#====================================================================
# Stratify Actigraph by version:
MOS = grep(pattern = "MOS", x = DATA$sn)
CLE = grep(pattern = "CLE", x = DATA$sn)
DATA$brand[MOS] = "ActigraphMOS"
DATA$brand[CLE] = "ActigraphCLE"

#========================================================================================
# Aggregate epochs to one value per shaker frequency x serial number combination.
D = aggregate(x = DATA[,c("EN", "SDX", "SDY", "SDZ", "shakefreq")],
              by = list(DATA$HEADER_TIME_STAMP, DATA$brand, DATA$ses_name, DATA$sn, DATA$sf, DATA$DR), FUN = mean)
colnames(D)[1:6] = c("time", "brand", "ses_name", "sn", "sf", "DR")
# Remove epochs with rare frequencies, these are the frequencies
# that result from the frequency transitions
Dnew = c()
for (ses_name in c("pro2_ses1", "pro2_ses2", "pro2_ses3", "pro3_ses1", "pro3_ses2")) { #"pro3_ses3" ,
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

D$SDX = D$SDX * 1000
D$SDY = D$SDY * 1000
D$SDZ = D$SDZ * 1000

NN = 100
compdf = data.frame(median_sd_x=numeric(NN),
                    median_sd_y=numeric(NN),
                    median_sd_z=numeric(NN),
                    mean_sd_x=numeric(NN),
                    mean_sd_y=numeric(NN),
                    mean_sd_z=numeric(NN),
                    p95_sd_x=numeric(NN),
                    p95_sd_y=numeric(NN),
                    p95_sd_z=numeric(NN),
                    sd_sdx=numeric(NN),
                    sd_sdy=numeric(NN),
                    sd_sdz=numeric(NN),
                    N=numeric(NN),
                    sf=numeric(NN),
                    dynrange=numeric(NN),
                    brand=character(NN))
cnt = 1
for (brand in c("GENEActiv","Axivity","ActigraphCLE","ActigraphMOS", "Activpal")) {
  for (dynrange in unique(D$DR)) {
    for (samfre in unique(D$sf)) {
      selection = which(D$brand==brand & D$shakefreq==0 & D$sf == samfre & D$DR == dynrange)
      mx = median(D$SDX[selection], na.rm = T)
      if (!is.na(mx)) {
        compdf$median_sd_x[cnt] = round(median(D$SDX[selection], na.rm = T), digits = 2)
        compdf$median_sd_y[cnt] = round(median(D$SDY[selection], na.rm = T), digits = 2)
        compdf$median_sd_z[cnt] = round(median(D$SDZ[selection], na.rm = T), digits = 2)
        compdf$mean_sd_x[cnt] = round(mean(D$SDX[selection], na.rm = T), digits = 2)
        compdf$mean_sd_y[cnt] = round(mean(D$SDY[selection], na.rm = T), digits = 2)
        compdf$mean_sd_z[cnt] = round(mean(D$SDZ[selection], na.rm = T), digits = 2)
        compdf$p95_sd_x[cnt] = round(quantile(D$SDX[selection], probs = 0.95, na.rm = T), digits = 2)
        compdf$p95_sd_y[cnt] = round(quantile(D$SDY[selection], probs = 0.95, na.rm = T), digits = 2)
        compdf$p95_sd_z[cnt] = round(quantile(D$SDZ[selection], probs = 0.95, na.rm = T), digits = 2)
        compdf$sd_sdx[cnt] = round(sd(D$SDX[selection], na.rm = T), digits = 2)
        compdf$sd_sdy[cnt] = round(sd(D$SDY[selection], na.rm = T), digits = 2)
        compdf$sd_sdz[cnt] = round(sd(D$SDZ[selection], na.rm = T), digits = 2)
        compdf$dynrange[cnt] = dynrange
        compdf$sf[cnt] = samfre
        compdf$brand[cnt] = brand
        compdf$N[cnt] = length(selection)
        cnt=cnt+1
      }
    }
  }
}
compdf = compdf[-c(cnt:nrow(compdf)),]
compdf = compdf[order(compdf$brand,compdf$sf),]
write.csv(x = compdf, file = paste0("~/data/VUMC/shaker_experiments/non_wear_analyses/stdev_epoch",
                                    epochsize, "sec_nonwear.csv"), row.names = FALSE)
kkkk
#=============================================================================
# Assess whether metrics differ across brands with repeated measures ANOVA
# brands_of_interest = c("GENEActiv", "Axivity", "ActigraphCLE", "ActigraphMOS", "Activpal")
# selection = which(D$ses_name=="pro2_ses2" & D$brand %in% brands_of_interest)
# fitEN = aov(EN ~ brand + Error(shakefreq), data=D[selection,])
# fitENMO = aov(ENMO ~ brand + Error(shakefreq),data=D[selection,])
# fitMAD = aov(ENMO ~ brand + Error(shakefreq),data=D[selection,])
# fitMIMSlight = aov(MIMSlight ~ brand + Error(shakefreq), data=D[selection,])
# fitAI = aov(AI ~ brand + Error(shakefreq), data=D[selection,])
# fitBFEN = aov(BFEN ~ brand + Error(shakefreq), data=D[selection,])
# fitHFEN = aov(HFEN ~ brand + Error(shakefreq), data=D[selection,])
# fitHFENplus = aov(HFENplus ~ brand + Error(shakefreq), data=D[selection,])

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
D$time=as.numeric(D$time) # convert to numeric to ease aggregation
D = aggregate(D, list(D$ses_name, D$shakefreq, D$brand, D$sn), FUN = mean)
colnames(D)[1:5] = c("ses_name", "shakefreq", "brand", "sn", "sf")
D = D[,c("brand","ses_name", "sn", "time", "EN", "SDX", "SDY", "SDZ", "shakefreq","sf")]
D$time=as.POSIXlt(D$time, tz = "Europe/Amsterdam", origin ="1970-01-01")
# D = D[-which(D$brand == "Activpal"),]
# D = D[which(D$shakefreq == 0),]

CX = 0.7
pdf(file = "~/data/VUMC/shaker_experiments/non_wear_analyses/inspect_metrics.pdf")
for (metric in c("EN", "SDX", "SDY", "SDZ")) { #
  par(mfrow=c(1,3))  
  YLIM = range(D[,metric], na.rm=TRUE)
  YLIM[2] = min(YLIM[2], 4)
  for (ses_name in sessionames) {
    GA = which(D$brand == "GENEActiv" & D$ses_name == ses_name)
    AX = which(D$brand == "Axivity" & D$ses_name == ses_name)
    AGC = which(D$brand == "ActigraphCLE" & D$ses_name == ses_name)
    AGM = which(D$brand == "ActigraphMOS" & D$ses_name == ses_name)
    AP = which(D$brand == "Activpal" & D$ses_name == ses_name)
    plot(D$time[GA], D[GA, metric], type="p", pch=20, cex=CX, main= ses_name, ylab=metric, 
         xlab="time", ylim=YLIM)
    lines(D$time[AX], D[AX, metric], type="p", pch=20, col="blue", cex=CX)
    lines(D$time[AGC], D[AGC, metric], type="p", pch=20, col="red", cex=CX)
    lines(D$time[AGM], D[AGM, metric], type="p",pch=20, col="green", cex=CX)
    lines(D$time[AP], D[AP, metric], type="p", pch=20, col="purple", cex=CX)
    legend("topleft",legend = c("GENEActiv","Axivity","Actigraph_CLE","Actigraph_MOS", "Activpal"),
           col = c("black","blue","red", "green", "purple"), lty=1) #, 
  }
}
dev.off()

