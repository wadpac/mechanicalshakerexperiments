rm(list = ls())
graphics.off()

# Update to be your local directory, if all goes well this is the only line you will have to update
#shaker_experiments_folder = "/media/vincent/DATA/VUMC/shaker_experiments"
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
#========================================================================================
# Most of these libraries are needed for running SummarizedActigraph and/or MIMSunit
options(digits.secs = 7)
options(scipen = 999)

#====================================================================================
# Specify file paths
extracted_data_path = paste0(shaker_experiments_folder, "/structured_raw_data")

#======================================================================================
# Load relevant files, apply calibration correction coefficients, and apply metrics
fns = dir(extracted_data_path, full.names = TRUE)
outdir = paste0(shaker_experiments_folder, "/sampling_rate_analyses")
if (!dir.exists(outdir)) dir.create(outdir)
outputfile = paste0(shaker_experiments_folder, "/sampling_rate_analyses/explore_samplerate.RData")
sessionnames = c("ms_mfcr") #"ms_hfcr", "ms_lfcr", "ms_hfmr", "ms_lfmr",
overwrite = FALSE
epochsize = 5
averageperws3 = function(x,sf,epochsize) {
  x2 = cumsum(c(0, x))
  select = seq(1, length(x2), by = sf * epochsize)
  x3 = diff(x2[round(select)]) / abs(diff(round(select)))
}
if (!file.exists(outputfile) | overwrite == TRUE) {
  combineddata <- list()
  sn_ignored = c()
  cnt  = 1
  for (ses_name in sessionnames) { #
    ses1 = grep(basename(fns), pattern = ses_name)
    # pdf(file = paste0(shaker_experiments_folder, "/metric_analyses/explore_metrics",ses_name,".pdf"))
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
      } else if (length(grep(fn, pattern = "MOX")) > 0) {
        brand = "MOX"
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
          } else if (brand == "Axivity" | brand == "GENEActiv" ) {
            tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz = "Europe/Amsterdam")
            tmp = tmp[, c("time","x","y","z","shaking_frequency")] #
            colnames(tmp)[2:4] = c("X", "Y", "Z")
          }
          colnames(tmp) = tolower(colnames(tmp))
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
                              tmp$HEADER_TIME_STAMP[time_gaps[j] + 1], by = 1 /
                                sf)
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
                  tmp2 = rbind(tmp2[1:cnt2, ], inpute, tmp[(time_gaps[j] + 1):nrow(tmp), ])
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
          
          #===================================================================
          # Calculate metrics
          if (brand == "MOX") {
            domAx = "y"
          } else {
            domAx = "x"
          }
          
          # calculate stdev over dominant axis
          # absolute relative difference
          stdev = abs(tmp[, domAx]) # mean oabs will equal sd
          stdev = averageperws3(stdev, sf, epochsize = 5)
          TIMESTAMPS = tmp$HEADER_TIME_STAMP[seq(1, nrow(tmp), by = sf * 5)]
          #===================================================================
          # Standardise size of objects and put in single data.frame
          checklen = function(x, y, RR = 0) {
            # x is acc metric vector, y is timestamp vector
            if (length(x) > length(y)) {
              x = x[1:length(y)]
            } else if (length(x) < length(y)) {
              x = c(x, rep(RR, length(y) - length(x)))
            }
            return(x)
          }
          stdev  = checklen(stdev, TIMESTAMPS, RR = 0)
          shakefreq = checklen(shakefreq, TIMESTAMPS)
          S = data.frame(HEADER_TIME_STAMP = TIMESTAMPS,
                         brand = brand, sf = sf,
                         stdev = stdev, ses_name = ses_name,
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
#====================================================================
# Stratify Actigraph by version:
MOS = grep(pattern = "MOS", x = DATA$sn)
CLE = grep(pattern = "CLE", x = DATA$sn)
DATA$brand[MOS] = "ActigraphMOS"
DATA$brand[CLE] = "ActigraphCLE"

#========================================================================================
# Aggregate epochs to one value per shaker frequency x serial number combination.
D = aggregate(x = DATA[,c("stdev", "shakefreq")],
              by = list(DATA$HEADER_TIME_STAMP, DATA$brand, DATA$ses_name, DATA$sn, DATA$sf), FUN = mean)
colnames(D)[1:5] = c("time", "brand", "ses_name", "sn", "sf")
# Remove epochs with rare frequencies, these are the frequencies
# that result from the frequency transitions
Dnew = c()
for (ses_name in sessionnames) { #"pro3_ses3" ,
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

# ignore all sample frequencies above 100
D = D[-which(D$sf > 100), ]

#=============================================================================
# Assess whether metrics differ across brands with repeated measures ANOVA
brands_of_interest = c("GENEActiv",
                       "Axivity",
                       "ActigraphCLE",
                       "ActigraphMOS",
                       "Activpal", 
                       "MOX")
selection = which(D$ses_name == "ms_mfcr" &
                    D$brand %in% brands_of_interest)

#=============================================================================
# Create plot per protocol session
# to ease visualisation we only look at average per shaker frequency per device (so, no epoch values): 
D$time = as.numeric(D$time) # convert to numeric to ease aggregation
D = aggregate(D, list(D$ses_name, D$shakefreq, D$brand, D$sn, D$sf), FUN = mean)
colnames(D)[1:5] = c("ses_name", "shakefreq", "brand", "sn", "sf")
D = D[,c("brand","ses_name", "sn", "time", "sf", "stdev", "shakefreq")]
D$time = as.POSIXlt(D$time, tz = "Europe/Amsterdam", origin = "1970-01-01")
# D = D[-which(D$brand == "Activpal"),]

D = D[which(D$ses_name == sessionnames),]
D$brand[which(D$brand %in% c("ActigraphCLE", "ActigraphMOS") == TRUE)] = "ActiGraph"
D2 = aggregate(D[, c("stdev")],
               by = list(D$sf, D$shakefreq, D$brand), FUN = mean)
colnames(D2) = c("sf", "shakefreq", "brand", "stdev")

CXA = 1
CXL = 1
CXM = 1.1
CX = 1
brands2lookat = c("Axivity", "ActiGraph", "GENEActiv", "MOX")#unique(D$brand)
pdf(file = paste0(outdir, "/Figure6_SamplingRate.pdf"),
    width = 9, height = 6)
jpeg(file = paste0(outdir, "/Figure6_SamplingRate.jpeg"),
    width = 500, height = 350)
colors = rep("black", 20) #gray.colors(n = 20, start = 0, end = 0.2)

par(mfrow = c(1, 4), mar = c(4,3,2, 0.5), mgp = c(2,1,0))
for (brandi in brands2lookat) {
  ci = 1  
  for (shakef in unique(D2$shakefreq)) {
    if (shakef %in% c(30, 87, 100, 125, 150, 175, 200, 225, 250)) {
      YLIM = c(0, 0.8)
      XLIM = c(10, 115) #range(c(D2$sf[which(D2$brand == brandi)], 115), na.rm = TRUE)
      GA = which( D2$shakefreq == shakef &
                    D2$brand == brandi)
      if (shakef == 30) {
        plot(D2$sf[GA], D2$stdev[GA], type = "b", pch = 20, cex = CX,
             cex.axis = CXA, cex.lab = CXL, cex.main = CXM,
             main = brandi, ylab =
               substitute(paste("Acceleration (", italic("g"),")")), #< maak italic
             xlab = "Sampling rate (Hertz)", xlim = XLIM, ylim = YLIM, bty = "l", col = colors[ci])
      } else {
        lines(D2$sf[GA], D2$stdev[GA], type = "b", pch = 20, cex = CX,
              cex.axis = CXA, cex.lab = CXL, cex.main = CXM, col = colors[ci])
      }
      
      text(x = 109, y = D2$stdev[max(GA)], paste0(shakef, " rpm"), cex = 0.8, col = colors[ci])
    }
    ci = ci + 1
  }
}
dev.off()