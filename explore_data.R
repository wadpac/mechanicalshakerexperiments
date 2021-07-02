rm(list=ls())
graphics.off()
extracted_data_path = "~/data/VUMC/shaker_experiments/extracteddata"

# Exploration of MIMSunit and how it compares across accelerometer brands
library("MIMSunit")
# identify subset of files relevant to experimental session
fns = dir(extracted_data_path, full.names = TRUE)
outputfile = "~/data/VUMC/shaker_experiments/explore_MIMS.RData"
sessionames = c("pro3_ses3" , "pro2_ses1", "pro2_ses2", "pro2_ses3")
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
          S = MIMSunit::mims_unit(df = tmp, epoch = '5 sec', dynamic_range = c(-DR, DR), output_mims_per_axis = TRUE)
          # calculate EN to check data alignment
          averageperws3 = function(x,sf,ws3) {
            x2 =cumsum(c(0,x))
            select = seq(1,length(x2),by=sf*ws3)
            x3 = diff(x2[round(select)]) / abs(diff(round(select)))
          }
          EN_raw = sqrt(tmp$X^2 + tmp$Y^2 + tmp$Z^2)
          EN = averageperws3(EN_raw,sf,ws3=5)
          
          if (length(EN) > nrow(S)) {
            EN = EN[1:nrow(S)]
          } else if (length(EN) < nrow(S)) {
            EN = c(EN, rep(NA, nrow(S) - length(EN)))
          }
          S$brand = brand
          S$sf = sf
          S$EN = EN
          S$ses_name = ses_name
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
# aggregate
D2 = aggregate(x = DATA[,c("MIMS_UNIT")],by = list(DATA$HEADER_TIME_STAMP, DATA$brand, DATA$ses_name), FUN = mean)
myq1 = function(x) as.numeric(quantile(x = x, probs=0.25))
myq3 = function(x) quantile(x, probs=0.75)
D1 = aggregate(x = DATA[,c("MIMS_UNIT")],by = list(DATA$HEADER_TIME_STAMP, DATA$brand, DATA$ses_name), FUN = myq1)
D3 = aggregate(x = DATA[,c("MIMS_UNIT")],by = list(DATA$HEADER_TIME_STAMP, DATA$brand, DATA$ses_name), FUN = myq3)
tidyupname = function(x) {
  # x = x[,-which(colnames(x) == "brand")]
  colnames(x)[1:4] = c("time", "brand", "ses_name", "MIMS_UNIT")
  return(x)
}
D1 = tidyupname(D1)
D2 = tidyupname(D2)
D3 = tidyupname(D3)
D = merge(D1, D3, by = c("time", "brand", "ses_name"), suffixes = c("q1","q3"))
D = merge(D, D2, by = c("time", "brand", "ses_name"))
# create plot per session
for (ses_name in c("pro3_ses3" , "pro2_ses1", "pro2_ses2", "pro2_ses3")) {
  x11()
  par(mfrow=c(2,1))
  GA = which(D$brand == "GENEActiv" & D$ses_name == ses_name)
  AX = which(D$brand == "Axivity" & D$ses_name == ses_name)
  AG = which(D$brand == "Actigraph" & D$ses_name == ses_name)
  plot(D$time[GA], D$MIMS_UNIT[GA], type="l", lwd=1.5, main= ses_name)
  lines(D$time[GA], D$MIMS_UNITq1[GA], type="l", lty=2)
  lines(D$time[GA], D$MIMS_UNITq3[GA], type="l", lty=2)
  lines(D$time[AX], D$MIMS_UNIT[AX], type="l", col="blue", lwd=1.5)
  lines(D$time[AX], D$MIMS_UNITq1[AX], type="l", lty=2, col="blue")
  lines(D$time[AX], D$MIMS_UNITq3[AX], type="l", lty=2, col="blue")
  lines(D$time[AG], D$MIMS_UNIT[AG], type="l", col="red", lwd=1.5)
  lines(D$time[AG], D$MIMS_UNITq1[AG], type="l", lty=2, col="red")
  lines(D$time[AG], D$MIMS_UNITq3[AG], type="l", lty=2, col="red")
  plot(D$time[GA],  D$MIMS_UNIT[GA] /  D$MIMS_UNIT[AX], col="green", type="l", ylim=c(0.9, 1.5))
  abline(h = 1, col="black", lwd=2)
}