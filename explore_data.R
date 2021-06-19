rm(list=ls())
graphics.off()
extracted_data_path = "~/data/VUMC/shaker_experiments/extracteddata"

# Exploration of MIMSunit and how it compares across accelerometer brands
library("MIMSunit")
# identify subset of files relevant to experimental session
fns = dir(extracted_data_path, full.names = TRUE)
for (ses_name in c("pro3_ses3", "pro2_ses1", "pro2_ses2", "pro2_ses3")) {
  ses1 = grep(basename(fns), pattern = ses_name)
  combineddata <- list()
  cnt  = 1
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
        sn = as.numeric(extractedata$specifications[i, "serial_number"])
        tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
        if (brand == "Actigraph") {
          tmp$time = as.POSIXct(tmp$time)
          tmp = tmp[, c("time","X","Y","Z")]
        } else if (brand == "Axivity" | brand == "GENEActiv") {
          tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
          tmp = tmp[, c("time","x","y","z")]
          colnames(tmp)[2:4] = c("X", "Y", "Z")
        }
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