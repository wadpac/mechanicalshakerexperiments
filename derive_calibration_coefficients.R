rm(list=ls())
graphics.off()

options(digits.secs = 7)
options(scipen=999)
#-----------------------------------

extracted_data_path = "~/data/VUMC/shaker_experiments/extracteddata"
fns = dir(extracted_data_path, full.names = TRUE)
outputfile = "~/data/VUMC/shaker_experiments/explore_MIMS.RData"
my_functions_folder =   "/home/vincent/projects/mechanicalshakerexperiments/R"
calib_files = "~/data/VUMC/shaker_experiments/calibration"

if (!dir.exists(calib_files)) {
  dir.create(calib_files)
}
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

sessionames = c("pro2_ses3") # only perform autocalibration based on session 3 because that is where non-movement was simulated
overwrite= TRUE
epochsize = 5
averageperws3 = function(x,sf,epochsize) {
  x2 =cumsum(c(0,x))
  select = seq(1,length(x2),by=sf*epochsize)
  x3 = diff(x2[round(select)]) / abs(diff(round(select)))
}
for (ses_name in sessionames) { #
  ses = grep(basename(fns), pattern = ses_name)
  for (fn in fns[ses]) {
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
        
        shakefreq = averageperws3(x= tmp$shaking_frequency,sf,epochsize=5)
        shakefreq[which(shakefreq < 0)] = -1
        tmp = tmp[,-which(colnames(tmp) == "shaking_frequency")]
        # apply autocalibration:
        C = autocalibration(data=tmp, sf)
        # store results
        calib = list(scale=C$scale, offset=C$offset, sn=sn, C=C)
        save(calib, file = paste0(calib_files, "/calib_sn_",sn,".RData"))
      }
    }
  }
}
