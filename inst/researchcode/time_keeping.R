shaker_experiments_folder = "~/data/VUMC/shaker_experiments"
my_functions_folder =   "/home/vincent/projects/mechanicalshakerexperiments/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions


#-----------------------------------
options(digits.secs = 7)
options(scipen=999)

extracted_data_path = paste0(shaker_experiments_folder, "/structured_raw_data")
fns = dir(extracted_data_path, full.names = TRUE)


if (!dir.exists(extracted_data_path)) {
  stop("\nLabelled data folder not recognised. Did you specify shaker_experiments_folder and did you run main.R?")
}

sessionames = c("timer_check") # only perform autocalibration based on session 3 because that is where non-movement was simulated
overwrite = TRUE
epochsize = 5
averageperws3 = function(x,sf,epochsize) {
  x2 =cumsum(c(0,x))
  select = seq(1,length(x2),by=sf*epochsize)
  x3 = diff(x2[round(select)]) / abs(diff(round(select)))
}

experimentfile = system.file("datadescription/data_description.xlsx", package = "mechanicalshakerexperiments")[1]

experiments <- gdata::read.xls(experimentfile, header = TRUE, sheet = 1)
experiments = experiments[which(experiments$event == "turn_box"), c("accelerometers_used", "start_time")]

# define object to keep track of calibration succesrate per brand
success_log = list(Actigraph = c(), Axivity = c(), GENEActiv = c(), Activpal = c())
update_success_log = function(brand, success_log, success) {
  success_log[[brand]] = c(success_log[[brand]], success)
  return(success_log)
}
# loop over sessions and data files to perform auto-calibration procedure
for (ses_name in sessionames) { #
  ses = grep(basename(fns), pattern = ses_name)
  ses2 = grep(fns[ses], pattern = "Acttrust", invert = TRUE) # ignore Acttrust, because it does not have raw data
  for (fn in fns[ses[ses2]]) {
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
    } else if (length(grep(fn, pattern = "MOX")) > 0) {
      brand = "MOX"
    }
    for (i in 1:length(extracteddata$data)) {
      tmp = extracteddata$data[[i]]
      if (length(tmp) > 0) {
        # apply aggregation function
        # check that this goes well for Axivity AX6
        DR = as.numeric(extracteddata$specifications[i, "dynamic_range"])
        sn = as.character(extracteddata$specifications[i, "serial_number"])
        tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
        
        if (brand == "Actigraph" | brand == "Activpal") {
          tmp = tmp[, c("time","X","Y","Z")]
        } else if (brand == "Axivity" | brand == "GENEActiv" ) {
          tmp = tmp[, c("time","x","y","z")] #
          colnames(tmp)[2:4] = c("X", "Y", "Z")
        } else if (brand == "MOX") {
          tmp = tmp[, c("time","AccX","AccY","AccZ")]
          colnames(tmp)[2:4] = c("X", "Y", "Z")
        }
        row.names(tmp) = 1:nrow(tmp)
        colnames(tmp)[1] = "HEADER_TIME_STAMP"
        
        turning_times = experiments$start_time[grep(pattern = brand, x = experiments$accelerometers_used, ignore.case = TRUE)]
        turning_times = as.POSIXlt(x = paste0("2020-11-26 ", turning_times), tz = "Europe/Amsterdam")
        for (j in 1:length(turning_times)) {
          # graphics.off()
          turning_point = which(tmp$HEADER_TIME_STAMP == turning_times[j])
          nearby = which(tmp$HEADER_TIME_STAMP > (turning_times[j] - 60) &
                           tmp$HEADER_TIME_STAMP < (turning_times[j] + 60))
          print(range(tmp$HEADER_TIME_STAMP[nearby]))
          # x11()
          # plot(tmp$HEADER_TIME_STAMP[nearby], tmp$X[nearby], type="l")
          # lines(tmp$HEADER_TIME_STAMP[nearby], tmp$Y[nearby], type="l", col = "blue")
          # lines(tmp$HEADER_TIME_STAMP[nearby], tmp$Z[nearby], type="l", col = "red")
        }
        if (length(nearby) == 0) {
          success_log = update_success_log(brand, success_log, success = FALSE)
        } else {
          success_log = update_success_log(brand, success_log, success = TRUE)
        }
      } else {
        success_log = update_success_log(brand, success_log, success = FALSE)
      }
    }
  }
}
# for (brand in c("Actigraph", "GENEActiv", "Axivity", "Activpal", "MOX")) {
#   cat(paste0("\nSuccessful calibrations for ", brand,"\n"))
#   print(table(success_log[[brand]]))
# }
