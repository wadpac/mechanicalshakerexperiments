shaker_experiments_folder = "~/data/VUMC/shaker_experiments"
my_functions_folder =   "/home/vincent/projects/mechanicalshakerexperiments/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions


#-----------------------------------
options(digits.secs = 7)
options(scipen=999)

extracted_data_path = paste0(shaker_experiments_folder, "/structured_raw_data")
fns = dir(extracted_data_path, full.names = TRUE)
calib_files = paste0(shaker_experiments_folder, "/autocalibration_results")

if (!dir.exists(extracted_data_path)) {
  stop("\nLabelled data folder not recognised. Did you specify shaker_experiments_folder and did you run main.R?")
}
if (!dir.exists(calib_files)) dir.create(calib_files)

if (!dir.exists(calib_files)) {
  dir.create(calib_files)
}

sessionames = c("box") # only perform autocalibration based on session 3 because that is where non-movement was simulated
overwrite= TRUE
epochsize = 5
averageperws3 = function(x,sf,epochsize) {
  x2 =cumsum(c(0,x))
  select = seq(1,length(x2),by=sf*epochsize)
  x3 = diff(x2[round(select)]) / abs(diff(round(select)))
}

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
        sf = as.numeric(extracteddata$specifications[i, "sampling_frequency"])
        sn = as.character(extracteddata$specifications[i, "serial_number"])
        tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz="Europe/Amsterdam")
        
        if (brand == "Actigraph" | brand == "Activpal") {
          tmp = tmp[, c("time","X","Y","Z","shaking_frequency")]
        } else if (brand == "Axivity" | brand == "GENEActiv" ) {
          tmp = tmp[, c("time","x","y","z","shaking_frequency")] #
          colnames(tmp)[2:4] = c("X", "Y", "Z")
        } else if (brand == "MOX") {
          tmp = tmp[, c("time","AccX","AccY","AccZ","shaking_frequency")]
          colnames(tmp)[2:4] = c("X", "Y", "Z")
        }
        row.names(tmp) = 1:nrow(tmp)
        colnames(tmp)[1] = "HEADER_TIME_STAMP"
        
        shakefreq = averageperws3(x= tmp$shaking_frequency, sf, epochsize=5)
        shakefreq[which(shakefreq < 0)] = -1
        tmp = tmp[,-which(colnames(tmp) == "shaking_frequency")]
        # apply autocalibration:
        C = autocalibration(data=tmp, sf, printsummary = FALSE, brand)
        # store results
        calib = list(scale=C$scale, offset=C$offset, sn=sn, C=C)
        save(calib, file = paste0(calib_files, "/calib_",brand,"_sn_",sn,".RData"))
        if (all(C$offset == c(0,0,0)) & all(C$scale == c(1,1,1))) {
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
for (brand in c("Actigraph", "GENEActiv", "Axivity", "Activpal", "MOX")) {
  cat(paste0("\nSuccessful calibrations for ", brand,"\n"))
  print(table(success_log[[brand]]))
}
