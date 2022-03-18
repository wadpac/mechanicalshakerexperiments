
options(digits.secs = 7)
options(scipen = 999)

# specify file and function paths
shaker_experiments_folder = "~/data/VUMC/shaker_experiments"
my_functions_folder =   "/home/vincent/projects/mechanicalshakerexperiments/R"
time_keeping_results_file = paste0(shaker_experiments_folder, "/analyses/time_keeping.RData")

# load functions (used when developing the code)
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) 

do.rerun = TRUE
if (do.rerun == TRUE) {
  
  # declare helper functions:
  averageperws3 = function(x,sf,epochsize) {
    x2 = cumsum(c(0, x))
    select = seq(1,length(x2), by = sf * epochsize)
    x3 = diff(x2[round(select)]) / abs(diff(round(select)))
  }
  update_success_log = function(brand, success_log, success) {
    success_log[[brand]] = c(success_log[[brand]], success)
    return(success_log)
  }
  
  # main code:
  extracted_data_path = paste0(shaker_experiments_folder, "/structured_raw_data")
  fns = dir(extracted_data_path, full.names = TRUE)
  
  
  if (!dir.exists(extracted_data_path)) {
    stop("\nLabelled data folder not recognised. Did you specify shaker_experiments_folder and did you run main.R?")
  }
  
  sessionames = c("timer_check") # only perform autocalibration based on session 3 because that is where non-movement was simulated
  overwrite = TRUE
  epochsize = 5
 
  experimentfile = system.file("datadescription/data_description.xlsx", package = "mechanicalshakerexperiments")[1]
  experiments <- gdata::read.xls(experimentfile, header = TRUE, sheet = 1)
  experiments = experiments[which(experiments$event == "turn_box"), c("accelerometers_used", "start_time")]
  
  # define object to keep track of calibration succesrate per brand
  success_log = list(Actigraph = c(), Axivity = c(), GENEActiv = c(), Activpal = c())
  
  results = data.frame(brand = character(500), sn = character(500), sf = numeric(500), turn = numeric(500), 
                       turn_clock_time = character(500), turn_signal = numeric(500), turn_signal_relative = numeric(500),
                       elapsed_time_clock = numeric(500),
                       timestampError = numeric(500), stringsAsFactors = FALSE)
  cnt = 1
  # loop over sessions and data files to perform auto-calibration procedure
  for (ses_name in sessionames) { #
    ses = grep(basename(fns), pattern = ses_name)
    ses2 = grep(fns[ses], pattern = "Acttrust|Activpal", invert = TRUE) # ignore Acttrust, because it does not have raw data
    for (fn in fns[ses[ses2]]) {
      if ((nrow(results) - cnt) < 500) { # add rows
        results[nrow(results) + 500,] = NA
      }
      print(fn)
      load(fn)
      if (length(grep(fn, pattern = "Actigraph")) > 0) {
        brand = "Actigraph"
      } else if (length(grep(fn, pattern = "Axivity")) > 0) {
        brand = "Axivity"
      } else if (length(grep(fn, pattern = "GENEActiv")) > 0) {
        brand = "GENEActiv"
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
          sf = extracteddata$specifications[i, "sampling_frequency"]
          if (brand == "MOX") sf = 25
          tmp$time = as.POSIXct(tmp$time, origin = "1970-01-01", tz = "Europe/Amsterdam")
          
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
          Nturn = length(turning_times)
          results$brand[cnt:(cnt + Nturn - 1)] = rep(brand, Nturn)
          results$sn[cnt:(cnt + Nturn - 1)] = rep(sn, Nturn)
          results$sf[cnt:(cnt + Nturn - 1)] = rep(sf, Nturn)
          
          results$turn[cnt:(cnt + Nturn - 1)] = 1:Nturn
          results$turn_clock_time[cnt:(cnt + Nturn - 1)] = as.character(turning_times)
          results$elapsed_time_clock = as.numeric(turning_times - turning_times[1])
          tmp$HEADER_TIME_STAMP_rounded = round(tmp$HEADER_TIME_STAMP)
          for (j in 1:Nturn) {
            # identify clock based turning times
            turning_point = which(tmp$HEADER_TIME_STAMP_rounded == turning_times[j])
            # defined 5 minute time window to search for turning points in signal
            nearby = which(tmp$HEADER_TIME_STAMP > (turning_times[j] - 150) &
                             tmp$HEADER_TIME_STAMP < (turning_times[j] + 150))
            # identify turning point
            dx = abs(diff(tmp$X[nearby]))
            dy = abs(diff(tmp$Y[nearby]))
            dz = abs(diff(tmp$Z[nearby]))
            dtotal = dx + dy + dz
            turn_signal = nearby[which.max(dtotal)[1]]
            results$turn_signal[cnt] = turn_signal
            if (j == 1) {
              turn_signal_first = turn_signal
            }
            results$turn_signal_relative[cnt] = turn_signal - turn_signal_first
            
            # timestamp based evaluation
            timestampError = tmp$HEADER_TIME_STAMP[turning_point] - tmp$HEADER_TIME_STAMP[turn_signal]
            timestampError = -timestampError[which.min(abs(timestampError))]
            units(timestampError) = "secs"
            results$timestampError[cnt] = as.numeric(timestampError)
            # sample rate based evaluation
            results$sf_observed[cnt] = results$turn_signal_relative[cnt] / results$elapsed_time_clock[cnt]
            turn_signal_relative2 = results$turn_signal_relative[cnt] / as.numeric(results$sf[cnt])
            results$samplingError_seconds[cnt] = turn_signal_relative2 - results$elapsed_time_clock[cnt]
            cnt  = cnt + 1
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
  results = results[which(results$brand != ""),]
  for (brand in c("Actigraph", "GENEActiv", "Axivity", "MOX")) { #"Activpal", 
    cat(paste0("\nSuccessful turning point extraction for ", brand,"\n"))
    print(table(success_log[[brand]]))
  }
  save(results, file = time_keeping_results_file)
} else {
  load(file = time_keeping_results_file)
}
results$sf = as.numeric(results$sf)
results$elapsed_time_clock = results$elapsed_time_clock / 3600
results = results[which(results$turn != 1), ]

for (brand in c("Actigraph", "GENEActiv", "Axivity", "MOX")) { #"Activpal", 
  sel = which(results$brand == brand)
  results_agg = aggregate(x = results[sel,c("timestampError", "samplingError_seconds", "sf","sf_observed", "elapsed_time_clock")],
                          by = list(results$brand[sel], results$sf[sel], results$turn[sel]), FUN = mean)
  
  names(results_agg)[1:3] = c("brand", "sf", "turn")
  
  print(results_agg)
}
#---------------------------------------------------------------------
# Comparisons:
# - timestampError: Time stamp turning point signal - Timestamp turning point clock
# - Expected (sf) and observed sample rate (sf_observed)
# - samplingError_seconds: Delta samples signal * sample frequency - Delta time turning turning point clock

