#---------------------------------------------------------------------
# This script performs a check of the time keeping ability of various sensor brands.
# The following comparison are made:
# - accelerometer_timestamp_error_seconds: Accelerometer time stamp when turning is visable in signal - Timestamp of turning according to reference clock
# - Expected (sf) and observed sample rate (sf_observed) based on number of samples that were collected between the turning points visible in the signal
# - accelerometer_sampling_error_seconds: Delta samples signal * expected sample frequency - Delta time of the turning point according to reference clock
rm(list = ls())
options(digits.secs = 5)
options(scipen = 999)
options(digits = 20)

# specify file and function paths
#shaker_experiments_folder = "/media/vincent/DATA/VUMC/shaker_experiments"
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine" # Update to be your local directory
#output_directory = "/media/vincent/DATA/VUMC/shaker_experiments/analyses"
output_directory = paste0(shaker_experiments_folder, "/analyses")

#my_functions_folder =   "~/projects/mechanicalshakerexperiments/R"
time_keeping_results_file = paste0(output_directory, "/time_keeping.RData")
tz = "Europe/Amsterdam"
# load functions (used when developing the code)
#for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) 

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
  #-----------------------------------------------------------
  # main code:
  extracted_data_path = paste0(shaker_experiments_folder, "/structured_raw_data")
  fns = dir(extracted_data_path, full.names = TRUE)
  
  if (!dir.exists(extracted_data_path)) {
    stop("\nLabelled data folder not recognised. Did you specify shaker_experiments_folder and did you run main.R?")
  }
  
  sessionames = c("timer_check")
  epochsize = 5
  
  experimentfile = system.file("datadescription/data_description.xlsx", package = "mechanicalshakerexperiments")[1]
  experiments <- gdata::read.xls(experimentfile, header = TRUE, sheet = 1)
  experiments = experiments[which(experiments$event == "turn_box"), c("accelerometers_used", "start_time")]
  
  success_log = list(Actigraph = c(), Axivity = c(), GENEActiv = c(), Activpal = c())
  
  results = data.frame(brand = character(500), sn = character(500), sf = numeric(500), turn = numeric(500), 
                       turn_clock_time = character(500), turn_signal = numeric(500), turn_signal_relative = numeric(500),
                       elapsed_time_atomclock_hours = numeric(500),
                       accelerometer_timestamp_error_seconds = numeric(500), stringsAsFactors = FALSE)
  cnt = 1
  for (ses_name in sessionames) { #
    ses = grep(basename(fns), pattern = ses_name)
    ses2 = grep(fns[ses], pattern = "Acttrust", invert = TRUE) # ignore Acttrust, because it does not have raw data
    # ses2 = grep(fns[ses], pattern = "Acttrust|Activpal", invert = TRUE) # ignore Acttrust, because it does not have raw data
    # ses2 = grep(fns[ses], pattern = "Axivity|GENEActiv|MOX|Acttrust|Activpal", invert = TRUE) # ignore Acttrust, because it does not have raw data
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
      } else if (length(grep(fn, pattern = "Activpal")) > 0) {
        brand = "Activpal"
      }
      for (i in 1:length(extracteddata$data)) {
        options(digits.secs = 5)
        options(scipen = 999)
        tmp = extracteddata$data[[i]]
        if (length(tmp) > 0) {
          # apply aggregation function
          # check that this goes well for Axivity AX6
          DR = as.numeric(extracteddata$specifications[i, "dynamic_range"])
          sn = as.character(extracteddata$specifications[i, "serial_number"])
          sf = as.numeric(extracteddata$specifications[i, "sampling_frequency"])
          if (brand == "MOX") sf = 25
          if (brand != "Actigraph" & brand != "Activpal") {
            tmp$time = as.POSIXlt(tmp$time, origin = "1970-01-01", tz = "Europe/Amsterdam")
          }
          tmp = tmp[, c("time","x","y","z")] 
          
          row.names(tmp) = 1:nrow(tmp)
          colnames(tmp)[1] = "HEADER_TIME_STAMP"
          turning_times = experiments$start_time[grep(pattern = brand, x = experiments$accelerometers_used, ignore.case = TRUE)]
          turning_times = as.POSIXlt(x = paste0("2020-11-26 ", turning_times), tz = "Europe/Amsterdam")
          Nturn = length(turning_times)
          results$brand[cnt:(cnt + Nturn - 1)] = rep(brand, Nturn)
          #if (brand == "Actigraph") {
           # brand_sn = paste0(brand, "_", substr(x = sn,start = 1, stop = 3))
            #results$brand_subtype[cnt:(cnt + Nturn - 1)] = rep(brand_sn, Nturn)
        #  } else {
            results$brand_subtype[cnt:(cnt + Nturn - 1)] = rep(brand, Nturn)
         # }
          results$sn[cnt:(cnt + Nturn - 1)] = rep(sn, Nturn)
          results$sf[cnt:(cnt + Nturn - 1)] = rep(sf, Nturn)
          results$turn[cnt:(cnt + Nturn - 1)] = 1:Nturn
          results$turn_clock_time[cnt:(cnt + Nturn - 1)] = as.character(turning_times)
          results$elapsed_time_atomclock_hours[cnt:(cnt + Nturn - 1)] = as.numeric(turning_times - turning_times[1])
          tmp$HEADER_TIME_STAMP_rounded = round(tmp$HEADER_TIME_STAMP)
          for (j in 1:Nturn) {
            # identify clock based turning times
            turning_point = which(tmp$HEADER_TIME_STAMP_rounded == turning_times[j])
            if (length(turning_point) > 0) {
              # for Activpal we may not be able to find the turning point, because it falls a sleep easily
              # we could extract the turning point by looking at nearest timestamp, like in the line below
              # turning_point = which.min(abs(tmp$HEADER_TIME_STAMP_rounded - turning_times[j]))
              # however, then it would not be a good evaluation of time keeping anymore, because all error will
              # come from the device being turned off during the moment of turning around.
              
              # define 3 minute time window to search for turning points in signal
              nearby = which(tmp$HEADER_TIME_STAMP > (turning_times[j] - 90) &
                               tmp$HEADER_TIME_STAMP < (turning_times[j] + 90))
              # identify turning point
              dx = abs(diff(tmp$x[nearby]))
              dy = abs(diff(tmp$y[nearby]))
              dz = abs(diff(tmp$z[nearby]))
              dtotal = dx + dy + dz
              turn_signal = nearby[which.max(dtotal)[1]]
              results$turn_signal[cnt] = turn_signal
              if (j == 1) {
                turn_signal_first = turn_signal
              }
              results$turn_signal_relative[cnt] = turn_signal - turn_signal_first
              # timestamp based evaluation
              accelerometer_timestamp_error_seconds = tmp$HEADER_TIME_STAMP[turning_point] - tmp$HEADER_TIME_STAMP[turn_signal]
              accelerometer_timestamp_error_seconds = -accelerometer_timestamp_error_seconds[which.min(abs(accelerometer_timestamp_error_seconds))]
              units(accelerometer_timestamp_error_seconds) = "secs"
              results$accelerometer_timestamp_error_seconds[cnt] = as.numeric(accelerometer_timestamp_error_seconds)
              # sample rate based evaluation
              if (brand != "Activpal") {
                results$sf_observed[cnt] = results$turn_signal_relative[cnt] / results$elapsed_time_atomclock_hours[cnt]
                turn_signal_relative2 = results$turn_signal_relative[cnt] / as.numeric(results$sf[cnt])
                results$accelerometer_sampling_error_seconds[cnt] = turn_signal_relative2 - results$elapsed_time_atomclock_hours[cnt]
              } else {
                is.na(results$sf_observed[cnt]) = TRUE
                is.na(results$accelerometer_sampling_error_seconds[cnt]) = TRUE
              }
            } else {
              nearby = NULL
            }
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
  for (brand in c("Actigraph", "GENEActiv", "Axivity", "MOX", "Activpal")) { #, 
    cat(paste0("\nSuccessful turning point extraction for ", brand,"\n"))
    print(table(success_log[[brand]]))
  }
  save(results, file = time_keeping_results_file)
} else {
  load(file = time_keeping_results_file)
}
results$sf = as.numeric(results$sf)
results$elapsed_time_atomclock_hours = results$elapsed_time_atomclock_hours / 3600
results = results[which(results$turn != 1 & results$turn_signal != 0), ]

# Aggregate per brand
out = c()
cnt = 1
for (brand in c("Actigraph", "GENEActiv", "Axivity", "MOX", "Activpal")) {
#for (brand in c("Actigraph_CLE", "Actigraph_MOS", "GENEActiv", "Axivity", "MOX", "Activpal")) {
  sel = which(results$brand_subtype == brand)
  if (length(sel) != 0) {
    results_agg = aggregate(x = results[sel,c("elapsed_time_atomclock_hours", "accelerometer_timestamp_error_seconds", 
                                              "accelerometer_sampling_error_seconds",
                                              "sf", "sf_observed")],
                            by = list(results$brand_subtype[sel], results$turn[sel]),
                            FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                min = min(x),  max = max(x), n = length(x)))
    names(results_agg)[1:2] = c("brand_subtype", "turn")
    
    if (cnt == 1) {
      out = results_agg
    } else {
      out = rbind(out, results_agg)
    }
    cnt = cnt + 1
  }
}


# Reformat output to ease creating table
out[,3:7] = round(out[,3:7], digits = 2)

out2 = out
onm = names(out)
for (i in 3:7) {
  if (onm[i] %in% c("elapsed_time_atomclock_hours") == TRUE) {
    out2[,i] = apply(X = out[,i], MAR = 1, FUN = function(x) x[1])
  } else if (onm[i] %in% c("accelerometer_timestamp_error_seconds", 
                           "accelerometer_sampling_error_seconds",
                           "sf_observed") == TRUE) {
    out2[,i] = apply(X = out[,i], MAR = 1, FUN = function(x) paste0(x[1], " \u00B1 ", x[2],
                                                                    " (",x[3]," ; ",x[4], ")", collapse = " "))
  } else if (onm[i] == "sf") {
    out2[,i] = apply(X = out[,i], MAR = 1, FUN = function(x) paste0(x[1], " (N=",x[5],")", collapse = " "))
  }
}
names(out2)[which(names(out2) == "sf")] = "sf_expected"
out2 = out2[which(out2$elapsed_time_atomclock_hours > 2),]

print(out2)
write.csv(out2, file = paste0(output_directory, "/time_keep_check.csv"), row.names = FALSE)