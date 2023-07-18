#' structure_data
#'
#' @description 'structure_data' is the first step in the processing pipeline and loads the raw unstructured data and structures it to ease post-procressing
#'
#' @param brands Character vector with sensor brands to consider, which may include: "Actigraph", "Activpal", "Acttrust", "Axivity", "GENEActiv", and "MOX".
#' @param experiments Character vector of experiments to load, which may include: "timer_check", "ms_hrcr", "ms_lrcr", "ms_hrmr", "ms_lrmr", "ms_bag", and "box".
#' @param rawdatadir Path to the root of the experimental data (rawdatadir)
#' @param outputdir Output directory
#' @param experimentfile .xlsx file with protocol description, defaults to file stored inside the code
#' @return No output is given, data is stored in .RData files
#' @export

structure_data = function(brands, experiments, rawdatadir, outputdir, experimentfile = c()) {
  backup_options <- options()
  options(digits.secs = 7)
  on.exit(options(backup_options))
  checkdimensions = function(x) {
    if (length(x$data) > 0) {
      print(dim(x$data[[1]]))
    }
  }
  
  if (!dir.exists(outputdir)) dir.create(outputdir)
  if (!dir.exists(rawdatadir)) {
    stop(paste0("\nCannot find folder unstructured_raw_data. Make sure all folders with the ",
                "raw accelerometer files are stored inside a folder named unstructured_raw_data."))
  }
  if (length(experimentfile) == 0) {
    experimentfile = system.file("datadescription/data_description.xlsx", package = "mechanicalshakerexperiments")[1]
  }
  for (brand in brands) {
    for (experiment in experiments) {
      if (brand != "Axivity" & endsWith(experiment, "mr")) { #To avoid loading mixed dynamic range experiments for other devices
        cat(paste0("\nThis device was not included in experiment:"), experiment)
        next
      } else{
        extracteddata <- loaddata(path = rawdatadir,
                                  brand = brand, experiment = experiment, experimentfile = experimentfile)
        cat(paste0("\nCheck dimensions of ", brand, ": Experiment ",experiment,"\n"))
        checkdimensions(extracteddata)
        save(extracteddata, file = paste0(outputdir, "/", brand, "_",experiment, ".RData"))
        rm(extracteddata)
      }
    }
  }
}