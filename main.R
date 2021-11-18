### Load data and select windows
rm(list=ls()) # freeing up memory
# user input required:
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"  #my_data_folder = "/home/vincent/data/VUMC/shaker_experiments" 
# Following lines only needed when running debugging code:


#----------------------------------------------------------------
experimentfile = paste0(shaker_experiments_folder, "/unstructured_raw_data/data_description_V5.xlsx")
outputdir = paste0(shaker_experiments_folder, "/structured_raw_data")
rawdatadir = paste0(shaker_experiments_folder, "/unstructured_raw_data")

if (!dir.exists(outputdir)) dir.create(outputdir)
if (!dir.exists(rawdatadir)) {
  stop(paste0("\nCannot find folder unstructured_raw_data. Make sure all folders with the ",
              "raw accelerometer files are stored inside a folder named unstructured_raw_data."))
}
# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine/mechanicalshakerexperiments/R"  #my_functions_folder =   "/home/vincent/projects/mechanicalshakerexperiments/R" 

for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions



#=========================================================================================
# installing GGIR separately for now to aid in experimenting with GGIR implementation
library("remotes")
remotes::install_github("wadpac/GGIR")
library("GGIR")

# Check for missing packages and install if missing:
packages = c("gdata", "doParallel", "remotes", "GENEAread", "THLfi/read.gt3x", "data.table") #"GGIR",
slash_in_name = sapply(packages, FUN = function(x) length(unlist(strsplit(x, "/"))) == 2)
if (length(which(slash_in_name == FALSE)) > 0) {
  CRANpackages_to_get = packages[which(packages[!slash_in_name] %in% rownames(installed.packages()) == FALSE)]
  if (length(CRANpackages_to_get) > 0) {
    install.packages(CRANpackages_to_get )
  }
}
if (length(which(slash_in_name == TRUE)) > 0) {
  GitHub_packages = as.character(sapply(packages[slash_in_name], FUN = function(x) unlist(strsplit(x, "/"))[2]))
  GitHubpackages_to_get = packages[slash_in_name[which(GitHub_packages %in% rownames(installed.packages()) == FALSE)]]
  if (length(GitHubpackages_to_get) > 0) {
    remotes::install_github(GitHubpackages_to_get)
  }
}

checkdimensions = function(x) {
  if (length(x$data) > 0) {
    print(dim(x$data[[1]]))
  }
}
options(digits.secs = 7)
brands_to_extract = "Axivity" #c("Actigraph", "Activpal", "Axivity", "GENEActiv", "Acttrust")
# To avoid loading all data at once as that will never be needed: don't include experiment "timer_check"
experiments_to_extract <- "box" #c("ms_hfcr", "ms_lfcr", "ms_mfcr", "ms_hfmr", "ms_lfmr", "ms_bag", "door") #Does not work for box yet
for (brand in brands_to_extract) {
  for (experiment in experiments_to_extract) {
    if (brand != "Axivity" & endsWith(experiment, "mr")) { #To avoid loading mixed dynamic range experiments for other devices
      cat(paste0("\nThis device was not included in experiment:"), experiment)
      next
    } else{
      extracteddata <- loaddata(path = rawdatadir, 
                                brand = brand, experiment = experiment, experimentfile = experimentfile)
      cat(paste0("\nCheck dimensions of ", brand, ": Experiment ",experiment,"\n"))
      checkdimensions(extractedata)
      save(extracteddata, file = paste0(outputdir, "/", brand, "_",experiment))
      rm(extracteddata)
    }
  }
}

