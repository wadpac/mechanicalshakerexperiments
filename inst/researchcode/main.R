### Script to structure data

# user input required:
# shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
shaker_experiments_folder = "/home/vincent/data/VUMC/shaker_experiments"

# source functions directly from file, to be replaced by package installation:
# my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine/mechanicalshakerexperiments/R"
my_functions_folder =   "/home/vincent/projects/mechanicalshakerexperiments/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

#----------------------------------------------------------------
experimentfile = paste0(shaker_experiments_folder, "/unstructured_raw_data/data_description_V7.xlsx")
outputdir = paste0(shaker_experiments_folder, "/structured_raw_data")
rawdatadir = paste0(shaker_experiments_folder, "/unstructured_raw_data")

# Load all packages
packages = c("gdata", "doParallel", "remotes", "GENEAread", "THLfi/read.gt3x", "data.table", "GGIR") #"GGIR",
lapply(packages, FUN = function(X) {
  tmp= unlist(strsplit(X, "/"))
  X = tmp[length(tmp)]
  do.call("require", list(X)) 
})
#===================================================================================================

brands = c("MOX", "Actigraph", "GENEActiv", "Axivity", "Activpal", "Acttrust") # "Shimmer")
experiments <- c("box", "ms_hfcr", "ms_lfcr", "ms_mfcr", "ms_hfmr", "ms_lfmr") #, "timer_check") #, "ms_bag")
structure_data(brands = brands, experiments, rawdatadir, outputdir, experimentfile)
