### Script to structure data

# user input required:
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine"
#shaker_experiments_folder = "/media/vincent/DATA/VUMC/shaker_experiments"

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine/mechanicalshakerexperiments/R"
#my_functions_folder =   "/home/vincent/projects/mechanicalshakerexperiments/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

#----------------------------------------------------------------
outputdir = paste0(shaker_experiments_folder, "/structured_raw_data")
rawdatadir = paste0(shaker_experiments_folder, "/unstructured_raw_data")
# install.packages(pkgs = c("gdata", "doParallel", "remotes", "GENEAread", "THLfi/read.gt3x", "data.table"))

library(remotes)
remotes::install_github("wadpac/GGIR")
remotes::install_github("wadpac/GGIRread")

# Load all packages
packages = c("gdata", "doParallel", "remotes", "GENEAread", "THLfi/read.gt3x", "data.table") #"GGIR",
lapply(packages, FUN = function(X) {
  tmp = unlist(strsplit(X, "/"))
  X = tmp[length(tmp)]
  do.call("require", list(X))
})


#===================================================================================================

brands = c("ActiGraph", "activPAL", "Axivity", "GENEActiv", "MOX") #, "Acttrust", "Shimmer")
experiments <- c("ms_hrcr", "ms_lrcr", "ms_hrmr", "ms_lrmr", "ms_mrcr") #, "ms_bag", "timer_check", "box")
structure_data(brands = brands, experiments, rawdatadir, outputdir)
