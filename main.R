### Load data and select windows

# user input required:
# Obtain path to data folder
# this is csv file with two columns: my_data_folder and my_functions_folder
# in the second row you will store the respective folder paths as a character
filepaths = read.csv("data/filepaths.csv")
my_data_folder = filepaths$my_data_folder
my_functions_folder = filepaths$my_functions_folder

setwd(my_data_folder) #set path
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

# TO DO:
# How to select windows for protocol 1? For now: range between start and end time used, based on used sensors. Events turn box not yet included (function selectwindows)
# Axivity: protocol3_ses2 data load failed
# Acttrust: which variables for acceleration to select?
# How to select the data for protocol session 4 (subset of devices)

# Check for missing packages and install if missing:
packages = c("GGIR", "gdata", "doParallel", "remotes", "GENEAread", "THLfi/read.gt3x")
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

## Actigraph
# Protocol 1
data_time_xyz_actigraph_pro1 <- loaddata(path = my_data_folder, brand = "Actigraph", protocol = 1, session = 1)
# Protocol 2
data_time_xyz_actigraph_pro2_ses1 <- loaddata(path = my_data_folder, brand = "Actigraph", protocol = 2, session = 1)
data_time_xyz_actigraph_pro2_ses2 <- loaddata(path = my_data_folder, brand = "Actigraph", protocol = 2, session = 2)
data_time_xyz_actigraph_pro2_ses3 <- loaddata(path = my_data_folder, brand = "Actigraph", protocol = 2, session = 3)
# Protocol 3
data_time_xyz_actigraph_pro3 <- loaddata(path = my_data_folder, brand = "Actigraph", protocol = 3, session = 3)

## Activpal
# Protocol 1
data_time_xyz_activpal_pro1 <- loaddata(path = my_data_folder, brand = "Activpal", protocol = 1, session = 1)
# Protocol 2
data_time_xyz_activpal_pro2_ses1 <- loaddata(path = my_data_folder, brand = "Activpal", protocol = 2, session = 1)
data_time_xyz_activpal_pro2_ses2 <- loaddata(path = my_data_folder, brand = "Activpal", protocol = 2, session = 2)
data_time_xyz_activpal_pro2_ses3 <- loaddata(path = my_data_folder, brand = "Activpal", protocol = 2, session = 3)
# Protocol 3
data_time_xyz_activpal_pro3 <- loaddata(path = my_data_folder, brand = "Activpal", protocol = 3, session = 3)

##Acttrust
# Protocol 1
data_time_xyz_acttrust_pro1 <- loaddata(path = my_data_folder, brand = "Acttrust", protocol = 1, session = 1)
# Protocol 2
data_time_xyz_acttrust_pro2_ses1 <- loaddata(path = my_data_folder, brand = "Acttrust", protocol = 2, session = 1)
data_time_xyz_acttrust_pro2_ses2 <- loaddata(path = my_data_folder, brand = "Acttrust", protocol = 2, session = 2)
data_time_xyz_acttrust_pro2_ses3 <- loaddata(path = my_data_folder, brand = "Acttrust", protocol = 2, session = 3)
# Protocol 3
data_time_xyz_acttrust_pro3 <- loaddata(path = my_data_folder, brand = "Acttrust", protocol = 3, session = 3)

## Axivity
# Protocol 1
data_time_xyz_axivity_pro1 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 1, session = 1)
#Protocol 2
data_time_xyz_axivity_pro2_ses1 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 2, session = 1)
data_time_xyz_axivity_pro2_ses2 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 2, session = 2)
data_time_xyz_axivity_pro2_ses3 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 2, session = 3)
# Protocol 3
data_time_xyz_axivity_pro3_ses1 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 3, session = 1)
data_time_xyz_axivity_pro3_ses2 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 3, session = 2)
data_time_xyz_axivity_pro3_ses3 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 3, session = 3)
