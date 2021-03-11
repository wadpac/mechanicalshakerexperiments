### Load data and select windows

# user input required:
# Set path to data folder
path <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine/mechanicalshakerexperiments/data"

# no user input required from here onward:
setwd(path) #set path
# install_github("wadpac/mechanicalshakermachineexperiments")
for (i in dir("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine/mechanicalshakerexperiments/scripts", full.names = T)) source(i) #load functions
rm(i)


# TO DO:
# How to select windows for protocol 1? For now: start and end time used, based on used sensors (function selectwindows)
# Check activPAL timestamps, windows are not yet selected properly

## Actigraph
# Protocol 1
data_time_xyz_actigraph_pro1 <- loaddata(path = path, brand = "Actigraph", protocol = 1, session = 1)
# Protocol 2
data_time_xyz_actigraph_pro2_ses1 <- loaddata(path = path, brand = "Actigraph", protocol = 2, session = 1)
data_time_xyz_actigraph_pro2_ses2 <- loaddata(path = path, brand = "Actigraph", protocol = 2, session = 2)
data_time_xyz_actigraph_pro2_ses3 <- loaddata(path = path, brand = "Actigraph", protocol = 2, session = 3)
# Protocol 3
data_time_xyz_actigraph_pro3 <- loaddata(path = path, brand = "Actigraph", protocol = 3, session = 3)

