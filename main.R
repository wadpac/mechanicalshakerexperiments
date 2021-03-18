### Load data and select windows

# user input required:
# Set path to data folder
path <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine/mechanicalshakerexperiments/data"
setwd(path) #set path
# install_github("wadpac/mechanicalshakermachineexperiments")
#load functions
for (i in dir("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine/mechanicalshakerexperiments/scripts", full.names = T)) source(i) #load functions
rm(i)

# TO DO:
# How to select windows for protocol 1? For now: range between start and end time used, based on used sensors. Events turn box not yet included (function selectwindows)
# Axivity: protocol3_ses2 data load failed
# How to select the data for protocol session 4 (subset of devices)

## Actigraph
# Protocol 1
data_time_xyz_actigraph_pro1 <- loaddata(path = path, brand = "Actigraph", protocol = 1, session = 1)
# Protocol 2
data_time_xyz_actigraph_pro2_ses1 <- loaddata(path = path, brand = "Actigraph", protocol = 2, session = 1)
data_time_xyz_actigraph_pro2_ses2 <- loaddata(path = path, brand = "Actigraph", protocol = 2, session = 2)
data_time_xyz_actigraph_pro2_ses3 <- loaddata(path = path, brand = "Actigraph", protocol = 2, session = 3)
# Protocol 3
data_time_xyz_actigraph_pro3 <- loaddata(path = path, brand = "Actigraph", protocol = 3, session = 3)

## Activpal
# Protocol 1
data_time_xyz_activpal_pro1 <- loaddata(path = path, brand = "Activpal", protocol = 1, session = 1)
# Protocol 2
data_time_xyz_activpal_pro2_ses1 <- loaddata(path = path, brand = "Activpal", protocol = 2, session = 1)
data_time_xyz_activpal_pro2_ses2 <- loaddata(path = path, brand = "Activpal", protocol = 2, session = 2)
data_time_xyz_activpal_pro2_ses3 <- loaddata(path = path, brand = "Activpal", protocol = 2, session = 3)
# Protocol 3
data_time_xyz_activpal_pro3 <- loaddata(path = path, brand = "Activpal", protocol = 3, session = 3)

## Axivity
# Protocol 1
data_time_xyz_axivity_pro1 <- loaddata(path = path, brand = "Axivity", protocol = 1, session = 1)
#Protocol 2
data_time_xyz_axivity_pro2_ses1 <- loaddata(path = path, brand = "Axivity", protocol = 2, session = 1)
data_time_xyz_axivity_pro2_ses2 <- loaddata(path = path, brand = "Axivity", protocol = 2, session = 2)
data_time_xyz_axivity_pro2_ses3 <- loaddata(path = path, brand = "Axivity", protocol = 2, session = 3)
# Protocol 3
data_time_xyz_axivity_pro3_ses1 <- loaddata(path = path, brand = "Axivity", protocol = 3, session = 1)
data_time_xyz_axivity_pro3_ses2 <- loaddata(path = path, brand = "Axivity", protocol = 3, session = 2)
data_time_xyz_axivity_pro3_ses3 <- loaddata(path = path, brand = "Axivity", protocol = 3, session = 3)
