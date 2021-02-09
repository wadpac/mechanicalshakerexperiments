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
# Check activpal timestamps, windows are not yet selected properly

## Actigraph
# Protocol 1
data_time_xyz_actigraph_pro1 <- loaddata(path = path, folder = "Actigraph_pro1", brand = "Actigraph", data_description = "data_description_V1.xlsx", protocol = 1, session = 1)
# Protocol 2
data_time_xyz_actigraph_pro2_ses1 <- loaddata(path = path, folder = "Actigraph_pro2_ses1", brand = "Actigraph", data_description = "data_description_V1.xlsx", protocol = 2, session = 1)
data_time_xyz_actigraph_pro2_ses2 <- loaddata(path = path, folder = "Actigraph_pro2_ses2", brand = "Actigraph", data_description = "data_description_V1.xlsx", protocol = 2, session = 2)
data_time_xyz_actigraph_pro2_ses3 <- loaddata(path = path, folder = "Actigraph_pro2_ses3", brand = "Actigraph", data_description = "data_description_V1.xlsx", protocol = 2, session = 3)
# Protocol 3
data_time_xyz_actigraph_pro3 <- loaddata(path = path, folder = "Actigraph_pro3", brand = "Actigraph", data_description = "data_description_V1.xlsx", protocol = 3, session = 3)

## Activpal
# Protocol 1
data_time_xyz_activpal_pro1 <- loaddata(path = path, folder = "Activpal_pro1", brand = "Activpal", windows = FALSE)

#data_time_xyz_activpal_pro1 <- loaddata(path = path, folder = "Activpal_pro1", brand = "Activpal", data_description = "data_description_V1.xlsx", protocol = 1, session = 1)
# Protocol 2
data_time_xyz_activpal_pro2 <- loaddata(path = path, folder = "Activpal_pro2", brand = "Activpal", windows = FALSE)
# Protocol 3
data_time_xyz_activpal_pro3 <- loaddata(path = path, folder = "Activpal_pro3", brand = "Activpal", windows = FALSE)

## Acttrust
# Protocol 1, 2, 3
data_time_xyz_acttrust_pro123 <- loaddata(path = path, folder = "Acttrust_Condor", brand = "Acttrust")

## Axivity
# Protocol 1
data_time_xyz_axivity_pro1 <- loaddata(path = path, folder = "Axivity_pro1", brand = "Axivity")
# Protocol 2
data_time_xyz_axivity_pro2_ses1 <- loaddata(path = path, folder = "Axivity_pro2_ses1", brand = "Axivity") #session 1
data_time_xyz_axivity_pro2_ses2 <- loaddata(path = path, folder = "Axivity_pro2_ses2", brand = "Axivity") #session 2
data_time_xyz_axivity_pro2_ses3 <- loaddata(path = path, folder = "Axivity_pro2_ses3", brand = "Axivity") #session 3
# Protocol 3
data_time_xyz_axivity_pro3_ses1 <- loaddata(path = path, folder = "Axivity_pro3_ses1", brand = "Axivity") #session 1
data_time_xyz_axivity_pro3_ses2 <- loaddata(path = path, folder = "Axivity_pro3_ses2", brand = "Axivity") #session 2
data_time_xyz_axivity_pro3_ses3 <- loaddata(path = path, folder = "Axivity_pro3_ses3", brand = "Axivity") #session 3

## GENEActiv
# Protocol 1
data_time_xyz_geneactiv_pro1 <- loaddata(path = path, folder = "GENEActiv_pro1", brand = "GENEActiv")
# Protocol 2
data_time_xyz_geneactiv_pro2_ses1 <- loaddata(path = path, folder = "GENEActiv_pro2_ses1", brand = "GENEActiv") #session 1
data_time_xyz_geneactiv_pro2_ses2 <- loaddata(path = path, folder = "GENEActiv_pro2_ses2", brand = "GENEActiv") #session 2
data_time_xyz_geneactiv_pro2_ses3 <- loaddata(path = path, folder = "GENEActiv_pro2_ses3", brand = "GENEActiv") #session 3
# Protocol 3
data_time_xyz_geneactiv_pro3 <- loaddata(path = path, folder = "GENEActiv_pro3", brand = "GENEActiv")

## MOX
# Protocol 1
data_time_xyz_mox_pro1 <- loaddata(path = path, folder = "MOX_exportedCSV/exportedCSV/MOX_pro1", brand = "MOX")
# Protocol2
data_time_xyz_mox_pro2_ses1 <- loaddata(path = path, folder = "MOX_exportedCSV/exportedCSV/MOX_pro2_ses1", brand = "MOX") #session 1
data_time_xyz_mox_pro2_ses2 <- loaddata(path = path, folder = "MOX_exportedCSV/exportedCSV/MOX_pro2_ses2", brand = "MOX") #session 2
data_time_xyz_mox_pro2_ses3 <- loaddata(path = path, folder = "MOX_exportedCSV/exportedCSV/MOX_pro2_ses3", brand = "MOX") #session 3
##Protocol 3
data_time_xyz_mox_pro3 <- loaddata(path = path, folder = "MOX_exportedCSV/exportedCSV/MOX_pro3", brand = "MOX")

## Shimmer
# Protocol 1
data_time_xyz_shimmer_pro1 <- loaddata(path = path, folder = "Shimmer_pro1", brand = "Shimmer")
# Procotol 2
data_time_xyz_shimmer_pro2_ses1 <- loaddata(path = path, folder = "Shimmer_pro2_ses1", brand = "Shimmer") #session 1
data_time_xyz_shimmer_pro2_ses2 <- loaddata(path = path, folder = "Shimmer_pro2_ses2", brand = "Shimmer") #session 2
data_time_xyz_shimmer_pro2_ses3a <- loaddata(path = path, folder = "Shimmer_pro2_ses3A", brand = "Shimmer") #session 3a
data_time_xyz_shimmer_pro2_ses3b <- loaddata(path = path, folder = "Shimmer_pro2_ses3B", brand = "Shimmer") #session 3b
# Protocol 3
data_time_xyz_shimmer_pro3 <- loaddata(path = path, folder = "Shimmer_pro3", brand = "Shimmer")