### Load data and select windows
rm(list=ls()) # freeing up memory

# user input required:
# specify path to data folder
my_data_folder = "/home/vincent/data/VUMC/shaker_experiments" 
protocolfile = paste0(my_data_folder, "/data_description_V3.xlsx")

# Following lines only needed when running debugging code:
my_functions_folder =   "/home/vincent/projects/mechanicalshakerexperiments/R" 
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

# TO DO:
# How to select windows for protocol 1? For now: range between start and end time used, based on used sensors. Events turn box not yet included (function selectwindows)
# Axivity: protocol3_ses2 data load failed
# Acttrust: which variables for acceleration to select?
# How to select the data for protocol session 4 (subset of devices)

# Check for missing packages and install if missing:
packages = c("GGIR", "gdata", "doParallel", "remotes", "GENEAread", "THLfi/read.gt3x", "data.table")
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
  for (j in 1:length(x)) {
    print(dim(x[[j]]))
  }
}

focus_pro1 = TRUE # to avoid loading all data at once as that will never be needed
if (focus_pro1 == TRUE) {
  actigraph_pro1 <- loaddata(path = my_data_folder, brand = "Actigraph", protocol = 1, session = 1, protocolfile=protocolfile)
  activpal_pro1 <- loaddata(path = my_data_folder, brand = "Activpal", protocol = 1, session = 1, protocolfile=protocolfile)
  acttrust_pro1 <- loaddata(path = my_data_folder, brand = "Acttrust", protocol = 1, session = 1, protocolfile=protocolfile)
  axivity_pro1 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 1, session = 1, protocolfile=protocolfile)
  geneactiv_pro1 <- loaddata(path = my_data_folder, brand = "GENEActiv", protocol = 1, session = 1, protocolfile=protocolfile)
  
  print("Check dimensions of Axivity data: Protocol 1")
  checkdimensions(axivity_pro1$data)
  print("Check dimensions of GENEActiv data: Protocol 1")
  print(checkdimensions(geneactiv_pro1$data))
} else {
  actigraph_pro2_ses1 <- loaddata(path = my_data_folder, brand = "Actigraph", protocol = 2, session = 1, protocolfile=protocolfile)
  actigraph_pro2_ses2 <- loaddata(path = my_data_folder, brand = "Actigraph", protocol = 2, session = 2, protocolfile=protocolfile)
  actigraph_pro2_ses3 <- loaddata(path = my_data_folder, brand = "Actigraph", protocol = 2, session = 3, protocolfile=protocolfile)
  actigraph_pro3 <- loaddata(path = my_data_folder, brand = "Actigraph", protocol = 3, session = 3)
  
  activpal_pro2_ses1 <- loaddata(path = my_data_folder, brand = "Activpal", protocol = 2, session = 1, protocolfile=protocolfile)
  activpal_pro2_ses2 <- loaddata(path = my_data_folder, brand = "Activpal", protocol = 2, session = 2, protocolfile=protocolfile)
  activpal_pro2_ses3 <- loaddata(path = my_data_folder, brand = "Activpal", protocol = 2, session = 3, protocolfile=protocolfile)
  activpal_pro3 <- loaddata(path = my_data_folder, brand = "Activpal", protocol = 3, session = 3, protocolfile=protocolfile)
  print("Check dimensions of Activpal data: Protocol 2")
  checkdimensions(activpal_pro2_ses1$data)
  checkdimensions(activpal_pro2_ses2$data)
  checkdimensions(activpal_pro2_ses3$data)
  checkdimensions(activpal_pro3$data)
  
  acttrust_pro2_ses1 <- loaddata(path = my_data_folder, brand = "Acttrust", protocol = 2, session = 1, protocolfile=protocolfile)
  acttrust_pro2_ses2 <- loaddata(path = my_data_folder, brand = "Acttrust", protocol = 2, session = 2, protocolfile=protocolfile)
  acttrust_pro2_ses3 <- loaddata(path = my_data_folder, brand = "Acttrust", protocol = 2, session = 3, protocolfile=protocolfile)
  acttrust_pro3 <- loaddata(path = my_data_folder, brand = "Acttrust", protocol = 3, session = 3, protocolfile=protocolfile)
  
  axivity_pro2_ses1 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 2, session = 1, protocolfile=protocolfile)
  axivity_pro2_ses2 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 2, session = 2, protocolfile=protocolfile)
  axivity_pro2_ses3 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 2, session = 3, protocolfile=protocolfile)
  axivity_pro3_ses1 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 3, session = 1, protocolfile=protocolfile)
  axivity_pro3_ses2 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 3, session = 2, protocolfile=protocolfile)
  axivity_pro3_ses3 <- loaddata(path = my_data_folder, brand = "Axivity", protocol = 3, session = 3, protocolfile=protocolfile)
}
