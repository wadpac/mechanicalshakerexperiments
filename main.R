### Load data and select windows
rm(list=ls()) # freeing up memory
# user input required:
my_data_folder = "/home/vincent/data/VUMC/shaker_experiments"
protocolfile = paste0(my_data_folder, "/data_description_V4.xlsx")
outputdir = "~/data/VUMC/shaker_experiments/extracteddata"

# Following lines only needed when running debugging code:
my_functions_folder =   "/home/vincent/projects/mechanicalshakerexperiments/R"


#=========================================================================================
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

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
brands_to_extract = c("Actigraph", "Activpal") # c("Actigraph", "Axivity", "GENEActiv") #"Activpal", "Acttrust",
focus_pro1 = FALSE # to avoid loading all data at once as that will never be needed
if (focus_pro1 == TRUE) {
  for (brand in brands_to_extract) {
    protocol = 1
    session = 1
    extractedata <- loaddata(path = my_data_folder, brand = brand, 
                             protocol = protocol, session = session, protocolfile=protocolfile)
    cat(paste0("\nCheck dimensions of ", brand, ": Protocol ",protocol,"\n"))
    checkdimensions(extractedata)
    save_data(extractedata,outputdir=outputdir, objectname=paste0("_", brand, "_pro",protocol,"_ses", session))
    rm(extractedata)
  }
} else {
  for (brand in brands_to_extract) {
    for (protocol in 2:3) {
      for (session in 1:3) {
        if (protocol == 2 | (protocol == 3 & session == 3) | (protocol == 3 & brand == "Axivity")) {
          extractedata <- loaddata(path = my_data_folder, 
                                   brand = brand, protocol = protocol, session = session, protocolfile=protocolfile)
          cat(paste0("\nCheck dimensions of ", brand, ": Protocol ",protocol," session", session,"\n"))
          checkdimensions(extractedata)
          save(extractedata, file = paste0(outputdir, "/", brand, "_pro",protocol,"_ses", session))
          rm(extractedata)
        }
      }
    }
  }
}
