#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-19
#'@email : 
#'#*******************************************************************************************************************
#'@description :  This is the main script to generate input points for Ichthyop simulations
#'#*******************************************************************************************************************
#'@revisions
#'2021-05-25: added second part, to generate .xml files to run Ichthyop on cluster
#'2021-07-20: modifications of second part
#'2021-08-19: change main functions to sub-routines
#'#*******************************************************************************************************************


#' ***************
#' Get libraries:
#' ***************
source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("dplyr","sf","rnaturalearthdata","progress","rnaturalearthdata","ggplot2",
                     "sp","rgeos","RNetCDF","gridExtra","rgdal","cleangeo","rworldmap",
                     "tictoc","RANN","spatialEco","geosphere","lwgeom","doParallel",
                     "parallel","units","abind", "lubridate")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)

#' *************************
#' @0. Initialize analysis:
#' *************************
source(file.path(ROUT_PATH,'0.init.R'))


#'***********************************
#'@1. Generate the input locations
#'***********************************
source(file.path(ROUT_PATH, "1.nlog_inputs.R"))


#'****************************************
#'@2. Generate the cfg files and pbs jobs
#'****************************************
if (input_method == "allMask" & generate_xml == T){
  source(file.path(ROUT_PATH, "2.input_to_xml.R"))
}


#' ********************
#' Clean environment:
#' ********************
rm(list = ls()[!ls() %in% toKeep]) 