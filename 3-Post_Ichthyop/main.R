#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-13
#'@email : 
#'#*******************************************************************************************************************
#'@description :  This is the main script to build maps from the Ichthyop simulation results
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


#' ***************
#' Get libraries:
#' ***************
source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("RNetCDF","RANN","raster","foreach","tictoc","pryr","plyr","lubridate","parallel","crayon",
                     "dplyr","sf","ggplot2","ggspatial", "doParallel", "ggpubr","abind", "progress","tidyr", "doSNOW")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)


#' *************************
#' @0. Initialize analysis:
#' *************************
source(file.path(ROUT_PATH,'0.init.R'))


#' ****************************************
#' @1. Get the cover for each input point:
#' ****************************************
source(file.path(ROUT_PATH,'1.link_cover_input.R'))


#' *******************************************************
#' @2. Calculate weights depending on the chosen method:
#' *******************************************************
source(file.path(ROUT_PATH, "2.information_on_point.R"))


#' ***********************************
#' @3. Apply weights and mortality:
#' ***********************************
source(file.path(ROUT_PATH, "3.weight_and_mortality.R"))


#' ***************
#' @4. Build maps:
#' ***************
source(file.path(ROUT_PATH, "4.build_maps.R"))


#' ********************
#' Clean environment:
#' ********************
rm(list = ls()[!ls() %in% toKeep]) 
