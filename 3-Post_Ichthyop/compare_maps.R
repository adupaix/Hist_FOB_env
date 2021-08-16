#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-16
#'@email : 
#'#*******************************************************************************************************************
#'@description :  This is the main script to build a map containing the difference between two maps
#'
#' Explanations on the provided arguments are available in main.R
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


#' empty environment
rm(list = ls())
invisible(gc())

#' generate basic paths
WD <- file.path(getwd(),'3-Post_Ichthyop')
DATA_PATH <- file.path(getwd(),'0-Data')

OUTPUT_PATH <- file.path(WD,"Outputs")
FUNC_PATH <- file.path(WD,"Functions")
ROUT_PATH <- file.path(WD, "Sub-routines")
RESOURCE_PATH <- file.path(WD,"Resources")

set.seed(10) # set for reproductibility

#' **********
#' @arguments
#' **********
#' 
#' @general
RESET = rep(F, 4)
Parallel = c(T, 1/2)
#' 
#' 
#' @common
agg.time_scale = "month"
agg.function = "mean"

area_to_map = c(20,140,-40,40)
#' c(20,140,-40,40): whole Indian Ocean basin
#' c(20,80, -30,30): western Indian Ocean
#' c(80,110, 0,30): gulf of Bengal

log_color_scale = F
common_scale_max = T
color_scale_pos = "out_panel" # one of c("null", "in_panel", "out_panel")

n_points_per_dir = 28*8*5

#' 
#' @map1

map1_arguments <- list(

  year = 2000,
  
  ltime=360,
  ltime_method = 2,
  ltime_sd = 30,
  
  weight_method = 3,
  
  gsize = 2,
  
  thr_disch = 100,
  
  forcing="PHILIN12.L75",
  input_location="river",
  input_method = "allMask",
  
  dist=100,
  bouncing=F

)


#' @map2
#' 

map2_arguments <- list(
  
  year = 2000,

  ltime=360,
  ltime_method = 2,
  ltime_sd = 30,

  weight_method = 1,

  gsize = 2,

  thr_disch = 100,

  forcing="PHILIN12.L75",
  input_location="river",
  input_method = "allMask",
  
  dist=100,
  bouncing=F

)


#' ***************
#' Get libraries:
#' ***************
source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("RNetCDF","RANN","raster","foreach","tictoc","pryr","plyr","lubridate","parallel","crayon",
                     "dplyr","sf","ggplot2","ggspatial", "doParallel", "ggpubr","abind", "progress","tidyr", "doSNOW")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)


#' ***************
#' get mean array 1
#' ***************
list2env(map1_arguments, globalenv())

source(file.path(ROUT_PATH,'0.init.R'))
source(file.path(ROUT_PATH,'1.link_cover_input.R'))
source(file.path(ROUT_PATH, "2.information_on_point.R"))
source(file.path(ROUT_PATH, "3.weight_and_mortality.R"))
source(file.path(ROUT_PATH, "4.build_maps.R"))

year1 <- year
mean_array1 <- mean_agg_array


#' ***************
#' get mean array 2
#' ***************
list2env(map2_arguments, globalenv())

source(file.path(ROUT_PATH,'0.init.R'))
source(file.path(ROUT_PATH,'1.link_cover_input.R'))
source(file.path(ROUT_PATH, "2.information_on_point.R"))
source(file.path(ROUT_PATH, "3.weight_and_mortality.R"))
source(file.path(ROUT_PATH, "4.build_maps.R"))

year2 <- year
mean_array2 <- mean_agg_array


#' *********************************************
#' calculate the difference between the 2 arrays
#' *********************************************
if (!identical(dim(mean_array1), dim(mean_array2))){
  stop("Error: please review arguments, the arrays used to build the maps do not have the same dimensions")
}

#' select year of interest
is_of_year_of_interest <- grepl(year1, dimnames(mean_array1)[[3]])
nm <- dimnames(mean_array1)[[3]][is_of_year_of_interest]
mean_array1 <- mean_array1[,,is_of_year_of_interest]

is_of_year_of_interest <- grepl(year2, dimnames(mean_array2)[[3]])
nm <- dimnames(mean_array2)[[3]][is_of_year_of_interest]
mean_array2 <- mean_array2[,,is_of_year_of_interest]


## normalisation des arrays
mean_array1 <- sweep(mean_array1, 3, apply(mean_array1, 3, max), "/")
mean_array2 <- sweep(mean_array2, 3, apply(mean_array2, 3, max), "/")

mean_array <- mean_array1 - mean_array2


