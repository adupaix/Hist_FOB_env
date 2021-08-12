#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-02-24
#'@email : 
#'#*******************************************************************************************************************
#'@description :  This is the main script to build maps from the Ichthyop simulation results
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

#' @arguments :
#' **********
#' 

#'## ltime (num): life time of logs to be used                         
#'## ltime_method (num) : filter used for the life time : 1. lifetimes generated using a normal law
#'                                                        so that mean(ltimes)=ltime
#'                                                        2. lifetimes generated using a poisson law with lambda = ltime
#'                                                        3. life time fixed at ltime
#'## ltime_sd (num): standard deviation of the normal law, if ltime_method = 1
ltime=360
ltime_method = 2
ltime_sd = 30 # in days, used if ltime_method == 1

#'## weight_method (num): method used to give weight to release points
#'   1. homogeneous weight for each particle
#'   2. weight proportional to mean water discharge of rivers
#'   3. weight proportional to the surface of forest cover associated with the release point
#'   4. weight proportional to the surface of forest cover in the river basins associated with the release point
#'   5. weight proportional to the surface of coastal forest cover associated with the release point
#'   6. weight proportional to the precipitations at the release point
weight_method = 3

#'## size of the grid cells used (2° or a multiple of 2°)
#' fixed because of the post-processing script performed on the cluster
gsize = 2

#'## the mean water discharge of the input river is used as a filter 
#' only particles originating from a river with a maximum value > thr_disch (in m3/s) are kept 
#' if thr_disch = 0, only rivers with a maximum discharge > 0 m3/s will be kept
#' if you don't want any filter to be used, set thr_disch = NULL
#'@!! a threshold of 100 m3/s was used to map the forest cover around the rivers
thr_disch = 100

# Arguments used for the Ichthyop simulation
#'****************************************
### forcing (chr): forcing product used in ICHTHYOP simulation (oscar/globcurrent/nemo)     
forcing="PHILIN12.L75"
### input_location (chr): either "river" or "mangrove"
input_location="river"
### input_method (chr): either "onMask" put points on the closest point on the      
#                                     current product                               
#                      or "kFromCoast" put points at dist km from the coast
#                      or "allMask" put points on each coastal point of the current product
input_method = "allMask"

### dist (num): distance specified if the input_method above is 'kFromCoast'
dist=100

### bouncing (log): weither the particles in the ICHTHYOP simulation were bouncing on coast 
#                 (TRUE) or if they were beaching (FALSE, default)                        
#                                                                                         
bouncing=F

#'# Year of the simulation
year = 2000

#'# number of simulations saved in each sub-directory of Ichthyop output
n_points_per_dir = 28*8*5

#'# Run in parallel ?
#'********************
#' First element of the vector:
#'    If F, runs in sequential
#'    if T, runs in parallel
#' Second element of the vector: fraction of the number of cores to be used
#'
#'# @!! If the script is running on a Windows machine, the script is executed sequentially
Parallel = c(T, 1/2)

#' Reset ?
#' *****
#' Whether to delete the results obtained for these arguments (T) or not (F):
#'  each element of RESET concerns the outputs obtained from the corresponding sub-routine (1 to 4)
RESET = c(F,F,F,F)

#' Arguments used for the ggplot:
#'******************************
#'## time scale used to aggregate the dates (month, quarter or year)
#'# @!! quarter starts in December: for example, first quarter of 2012 is from 12.2011 to 02.2012 included
agg.time_scale = "month"

#' specify which function is used to aggregate the array before building the maps
#' if "mean', build maps of the mean density of particles for every month/quarter/year
#' if "sd", build maps of standard deviation
agg.function = "mean"

# specify if the color scale is log transformed (T) or not (F)
log_color_scale = F

# choose to fix the max of the color scale
# if = F, the max is not fixed
# if T, the max is fixed at the maximum value of the array
common_scale_max = T

# specify the position of the color legend, one of c("null", "in_panel", "out_panel")
# "null": no color legend shown
# "in_panel": color legend in the bottom right corner of the map
# "out_panel": color legend out of the map, on the right
color_scale_pos = "out_panel"


#' Get libraries:
#' ***************

source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("RNetCDF","RANN","raster","foreach","tictoc","pryr","plyr","lubridate","parallel","crayon",
                     "dplyr","sf","ggplot2","ggspatial", "doParallel", "ggpubr","abind", "progress","tidyr", "doSNOW")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)


#' Initialize analysis:
#' ********************

source(file.path(ROUT_PATH,'0.init.R'))


#' 1. Get the cover for each input point:
#' ****************************************

source(file.path(ROUT_PATH,'1.link_cover_input.R'))


#' 2. Calculate weights depending on the chosen method:
#' **************************************************

source(file.path(ROUT_PATH, "2.information_on_point.R"))

#' 3. Apply weights and mortality:
#' ******************************

source(file.path(ROUT_PATH, "3.weight_and_mortality.R"))

#' 4. Build maps:
#' ***************

source(file.path(ROUT_PATH, "4.build_maps.R"))


#' Clean environment:
#' ***************
rm(list = ls()[!ls() %in% toKeep]) 
