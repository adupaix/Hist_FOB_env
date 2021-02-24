rm(list = ls())
gc()

WD <- file.path(getwd(),'2-Post_Ichthyop')
DATA_PATH <- file.path(getwd(),'0-Data')

OUTPUT_PATH <- file.path(WD,"Outputs")
FUNC_PATH <- file.path(WD,"Functions")
ROUT_PATH <- file.path(WD, "Sub-routines")
RESOURCE_PATH <- file.path(WD,"Resources")

set.seed(10) # set for reproductibility (lognormale distribution in apply.ltime.1)

# ARGUMENTS :                                                                             
#===========
#

### ltime (num): life time of logs to be used                         
### ltime_method (num) : filter used for the life time : 1. lifetimes generated using a log normal law
#                                                         so that mean(ltimes)=ltime
#                                                      2. life time fixed at ltime        
ltime=180
ltime_method = 2

### weight_method (num): method used to give weight to input points
# can be different from one only if the input_location is river
#   1. homogeneous weight for each particle
#   2. weight proportional to mean water discharge of rivers
#   3. weight proportional to the percentage of forest cover in the river basin
#   4. weight proportional to the surface of forest cover in the river basin
weight_method = 1

### size of the grid cells used (either 1, 2 or 5)
gsize = 2

### time scale used to aggregate the dates (day, month, quarter or year)
## !! quarter start in December: for example, first quarter of 2012 is from 12.2011 to 02.2012 included
agg.time_scale = "day"

### the mean water discharge of the input river is used as a filter 
# only particles originating from a river with a value > thr_disch (in m3/s) are kept 
# if thr_disch = 0, only rivers with a mean discharge > 0 m3/s will be kept
# if you don't want any filter to be used, set thr_disch = NULL
thr_disch = 0

### origin of the time in the output results
# normally will not be used, as it is saved in the attributes of time by Ichthyop
# except for simulations with PHILIN75.L12, where because the product mask is bad,
# I had to overide the reading of attributes by Ichthyop
## !! the format of origin_time has to be kept as it is
origin_time = "year 1900 month 01 day 01 at 00:00"


# Arguments used for the Ichthyop simulation
#-------------------------------------------
### forcing (chr): forcing product used in ICHTHYOP simulation (oscar/globcurrent/nemo)     
forcing="PHILIN12.L75"
### input_location (chr): either "river" or "mangrove"
input_location="river"
### input_method (chr): either "onMask" put points on the closest point on the      
#                                     current product                               
#                      or "kFromCoast" put points at dist km from the coast         
input_method = "onMask"

### dist (num): distance specified if the input_method above is 'kFromCoast'
dist=100

### timestep (num): time step in ICHTHYOP simulation
timestep=6 
### dispersion (num): dispersion coefficient in ICHTHYOP simulation. Enter 9 for 10^-9, and 
#                   0 for no dispersion
dispersion=9

### bouncing (log): weither the particles in the ICHTHYOP simulation were bouncing on coast 
#                 (TRUE) or if they were beaching (FALSE, default)                        
#                                                                                         
bouncing=F

## Run in parallel ?
# First element of the vector:
#    If F, runs in sequential
#    if T, runs in parallel
# Second element of the vector: fraction of the cores to be used
#
## !! One part of the script can rapidly fill the RAM memory, to be taken into account when choose to run in parallel or not
## !! If the script is running on a Windows machine, the script is executed in sequential
Parallel = c(F, 1/2)

# Whether to delete the results obtained for these arguments (T) or not (F):
# 1: arrays and matrices obtained with 1.read.ncs and maps obtained with 2.map.array
# 2: only maps obtained with 2.map.array
RESET = c(T,T)

# Arguments used for the ggplot:
#-------------------------------
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


# Get libraries:
# ==============

source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("RNetCDF","RANN","raster","foreach","tictoc","pryr","plyr","lubridate","parallel","crayon",
                     "dplyr","sf","ggplot2","ggspatial", "doParallel", "ggpubr","abind")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)


# Initialize analysis:
#====================

source(file.path(ROUT_PATH,'0.init.R'))


# 1. Generate arrays:
#=================

source(file.path(ROUT_PATH,'1.read.ncs.R'))


# 2. Build maps:
#==============
if (agg.time_scale != "day"){
source(file.path(ROUT_PATH,'2.map.array.R'))
}
