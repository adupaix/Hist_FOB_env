#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-17
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description :  cfg file to run the analysis
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

#' empty environment
rm(list = ls())
invisible(gc())

STUDY_DIR <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/"

#' generate basic paths
WD <- file.path(STUDY_DIR,'2-Post_Ichthyop')
DATA_PATH <- file.path(STUDY_DIR,'0-Data')

OUTPUT_PATH <- file.path(WD,"Outputs")
FUNC_PATH <- file.path(WD,"Functions")
ROUT_PATH <- file.path(WD, "Sub-routines")
RESOURCE_PATH <- file.path(WD,"Resources")

set.seed(10) # set for reproductibility

#' **********
#' @arguments
#' **********

#' General arguments
#'********************
#'# @Parallel (num): Run some parts of the script in parallel ?
#' First element of the vector:
#'    If F, runs in sequential
#'    if T, runs in parallel
#' Second element of the vector: fraction of the total number of cores to be used
#'
#'# @! If the script is running on a Windows machine, the script is executed sequentially
Parallel = c(T, 1/2)

#' @RESET @!!! (log): Reset ?
#' Whether to delete the results obtained for these arguments (T) or not (F):
#'  each element of RESET concerns the outputs obtained from the corresponding sub-routine (1 to 4)
RESET = c(F,F,F,F)

#' @cluster (log): script running on the cluster ?
#'  allows to choose how the foreach loops are run in parallel (if Parallel[1] is TRUE)
#'  use doParallel if T and use doSNOW if F
cluster = T

#' Arguments on the current study
#' ******************************
#'## @ltime (num): life time of logs to be used                         
#'## @ltime_method (num) : filter used for the life time : 1. lifetimes generated using a normal law
#'                                                        so that mean(ltimes)=ltime
#'                                                        2. lifetimes generated using a poisson law with lambda = ltime
#'                                                        3. life time fixed at ltime
#'## @ltime_sd (num): standard deviation of the normal law, if ltime_method = 1
ltime=360
ltime_method = 2
ltime_sd = 30 # in days, used only if ltime_method == 1

#'## @weight_method (num): method used to give weight to release points
#'   1. @w=1 to each release point
#'   2. weight proportional to the length of @coastline associated with the release point
#'   3. weight proportional to the surface of @coastal_forest_cover associated with the release point
#'   4. weight proportional to the surface of @river_forest_cover multiplied by the @discharge at river mouth
#'   5. weight proportional to the @total surface of forest cover associated with the release point (@w3+w4)
#'   6. weight proportional to the surface of @coastal_forest_cover multiplied by the @precipitations at the release point (@w3*precip)
#'   7. weight proportional to the surface of @river_forest_cover multiplied by the @precipitations at the release point (@w4*precip)
#'   8. weight proportional to the @total surface of forest cover multiplied by the @precipitations at the release point
#'   9. weight proportional to the @total surface of forest cover (w3 + w4 without discharge)
weight_method = 9

#'## @gsize (num): size of the grid cells used (1° or a multiple of 1°)
#' fixed because of the post-processing script performed on the cluster
# gsize = 2

#'## @thr_disch (num): the mean water discharge of the input river is used as a filter 
#' only particles originating from a river with a maximum value > thr_disch (in m3/s) are kept 
#' if thr_disch = 0, only rivers with a maximum discharge > 0 m3/s will be kept
#' if you don't want any filter to be used, set thr_disch = NULL
#'@!!Fixed: a threshold of 100 m3/s was used to map the forest cover around the rivers
thr_disch = 100


#' Arguments used previously for the Ichthyop simulation
#'*******************************************************
#'## @forcing (chr): forcing product used in ICHTHYOP simulation (oscar/globcurrent/nemo/PHILIN12.L75)     
forcing="PHILIN12.L75"
#'## @input_location (chr): either "river" or "mangrove"
#'@only "river" used in this study
input_location="river"
#'## @input_method (chr): either "onMask" put points on the closest point on the      
#'                                     current product                               
#'                       or "kFromCoast" put points at dist km from the coast
#'                       or "allMask" put points on each coastal point of the current product
#'                       @only "allMask" used in this study
input_method = "allMask"

#'## @dist (num): distance specified if the input_method above is 'kFromCoast'
dist=100

#'## @bouncing (log): weither the particles in the ICHTHYOP simulation were bouncing on coast 
#'                    (TRUE) or if they were beaching (FALSE, default)                        
#'                                                                                         
bouncing=F

#'## @year (num): Year of the simulation
year = 2000

#'## @n_points_per_dir (num): number of simulations saved in each sub-directory of Ichthyop output
n_points_per_dir = 28*8*5

#'## @sim_output_path (chr): path to the directory where the Ichthyop outputs are stored
#'                           if sim_output_path = "", it is automatically set to file.path(DATA_PATH, "Output_Ichthyop", sim_name, year)
sim_output_path = ""

#' Arguments used for the ggplot:
#'******************************
#'
#'## @agg.time_scale (chr): time scale used to aggregate the dates (month, quarter or year)
#' @!! quarter starts in December: for example, first quarter of 2012 is from 12.2011 to 02.2012 included
agg.time_scale = "year"

#'## @agg.function (chr): specify which function is used to aggregate the array before building the maps
#' if "mean', build maps of the mean density of particles for every month/quarter/year
#' if "sd", build maps of standard deviation
agg.function = "mean"

#'## @log_color_scale (log): specify if the color scale is log transformed (T) or not (F)
log_color_scale = T

#'## @common_scale_max (log): choose whether to fix the max of the color scale or not
#' if = F, the max is not fixed (the scales of each maps are different)
#' if T, the max is fixed at the maximum value of all the maps (same scale for all the maps)
common_scale_max = T

#'## @color_scale_pos (chr): specify the position of the color legend, one of c("null", "in_panel", "out_panel")
# "null": no color legend shown
# "in_panel": color legend in the bottom right corner of the map
# "out_panel": color legend out of the map, on the right
color_scale_pos = "out_panel"

#'## @area_to_map (num): specify the area of interest for the map
#' vector containing c(xmin, xmax, ymin, ymax)
#' c(20,140,-40,40): whole Indian Ocean basin
#' c(20,80, -30,30): western Indian Ocean
#' c(80,110, 0,30): gulf of Bengal
area_to_map = c(20,140,-40,40)

#'## @del_low_values (log): specify if a filter is applied to the low density values (T) or not (F)
del_low_values = T


#' ***************
#' Launch analysis
#' ***************
 
source(file.path(WD,'main.R'))
