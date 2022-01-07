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


WD <- file.path(getwd(),'1-Input_Ichthyop/')

DATA_PATH <- file.path(getwd(), "0-Data/")

FUNC_PATH <- file.path(WD,"Functions")
RESOURCE_PATH <- file.path(WD,"Resources")
OUTPUT_PATH <- file.path(WD, "Outputs")
ROUT_PATH <- file.path(WD, "Sub-routines")


#'@arguments:
#'#**********
#'# Run in parallel ?
#' (in kFromCoast sub script)
#' First element of the vector:
#'    If F, runs in sequential
#'    if T, runs in parallel
#' Second element of the vector: fraction of the cores to be used
#'
Parallel = c(T, 1/2)
                  
#'
#' RESET (logical) :
#'    First element of the vector:
#'                   if T, calculates the input points position in any case
#'                   if F, tries to read the input points positions in the output folder
#'    Second element of the vector:
#'                   if T, delete all the cfg files and rebuild them
#'                   if F, build cfg files only if they do not exist
RESET = c(F,T)

#'                                                                           
#' save (logical) : if T, save a txt file ready for use in ichthyop                  
#' return_format (chr): either "sf", returns an sf object with points and associated 
#'                                   river characteristics                           
#'                      or "mat", returns a matrix containing only the coordinates of
#'                               the input points (as saved in the txt file)       
save = T
return_format = "sf" # one of c("sf", "mat")

#'                                                                                   
#'## input_location (chr): either "river" or "mangrove"                              
#'## input_method (chr): either "onMask" put points on the closest point on the      
#'                                    current product                               
#'                      or "kFromCoast" put points at dist km from the coast
#'                      or "allMask" put points on the current product cells which
#'                                  are close to the coastline  
#'
input_location = "river" # one of c("river","mangrove")
input_method = "allMask" # one of c("onMask","kFromCoast", "allMask")

#'                                                                                   
#'## dist (num) : distance from the coast at which we want the points (in deg)       
#'## curr_prod (chr): which current mask product to use if put the input points on   
#'             the closest point of the product mask                                
#'
dist = 1 # used if input_method == kFromCoast (distance from coast in degrees)
curr_prod = "PHILIN12.L75" # one of c("PHILIN12.L75","oscar","nemo","globcurrent","nemo15m") # used if input_method == onMask ou allMask

#'
#' Arguments to generate Ichthyop cfg files (only if input_method == allMask)
#' 
#'## generate_xml (log): generate config files or not
#'## xml_template (chr): path of the resource file to generate cfg files from
#'## sim_input/output_path (chr): paths to be saved in the cfg files
#'
#'## transport_duration (num): number of days that the particles are to be transported (in days)
#'## first/last_release_date (chr): start and end dates of the Ichthyop simulations
#'## release_period (num): period between two particles release (in days)
#'## record_period (num): period between two savings of the particles positions (in days)
#'
#'## n_cfg_per_dir (num): number of cfg files to save per directory
#'## n_pbs_jobs (num): number of pbs jobs to run on the cluster

generate_xml = T

# xml_template <- file.path(RESOURCE_PATH, "template_cfg.xml")

# sim_input_path <- "/home/adupaix/Documents/ichthyop-private/input"
sim_input_path <- "/home1/datawork/adupaix/input-ichthyop"
# sim_output_path <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/2-Launch_Ichthyop_datarmor/ichthyop-output"
sim_output_path <- "/home1/scratch/adupaix/ichthyop-output"


#~ Arguments to generate the xml files
transport_duration = 500 #in days

first_release_year = 1989 #' release from transport_duration days before first_release_year-01-01.
                          #' For example, transport_duration = 500
                          #'          and first_release_year = 2000
                          #' the first release will be on the 1998-08-19
last_release_year = 1990 #' The last release will be on January of the following year, depending on the release_period
                         #' For example, if the first release was on 1998-08-19
                         #'             and the release_period = 7 days
                         #'             and last_release_year = 2000
                         #'             then the last release date (L) will be 2001-01-03
                         #'             L = 1998-08-19 + 7 * n, with n being the smallest number such that L is in 2001
release_period = 7 # in days, (interval between two release)
record_period = 1 #in days (interval between two recorded positions)

n_cfg_per_dir = 28*10*4 # number of .xml file per directory

#~ Arguments to generate the pbs jobs
n_pbs_jobs = 3 #number of .pbs jobs to run in the cluster (to generate the command_list files)
n_mpi = rep(10, n_pbs_jobs) #number of mpi asked in each jobs (between 1 and 20)
walltime = rep(20, n_pbs_jobs) #time asked for the jobs (in hours)


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