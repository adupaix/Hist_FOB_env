#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-04-20
#'@email : 
#'#*******************************************************************************************************************
#'@description :  This is the main script to generate input points for Ichthyop simulations
#'#*******************************************************************************************************************
#'@revisions
#'2021-05-25: added second part, to generate .xml files to run Ichthyop on cluster
#'2021-07-20: modifications of second part
#'#*******************************************************************************************************************


WD <- file.path(getwd(),'1-Input_Ichthyop/')

DATA_PATH <- file.path(getwd(), "0-Data/")

FUNC_PATH <- file.path(WD,"Functions")
RESOURCE_PATH <- file.path(WD,"Resources")
OUTPUT_PATH <- file.path(WD, "Outputs")


#'@arguments:
#'#**********
arguments <- list(DATA_PATH = DATA_PATH,
                  OUTPUT_PATH = OUTPUT_PATH,
                  RESOURCE_PATH = RESOURCE_PATH,
                  
#'# Run in parallel ?
#' (in kFromCoast sub script)
#' First element of the vector:
#'    If F, runs in sequential
#'    if T, runs in parallel
#' Second element of the vector: fraction of the cores to be used
#'
Parallel = c(T, 1/2),
                  
#'
#' reset (logical) : if T, calculates the input points position in any case
#'                   if F, tries to read the input points positions in the output folder
reset = F,

#'                                                                           
#' save (logical) : if T, save a txt file ready for use in ichthyop                  
#' return_format (chr): either "sf", returns an sf object with points and associated 
#'                                   river characteristics                           
#'                      or "mat", returns a matrix containing only the coordinates of
#'                               the input points (as saved in the txt file)       
save = T,
return_format = "sf", # one of c("sf", "mat")

#'                                                                                   
#'## input_location (chr): either "river" or "mangrove"                              
#'## input_method (chr): either "onMask" put points on the closest point on the      
#'                                    current product                               
#'                      or "kFromCoast" put points at dist km from the coast
#'                      or "allMask" put points on the current product cells which
#'                                  are close to the coastline  
#'
input_location = "river", # one of c("river","mangrove")
input_method = "allMask", # one of c("onMask","kFromCoast", "allMask")

#'                                                                                   
#'## dist (num) : distance from the coast at which we want the points (in deg)       
#'## curr_prod (chr): which current mask product to use if put the input points on   
#'             the closest point of the product mask                                
#'
dist = 1, # used if input_method == kFromCoast (distance from coast in degrees)
curr_prod = "PHILIN12.L75") # one of c("PHILIN12.L75","oscar","nemo","globcurrent","nemo15m") # used if input_method == onMask ou allMask

#'
#' Arguments to generate Ichthyop cfg files (only if input_method == allMask)
#' 
#'## generate_xml (log): generate config files or not
#'## xml_template (chr): path of the resource file to generate cfg files from
#'## sim_input/output_path (chr): paths to be saved in the cfg files
#'
#'## transport_duration (num): number of days that the particles are to be transported (in days)
#'## first/last_release_date (chr): start and end dates of the Ichthyop simulations
#'## release_frequency (num): frequency at which the particles are released (in days)
#'## record_frequency (num): frequency at which the particles positions are to be save (in days)
#'
#'## n_cfg_per_dir (num): number of cfg files to save per directory
#'## n_pbs_jobs (num): number of pbs jobs to run on the cluster

generate_xml = T

xml_template <- file.path(RESOURCE_PATH, "template_cfg.xml")

# sim_input_path <- "/home/adupaix/Documents/ichthyop-private/input"
sim_input_path <- "/home1/datawork/adupaix/input-ichthyop"
# sim_output_path <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/ichthyop-output"
sim_output_path <- "/home1/scratch/adupaix/ichthyop-output"


#~ Arguments to generate the xml files
transport_duration = 500 #in days

first_release_year = 1989 # release from the first of December preceding of this year
last_release_year = 1990 # to the 15th of January following this year
release_frequency = 2 # nb of release per month
record_frequency = 1 #in days (interval between two recorded positions)

n_cfg_per_dir = 28*8*5 # number of .xml file per directory
n_pbs_jobs = 3 #number of .pbs jobs to run in the cluster (to generate the command_list files)



#'@import_libraries:
#'#********************

source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("dplyr","sf","rnaturalearthdata","progress","rnaturalearthdata","ggplot2",
                     "sp","rgeos","RNetCDF","gridExtra","rgdal","cleangeo","rworldmap",
                     "tictoc","RANN","spatialEco","geosphere","lwgeom","doParallel",
                     "parallel","units","abind", "lubridate")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)

# loading above arguments to the environment (instead of keeping them in the lists)
list2env(arguments, globalenv())




#'@Load_function_generating_the_input_locations:
#'#*********************************************
source(file.path(FUNC_PATH, "1.nlog_inputs.R"))

# calling the main function
output <- do.call(input.nlog, args = arguments)



#'@Load_function_generating_the_cfg_files:
#'#****************************************

if (input_method == "allMask" & generate_xml == T){
  
  source(file.path(FUNC_PATH, "2.input_to_xml.R"))
  
  # calling the function
  input.to.xml(DATA_PATH, OUTPUT_PATH, RESOURCE_PATH,
               # arguments to retrieve input locations
               input_location,
               input_method,
               dist,
               curr_prod,
               # arguments to generate the cfg files
               sim_input_path,
               sim_output_path,
               
               transport_duration,
               first_release_year,
               last_release_year,
               release_frequency,
               record_frequency,
               n_cfg_per_dir,
               n_pbs_jobs
  )
}
