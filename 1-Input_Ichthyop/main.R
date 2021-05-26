#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-04-20
#'@email : 
#'#*******************************************************************************************************************
#'@description :  This is the main script to generate input points for Ichthyop simulations
#'#*******************************************************************************************************************
#'@revision
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
#'## xml_template (chr): resource file to generate cfg files from
#'## sim_input/output_path (chr): paths to save in the cfg files
#'
#'## transport_duration (num): number of days that the particles are to be transported (in days)
#'## first/last_release_date (chr): start and end dates of the Ichthyop simulations
#'## release_frequency (num): frequency at which the particles are released (in days)
#'## record_frequency (num): frequency at which the particles positions are to be save (in days)
#'
#'## n_cfg_per_dir (num): number of cfg files to save per directory
#'## ICHTHYOP_PATH (chr): path to where the Ichthyop version to be used is installed

generate_xml = T

xml_template <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/template_cfg.xml"

sim_input_path <- "/home/adupaix/Documents/ichthyop-private/input"
sim_output_path <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/ichthyop-output"

#~ Fixed arguments
transport_duration = 180 #in days

first_release_date = "1980/01/02"
last_release_date = "1981/12/02"
release_frequency = 14 # in days
record_frequency = 5 #in days (interval between two recorded positions)

n_cfg_per_dir = 560



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
               first_release_date,
               last_release_date,
               release_frequency,
               record_frequency,
               n_cfg_per_dir
  )
}
