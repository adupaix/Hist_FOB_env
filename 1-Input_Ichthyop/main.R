#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-02-24
#'@email : 
#'#*******************************************************************************************************************
#'@description :  This is the main script allowing to generate input points for Ichthyop
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


WD <- file.path(getwd(),'1-Input_Ichthyop/')

DATA_PATH <- file.path(getwd(), "0-Data/")

FUNC_PATH <- file.path(WD,"Functions")
RESOURCE_PATH <- file.path(WD,"Resources")
OUTPUT_PATH <- file.path(WD, "Outputs")


# ARGUMENTS:                                                                        
# ===========                                                                       
arguments <- list(DATA_PATH = DATA_PATH,
                  OUTPUT_PATH = OUTPUT_PATH,
                  RESOURCE_PATH = RESOURCE_PATH,
                  
## Run in parallel ?
# (in kFromCoast sub script)
# First element of the vector:
#    If F, runs in sequential
#    if T, runs in parallel
# Second element of the vector: fraction of the cores to be used
#
Parallel = c(T, 1/2),
                  
#                            
# save (logical) : if T, save a txt file ready for use in ichthyop                  
# return_format (chr): either "sf", returns an sf object with points and associated 
#                                   river characteristics                           
#                      or "mat", returns a matrix containing only the coordinates of
#                               the input points (as saved in the txt file)       
save = T,
return_format = "sf", # one of c("sf", "mat")

#                                                                                   
### input_location (chr): either "river" or "mangrove"                              
### input_method (chr): either "onMask" put points on the closest point on the      
#                                     current product                               
#                      or "kFromCoast" put points at dist km from the coast         
#
input_location = "river", # one of c("river","mangrove")
input_method = "onMask", # one of c("onMask","kFromCoast")

#                                                                                   
### dist (num) : distance from the coast at which we want the points (in deg)       
### curr_prod (chr): which current mask product to use if put the input points on   
#              the closest point of the product mask                                
#
dist = 1, # used if input_method == kFromCoast (distance from coast in degrees)
curr_prod = "PHILIN12.L75" # one of c("PHILIN12.L75","oscar","nemo","globcurrent","nemo15m") # used if input_method == onMask

)

# GETTING LIBRARIES:
# ==================

source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("dplyr","sf","rnaturalearthdata","progress","rnaturalearthdata","ggplot2",
                     "sp","rgeos","RNetCDF","gridExtra","rgdal","cleangeo","rworldmap",
                     "tictoc","RANN","spatialEco","geosphere","lwgeom","doParallel",
                     "parallel","units","abind")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)


# LOADING THE MAIN FUNCTION:
#==========================
source(file.path(FUNC_PATH, "1.nlog_inputs_modif.R"))

# calling the main function
output <- do.call(input.nlog, args = arguments)

