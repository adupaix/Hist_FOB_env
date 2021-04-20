#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-04-19
#'@email : 
#'#*******************************************************************************************************************
#'@description :  Function generating Ichthyop config files (xml) from the input_icht.txt
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

source(file.path(FUNC_PATH,'2.1.write_xml.R'))


#####################################################################################
#         READS INPUT TXT FILE and GENERATE ICHTHYOP CONFIG FILES                   #
#####################################################################################
# ARGUMENTS:                                                                        #
# ===========                                                                       #
# DATA_PATH: (chr) path to the data directory                                       #
# OUTPUT_PATH: (chr) path to the output directory                                   #
# RESOURCE_PATH: (chr) path to the directory containing the xml template            #
#                                                                                   #
### input_location (chr): either "river" or "mangrove"                              #
### input_method (chr): either "onMask" put points on the closest point on the      #
#                                     current product                               #
#                      or "kFromCoast" put points at dist km from the coast         #
#                      of "allMask" put points on all the points of the mask close  #
#                                   to the coast and generate a txt file for each   #
#                                   river/mangrove with the corresponding mask point#
#                                                                                   #
### dist (num) : distance from the coast at which we want the points (in deg)       #
### curr_prod (chr): which current mask product to use if put the input points on   #
#              the closest point of the product mask                                #
#                                                                                   #
#                                                                                   #
#                                                                                   #
#                                                                                   #
# OUTPUT:                                                                           #
#####################################################################################

input.to.xml <- function(DATA_PATH, OUTPUT_PATH, RESOURCE_PATH,
                         
                         # arguments to retrieve input locations
                         input_location = c("river","mangrove"),
                         input_method = c("onMask","kFromCoast", "allMask"),
                         dist = 1, # used if input_method == kFromCoast
                         curr_prod = c("oscar","nemo"), # used if input_method == onMask
                         
                         # arguments to generate the cfg files
                         transport_duration,
                         first_release_date,
                         last_release_date,
                         release_frequency,
                         ICHTHYOP_PATH
){
  
  #~ Get the file name
  dir_name <- paste0(input_location, "_nlog_input_")
  if (input_method == "kFromCoast"){
    dir_name <- paste0(dir_name, dist*100,"k_from_coast")
  } else if (input_method == "onMask"){
    dir_name <- paste0(dir_name, curr_prod,"_mask")
  } else if (input_method == "allMask"){
    dir_name <- paste0(dir_name, curr_prod, "_allMask")
  }
  
  dir_path <- file.path(OUTPUT_PATH, dir_name)
  
  # read the input coordinates generated in input.nlog()
  input_coords <- read.table(file = file.path(dir_path, "input_icht.txt"),
              header = F, sep=" ")
  input_long <- input_coords[,1]
  input_lat <- input_coords[,2]
  
  # Get the cfg template path
  template <- file.path(RESOURCE_PATH, "template_cfg.xml")
  
  #Create the directory to save the cfg files
  dir.create(file.path(dir_path, "cfg"))
  
  # generate the cfg numbers, to name the files
  cfg_nbs <- seq(1, dim(input_coords)[1], 1)
  
  # generate the paths to be saved in the cfg files
  ## path to where the Ichthyop outputs will be save
  sim_output_path <- file.path(ICHTHYOP_PATH, "output",
                               paste0(curr_prod, "_",
                                      input_location, "_",
                                      input_method, "_d9_pt",
                                      formatC(cfg_nbs, width = 5, flag = "0"),
                                      "/"))
  ## path to where the current products to use in the Ichthyop simulation are saved
  sim_input_path <- file.path(ICHTHYOP_PATH, "input/")
  
  # generate the initial times from the start/end dates and the frequency
  initial_time <- seq(as.POSIXct(paste(first_release_date, "00:00")),
                      as.POSIXct(paste(last_release_date, "00:00")),
                      as.difftime(release_frequency, units = "weeks"))
  
  # Function generating the cfg files from the template
  mapply(write.cfg.xml, long = input_long, lat = input_lat, cfg_nb = cfg_nbs,
         template = template, sim_output_path = sim_output_path,
         MoreArgs = list(transport_duration = transport_duration,
                         initial_time = initial_time,
                         sim_input_path = sim_input_path,
                         dir_path = dir_path))
  
}


