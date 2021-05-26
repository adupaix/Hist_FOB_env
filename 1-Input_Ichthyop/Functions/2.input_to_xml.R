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
                         sim_input_path,
                         sim_output_path,
                         
                         transport_duration,
                         first_release_date,
                         last_release_date,
                         release_frequency,
                         record_frequency,
                         n_cfg_per_dir
){
  
  cat(crayon::bold("\n\n###### Generating xml cfg files from points\n"))
  
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
  input_coords <- read.table(file = file.path(dir_path, "IDs.txt"),
              header = F, sep=" ")
  
  # get the number of directories to generate
  n_dirs <- floor(dim(input_coords)[1]/n_cfg_per_dir)
  
  n_iter <- dim(input_coords)[1]
  pb <- txtProgressBar(min = 0, 
                       max = n_iter,
                       style = 3)
  
  # Get the cfg template path
  template <- file.path(RESOURCE_PATH, "template_cfg.xml")
  
  #Create the directory to save the cfg files
  cfg_dir <- file.path(OUTPUT_PATH, dir_name, "cfgs")
  dir.create(file.path(dir_path, "cfgs"), showWarnings = F)
  
  # generate the initial times from the start/end dates and the frequency
  initial_time <- as.POSIXct(paste(seq(as.Date(first_release_date),
                                       as.Date(last_release_date),
                                       as.difftime(release_frequency, units = "days")),
                                   "00:00"))
  
  #~ Read the template file
  tplate <- scan(template, what = "", sep = "\n", quiet = T)
  
  for (i in 1:n_dirs){
    
    t <- paste(rep(0, 2), collapse = "")
    i_chr <- as.character(i)
    i_chr <- paste0(substr(t, 1, nchar(t)-nchar(i_chr)), i_chr)
    
    dir_name.i <- file.path(cfg_dir, paste0("cfgs_", i_chr))
    
    dir.create(dir_name.i)
    
    for (j in 1:n_cfg_per_dir){
      long <- input_coords[n_cfg_per_dir*(i-1)+j,1]
      lat <- input_coords[n_cfg_per_dir*(i-1)+j,2]
      cfg_nb <- input_coords[n_cfg_per_dir*(i-1)+j,3]
      
      #~ Transform cfg_nb into "000xx" format
      t <- paste(rep(0, 5), collapse = "")
      cfg_nb <- as.character(cfg_nb)
      nb <- paste0(substr(t, 1, nchar(t)-nchar(cfg_nb)), cfg_nb)
      
      #~ Create the xml cfg for ichthyop
      write.cfg.xml(initial_time,
                    tplate,
                    sim_input_path,
                    sim_output_path,
                    record_frequency,
                    dir_name.i,
                    
                    long, lat,
                    nb
      )
      
      setTxtProgressBar(pb, n_cfg_per_dir*(i-1)+j)
    }
    
  }
  
  i=n_dirs+1
  t <- paste(rep(0, 2), collapse = "")
  i_chr <- as.character(i)
  i_chr <- paste0(substr(t, 1, nchar(t)-nchar(i_chr)), i_chr)
  
  dir_name.i <- file.path(cfg_dir, paste0("cfgs_", i_chr))
  
  dir.create(dir_name.i, showWarnings = F)
  
  for (j in 1:(n_iter - n_dirs*n_cfg_per_dir)){
    long <- input_coords[n_cfg_per_dir*(i-1)+j,1]
    lat <- input_coords[n_cfg_per_dir*(i-1)+j,2]
    cfg_nb <- input_coords[n_cfg_per_dir*(i-1)+j,3]
    
    #~ Transform cfg_nb into "000xx" format
    t <- paste(rep(0, 5), collapse = "")
    cfg_nb <- as.character(cfg_nb)
    nb <- paste0(substr(t, 1, nchar(t)-nchar(cfg_nb)), cfg_nb)
    
    #~ Create the xml cfg for ichthyop
    write.cfg.xml(initial_time,
                  tplate,
                  sim_input_path,
                  sim_output_path,
                  record_frequency,
                  dir_name.i,
                  
                  long, lat,
                  nb
    )
    
    setTxtProgressBar(pb, n_cfg_per_dir*(i-1)+j)
    
  }
  
  close(pb)
  
}


