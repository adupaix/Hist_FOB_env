
###################################################################################
#     GET THE INITIAL TRAJECTORIES (with no FOB type) DATA FROM SEVERAL YEARS     #
#                         to be used in plot.desactivation.buoy()                 #
###################################################################################
# ARGUMENTS:                                                                      #
# DATA_PATH: (chr) path to the data directory                                     #
# year: (vector) the years we're interested in                                    #
###################################################################################


get.init.GPS.data <- function(DATA_PATH, year){
  if (length(year)==1){
    filename = file.path(DATA_PATH,"buoys_data_location_filtered",paste0("buoys_data_location_filtered_", year, ".rds"))
    data<-readRDS(file = filename)
    rm(filename)
  } else {
    
    data <- list()
    for (i in 1:length(year)){
      filename = file.path(DATA_PATH,"buoys_data_location_filtered",paste0("buoys_data_location_filtered_", year[i], ".rds"))
      data[[i]]<-readRDS(file = filename)
    }
    data <- bind_rows(data)
  }
  data <- data[order(data$activation_date, data$position_date),]
  return(data)
}


###################################################################################
#              GET THE RECONSTRUCTED TRAJECTORIES DATA FROM SEVERAL YEARS         #
#                         to be used in dispersion analysis                       #
###################################################################################
# ARGUMENTS:                                                                      #
# DATA_PATH: (chr) path to the data directory                                     #
# year: (vector) the years we're interested in                                    #
###################################################################################


get.reg.GPS.data <- function(DATA_PATH, year){
  if (length(year)==1){
    filename = file.path(DATA_PATH,"regular_traj",paste0("GPS_with_fob_type_RegTime_", year, "_6h.rds"))
    data<-readRDS(file = filename)
    rm(filename)
  } else {
    
    data <- list()
    for (i in 1:length(year)){
      filename <- file.path(DATA_PATH,"regular_traj",paste0("GPS_with_fob_type_RegTime_", year[i], "_6h.rds"))
      data[[i]]<-readRDS(file = filename)
    }
    data <- bind_rows(data)
  }
  data <- data[order(data$trajId, data$time),]
  return(data)
}


###################################################################################
#                  GET THE FOB_LOCATION DATA FROM SEVERAL YEARS                   #
#                         to be used in regular.timestamp()                       #
###################################################################################
# ARGUMENTS:                                                                      #
# DATA_PATH: (chr) path to the data directory                                     #
# year: (vector) the years we're interested in                                    #
###################################################################################

get.GPS.data <- function(DATA_PATH, year){
  if (length(year)==1){
    filename = file.path(DATA_PATH,"location_fob",paste0("GPS_buoys_with_fob_type_", year, ".rds"))
    data<-readRDS(file = filename)
    rm(filename)
  } else {
    
    data <- list()
    for (i in 1:length(year)){
      filename <- file.path(DATA_PATH,"location_fob",paste0("GPS_buoys_with_fob_type_", year[i], ".rds"))
      data[[i]]<-readRDS(file = filename)
    }
    data <- bind_rows(data)
  }
  data <- data[order(data$buoy_traj_id, data$position_date),]
  return(data)
}


#######################################################################################
#           GET THE DATA FROM SEVERAL YEARS OF ICHTHYOP SIMULATIONS                   #
#                         to be used in plot.traj.occurence()                         #
#######################################################################################
# ARGUMENTS:                                                                          #
# DATA_PATH: (chr) path to the data directory                                         #
# sim_name: (chr) name of the simulation file, as generated in generate.sim_name()    #
# year: (vector) the years we're interested in                                        #
#######################################################################################

get.sim.data <- function(DATA_PATH, sim_name, year){
  if (length(year)==1){
    fichiers <- list.files(file.path(DATA_PATH, sim_name)) # list the files in the sim folder
    file_pos <- grep(pattern = year, fichiers) # get the position of the file containing the year of interest in its name
    filename = file.path(DATA_PATH, sim_name, fichiers[file_pos]) # complete path to the file of interest
    data<-readRDS(file = filename) # read the file
    rm(filename)
  } else {
    
    data <- list()
    fichiers <- list.files(file.path(DATA_PATH, sim_name)) # list the files in the sim folder
    for (i in 1:length(year)){
      file_pos <- grep(pattern = year[i], fichiers) # get the position of the file containing the year of interest in its name
      filename = file.path(DATA_PATH, sim_name, fichiers[file_pos]) # complete path to the file of interest
      data[[i]]<-readRDS(file = filename)
    }
    data <- bind_rows(data)
  }
  data <- data[order(data$trajId, data$position_date),]
  return(data)
}


#######################################################################################
#                SUBFUNCTION WHICH GENERATES THE SIMULATION NAME FROM                 #
#                         ALL THE SIMULATION ARGUMENTS                                #
#######################################################################################
# ARGUMENTS:                                                                          #
# forcing (chr): forcing product used ("oscar","globcurrent","nemo", or "nemo15m")    #
# input_location (chr): "kFromCoast" or "onMask"                                      #
# input_location (chr): "river" or "mangrove"                                         #
# dist (num): if input_method = "kFromCoast", the distance from the coast on the input#
#                                                                                     #
# timestep (num): the timestep used in the Ichthyop simulation                        #
# dispersion (num): the dispersion coefficient used in the simualtion (enter 9 for a  #
#                   coefficient equal to 10^-9                                        #
# bouncing (log): behaviour of particles on the coast (T: bouncing, F: beaching)      #
# ltime (num): life time of the particles                                             #
# ltime_method (num): either 1 or 2, see life.time() in 8.Post_ichthyop/1.1.2...R for #
#                     details of the methods                                          #
#                                                                                     #
# OUTPUT:                                                                             #
# sim_name (chr): name of the folder containing the simulation results                #
#######################################################################################

generate.sim_name <- function(forcing,
                              input_location,
                              input_method,
                              dist,
                              timestep,
                              dispersion,
                              bouncing,
                              ltime = 0,
                              ltime_method = 0){
  
  sim_name <- paste0(forcing,"_",input_location)
  
  if(input_method == "kFromCoast"){
    sim_name <- paste0(sim_name, "_",dist, input_method)
  } else if (input_method == "onMask"){
    sim_name <- paste0(sim_name, "_", input_method)
  }
  
  # if(input_location == "river"){
  #   sim_name <- paste0(sim_name,"_ds",thr_disch)
  # }
  
  sim_name <- paste0(sim_name, "_ts",timestep,"_d",dispersion)
  
  if (bouncing == T){
    sim_name <- paste0(sim_name,"_bouncing")
  }
  
  if (ltime != 0){
    sim_name <- paste0(sim_name,"_ltime",ltime_method,"-",ltime)
  }
  
  return(sim_name)
  
}

