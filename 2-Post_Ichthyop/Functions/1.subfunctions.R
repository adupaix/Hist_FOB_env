
#######################################################################################
#                SUBFUNCTION WHICH GENERATES THE SIMULATION NAME FROM                 #
#                         ALL THE SIMULATION ARGUMENTS                                #
#######################################################################################
# ARGUMENTS:                                                                          #
# forcing (chr): forcing product used ("oscar","globcurrent","nemo", or "nemo15m")    #
# input_location (chr): "kFromCoast" or "onMask"                                      #
# input_location (chr): "river" or "mangrove"                                         #
# dist (num): if input_method = "kFromCoast", the distance from the coast on the input#
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

#######################################################################################
#                     GENERATE WEIGHTS DEPENDING ON THE CHOSEN METHOD                 #
#######################################################################################


get.weight <- function(init_pos, weight_method = 1){
  
  # method 1 : don't apply any weight
  if (weight_method == 1 | input_location == "mangrove"){
    return(rep(1, dim(init_pos)[1])) 
  
  # method 2 : apply a weight depending on the water discharge of the river
  }  else if (weight_method==2){
    return(init_pos$dis_m3_pyr)
  
  # method 3: apply a weight depending on the percentage of the river basin covered by forest
  } else if (weight_method == 3){
    return(init_pos$for_pc_use)
  
  # method 4: apply a weight depending on the area covered by forest in the river bassin
    # (percentage of the forest * river area in km2)
  } else if (weight_method == 4){
    return(init_pos$for_km_usu)
  }
  
}



#######################################################################################
#                     CREATE AN EMPTY RASTER IN THE INDIAN OCEAN                      #
#######################################################################################
# ARGUMENTS:                                                                          #
# gsize (num): size of the grid cells, in degree (either 1, 2, or 5)                  #
#######################################################################################


create.raster <- function(gsize){
  
  if (gsize == 5) {
    r <- raster(
      nrows = 14,
      ncols = 18,
      xmn = 30,
      xmx = 120,
      ymn = -40,
      ymx = 30
    )
  } else if (gsize == 1) {
    r <- raster(
      nrows = 70,
      ncols = 90,
      xmn = 30,
      xmx = 120,
      ymn = -40,
      ymx = 30
    )
  } else if (gsize == 2) {
    r <- raster(
      nrows = 35,
      ncols = 45,
      xmn = 30,
      xmx = 120,
      ymn = -40,
      ymx = 30
    )
  }
  
  r[] <- 0
  names(r) <- "occ"
  
  return(r)
}


#######################################################################################
#       APPLY THE LIFE TIME FILTER (method 1) ON THE MORTALITY                        #
#######################################################################################
# ARGUMENTS:                                                                          #
# mortality.i (num vector): mortality vector of one particle (0=alive)                #
# ltime (num): mean life time of the particles                                        #
# t_between_pos (num): time in days seperating two positions in the vector mortality.i#
#######################################################################################


apply.ltime.1 <- function(mortality.i, ltime, t_between_pos){
  
  # timestep when the log sinks
  k <- round(rlnorm(1, log(ltime/t_between_pos), sdlog = 0.1))

  # each position after sinking are replaced by 1
  mortality.i[min(k,length(mortality.i)) : length(mortality.i)] <- 1

  return(mortality.i)
}
