
#######################################################################################
#                SUBFUNCTION WHICH GENERATES THE SIMULATION NAME FROM                 #
#                         ALL THE SIMULATION ARGUMENTS                                #
#######################################################################################
# ARGUMENTS:                                                                          #
# forcing (chr): forcing product used ("oscar","globcurrent","nemo", or "nemo15m")    #
# input_location (chr): "kFromCoast" or "onMask"                                      #
# input_location (chr): "river" or "mangrove"                                         #
# dist (num): if input_method = "kFromCoast", the distance from the coast on the input#
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
                              dispersion,
                              bouncing,
                              ltime = 0,
                              ltime_method = 0){
  
  sim_name <- paste0(forcing,"_",input_location)
  
  if(input_method == "kFromCoast"){
    sim_name <- paste0(sim_name, "_",dist, input_method)
  } else if (input_method %in% c("onMask","allMask")){
    sim_name <- paste0(sim_name, "_", input_method)
  }
  
  # sim_name <- paste0(sim_name,"_d",dispersion)
  
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
# 
# 
# get.weight <- function(init_pos, weight_method = 1){
#   
#   # method 1 : don't apply any weight
#   if (weight_method == 1 | input_location == "mangrove"){
#     return(rep(1, dim(init_pos)[1])) 
#   
#   # method 2 : apply a weight depending on the water discharge of the river
#   }  else if (weight_method==2){
#     return(init_pos$dis_m3_pyr)
#   
#   # method 3: apply a weight depending on the percentage of the river basin covered by forest
#   } else if (weight_method == 3){
#     return(init_pos$for_pc_use)
#   
#   # method 4: apply a weight depending on the area covered by forest in the river basin
#     # (percentage of forest * river area in km2)
#   } else if (weight_method == 4){
#     return(init_pos$for_km_usu)
#   }
#   
# }



#######################################################################################
#                     CREATE AN EMPTY RASTER IN THE INDIAN OCEAN                      #
#######################################################################################
# ARGUMENTS:                                                                          #
# gsize (num): size of the grid cells, in degree (either 1, 2, or 5)                  #
#######################################################################################


create.raster <- function(gsize){
  
  r <- raster(
    res = gsize,
    xmn = 20,
    xmx = 140,
    ymn = -40,
    ymx = 40
  )
  
  r[] <- 0
  names(r) <- "occ"
  
  return(r)
}



#######################################################################################
#            GET THE NUMBER OF COVER POINTS ASSOCIATED WITH AN INPUT POINT            #
#######################################################################################
# ARGUMENTS:                                                                          #
# indexes (vector): number of the sub-sampled cover points used                       #
# coastal_cover (sf): cover points, after the ones associated with rivers were removed#
# input_points (sf): points used as input in Ichthyop                                 #
#######################################################################################

get.nb.cover.per.input <- function(indexes, coastal_cover, input_points){
  
  sub_coastal_cover <- data.frame(cbind(coastal_cover$id[indexes],
                                        st_coordinates(coastal_cover[indexes,])))
  names(sub_coastal_cover) <- c("id_cover", "x", "y")
  
  x_input <- t(matrix(input_points$x,
                      nrow = length(input_points$x),
                      ncol = length(sub_coastal_cover$x)))
  
  x_cover <- matrix(sub_coastal_cover$x,
                    nrow = length(sub_coastal_cover$x),
                    ncol = length(input_points$x))
  
  y_input <- t(matrix(input_points$y,
                      nrow = length(input_points$y),
                      ncol = length(sub_coastal_cover$y)))
  
  y_cover <- matrix(sub_coastal_cover$y,
                    nrow = length(sub_coastal_cover$y),
                    ncol = length(input_points$y))
  
  
  dist_mat <- sqrt((x_cover - x_input)^2 + (y_cover - y_input)^2)
  
  # get the closest input point for each cover point
  n_cover_per_points <- summary( as.factor( input_points$id_curr[apply(dist_mat, 1, function(x) which(x == min(x)))] ))
  
  objects.list <- list("sub_coastal_cover","x_input","x_cover","y_input","y_cover","dist_mat")
  rm(list = objects.list) ; invisible(gc())
  
  return(n_cover_per_points)
  
}
