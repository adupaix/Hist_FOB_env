

#######################################################################################
#     SUBFUNCTION OF plot.traj() WHICH READS DATA, CREATES A RASTER AND CALCULATES    #
#                    THE MEAN NB OF FOB PER DAY OF THE VAR COEFF                      #
#######################################################################################
# ARGUMENTS:                                                                          #
# year (vector or num): years for which the subfunction runs                          #
# weight_method (num) : method used to add a weight to particles                      #
#                             1. equal weight to every particles                      #
#                             2. weight depending on mean yearly river discharge      #
# all the other arguments are described in the main function header (plot.traj())     #
#                                                                                     #
# OUTPUT:                                                                             #
# Raster with the variation coefficient of the number of fob per cell per day or the  #
# mean number of for per cell per day                                                 #
#######################################################################################

### SUBFUNCTION TO DO THE STUDY PER YEAR IN ORDER TO SAVE MEMORY SPACE
### OTHERWISE THE SCRIPT DOES NOT RUN FOR SIMULATIONS

read.and.count.traj <- function(year,
                                simulation, DATA_PATH, sim_name, trimestre,
                                fob_type, gsize, coeff_var, timestamp,
                                weight_method = 1, thr_disch = 0) {
  
  cat(red("###### YEAR :", year, "\n"))
  tic(msg = "## READING DATA", quiet = FALSE)
  if (simulation == F) {
    data = get.reg.GPS.data(DATA_PATH, year)
  } else {
    data <- do.call(get.sim.data, args = list(DATA_PATH=DATA_PATH, sim_name=sim_name, year=year))
  }
  
  ## dividing data by trimester
  if (trimestre == FALSE) {
    data_trim <- data[data$fob_type == fob_type, ]
  } else if (trimestre == TRUE) {
    data_trim <- list()
    
    trim = list(c(12, 1, 2), c(3, 4, 5), c(6, 7, 8), c(9, 10, 11))
    
    for (i in 1:4) {
      data_trim[[i]] = data[data$fob_type == fob_type &
                              month(data$position_date) %in% trim[[i]], ]
      
      if (dim(data_trim[[i]])[1]==0){
        cat(red(paste("Error: no data for trimester", i)))
      }
    }
  }
  
  rm(data)
  gc()
  toc()
  
  ## b. CREATING RASTER ----
  ## WITH PROPER COORDINATES AND CONTAINING 0s
  tic(msg = "## CREATING RASTERS", quiet = FALSE)
  
  r <- create.raster(gsize)
  
  if (trimestre == TRUE) {
    r <- list(r, r, r, r)
  }
  toc()
  
  ## c. COUNTING THE OCCURRENCE PER CELL ----
  tic(msg = "## COUNTING OCCURENCE PER CELL", quiet = FALSE)
  if (trimestre == FALSE) {
    if (coeff_var == F) {
      
      data_trim <- add.weight.rivers(DATA_PATH, data_trim, weight_method, thr_disch)
      
      r <- total.nb.per.cell(data_trim, r)
      
      n_days = as.numeric(difftime(as.Date(paste0(year[length(year)],"-12-31")),as.Date(paste0(year[1],"-01-01")),units="days"))
      
      r <- from.tot.nb.to.mean.per.day(r, n_days, timestamp)
      
      
    } else if (coeff_var == T) {
      
      r <- coeff.var.per.cell(data_trim, r, timestamp)
      
    }
    
    
  } else if (trimestre == TRUE) {
    
    for (k in 1:length(trim)){
      
      if (coeff_var == F){
        
        data_trim[[k]] <- add.weight.rivers(DATA_PATH, data_trim[[k]], weight_method, thr_disch)
        
        r[[k]] <- total.nb.per.cell(data_trim[[k]], r[[k]])
        
        n_days=0
        for (j in 1:length(year)){
          n_days <- n_days + sum(days_in_month(as.Date(paste0(year[j],"-",trim[[i]],"-01"))))
        }
        
        r[[k]] <- from.tot.nb.to.mean.per.day(r[[k]], n_days, timestamp)
        
      } else if (coeff_var == T){
        
        r[[k]] <- coeff.var.per.cell(data_trim[[k]], r[[k]], timestamp)
        
      }
      
    }
    
  }
  
  rm(data_trim)
  gc()
  
  toc()
  
  return(r)
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
#                           COUNTS THE TOTAL NB OF FOB PER CELL                       #
#             applies a weight to positions if data contains a column "weight"        #
#######################################################################################
# ARGUMENTS:                                                                          #
# data_trim (df): data frame with all the GPS positions of reconstructed buoys        #
#                  trajectories or simulated particules trajectories                  #
# r (raster) : empty raster to be filled with the counts                              #
#                                                                                     #
# OUTPUT:                                                                             #
# Raster with the total number of FOB GPS emissions per cell                          #
#######################################################################################

total.nb.per.cell <- function(data_trim, r){
  
  if ("weight" %in% names(data_trim)){ # if the data contains a column with weights, use them
    weight <- data_trim$weight
  } else { # if it does not contain a column with weights, give a weight of 1 for each line
    weight <- rep(1, dim(data_trim)[1])
  }
  
  coords <-
    as.data.frame(cbind(data_trim$longitude, data_trim$latitude))
  sp <- SpatialPoints(coords)
  counts <- cellFromXY(r, sp) #counts: vector with the cell nb corresponding to each buoy position
  
  cells <- data.frame(cbind( counts = counts,
                             weight = weight))
  
  tab <- xtabs(weight ~ counts, data = cells) # count the number of occurence per cell, weighted by the weights given as argument
  
  r[as.numeric(names(tab))] <- tab
  
  return(r)
}



###
# Given a raster with the total number of GPS emissions per cell (r),
#     the total number of days in the study period (n_days),
#     and the time between two GPS emissions (timestamp),
#
# Returns a raster with the mean number of FOB per day in cells.

from.tot.nb.to.mean.per.day <- function(r, n_days, timestamp){
  r[] <- r[] / (24 / timestamp) #one emission per object per day
  
  # Divide the occurence numbers by the total number of days in the study period
  # in order to have a scale giving the mean number of FOB present per cell
  r[] <- r[] / n_days
  
  return(r)
}



#######################################################################################
#     CALCULATE THE VARIATION COEFFICIENT OF THE NUMBER OF FOB PER CELL PER DAY       #
#######################################################################################
# ARGUMENTS:                                                                          #
# data_trim (df): data frame with all the GPS positions of reconstructed buoys        #
#                  trajectories or simulated particules trajectories                  #
# r (raster) : empty raster to be filled with the counts                              #
# timestamp (num) : the time between two GPS emissions (in hours)                     #
#                                                                                     #
# OUTPUT:                                                                             #
# Raster with the variation coefficient of the number of fob per cell per day         #
#######################################################################################

coeff.var.per.cell <- function(data_trim, r, timestamp){
  
  name <- names(r)
  
  # selectionne les colonnes d'interet et en cree une avec le jour de l'emission GPS
  data_trim %>% dplyr::select(longitude, latitude, position_date) %>%
    mutate(day = as.Date(position_date)) -> coords
  
  # liste les jours avec des observations
  nb_days <- unique(as.Date(data_trim$position_date))
  
  stack <- list()
  
  # pour chaque jour
  for (i in 1:length(nb_days)) {
    r[] <- 0
    # garde uniquement les donnees de la journee
    coords %>%
      dplyr::filter(day == nb_days[i]) -> subset
    
    # compte le nombre d'occurences par cellule
    sp <- st_as_sf(subset,
                   coords = c("longitude", "latitude"),
                   crs = 4326)
    
    tab <- table(cellFromXY(r, st_coordinates(sp)))
    r[as.numeric(names(tab))] <- tab
    r[] <- r[] / (24 / timestamp)
    # rajoute le raster a la liste
    stack[[i]] <- r
    names(stack[[i]]) <- nb_days[i]
  }
  
  # transform la liste en stack
  stack <- stack(stack)
  # calcul l'ecart type
  r_sd <- calc(stack, sd)
  #calcul la moyenne
  r_mean <- calc(stack, mean)
  # calcul le coefficient de variation
  r <- r_sd / r_mean
  
  names(r) <- name
  
  return(r)
  
}


###########################################################################################
#                 ADD A COLUMN WITH WEIGHTS TO THE PARTICLES POSITIONS DATA FRAME         #
###########################################################################################
# ARGUMENTS :                                                                             #
# DATA_PATH (chr) : path to the directory containing data                                 #
# data (df) : contains long lat, mort and river_id generated from Ichthyop output         #
# weight_method (num): method to generate weights:                                        #
#                      1: no weight (1 to every particle)                                 #
#                      2: weighted by the mean discharge of the input river               #
# thr_disch (num): apply a filter on the particles depending on the discharge of the river#
#                   of input. If the threshold is at 0, does not apply any filter         #
#                                                                                         #
# OUTPUT : Data frame with the river id corresponding to each released particle           #
###########################################################################################


add.weight.rivers <- function(DATA_PATH, data, weight_method, thr_disch = 0){
  
  if ("river_id" %in% names(data)){ ## ajoute un poids seulement si la simulation a ete faite a partir des rivieres
    if (weight_method == 1 & thr_disch == 0){ 
      # if we don't filter by discharge and the weight is uniform, we only create a column weight
      data %>% mutate(weight = 1) -> data
      
      
    } else {
      # if we apply a filter or if we apply a weight
      
      # we read the river data
      rivers_IO <- readRDS(file.path(DATA_PATH, "river_data","rivers_IO.rds"))
      # and keep only the mean discharge and river_id of the estuaries segments
      rivers_IO %>% dplyr::filter(MAIN_RIV %in% data$river_id) %>%
        dplyr::filter(NEXT_DOWN == 0) %>%
        st_drop_geometry() %>%
        dplyr::select(MAIN_RIV, dis_m3_pyr) -> ref_river
      
      if (weight_method == 1){
        # if no weight is applied, we merge the simulation data with the river data
        # apply a filter on the discharge and create a column of weight containing ones
        merge(data, ref_river, by.x = "river_id", by.y = "MAIN_RIV") %>%
          dplyr::filter(dis_m3_pyr >= thr_disch) %>%
          mutate(weight = 1) %>%
          dplyr::select(-dis_m3_pyr) -> data
        
      } else if (weight_method == 2){
        
        # if a weight is applied (following method 2), we merge the simulation data with the river data
        # apply a filter on the discharge and keep the discharge as weight
        merge(data, ref_river, by.x = "river_id", by.y = "MAIN_RIV") %>%
          dplyr::filter(dis_m3_pyr >= thr_disch) %>%
          dplyr::rename("weight" = "dis_m3_pyr") -> data
        
      }
      
    }
      ## reflechir a comment on attribue le poids :
      # si on garde la valeur moyenne de debit des rivieres, on donne beaucoup de poids aux grosses rivieres (jusqu'a 400 fois plus). Donc la distribution finale sera principalement due a ces grosses rivieres
      # transformer en log ? Sous quelle justification ?
  
  }
  
  
  return(data)
}
