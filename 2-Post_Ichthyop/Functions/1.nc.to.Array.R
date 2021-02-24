#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-02-24
#'@email : 
#'#*******************************************************************************************************************
#'@description :  reads one .nc file and generates a 3D array with the number of particles per cell
#' for each timestep
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************



nc.to.Array <- function(NC_PATH, nc_name, sim_nb, ltime = 180, ltime_method = 2,
                        input_method = "onMask", dist = 100, gsize = 2,
                        river_id, river_data, weight_method,
                        origin_time){
  
  tic(msg = paste0("    End sim. N ",sim_nb), quiet = F)
  cat(paste("\n   Simulation", sim_nb, "\n"))
  
  nc <- open.nc(file.path(NC_PATH, nc_name))
  
  ### 1. Recupere tous les pas de temps de la simulation ----
  ## time stamps
  time <- var.get.nc(nc, variable = "time")
  
  ## try to read the attribute origin of time. In some cases, the attribute is not saved by Ichthyop, to
  ## solve a bug with the mask (PHILIN products). Then, if there is no attribute origin, we fix the
  ## date of origin on 1900/01/01 at 00:00 ~~~  ADD AS A PARAMETER IN main.R
  t0 <- try(t0 <- att.get.nc(nc, variable="time", attribute = "origin"), silent = T)
  if (class(t0) == "try-error"){ t0 <- origin_time ; cat(bgRed(white("Manually setting time origin to:"))) ; cat(" ",origin_time, "\n")}
  
  position_date <- as.POSIXct(t0, tz="GMT", format = "year %Y month %m day %d at %H:%M") + 
    as.difftime(time/(3600*24),units = "days")
  
  ## Coupe les donnees a ltime si ltime_method = 2
  if (ltime_method == 2){
    is_before_ltime <- difftime(as.Date(position_date), as.Date(position_date[1]), units = "days") <= ltime
    position_date <- position_date[is_before_ltime]
  }
  
  
  ### 2. Recupere les river id correspondant a chaque particule ----
  ## Initial positions
  init_long <- var.get.nc(nc, variable = "lon", count = c(NA,1)) #lit la longitude pour le premier pas de temps
  init_lat <- var.get.nc(nc, variable = "lat", count = c(NA,1)) #lit la latitude pour le premier pas de temps
  init_pos <- as.data.frame(cbind(init_long, init_lat))
  
  # get the river_id corresponding to each particle et les stock dans la df init_pos
  if (input_location == "river"){
    init_pos %>% mutate(line_id = seq(1,dim(init_pos)[1],1)) -> init_pos
    
    init_pos %>% dplyr::filter(!is.na(init_long)) -> r_init_pos
    
    if (input_method == "kFromCoast"){
      # si la methode utilisee pour generee l'input est kFromCoast, il y a des particules lachees sur terre, donc on prend pour chaque particules,
      # la rivière la plus proche
      r_init_pos$river_id <- river_id$MAIN_RIV[nn2(river_id[,c("init_longitude","init_latitude")], r_init_pos[,1:2], k = 1)$nn.idx]
    } else if (input_method == "onMask"){
      # si les points ont ete relachee sur le mask directement, on n'a pas de NAs, et l'ordre des particules est conservé par Ichthyop
      # donc on copie juste MAIN_RIV de river_id
      r_init_pos$river_id <- river_id$MAIN_RIV
    }
    
    init_pos <- merge(init_pos, r_init_pos, all.x = T, sort = F)
  }
  
  
  ### 3. Met a jour la mortalite (pour appliquer des filtres) ----
  
  mortality <- var.get.nc(nc, variable = "mortality")
  
  ### a. filter the particles depending on the mean discharge of the river (threshold at thr_disch)
  # river_data is generated above, in read.ncs()
  if (input_location == "river"){
    init_pos <- merge(init_pos, river_data, by.x = "river_id", by.y = "MAIN_RIV", all.x = T)
    
    mortality[which(init_pos$is_bellow_thr),] <- -99
  }
  
  ### b. Filtre ltime si ltime_method = 1
  if (ltime_method == 1){
    
    # get the time between two positions
    t_between_pos <- as.numeric((lead(position_date) - position_date)[1], units = "days")
    
    # get life time values randomly from a log normal distribution
    ddays <- round(rlnorm(dim(mortality)[1], log(ltime/t_between_pos), sdlog = 0.1))
    
    # for every day after dday, replace mortality value by 1
    for (i in 1:dim(mortality)[1]){
      mortality[ i, min(ddays[i],dim(mortality)[2]) : dim(mortality)[2]] <- 1
    }
    
  }
  
   
  ### 4. calculate a weight per river_id, depending on the weight_method chosen above ----
  weight <- get.weight(init_pos, weight_method)
  
  nb_p_per_timestep <- foreach(i = seq(1, length(position_date),1),
                               .packages = c("raster"),
                               .combine = function(x,y) abind::abind(x,y, along = 3)) %do% {
                                 
                                 longitude <- var.get.nc(nc, variable = "lon", start = c(1,i), count = c(NA,1)) #lit la longitude pour le pas de temps i
                                 latitude <- var.get.nc(nc, variable = "lat", start = c(1,i), count = c(NA,1)) #lit la latitude pour le pas de temps i
                                 mortality.i <- mortality[,i] #lit la mortalite (modifiee en amont) pour le pas de temps i
                                 
                                 # remove not released, filtered (discharge threshold or ltime filter 1) and beached particles
                                 longitude <- longitude[which(mortality.i == 0)]
                                 latitude <- latitude[which(mortality.i == 0)]
                                 weight.i <- weight[which(mortality.i == 0)]
                                 
                                 # create a raster of the area of interest
                                 r.i <- create.raster(gsize)
                                 
                                 # get the cell number where each particle is
                                 cell_pos <- cellFromXY(r.i, cbind(longitude,latitude))
                                 
                                 # count the number of particles in each cell, applying the weight
                                 nb_p_per_cell <- xtabs(weight.i ~ cell_pos)
                                 
                                 # fill the raster
                                 # the attribute of nb_p_per_cell used contains the cell number
                                 
                                 r.i[ as.numeric(attr(nb_p_per_cell, "dimnames")$cell_pos) ] <- nb_p_per_cell
                                 
                                 as.matrix(r.i)
                                 
                               }
  
  
  dimnames(nb_p_per_timestep)[[3]] <- format(position_date, "%Y-%m-%d_%H:%M:%S")
  
  # remove the matrices with no particles (after ltime) from the array
  # in order to gain RAM
  has_particles_left <- apply(nb_p_per_timestep, 3, sum) != 0
  nb_p_per_timestep <- nb_p_per_timestep[,,which(has_particles_left)]
  
  toc()
  
  rm(list=ls()[!ls() %in% 'nb_p_per_timestep'])
  invisible(gc())
  
  return(nb_p_per_timestep)
  
}
