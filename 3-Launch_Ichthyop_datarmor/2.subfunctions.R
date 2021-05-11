#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-05-04
#'@email : 
#'#*******************************************************************************************************************
#'@description :  reads one .nc file and generates a 3D array with the number of particles per cell
#' for each timestep
#'#*******************************************************************************************************************
#'@revision : simplified from 1.nc.to.Array.R
#'#*******************************************************************************************************************

nc.to.Array <- function(nc_name, gsize,
                        origin_time, sim_dir){
  
  nc <- open.nc(file.path(sim_dir,nc_name))
  
  ### 1. Recupere tous les pas de temps de la simulation ----
  ## time stamps
  time <- var.get.nc(nc, variable = "time")
  
  ## try to read the attribute origin of time. In some cases, the attribute is not saved by Ichthyop, to
  ## solve a bug with the mask (PHILIN products). Then, if there is no attribute origin, we fix the
  ## date of origin on 1900/01/01 at 00:00 ~~~  ADD AS A PARAMETER IN main.R
  t0 <- try(t0 <- att.get.nc(nc, variable="time", attribute = "origin"), silent = T)
  if (class(t0) == "try-error"){
    t0 <- origin_time
    # cat(bgRed(white("Manually setting time origin to:"))) ; cat(" ",origin_time, "\n")
  }
  
  position_date <- as.POSIXct(t0, tz="GMT", format = "year %Y month %m day %d at %H:%M") + 
    as.difftime(time/(3600*24),units = "days")
  
  nb_p_per_timestep <- foreach(i = seq(1, length(position_date),1),
                               .packages = c("raster"),
                               .combine = function(x,y) abind::abind(x,y, along = 3),
                               .export = c("create.raster")) %do% {
                                 
                                 longitude <- var.get.nc(nc, variable = "lon", start = c(1,i), count = c(NA,1)) #lit la longitude pour le pas de temps i
                                 latitude <- var.get.nc(nc, variable = "lat", start = c(1,i), count = c(NA,1)) #lit la latitude pour le pas de temps i
                                 mortality.i <- var.get.nc(nc, variable = "mortality", start = c(1,i), count = c(NA,1)) #lit la mortalite pour le pas de temps i

                                 # remove not released, filtered (discharge threshold or ltime filter 1) and beached particles
                                 longitude <- longitude[which(mortality.i == 0)]
                                 latitude <- latitude[which(mortality.i == 0)]

                                 # create a raster of the area of interest
                                 r.i <- create.raster(gsize)

                                 # get the cell number where each particle is
                                 cell_pos <- cellFromXY(r.i, cbind(longitude,latitude))

                                 # count the number of particles in each cell, applying the weight
                                 nb_p_per_cell <- table(cell_pos)

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
  
  rm(list=ls()[!ls() %in% 'nb_p_per_timestep'])
  invisible(gc())
  
  return(nb_p_per_timestep)
  
}


#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-05-04
#'@email : 
#'#*******************************************************************************************************************
#'@description :  create a raster with the correct dimensions in the Indian Ocean
#'#*******************************************************************************************************************
#'@revision : adapted from 1.subfunctions.R in 2-Post_Ichthyop/Functions
#'#*******************************************************************************************************************


create.raster <- function(gsize){
  
  xmin = 30
  xmax = 120
  ncols = (xmax - xmin) / gsize
  
  ymin = -40
  ymax = 30
  nrows = (ymax - ymin) / gsize
  
  r <- raster(
    nrows = nrows,
    ncols = ncols,
    xmn = xmin,
    xmx = xmax,
    ymn = ymin,
    ymx = ymax
    )
  
  r[] <- 0
  names(r) <- "occ"
  
  return(r)
}

