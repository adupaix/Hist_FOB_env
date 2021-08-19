#!/usr/bin/env Rscript

#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-19
#'@email : 
#'#*******************************************************************************************************************
#'@description :  Script to transform Ichthyop output to array and save in .rds
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

#'@libraries
#'**********
library(ncdf4)
library(raster)
library(foreach)
library(abind)

package.list <- c("ncdf4","raster","foreach","abind")

#'@sub-function
#'***************
#'
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


#'@read_args
#'**********
#'This script must be launched from the terminal as follow:
#' Rscript ichth_to_rds.R [path to sub-dir containing the outputs from 1 ichthyop simulation]
Args = commandArgs(trailingOnly = T)

sub_path <- as.character(Args[1])

year <- as.character(Args[2])

point_nb <- gsub(".*?/", "", sub_path)


dir = file.path("/home1/scratch/adupaix/ichthyop-output",year)
# dir = "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/0-Data/Output_Ichthyop/PHILIN12.L75_river_allMask/"

point_dir <- file.path(dir, sub_path)

origin_time = "year 1900 month 01 day 01 at 00:00"

gsize = 2

sims <- list.files(point_dir, pattern = ".nc")
  
for (j in 1:length(sims)){
    
  nc <- nc_open(file.path(point_dir, sims[j]))
    
  ### 1. Recupere tous les pas de temps de la simulation ----
  ## time stamps
  time <- ncvar_get(nc, varid = "time")
    
  ## try to read the attribute origin of time. In some cases, the attribute is not saved by Ichthyop, to
  ## solve a bug with the mask (PHILIN products). Then, if there is no attribute origin, we fix the
  ## date of origin on 1900/01/01 at 00:00 ~~~  ADD AS A PARAMETER IN main.R
  t0 <- try(t0 <- ncatt_get(nc, varid="time", attname = "origin"), silent = T)
  if (class(t0) == "list"){ t0 <- origin_time }
    
  position_date <- as.POSIXct(t0, tz="GMT", format = "year %Y month %m day %d at %H:%M") + 
    as.difftime(time/(3600*24),units = "days")
    
  nb_p_per_timestep <- foreach(k = seq(1, length(position_date),1),
                               .packages = package.list,
                               .combine = function(x,y) abind::abind(x,y, along = 3)) %do% {
                                   
                               longitude <- ncvar_get(nc, varid = "lon", start = c(1,k), count = c(-1,1)) #lit la longitude pour le pas de temps i
                               latitude <- ncvar_get(nc, varid = "lat", start = c(1,k), count = c(-1,1)) #lit la latitude pour le pas de temps i
                               mortality.i <- ncvar_get(nc, varid = "mortality", start = c(1,k), count = c(-1,1)) #lit la mortalite pour le pas de temps i
                                   
                               # remove not released, filtered and beached particles
                               longitude <- longitude[which(mortality.i == 0)]
                               latitude <- latitude[which(mortality.i == 0)]
                                   
                               # create a raster of the area of interest
                               r.i <- create.raster(gsize)
                                   
                               # get the cell number where each particle is
                               cell_pos <- cellFromXY(r.i, cbind(longitude,latitude))
                                   
                               # count the number of particles in each cell
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
    
  # rm(list=ls()[!ls() %in% 'nb_p_per_timestep'])
  # invisible(gc())
    
  outfile_name <- paste0(point_nb, "_", position_date[1], ".rds")
    
  saveRDS(nb_p_per_timestep, file = file.path(point_dir, outfile_name))
    
  nc_close(nc)

}
