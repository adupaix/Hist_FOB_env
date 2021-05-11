#!/usr/bin/env Rscript

library(RNetCDF)
library(crayon)
library(foreach)
library(raster)

# # get the current location
# initial.options <- commandArgs(trailingOnly = FALSE)
# file.arg.name <- "--file="
# script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
# this.dir <- dirname(script.name)

# args <- commandArgs(trailingOnly = TRUE)
# nb = args[1]
# gsize = args[2]
# sim_output_path = args[3]
# rds_output_path = args[4]

origin_time = "year 1900 month 01 day 01 at 00:00"

source(file.path(this.dir,"2.subfunctions.R"))

sim_files <- list.files(path = file.path(sim_output_path, nb), pattern = "\\.nc$")

arrays_per_sim <- lapply(sim_files, nc.to.Array,
                         gsize = gsize, origin_time = origin_time,
                         sim_dir = file.path(sim_output_path, nb))

try(file.remove(file.path(rds_output_path, list.files(rds_output_path))))

fname <- paste0("sim_from_point_", nb)

for (i in 1:length(arrays_per_sim)){
  # Recupere le numero des simulations
  nb <- as.numeric(sub("s", "", sub(".nc", "", unlist(strsplit(sim_files[i], "_"))[3])))
  
  saveRDS(arrays_per_sim[[i]], file.path(rds_output_path, paste0(fname, "_date_", nb, ".rds")))
}

# # d. Bind the results and aggregate per day ----
# #-----------------------------------------------
# 
# # bind the list of arrays into one global array
# glob_array <- abind::abind(arrays_per_sim, along = 3)
# rm(arrays_per_sim) ; invisible(gc())
# 
# # arrange the array by time
# glob_array <- glob_array[,,order(dimnames(glob_array)[[3]])]
# 
# nm <- dimnames(glob_array)[[3]]
# 
# # aggregate the array by timestamp, to have one data.frame per timestamp (instead of per timestamp per simulation)
# glob_array <- foreach(day.i = unique(nm),
#                       .combine = function(x,y) abind::abind(x,y, along = 3)) %do% {
# 
#                         array.i <- glob_array[,,which(dimnames(glob_array)[[3]] == day.i)]
#                         apply(array.i, c(1,2), sum)
# 
#                       }
# new_nm <- unique(nm)
# dimnames(glob_array)[[3]] <- new_nm
# 
# t <- paste(rep(0, 5), collapse = "")
# cfg_nb <- as.character(cfg_nb)
# nb <- paste0(substr(t, 1, nchar(t)-nchar(cfg_nb)), cfg_nb)
# fname <- paste0("sim_from_point_", nb, ".rds")
# 
# 
# saveRDS(glob_array, fname)

# create.nc("test.nc", format = "netcdf4")
# 
# nc_w <- open.nc("test.nc", write = T)
# 
# dim.def.nc(nc_w, "lon", dim(glob_array)[1])
# dim.def.nc(nc_w, "lat", dim(glob_array)[2])
# dim.def.nc(nc_w, "time", dim(glob_array)[3])
# 
# var.def.nc(nc_w, "dens", "NC_DOUBLE", c("lon", "lat", "time"))
# var.put.nc(nc_w, "dens", glob_array)
# att.put.nc(nc_w, "dens", "long_name", "NC_CHAR", "Number of particles per cell")
# 
# init_time <- as.POSIXct("1900-01-01_00:00:00")
# time <- as.numeric(difftime(new_nm, init_time, units = "secs"))
# var.def.nc(nc_w, "time", "NC_DOUBLE", c("time"))
# var.put.nc(nc_w, "time", time)
# att.put.nc(nc_w, "time", "unit", "NC_CHAR", "secs")
# att.put.nc(nc_w, "time", "init_time", "NC_CHAR", "1900-01-01_00:00:00")
# 
# close.nc(nc_w)
