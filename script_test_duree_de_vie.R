#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-19
#'@email : 
#'#*******************************************************************************************************************
#'@description :  Script to determine the number of particles as a function of time
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


WD <- file.path(getwd(),'2-Post_Ichthyop/')

DATA_PATH <- file.path(getwd(), "0-Data/")

FUNC_PATH <- file.path(WD,"Functions")
RESOURCE_PATH <- file.path(WD,"Resources")
OUTPUT_PATH <- file.path(WD, "Outputs")

# Load functions:

source(file.path(FUNC_PATH, "1.nc.to.Array.R"))
source(file.path(FUNC_PATH, "1.read.river.data.R"))
source(file.path(FUNC_PATH, "1.subfunctions.R"))
source(file.path(FUNC_PATH,'2.subfunctions.R'))


### ltime (num): life time of logs to be used                         
### ltime_method (num) : FIXE A 1 POUR CETTE ETUDE:  1. mean life time at ltime
ltime=180

### weight_method (num): method used to give weight to input points
# can be different from one only if the input_location is river
#   1. homogeneous weight for each particle
#   2. weight proportional to mean water discharge of rivers
#   3. weight proportional to the percentage of forest cover in the river basin
#   4. weight proportional to the surface of forest cover in the river basin
# FIXE A 1 POUR CETTE ETUDE

### size of the grid cells used (either 1, 2 or 5)
gsize = 2

### time scale used to aggregate the dates (day, month, quarter or year)
## !! quarter start in December: for example, first quarter of 2012 is from 12.2011 to 02.2012 included
agg.time_scale = "day"

### the mean water discharge of the input river is used as a filter 
# only particles originating from a river with a value > thr_disch (in m3/s) are kept 
# if thr_disch = 0, only rivers with a mean discharge > 0 m3/s will be kept
# if you don't want any filter to be used, set thr_disch = NULL
thr_disch = NULL

### origin of the time in the output results
# normally will not be used, as it is saved in the attributes of time by Ichthyop
# except for simulations with PHILIN75.L12, where because the product mask is bad,
# I had to overide the reading of attributes by Ichthyop
## !! the format of origin_time has to be kept as it is
origin_time = "year 1900 month 01 day 01 at 00:00"


# Arguments used for the Ichthyop simulation
#-------------------------------------------
### forcing (chr): forcing product used in ICHTHYOP simulation (oscar/globcurrent/nemo)     
forcing="PHILIN12.L75"

input_location = "river"
### input_method (chr): either "onMask" put points on the closest point on the      
#                                     current product                               
#                      or "kFromCoast" put points at dist km from the coast         
input_method = "allMask"

### dispersion (num): dispersion coefficient in ICHTHYOP simulation. Enter 9 for 10^-9, and 
#                   0 for no dispersion
dispersion=9

### bouncing (log): weither the particles in the ICHTHYOP simulation were bouncing on coast 
#                 (TRUE) or if they were beaching (FALSE, default)                        
#                                                                                         
bouncing=F

## Run in parallel ?
# First element of the vector:
#    If F, runs in sequential
#    if T, runs in parallel
# Second element of the vector: fraction of the cores to be used
#
## !! One part of the script can rapidly fill the RAM memory, to be taken into account when choose to run in parallel or not
## !! If the script is running on a Windows machine, the script is executed in sequential
Parallel = c(T, 1/2)



#'***************
#'@initialisation
#'***************

# Get libraries:
source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("RNetCDF","RANN","raster","foreach","tictoc","pryr","plyr","lubridate","parallel","crayon",
                     "dplyr","sf","ggplot2","ggspatial", "doParallel", "ggpubr","abind")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)


# Get simulation name:
sim_name <- generate.sim_name(forcing,
                              input_location,
                              input_method,
                              dist,
                              dispersion,
                              bouncing)


# For parallel study:
# On Windows, or if don't want to parralelize, set cores number to 1
if (.Platform$OS.type == "windows" | as.logical(Parallel[1]) == F) {
  nb_cores = 1
} else { #use a fraction of the available cores
  nb_cores = trunc(detectCores() * as.numeric(Parallel[2]))
}



#'***************
#'@reading_files
#'***************

NC_PATH <- file.path(DATA_PATH, "Output_Ichthyop", sim_name)
nc_files <- list.files(NC_PATH, pattern = "*.nc")

# check if there are files corresponding to the simulation of interest
if(length(nc_files)==0){
  sink(logName, append = T)
  cat("\n\n\n#####\nError: no available nc files for the given arguments\n#####")
  sink()
  stop("No available nc files for the given arguments")
}



simplified_read.nc <- function(NC_PATH, nc_name,
                               origin_time,
                               weight_method = 1
                               ){
  
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
  
  ### 2. Recupere les river id correspondant a chaque particule ----
  ## Initial positions
  init_long <- var.get.nc(nc, variable = "lon", count = c(NA,1)) #lit la longitude pour le premier pas de temps
  init_lat <- var.get.nc(nc, variable = "lat", count = c(NA,1)) #lit la latitude pour le premier pas de temps
  init_pos <- as.data.frame(cbind(init_long, init_lat))
  
  mortality <- var.get.nc(nc, variable = "mortality")
  weight <- get.weight(init_pos, weight_method)
  
  ### b. Filtre ltime si ltime_method = 1
  # get the time between two positions
  t_between_pos <- as.numeric((lead(position_date) - position_date)[1], units = "days")
    
  # get life time values randomly from a log normal distribution
  ddays <- round(rlnorm(dim(mortality)[1], log(ltime/t_between_pos), sdlog = 0.1))
    
  # for every day after dday, replace mortality value by 1
  for (i in 1:dim(mortality)[1]){
    mortality[ i, min(ddays[i],dim(mortality)[2]) : dim(mortality)[2]] <- 1
  }
    
  
  
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
  
  rm(list=ls()[!ls() %in% 'nb_p_per_timestep'])
  invisible(gc())
  
  return(nb_p_per_timestep)
  
}


cat("\n  Counting the number of particle per cell for each timestep")
arrays_per_sim <- parallel::mcmapply(simplified_read.nc, nc_name = nc_files,
                                     MoreArgs = list(NC_PATH = NC_PATH,
                                                     origin_time = origin_time),
                                     SIMPLIFY = F,
                                     mc.cores = nb_cores)


# bind the list of arrays into one global array
glob_array <- abind::abind(arrays_per_sim, along = 3)
# rm(arrays_per_sim) ; invisible(gc())

# arrange the array by time
glob_array <- glob_array[,,order(dimnames(glob_array)[[3]])]

nm <- dimnames(glob_array)[[3]]

# aggregate the array by timestamp, to have one data.frame per timestamp (instead of per timestamp per simulation)
glob_array <- foreach(day.i = unique(nm),
                      .combine = function(x,y) abind::abind(x,y, along = 3)) %do% {
                        
                        array.i <- glob_array[,,which(dimnames(glob_array)[[3]] == day.i)]
                        apply(array.i, c(1,2), sum)
                        
                      }
new_nm <- unique(nm)
dimnames(glob_array)[[3]] <- new_nm


nb_particles_per_day <- apply(glob_array, 3, sum)
toplot <- data.frame(cbind(nb_particles_per_day, names(nb_particles_per_day)))
rownames(toplot) <- NULL
names(toplot) <- c("nb", "date")
toplot$date <- as.POSIXct(toplot$date)
toplot$nb <- as.numeric(as.character(toplot$nb))

ggplot(toplot, aes(x=as.Date(date), y=nb))+
  geom_line(size = 0.2)+
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m")+
  xlab("Date")+
  ylab("Number of particles at sea")+
  ggtitle("Mean lifetime: 180 days")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5))



