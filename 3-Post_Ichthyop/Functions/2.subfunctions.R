#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-29
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description :  Sub-functions of the 2nd sub-routine which weights the simulations and applies mortality
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************
#'
#'
#'@sub-function 1
#'***************
#'read the link table generate in the sub-routine 1
read.link_cover_input <- function(OUTPUT_PATH, sim_name){
  
  link_table <- read.csv(file.path(OUTPUT_PATH, sim_name, "1.nb_cover", "number_of_cover_points_per_input_point.csv"), header = T)
  
  return(link_table)
}


#'@sub-function 2
#'***************
#' get the coordinates of release associated with the input point of interest
get.coords.release <- function(sim_input_path, point){
  
  # get the coordinates of the point
  coords <- read.table(file.path(sim_input_path, "IDs.txt"))
  
  point$x <- coords[coords$V3 == as.numeric(point$id),1]
  point$y <- coords[coords$V3 == as.numeric(point$id),2]
  
  # get the coordinates of the points which are at less than 1/4° of the point
  # dists <- sqrt((point$x-coords[,1])^2 + (point$y-coords[,2])^2)
  # df <- coords[which(dists < 1/4 & coords$V3 != as.numeric(point$id)),]
  # names(df) <- c("x","y","id")
  # point$close_points <- df
  
  return(point)
}

#'@sub-function 3
#'***************
#' get the precipitations associated with the point of interest
get.precipitations <- function(DATA_PATH,
                               point){
  
  x = point$x
  y = point$y
  
  ## open the netcdf file
  precip <- open.nc(con = file.path(DATA_PATH,"precip.mon.mean.nc"))
  
  # select the interval of interest
  time <- var.get.nc(precip, "time")
  init_nc <- as.Date("1800-01-01")
  time <- as.difftime(time, units = "days") + init_nc
  time_of_int <- which(paste0(year(time),"-",month(time)) == paste0(year(point$release_date),"-",month(point$release_date)))
  
  
  lon <- var.get.nc(precip, "lon")
  lon_of_int <- which(abs(lon - x) == min(abs(lon - x)))
  
  lat <- var.get.nc(precip, "lat")
  lat_of_int <- which(abs(lat - y) == min(abs(lat - y)))
  
  of_int <- c(lon_of_int, lat_of_int, time_of_int)
  
  # get the precipitation value
  point$precip <- var.get.nc(precip, "precip", start = of_int, count = rep(1,3))
  
  # close the netcdf file
  close.nc(precip)
  
  return(point)
}

#'@sub-function 4
#'***************
#' get the information on the rivers associated with the input point of interest
get.associated.rivers <- function(sim_input_path,
                                  n_cover_per_river,
                                  embouchures,
                                  point){
  
  rivers <- read.table(file.path(sim_input_path, "Link_table.txt"), header = T)
  
  point$rivers <- list()
  
  point$rivers$ids = (rivers %>% filter(id_curr == as.numeric(point$id)))$MAIN_RIV
  
  if (length(point$rivers) != 0){
    
    point$rivers$cover <- c()
    point$rivers$dis_m3_pyr <- c()
    point$rivers$dis_m3_pmn <- c()
    point$rivers$dis_m3_pmx <- c()
    for (i in 1:length(point$rivers)){
      point$rivers$cover[i] <- n_cover_per_river$nb_river_cover_points[n_cover_per_river$MAIN_RIV == point$rivers[i]]
      point$rivers$dis_m3_pyr[i] <- embouchures$dis_m3_pyr[embouchures$MAIN_RIV == point$rivers$ids[i]]
      point$rivers$dis_m3_pmn[i] <- embouchures$dis_m3_pmn[embouchures$MAIN_RIV == point$rivers$ids[i]]
      point$rivers$dis_m3_pmx[i] <- embouchures$dis_m3_pmx[embouchures$MAIN_RIV == point$rivers$ids[i]]
    }
  } else {
    point$rivers$cover <- 0
    point$rivers$dis_m3_pyr <- 0
    point$rivers$dis_m3_pmn <- 0
    point$rivers$dis_m3_pmx <- 0
  }
  
  return(point)
}

#'@sub-function 5
#'***************
#' get the number of forest cover points associated with the input point
#' forest_surface is in km2
get.number.of.cover.points <- function(link_table, point){
  
  point$nb_coastal_cover_points <- link_table$nb_coastal_cover_points[link_table$id_curr == as.numeric(point$id)]
  
  point$nb_cover_points <- link_table$nb_cover_points[link_table$id_curr == as.numeric(point$id)]
  
  point$forest_surface <- point$nb_cover_points * 900 / 10^6
  
  return(point)
}


#' @sub-function 6
#' ***************
#' weight point$data according to the method used
add.weight <- function(point, weight_method){
  
  # method 1 : don't apply any weight
  if (weight_method == 1){
    weight <- 1
    
    # method 2 : apply a weight depending on the water discharge of the river
  }  else if (weight_method==2){
    weight <- sum(point$rivers$dis_m3_pyr)
    
    # method 3: apply a weight depending on the surface of cover associated with the input point
  } else if (weight_method == 3){
    weight <- point$nb_cover_points
    
    # method 4: apply a weight depending on the area covered by forest in the river basins
  } else if (weight_method == 4){
    weight <- point$nb_coastal_cover_points + sum(point$rivers$cover * point$rivers$dis_m3_pyr)
  }
  
  point$data <- point$data * weight
  
  return(point)
}


#'@sub-function 7
#'***************
#' apply the mortality on the density map (modify point$data)

apply.mortality <- function(point, ltime, ltime_method, sd = 30){
  
  timestep <- as.POSIXct(dimnames(point$data)[[3]][2]) - as.POSIXct(dimnames(point$data)[[3]][1])
  
  total_time_sim <- dim(point$data)[3]
  
  if (ltime_method == 1){
    
    x <- seq(1, total_time_sim, 1)
    
    prop_alive <- 1 - cumsum(dnorm(x, ltime/as.numeric(timestep), sd/as.numeric(timestep)))
    
  } else if (ltime_method == 2){
    
    prop_alive <- c( rep(1, ltime/as.numeric(timestep)), rep(0, (total_time_sim-ltime)/as.numeric(timestep)))
    
  }
  
  # multiply the matrix of each timestep by the proportion of particles alive
  point$data <- sweep(point$data, 3, prop_alive, "*")
  
  # delete dates when no particles are left, to gain space
  has_particles_left <- apply(point$data, 3, sum) != 0
  point$data <- point$data[,,which(has_particles_left)]
  
  return(point)
}
