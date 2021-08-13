#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-29
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description :  Sub-functions of the 2nd sub-routine which calculates the weights
#' for each (release point)-(release date) pairs
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************
#'
#'
#'@sub-function 1
#'***************
#'read the link table generated in the sub-routine 1
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
get.associated.rivers <- function(link_river_input,
                                  n_cover_per_river,
                                  embouchures,
                                  point){
  
  point$rivers <- list()
  
  # get the river ids associated with the input point
  point$rivers$ids = (link_river_input %>% filter(id_curr == as.numeric(point$id)))$MAIN_RIV
  
  # for each of the ids
  if (length(point$rivers$ids) != 0){
    
    # get the forest cover and the discharge
    point$rivers$cover <- c()
    point$rivers$dis_m3_pyr <- c()
    point$rivers$dis_m3_pmn <- c()
    point$rivers$dis_m3_pmx <- c()
    for (i in 1:length(point$rivers$ids)){
      # if the river id is in the n_cover_per_river table, we get the cover value
      if (point$rivers$ids[i] %in% n_cover_per_river$MAIN_RIV){
        point$rivers$cover[i] <- n_cover_per_river$nb_river_cover_points[n_cover_per_river$MAIN_RIV == point$rivers$ids[i]]
      # else it mean that no forest was present in the river basin: put 0 in cover
      } else {
        point$rivers$cover[i] <- 0
      }
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
#' get the length of coastline associated with the point
get.coastline.length <- function(link_table, point){
  
  point$coastline_length <- link_table$nb_coastal_points[link_table$id_curr == as.numeric(point$id)]
  
  return(point)
  
}


#' @sub-function 7
#' ***************
#' weight point$data according to the method used
get.weights <- function(point){
  
  # method 1 : don't apply any weight
  w1 <- 1
  
  # method 2: apply a weight depending on the associated length of coastline
  # True homogeneous release on the coast
  w2 <- point$coastline_length
  
  # method 3 : apply a weight depending on the water discharge of the associated rivers
  w3 <- sum(point$rivers$dis_m3_pyr)
  
  # method 4: apply a weight depending on the surface of cover associated with the input point
  w4 <- point$forest_surface
  
  # method 5: apply a weight depending on the area covered by forest in the river basins
  w5 <- sum(point$rivers$cover * 900 / 10^6)
    
  # method 6: apply a weight depending on the coastal area covered by forest
  w6 <- point$forest_surface - sum(point$rivers$cover * 900 / 10^6)
  
  # method 7: apply a weight depending on the precipitations at the release point
  w7 <- point$precip
  
  return(c(w1,w2,w3,w4,w5,w6,w7))
}

