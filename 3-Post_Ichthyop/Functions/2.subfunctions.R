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
  
  link_table <- read.csv(file.path(OUTPUT_PATH, sim_name, "number_of_cover_points_per_input_point.csv"), header = T)
  
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
                                  filtered,
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
      point$rivers$dis_m3_pyr[i] <- filtered$dis_m3_pyr[filtered$MAIN_RIV == point$rivers$ids[i]]
      point$rivers$dis_m3_pmn[i] <- filtered$dis_m3_pmn[filtered$MAIN_RIV == point$rivers$ids[i]]
      point$rivers$dis_m3_pmx[i] <- filtered$dis_m3_pmx[filtered$MAIN_RIV == point$rivers$ids[i]]
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
#' forest_surface is in m2
get.number.of.cover.points <- function(link_table, point){
  
  point$nb_coastal_cover_points <- link_table$nb_coastal_cover_points[link_table$id_curr == as.numeric(point$id)]
  
  point$nb_cover_points <- link_table$nb_cover_points[link_table$id_curr == as.numeric(point$id)]
  
  point$forest_surface <- point$nb_cover_points * 900
  
  return(point)
}

#'
#'
#'
