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
get.precipitations <- function(precip,
                               point){
  
  x = point$x
  y = point$y
  
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
  
  return(point)
}

#'@sub-function 4
#'***************
#' get the information on the rivers associated with the input point of interest
get.associated.rivers.and.precip <- function(link_river_input,
                                             n_cover,
                                             embouchures,
                                             point,
                                             precip){
  
  point$rivers <- list()
  
  # get the river ids associated with the input point
  point$rivers$ids = (link_river_input %>% filter(id_curr == as.numeric(point$id)))$MAIN_RIV
  
  if(length(point$rivers$ids) != 0){
    
    point$rivers$data <- list()
    
    # get the river segments of the associated rivers with the associated data
    for (i in 1:length(point$rivers$ids)){
      
      # keep only the centroid of the segment
      data <- rivers_filtered %>% dplyr::filter(MAIN_RIV == point$rivers$ids[i]) %>%
        st_transform(3857) %>%
        st_centroid() %>%
        st_transform(4326)
      
      # get the precipitations associated with every segment
      xs <- st_coordinates(data)[,1]
      ys <- st_coordinates(data)[,2]
      
      data$precip <- NA
      
      for (k in 1:dim(data)[1]){
        x <- xs[k]
        y <- ys[k]
        
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
        data$precip[k] <- var.get.nc(precip, "precip", start = of_int, count = rep(1,3))
      }
      
      
      # get the number of cover points associated with each river segment
      data %>% left_join(n_cover_per_river, by = c("HYRIV_ID","MAIN_RIV")) %>%
        mutate(cover_surface = nb_river_cover_points * 900 / 10^6) -> data
      
      point$rivers$data[[i]] <- data
      
    }
  } else {
    
    point$rivers <- NA
    
  }
  
  
  return(point)
}


#'@sub-function 5
#'***************
#' get the number of forest cover points associated with the input point
#' forest_surface is in m2
get.number.of.cover.points <- function(link_table, point){
  
  point$nb_coastal_cover_points <- link_table$nb_coastal_cover_points[link_table$id_curr == as.numeric(point$id)]
  
  point$coastal_cover_surface <- point$nb_coastal_cover_points * 900/10^6
  
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
  
  #' method 1 : don't apply any weight
  w1 <- 1
  
  #' method 2: apply a weight depending on the associated length of coastline
  #' True homogeneous release on the coast
  w2 <- point$coastline_length
  
  #' method 3: apply a weight depending on the coastal area covered by forest
  #' take the total forest surface and substract the forest surface associated with
  #' river segments (others than the river mouth)
  w3 <- point$forest_surface
  if (!is.na(point$rivers)){
    w3 <- w3 - sum(unlist(lapply(point$rivers$data, function(x) x$cover_surface[x$HYRIV_ID != x$MAIN_RIV])))
  }
  
  
  #' method 4: apply a weight depending on the area covered by forest in the river basins multiplied by the river discharge (at each river segment)
  #' @modif: normaliser le debit (sinon w5 ~ w4) OU prendre en compte les unites ?
  if (!is.na(point$rivers)){
    w4 <- sum(unlist(lapply(lapply(point$rivers$data, function(x) x$dis_m3_pyr * x$cover_surface), sum)))
  } else {
    w4 <- 0
  }
  
  #' method 5: apply a weight depending on the total surface of cover associated with the input point
  w5 <- point$coastal_cover_surface + w4
  
  #' method 6: apply a weight depending on the coastal cover multiplied by the precipitations at the release point (and at the river mouths)
  #' @modif?
  w6 <- point$coastal_cover_surface * point$precip
  if (!is.na(point$rivers)){
    w6 <- w6 + 
      sum(unlist(lapply(point$rivers$data, function(x){
        x2 <- x %>% filter(HYRIV_ID == x$MAIN_RIV)
        return(x2$cover_surface * x2$precip)
      })))
  }
  
  
  #' method 7: apply a weight depending on the river cover multiplied by the precipitations at each river segments
  if (!is.na(point$rivers)){
    w7 <- sum(unlist(lapply(lapply(point$rivers$data, function(x) x$dis_m3_pyr * x$cover_surface * x$precip), sum)))
  } else {
    w7 <- 0
  }
  
  #' method 8 : apply a weight depending on the total cover associated with the release point multiplied by the precipitations
  w8 <- point$coastal_cover_surface * point$precip + w7
  
  #' method 9: apply a weight depending on the total cover associated with the release point
  #' @not_to_be_used_in_the_study
  w9 <- point$forest_surface
  
  #' object point is also in the function env (hence the -1)
  n_methods <- length(ls())-1
  
  
  return(unlist(mget(paste0("w", 1:n_methods))))
}

