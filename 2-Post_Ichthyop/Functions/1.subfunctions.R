#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-09-16
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description :  Sub-functions of the initialization sub-routine and
#' of the 1st sub-routine which gets, for each input point, the number of associated coastal points
#' and cover points (coastal and in river buffer)
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************
#'
#'


#######################################################################################
#                SUBFUNCTION WHICH GENERATES THE SIMULATION NAME FROM                 #
#                         ALL THE SIMULATION ARGUMENTS                                #
#######################################################################################
# ARGUMENTS:                                                                          #
# forcing (chr): forcing product used ("oscar","globcurrent","nemo", or "nemo15m")    #
# input_location (chr): "kFromCoast" or "onMask"                                      #
# input_location (chr): "river" or "mangrove"                                         #
# dist (num): if input_method = "kFromCoast", the distance from the coast on the input#
# dispersion (num): the dispersion coefficient used in the simualtion (enter 9 for a  #
#                   coefficient equal to 10^-9                                        #
# bouncing (log): behaviour of particles on the coast (T: bouncing, F: beaching)      #
# ltime (num): life time of the particles                                             #
# ltime_method (num): either 1 or 2, see life.time() in 8.Post_ichthyop/1.1.2...R for #
#                     details of the methods                                          #
#                                                                                     #
# OUTPUT:                                                                             #
# sim_name (chr): name of the folder containing the simulation results                #
#######################################################################################

generate.sim_name <- function(forcing,
                              input_location,
                              input_method,
                              dist,
                              bouncing,
                              ltime = 0,
                              ltime_method = 0){
  
  sim_name <- paste0(forcing,"_",input_location)
  
  if(input_method == "kFromCoast"){
    sim_name <- paste0(sim_name, "_",dist, input_method)
  } else if (input_method %in% c("onMask","allMask")){
    sim_name <- paste0(sim_name, "_", input_method)
  }
  
  if (bouncing == T){
    sim_name <- paste0(sim_name,"_bouncing")
  }
  
  if (ltime != 0){
    sim_name <- paste0(sim_name,"_ltime",ltime_method,"-",ltime)
  }
  
  return(sim_name)
  
}


#######################################################################################
#            GET THE NUMBER OF COVER POINTS ASSOCIATED WITH AN INPUT POINT            #
#######################################################################################
# ARGUMENTS:                                                                          #
# indexes (vector): number of the sub-sampled cover points used                       #
# coastal_cover (sf): cover points, after the ones associated with rivers were removed#
# input_points (sf): points used as input in Ichthyop                                 #
# count_cover (log): if T, take the "couvert" column into account and return the      #
#                          surface of forest                                          #
#                    if F, return the total surface in the coastal cover (forest +    #
#                          non forest)                                                #
#######################################################################################

get.nb.cover.per.input <- function(indexes, coastal_cover, input_points,
                                   count_cover = T){
  
  if (count_cover == F){
    sub_coastal_cover <- data.frame(cbind(coastal_cover$id[indexes],
                                          st_coordinates(coastal_cover[indexes,])))
    names(sub_coastal_cover) <- c("id_cover", "x", "y")
  } else if (count_cover == T){
    sub_coastal_cover <- data.frame(cbind(coastal_cover$id[indexes],
                                          coastal_cover$couvert[indexes],
                                          st_coordinates(coastal_cover[indexes,])))
    names(sub_coastal_cover) <- c("id_cover", "couvert", "x", "y")
  }

  x_input <- t(matrix(input_points$x,
                      nrow = length(input_points$x),
                      ncol = length(sub_coastal_cover$x)))
  
  x_cover <- matrix(sub_coastal_cover$x,
                    nrow = length(sub_coastal_cover$x),
                    ncol = length(input_points$x))
  
  y_input <- t(matrix(input_points$y,
                      nrow = length(input_points$y),
                      ncol = length(sub_coastal_cover$y)))
  
  y_cover <- matrix(sub_coastal_cover$y,
                    nrow = length(sub_coastal_cover$y),
                    ncol = length(input_points$y))
  
  
  dist_mat <- sqrt((x_cover - x_input)^2 + (y_cover - y_input)^2)
  
  n_closest_points <- unlist(lapply(apply(dist_mat, 1, function(x) which(x == min(x))), function(x) length(x)))
  if (any(n_closest_points > 1)){
    stop("Error: some cover points are at the exact same distance from two input points")
  }
  
  # get the closest input point for each cover point
  if (count_cover == F){
    n_cover_per_points <- summary( as.factor( input_points$id_curr[apply(dist_mat, 1, function(x) which(x == min(x)))] ), maxsum = 10^3)
    
    if (sum(n_cover_per_points) != dim(sub_coastal_cover)[1]){
      stop("Error: the number of associated points is not the same than the number of points to initialy associate")
    }
    
    # multiply by 8100 because each point correspond to 90x90m cells, hence cells of 8100m2
    n_cover_per_points <- n_cover_per_points*8100
    
  } else if (count_cover == T){
    associated_input_point <- data.frame(cbind(point_to_which_add = input_points$id_curr[apply(dist_mat, 1, function(x) which(x == min(x)))],
                                           surface_to_add_m2 = (sub_coastal_cover$couvert/100)*8100)) # /100 : from percentage to fraction; *8100: from number of cells to m2
    n_cover_per_points <- ddply(associated_input_point, "point_to_which_add", summarise, n = sum(surface_to_add_m2))$n
    names(n_cover_per_points) <- ddply(associated_input_point, "point_to_which_add", summarise, n = sum(surface_to_add_m2))$point_to_which_add
    
    if (sum(n_cover_per_points) != sum(8100*sub_coastal_cover$couvert/100)){
      stop("Error: the number of associated COVER points is not the same than the number of points to initialy associate")
    }
  }
  
  
  objects.list <- c("sub_coastal_cover","x_input","x_cover","y_input","y_cover","dist_mat")
  rm(list = objects.list) ; invisible(gc())
  
  return(n_cover_per_points)
  
}

#'@sub-function 3
#'***************
#' filter sf_points, to keep only the points in the study area (saved in the Resources folder)
#'     buffer_size: buffer used to also keep the points that are slightly inland (@! in m)
#'     return_format: either 'sf' or 'df'

keep.which.is.in.IO <- function(RESOURCE_PATH,
                                sf_points,
                                buffer_size,
                                return_format){
  
  # get column names
  names_for_df <- c(grep("geometry", names(sf_points), invert = T, value = T),
                    "x","y")
  
  # read IO boundaries from resources
  IO_boundaries <- read_sf(file.path(RESOURCE_PATH, "IO_definition.shp"))
  
  # get the points which are contained in the IO polygon
  contain <- st_contains(IO_boundaries %>%
                           st_transform(3857) %>%
                           st_buffer(dist = buffer_size),
                         sf_points %>% st_transform(3857))
  
  points_of_interest <- unique(unlist(contain))
  
  # keep only the contained points
  sf_points[points_of_interest,] -> sf_points_of_interest
  
  # return the result
  if (return_format == "sf"){
    return(sf_points_of_interest)
  } else if (return_format == "df"){
    df_points_of_interest <- as.data.frame(cbind(sf_points_of_interest %>% as.data.frame() %>% select(-geometry),
                                                 st_coordinates(sf_points_of_interest)))
    names(df_points_of_interest) <- names_for_df
    return(df_points_of_interest)
  }
  
}


#'@sub-function 4
#'***************
#' get the bbox of the forcing product used for the Ichthyop simulation
#'     forcing (chr): one of nemo, nemo15m, oscar, globcurrent, PHILIN12.L75

get.forcing.bbox <- function(RESOURCE_PATH, forcing){
  
  if (forcing %in% c("nemo","nemo15m","PHILIN12.L75")){
    if (forcing == "nemo15m"){forcing <- 'nemo'}
    mask <- ncdf4::nc_open(file.path(RESOURCE_PATH, "masks_current_products", paste0(forcing,".nc")))
    
    lon_name <- "longitude"
    lat_name <- "latitude"
    if(forcing == "PHILIN12.L75"){lon_name <- "nav_lon"; lat_name <- "nav_lat"}
    
    bbox <- c(xmin = min(ncdf4::ncvar_get(mask, varid = lon_name)),
              ymin = min(ncdf4::ncvar_get(mask, varid = lat_name)),
              xmax = max(ncdf4::ncvar_get(mask, varid = lon_name)),
              ymax = max(ncdf4::ncvar_get(mask, varid = lat_name)))
    
    ncdf4::nc_close(mask)
    
  } else if (forcing %in% c("oscar", "globcurrent")){
    mask <- read_sf(file.path(RESOURCE_PATH, "masks_current_products", paste0(forcing,".shp")))
    
    bbox <- as.numeric(st_bbox(mask))
    names(bbox) <- c("xmin","ymin","xmax","ymax")
  }
  
  return(bbox)
}

#'@sub-function 5
#'***************
#' rewriting of sf::st_crop, which works only for sf objects with POINT geometry
#'   bbox (num): object of class bbox or vector with, in that order, (xmin,ymin,xmax,ymax)

faster.st_crop.points <- function(points_sf, bbox){
  
  points_sf <- points_sf[
    (data.frame(st_coordinates(points_sf)) %>%
       mutate(is_to_keep = mapply(all,
                                  ifelse(X>=bbox[1],T,F),
                                  ifelse(Y>=bbox[2],T,F),
                                  ifelse(X<=bbox[3],T,F),
                                  ifelse(Y<=bbox[4],T,F))))$is_to_keep,
    ]
  
  return(points_sf)
}
