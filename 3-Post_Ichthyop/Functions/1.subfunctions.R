
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
#                     CREATE AN EMPTY RASTER IN THE INDIAN OCEAN                      #
#######################################################################################
# ARGUMENTS:                                                                          #
# gsize (num): size of the grid cells, in degree (either 1, 2, or 5)                  #
#######################################################################################


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



#######################################################################################
#            GET THE NUMBER OF COVER POINTS ASSOCIATED WITH AN INPUT POINT            #
#######################################################################################
# ARGUMENTS:                                                                          #
# indexes (vector): number of the sub-sampled cover points used                       #
# coastal_cover (sf): cover points, after the ones associated with rivers were removed#
# input_points (sf): points used as input in Ichthyop                                 #
#######################################################################################

get.nb.cover.per.input <- function(indexes, coastal_cover, input_points){
  
  sub_coastal_cover <- data.frame(cbind(coastal_cover$id[indexes],
                                        st_coordinates(coastal_cover[indexes,])))
  names(sub_coastal_cover) <- c("id_cover", "x", "y")
  
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
  if (any(n_closest_points != 1)){
    stop("Error: some cover points are at the exact same distance from two input points")
  }
  
  # get the closest input point for each cover point
  n_cover_per_points <- summary( as.factor( input_points$id_curr[apply(dist_mat, 1, function(x) which(x == min(x)))] ), maxsum = 10^3)
  
  objects.list <- c("sub_coastal_cover","x_input","x_cover","y_input","y_cover","dist_mat")
  rm(list = objects.list) ; invisible(gc())
  
  return(n_cover_per_points)
  
}


#################################################################################
#                    DELETE THE POINTS IN THE CHINESE SEA                       #
#################################################################################
# ARGUMENTS :                                                                   #
# coords (data.frame) : coordinate of the points, in rows c("long","lat")       #
# class (chr) : class of the "coords". Either "df" if it's a data.frame with    #
#                                     points coordinates                        #
#                                    Or "sf" if the argument is an sf object    #
#               The output has the same class as "coords"                       #
#                                                                               #
# OUTPUT : data.frame with the coordinates without the points in the chinese sea#
#################################################################################

delete.chinese.sea <- function(coords, class){
  
  
  ##Polygon of the Chinese Sea
  N = matrix(data=NA,nrow=7,ncol=2)
  N[1,] = c(140,-3);
  N[2,] = c(115,-5);
  N[3,] = c(110,-5);
  N[4,] = c(102,5);
  N[5,] = c(102,40);
  N[6,] = c(140,40);
  N[7,] = N[1,];
  
  N <- data.frame(N)
  names(N) <- c("longitude","latitude")
  tmp = Polygon(N)
  ttt = Polygons(list(tmp),ID=1)
  polys = SpatialPolygons(list(ttt))
  crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  proj4string(polys) = crs
  
  if (class == "df"){
    
    names(coords) <- c("x","y")
    
    ##SpatialPoints with the coordinates
    SPoints = SpatialPoints(coords,proj4string = crs)
    
    ## SpatialPoints left
    rm_points <- gDifference(SPoints,polys)
    
    coords <- data.frame(rm_points@coords)
    rownames(coords) <- c()
  } else if (class == "sf"){
    polys <- st_as_sf(polys) %>% st_transform(st_crs(coords))
    
    coords <- st_difference(coords, polys)
  }
  
  return(coords)
}


#################################################################################
#                    DELETE THE POINTS IN THE MEDITERRANEAN                     #
#                        AND CASPIAN SEAS                                       #
#################################################################################
# ARGUMENTS :                                                                   #
# coords (data.frame) : coordinate of the points, in rows c("long","lat")       #
# class (chr) : class of the "coords". Either "df" if it's a data.frame with    #
#                                     points coordinates                        #
#                                    Or "sf" if the argument is an sf object    #
#               The output has the same class as "coords"                       #
#                                                                               #
# OUTPUT : data.frame with the coordinates without the points in the chinese sea#
#################################################################################

delete.med.and.caspian.sea <- function(coords, class){
  
  
  ##Polygon of the NW part of the study area
  N = matrix(data=NA,nrow=9,ncol=2)
  N[1,] = c(20,20);
  N[2,] = c(30,20);
  N[3,] = c(30,30.5);
  N[4,] = c(40,30.5);
  N[5,] = c(50,35);
  N[6,] = c(60,35);
  N[7,] = c(60,40);
  N[8,] = c(20,40);
  N[9,] = N[1,];
  
  N <- data.frame(N)
  names(N) <- c("longitude","latitude")
  tmp = Polygon(N)
  ttt = Polygons(list(tmp),ID=1)
  polys = SpatialPolygons(list(ttt))
  crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  proj4string(polys) = crs
  
  if (class == "df"){
    
    names(coords) <- c("x","y")
    
    ##SpatialPoints with the coordinates
    SPoints = SpatialPoints(coords,proj4string = crs)
    
    ## SpatialPoints left
    rm_points <- gDifference(SPoints,polys)
    
    coords <- data.frame(rm_points@coords)
    rownames(coords) <- c()
  } else if (class == "sf"){
    polys <- st_as_sf(polys) %>% st_transform(st_crs(coords))
    
    coords <- st_difference(coords, polys)
  }
  
  return(coords)
}



keep.which.is.in.IO <- function(RESOURCE_PATH,
                                sf_points,
                                buffer_size,
                                return_format){
  
  names_for_df <- c(grep("geometry", names(sf_points), invert = T, value = T),
                    "x","y")
  
  IO_boundaries <- read_sf(file.path(RESOURCE_PATH, "IO_definition.shp"))
  
  contain <- st_contains(IO_boundaries %>%
                           st_transform(3857) %>%
                           st_buffer(dist = buffer_size),
                         sf_points %>% st_transform(3857))
  
  points_of_interest <- unique(unlist(contain))
  
  sf_points[points_of_interest,] -> sf_points_of_interest
  
  if (return_format == "sf"){
    return(sf_points_of_interest)
  } else if (return_format == "df"){
    df_points_of_interest <- as.data.frame(cbind(sf_points_of_interest %>% as.data.frame() %>% select(-geometry),
                                                 st_coordinates(sf_points_of_interest)))
    names(df_points_of_interest) <- names_for_df
    return(df_points_of_interest)
  }
  
}
