

#############################################################
#############################################################
#           SUB-FUNCTIONS USED IN input.nlog()              #
#         DURING THE INPUT POINTS CALCULATION               #
#############################################################
#############################################################



#################################################################################
#     FROM A POINT (point) AND A DATA FRAME OF SEVERAL POINTS (df_points)       #
#          RETURNS THE POINT OF df_points CLOSEST TO point                      #
#   columns associated with point are keeped in the returned sf object          #
#################################################################################
# ARGUMENTS :                                                                   #
#  point (sf): object with only one point                                       #
#  df_points (sf): object with several points                                   #
#                                                                               #
# OUTPUT : sf object with one point with the columns of point and df_points and #
#           with the geometry of the closest point to point in df_points        #
#################################################################################

closest.point <- function(point, df_points){
  
  ## starts by a buffer of 0.1 degree
  buffer = 0.1
  
  # sample the points of df_points which are in the 0.1 degree buffer around point
  sampl <- suppressMessages(suppressWarnings( st_crop(df_points, point %>% st_buffer(dist = buffer)) ))
  # keep the point of the sample which is at the minimum distance of point
  r_point <- sampl[which.min(st_distance(sampl, point)),]  %>%
    st_as_sf() %>% # transform in sf object
    bind_cols(st_drop_geometry(point))
  
  # if no point was contained in the buffer
  # we increase the buffer size and get the closest point again
  # the buffer size is increased until at least a point of df_points is in the buffer
  while(dim(r_point)[1] == 0){
    
    buffer <- buffer + 0.1
    
    sampl <- suppressMessages(suppressWarnings( st_crop(df_points, point %>% st_buffer(dist = buffer)) ))
    r_point <- sampl[which.min(st_distance(sampl, point)),]  %>%
      st_as_sf() %>% # transform in sf object
      bind_cols(st_drop_geometry(point)) # add the columns of the initial point
    
  }
  
  return(r_point)
}




#################################################################################
#               FROM ONE POINT CALCULATES THE CLOSEST POINT AT                  #
#                     A DISTANCE dist FROM THE COAST                            #
# adapted to nlog.input.method4.rivers()                                        #
#################################################################################
# ARGUMENTS :                                                                   #
# A (vector): coordinates of the point                                          #
# B (vector): coordinates of the closest point on the coastline                 #
# dist (num) : distance from the coast at which we want the point, in degrees   #
#                                                                               #
# OUTPUT : vector of the coordinates of the sea point                           #
#################################################################################

sea.point2 <- function(A, B, dist = 1, world = st_as_sf(clgeo_Clean(getMap()))){
  
  d <- sqrt( (A[1]-B[1])^2 + (A[2]-B[2])^2)
  C <-data.frame(matrix(nrow=2,ncol=2))
  if(B[1]==A[1] & B[2]==A[2]){
    return("Error1")
  }
  C[1,1] <- B[1] + dist*(B[1]-A[1])/d
  C[1,2] <- B[2] + dist*(B[2]-A[2])/d
  C[2,1] <- B[1] - dist*(B[1]-A[1])/d
  C[2,2] <- B[2] - dist*(B[2]-A[2])/d
  
  # If A is on land, we keep the C point which is the farther away from A
  # If A is in the sea (intersection with worldOI is null), we keep the C point closest to A
  sp <- st_as_sf(st_sfc(st_point(A)), crs = 4326) # transforme A en objet sf
  suppressMessages(in_land <- any(st_contains(world,sp, sparse = F)))# regarde si le point A est dans l'eau ou sur terre
  distances <- apply(C,1,distm,y=A) # distances entre les deux points C et A
  
  if(in_land){ # si le point A sur la terre
    C <- as.vector(C[which.max(distances),])
  } else{ # si le point A est en mer
    C <- as.vector(C[which.min(distances),])
  }
  
  C %>% as.numeric() %>% st_point() %>%
    st_sfc(crs = 4326) %>% st_as_sf() -> C
  
  return(C)
  
}


#####################################################################################
#     READS CURRENT MASK AND RETURNS AN SF OBJECT WITH ONLY THE SEA POINTS          #
#####################################################################################
# ARGUMENTS:                                                                        #
# RESOURCE_PATH: (chr) path to the data directory                                   #
# product (chr) : which current product ("nemo", "oscar")                           #
#                                                                                   #
# OUTPUT: sf (POINTS) with only the sea points of the NEMO mask                     #
#####################################################################################

get.current.mask <- function(RESOURCE_PATH, product = "nemo"){
  

  if (product == "nemo" | product == "nemo15m" | product == "PHILIN12.L75"){
    mask <- open.nc(file.path(RESOURCE_PATH, "masks_current_products","nemo.nc")) #ouvre le netcdf avec le masque
    
    ## get variables
    onland <-var.get.nc(mask, variable = "mask") # mask contains for each point either 1 = at sea, or 0 = on land
    longitude <- var.get.nc(mask, variable = "longitude")
    latitude <- var.get.nc(mask, variable = "latitude")
    
    coords <- expand.grid(longitude, latitude) # create a matrix with every combinations of long and lat
    names(coords) <- c("long", "lat")
    
    onland <- data.frame(is_at_sea = as.vector(onland)) #change the grid into one column
    
    land_sea <- as.data.frame(cbind(onland, coords))
    
    # sf with every point of the nemo grid, and the corresponding value
    ## (0 = on land, 1 = at sea)
    land_sea %>% dplyr::filter(is_at_sea == 1) %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) -> sea_points
    
  } else if (product == "PHILIN12.L75"){
    mask <- open.nc(file.path(RESOURCE_PATH, "masks_current_products", paste0(product,".nc"))) #ouvre le netcdf avec le masque
    
    ## get variables
    onland <-var.get.nc(mask, variable = "tmask") # mask contains for each point either 1 = at sea, or 0 = on land
    longitude <- var.get.nc(mask, variable = "nav_lon")
    latitude <- var.get.nc(mask, variable = "nav_lat")
    
    land_lon_lat = abind(onland, latitude, longitude, along = 3)
    
    land_sea = matrix(ncol = 3, nrow = dim(land_lon_lat)[1]*dim(land_lon_lat)[2])
    
    for (i in 1:dim(land_lon_lat)[1]){
      for (j in 1:dim(land_lon_lat)[2]){
        land_sea[i + dim(land_lon_lat)[1] * (j - 1), ] = land_lon_lat[i,j,]
      }
    }
    
    land_sea <- as.data.frame(land_sea)
    names(land_sea) <- c("is_at_sea","long","lat")
    
    # sf with every point of the nemo grid, and the corresponding value
    ## (0 = on land, 1 = at sea)
    land_sea %>% dplyr::filter(is_at_sea == 1) %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) -> sea_points
    
  } else if (product == "oscar" | product == "globcurrent"){
    
    sea_points <- read_sf(file.path(RESOURCE_PATH, "masks_current_products", paste0(product,".shp")))
    
  }
  
  return(sea_points)
  
}


