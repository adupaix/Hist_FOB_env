

#############################################################
#############################################################
#       SUB-FUNCTIONS USED BEFORE input.nlog()              #
#             FOR RESOURCE PREPARATION                      #
#############################################################
#############################################################



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
  N[1,] = c(140,-10);
  N[2,] = c(115,-5);
  N[3,] = c(110,-5);
  N[4,] = c(102,5);
  N[5,] = c(102,30);
  N[6,] = c(140,30);
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
    polys <- st_as_sf(polys)
    
    coords <- st_difference(coords, polys)
  }
  
  return(coords)
}




#####################################################################################
#         READS RAW RIVER DATA and SAVE RDS FILE WITH RIVERS OF THE IO              #
#                                                                                   #
#            RAW DATA IS ON THE "OTHER" PARTITION OF THE 1 Tb DISK                  #
#####################################################################################
# ARGUMENTS:                                                                        #
# DATA_PATH: (chr) path to the data directory                                       #
#                                                                                   #
# OUTPUT:                                                                           #
#####################################################################################

prep.river.data <- function(DATA_PATH){
  
  RIVER_PATH <- file.path(DATA_PATH, "river_data")
  ### Reading rivers data
  
  cat("READING DATA")
  tic(msg = "### READING DATA")
  africa <- read_sf(RIVER_PATH, "RiverATLAS_v10_af")
  mid_east <- read_sf(RIVER_PATH, "RiverATLAS_v10_eu")
  asia <- read_sf(RIVER_PATH, "RiverATLAS_v10_as")
  oceania <- read_sf(RIVER_PATH, "RiverATLAS_v10_au")
  IO <- read_sf(RIVER_PATH, "OI")
  toc ()
  
  ### Cropping data to keep only rivers around IO
  
  cat("CROPPING AFRICA DATA")
  tic(msg = "### CROPPING AFRICA DATA")
  africa_cropped <- st_crop(africa, st_bbox(IO))
  toc()
  
  cat("CROPPING MIDDLE EAST DATA")
  tic(msg = "### CROPPING MIDDLE EAST DATA")
  mid_east_cropped <- st_crop(mid_east, st_bbox(IO))
  toc()
  
  cat("CROPPING ASIA DATA")
  tic(msg = "### CROPPING ASIA DATA")
  asia_cropped <- st_crop(asia, st_bbox(IO))
  toc()
  
  cat("CROPPING OCEANIA DATA")
  tic(msg = "### CROPPING OCEANIA DATA")
  oceania_cropped <- st_crop(oceania, st_bbox(IO))
  toc()
  
  rm(africa, mid_east, asia, oceania)
  gc()
  
  cat("MERGING DATA AND DELETING CHINESE SEA")
  tic(msg = "### MERGING DATA AND DELETING CHINESE SEA")
  rivers_IO <- bind_rows(africa_cropped, mid_east_cropped, asia_cropped, oceania_cropped)
  rivers_IO <- delete.chinese.sea(rivers_IO, class = "sf")
  toc()
  
  cat("SAVING RDS FILE")
  tic(msg = "### SAVING RDS FILE")
  saveRDS(rivers_IO, file.path(DATA_PATH, "rivers_IO.rds"))
  toc()
  
}

#####################################################################################
#         READS ALL THE OSCAR nc FILES AND GENERATE A MASK OF POINTS WHICH ARE      #
#                              ALWAYS AT SEA                                        #
#####################################################################################
# ARGUMENTS:                                                                        #
# DATA_PATH: (chr) path to the data directory                                       #
# CURR_PATH: (chr) path to the directory containing all the current product data    #
#                                                                                   #
# OUTPUT: sf object (POINTS)                                                        #
#####################################################################################

generate.curr.mask <- function(CURR_PATH, DATA_PATH, RESOURCE_PATH, curr_prod, nb_cores = 5){
  
  if (curr_prod == "oscar"){
    curr_var <- "u"
    long_var <- "longitude"
    lat_var <- "latitude"
    
  } else if (curr_prod == "globcurrent"){
    curr_var <- "eastward_eulerian_current_velocity"
    long_var <- "lon"
    lat_var <- "lat"
    
  }
  
  cat("> Listing files \n")
  f <- list.files(CURR_PATH, pattern = "*.nc$") #only the files finishing by .nc
  
  nc <- open.nc(file.path(CURR_PATH, f[1]))
  longitude <- var.get.nc(nc, variable = long_var)
  latitude <- var.get.nc(nc, variable = lat_var)
  close.nc(nc)
  
  cat("> Generating a mask per file \n")
  
  registerDoParallel(cores = nb_cores)
  
  is_at_sea <- foreach(nc_file = f,
                       .combine = function(x,y){
                         z <- abind::abind(x,y, along = 3)
                         return(apply(z, c(1,2), any))
                         
                         }) %dopar% {
    
                                   # open the nc file of the year
                                   nc <- open.nc(file.path(CURR_PATH, nc_file))
                                   
                                   # generate a matrix with T is there is a current value (at sea), and F if there is an NA (on land)
                                   is_at_sea <- apply(var.get.nc(nc, curr_var), c(1,2), function(x) !anyNA(x))
                                   
                                   close.nc(nc)
                                   
                                   invisible(gc())
                                   
                                   is_at_sea
    
  }
  
  registerDoSEQ()
  
  cat("> Generating the global mask \n")
  # is_at_sea <- apply(is_on_land_per_file, c(1,2), function(x) any(x))
  dimnames(is_at_sea) <- list(longitude, latitude)
  # rm(is_on_land_per_file) ; invisible(gc())
  
  cat("> Transforming matrix in sf object\n")
  coords <- expand.grid(longitude, latitude)
  coords <- as.data.frame(coords)
  names(coords) <- c("long", "lat")
  
  coords$is_at_sea <- is_at_sea[which(round(as.numeric(dimnames(is_at_sea)[[1]]), digits = 4) == round(coords$long, digits = 4)) &
                                which(round(as.numeric(dimnames(is_at_sea)[[2]]), digits = 4) == round(coords$lat, digits = 4))]
  
  
  # sf with every point of the nemo grid, and the corresponding value
  ## (0 = on land, 1 = at sea)
  dimIO <- as.numeric(st_bbox(read_sf(DATA_PATH, "OI")))
  
  sea_points <- coords %>% dplyr::filter(is_at_sea == 1) %>%
    dplyr::filter(long > dimIO[1] & long < dimIO[3] & lat > dimIO[2] & lat < dimIO[4]) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  invisible(gc())
  
  cat("> Saving the sf object\n")
  st_write(sea_points, file.path(RESOURCE_PATH, "masks_current_products", paste0(curr_prod,".shp")))
  
}
