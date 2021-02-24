#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-02-24
#'@email : 
#'#*******************************************************************************************************************
#'@description :  Main function reading river/mangrove data and generating Ichthyop input points
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


source(file.path(FUNC_PATH,'1.1.subfunctions.pre.R'))
source(file.path(FUNC_PATH,'1.2.subfunctions.general.R'))
source(file.path(FUNC_PATH,'1.2.write_zone_xml.R'))
source(file.path(FUNC_PATH,'1.3.subfunctions.post.R'))


#####################################################################################
#         READS RAW RIVER DATA and SAVE RDS FILE WITH RIVERS OF THE IO              #
#####################################################################################
# ARGUMENTS:                                                                        #
# ===========                                                                       #
# DATA_PATH: (chr) path to the data directory                                       #
# save (logical) : if T, save a txt file ready for use in ichthyop                  #
# return_format (chr): either "sf", returns an sf object with points and associated #
#                                   river characteristics                           #
#                      or "mat", returns a matrix containing only the coordinates of#
#                               the input points (as saved in the txt file)         #
#                                                                                   #
### input_location (chr): either "river" or "mangrove"                              #
### input_method (chr): either "onMask" put points on the closest point on the      #
#                                     current product                               #
#                      or "kFromCoast" put points at dist km from the coast         #
#                                                                                   #
### dist (num) : distance from the coast at which we want the points (in deg)       #
### curr_prod (chr): which current mask product to use if put the input points on   #
#              the closest point of the product mask                                #
#                                                                                   #
#                                                                                   #
#                                                                                   #
#                                                                                   #
# OUTPUT:                                                                           #
#####################################################################################


input.nlog <- function(DATA_PATH, OUTPUT_PATH, RESOURCE_PATH,
                       save = T,
                       return_format = "sf",
                       input_location = c("river","mangrove"),
                       input_method = c("onMask","kFromCoast"),
                       dist = 1, # used if input_method == kFromCoast
                       curr_prod = c("oscar","nemo"), # used if input_method == onMask
                       Parallel = c(T, 1/2) # wether to run in Parallel, used if input_method == kFromCoast
){
  
  cat("\14")
  cat(crayon::bold("###### Generating input points for Ichthyop simulations\n"))
  
  ### 1. GENERATE THE POINTS ON LAND USED TO DETERMINE INPUT LOCATIONS
  ## EITHER RIVER ESTUARIES
  ## OR MANGROVE LOCATIONS
  
  ### For simulations from the rivers
  if (input_location == "river"){ 
    INPUT_PATH <- file.path(DATA_PATH, "river_data")
    
    
    cat("\n### Reading data\n")
    tic(msg = "   Data read")
    # read data obtained from prep.river.data()
    rivers_IO <- readRDS(file.path(INPUT_PATH, "rivers_IO.rds"))
    
    # keep only discharge columns +
    # NEXT_DOWN (if == 0, means that it is the river segment on the coast)
    rivers_IO %>% dplyr::select(NEXT_DOWN, MAIN_RIV, ENDORHEIC, HYBAS_L12) %>%
      
    # select only the last segments of the rivers
      dplyr::filter(NEXT_DOWN == 0 & ENDORHEIC == 0) %>%
    
    # take the centroid of river segments
    st_centroid() -> entry_points
    
    toc()
    
    
    ### For simulations from the mangroves
  } else if (input_location == "mangrove"){
    
    cat("\14")
    cat("\n### Reading data\n")
    tic(msg = "   Data read")
    INPUT_PATH <- file.path(DATA_PATH, "mangrove")
    
    coord_points <- readRDS(file = file.path(INPUT_PATH,"centroids_mangrove_no_chinese_sea.rds"))
    
    toc()
    
    cat("\n### Sampling data (10000 points)\n")
    tic(msg = "   Data sampled")
    ## SAMPLING 10 000 POINTS OUT OF coord_points
    set.seed(10)
    coord_points <- coord_points[sample(1:nrow(coord_points),10000),]
    
    entry_points <- st_as_sf(coord_points, coords = c("x","y"), crs = 4326)
    toc()
  }
  
  ### 2. GET THE SEA POINTS
  ## EITHER AT N KM FROM COAST
  ## OR ON THE CURRENT PRODUCT MASK
  
  if (input_method == "kFromCoast"){
    
    cat("\n### Determining closest coastal points\n")
    tic(msg = "    Closest coastal points determined:")
    
    # echantillonne la ligne de cote tous les km
    # besoin de changer la projection pour st_line_sample() fonctionne
    # besoin de la ligne precise, parce que sinon certaines arrivees par les iles
    # disparaissent du jeu de donnees(a cause du buffer dans la fct closest.point)
    
    load(file.path(RESOURCE_PATH, "coastline10.rda")) # highres coastline, downloaded at : https://github.com/ropensci/rnaturalearthhires/tree/master/data (last accessed 2021-02-25)
    coastline10 <- st_as_sf(coastline10)
    coastline_points <- st_line_sample(st_transform(coastline10, 3857), density = set_units(3, km)) %>% # je ne sais pas pourquoi, en mettant 3km, les points sont espacés d'environ 1km...
      st_transform(crs = 4326) %>% # repasse a la projection initiale
      st_cast("POINT") %>% # passe en "POINT" (plutot que MULTIPOINT)
      st_as_sf() # change la classe en sf
    
    pos_entry_in_coastline <- nn2(st_coordinates(coastline_points), st_coordinates(entry_points), k = 1)$nn.idx
    
    B_coords <- st_coordinates(coastline_points[pos_entry_in_coastline,])
    
    entry_points <- cbind(entry_points, B_coords)
    
    B_points <- st_as_sf(st_drop_geometry(entry_points),
                         coords = c("X","Y"),
                         crs = 4326)
    
    rm(coastline_points) ; invisible(gc())
    
    toc()
    
    cat("\n### Get points at dist degrees from coast\n")
    tic(msg = "    Points at dist degrees from coast determined")
    
        # recupere la carte du monde
    world = st_as_sf(clgeo_Clean(getMap()))
    
    # For parallel study:
    # On Windows, or if don't want to parralelize, set cores number to 1
    if (.Platform$OS.type == "windows" | as.logical(Parallel[1]) == F) {
      nb_cores = 1
    } else { #use a fraction of the available cores
      nb_cores = trunc(detectCores() * as.numeric(Parallel[2]))
    }
    
    # Progress bar implemented in the foreach. Works only in sequencial
    pb <- progress_bar$new(total = dim(B_points)[1])
    
    
    registerDoParallel(cores = nb_cores)
    
    
    input_points <- foreach(i = 1:dim(B_points)[1],
                            .combine = rbind) %dopar% {
                              A <- st_coordinates(entry_points[i,])
                              B <- st_coordinates(B_points[i,])
                              
                              C <- sea.point2(A,B, world = world)
                              
                              pb$tick()
                              
                              C
                            }
    
    registerDoSEQ()
    
    input_points <- bind_cols(input_points, st_drop_geometry(B_points))
    
    toc()
    
  } else if (input_method == "onMask"){
    
    cat(paste("\n### Get the closest point on the", curr_prod, "grid\n"))
    tic(msg = paste("    Closest points on", curr_prod, "grid determined"))
    cat(" # Getting product mask\n")
    sea_points <- get.current.mask(RESOURCE_PATH, curr_prod)
    
    
    cat(" # Getting closest points\n")
    
    pos_entry_in_sea <- nn2(st_coordinates(sea_points), st_coordinates(entry_points), k = 1)$nn.idx
    
    input_coords <- st_coordinates(sea_points[pos_entry_in_sea,])
    
    entry_points <- cbind(entry_points, input_coords)
    
    input_points <- st_as_sf(st_drop_geometry(entry_points),
                             coords = c("X","Y"),
                             crs = 4326)
    
    toc()
    
  }
  
  
  ### 3. GENERATE THE FILES TO BE SAVED
  
  
  input_coords <- round(st_coordinates(input_points), digits = 6) #get only the coordinates for the Ichthyop Input
  
  if (input_location == "river"){
    input_coords_id <- data.frame(cbind(input_points$MAIN_RIV, input_points$HYBAS_L12, input_coords))
    names(input_coords_id) <- c("MAIN_RIV", "HYBAS_L12", "X", "Y")
  } else if (input_location == "mangrove"){
    input_coords_id <- input_coords
  }
  
  if (save==T){
    
    # create the name on the dir where files will be saved, depending on method and input location
    dir_name <- paste0(input_location, "_nlog_input_")
    if (input_method == "kFromCoast"){
      dir_name <- paste0(dir_name, dist*100,"k_from_coast")
    } else if (input_method == "onMask"){
      dir_name <- paste0(dir_name, curr_prod,"_mask")
    }
    
    cat("\n### Saving input locations\n")
    dir_path <- file.path(OUTPUT_PATH, dir_name)
    try(dir.create(path = dir_path))
    #save points coordinates for input in Ichthyop
    # quote = F to save coordinates (which are characters) as numeric
    write.table(input_coords, file = file.path(dir_path, "input_icht.txt"),
                row.names = F, col.names = F, sep=" ")
    #save points coordinates with river id
    write.table(input_coords_id, file = file.path(dir_path, "IDs.txt"),
                row.names = F,col.names = F, sep=" ")
    #save sf object for post treatement after Ichthyop simulations
    write_sf(input_points, dsn = dir_path,
             layer = dir_name,
             driver = "ESRI Shapefile")
  }
  
  if (return_format == "mat"){
    return(input_coords)
  } else if (return_format == "sf"){
    return(input_points)
  }
  
  
}
