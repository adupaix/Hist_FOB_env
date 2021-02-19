

source(file.path(FUNC_PATH,'1.1.subfunctions.pre.R'))
source(file.path(FUNC_PATH,'1.2.subfunctions.general.R'))
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
### thr_disch (chr or num): threshold of river discharge. If "mean", keep only the  #
#                          rivers with discharge > to overall mean                  #
#                          If numerical, keep only the rivers with discharge >= to  #
#                          thr_disch (value in m3/s)                                #
#    CHANGED: THE PART ON THE THRESHOLD OF DISCHARGE WAS COMMENTED, THE FILTER IS   #
#             NOW APPLIED AFTER THE ICHTHYOP SIMULATION                             #
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
                       # thr_disch = 10, # used if input_location == river
                       dist = 1, # used if input_method == kFromCoast
                       curr_prod = c("oscar","nemo","nemo15m","globcurrent") # used if input_method == onMask
                       ){
  
  ### 1. GENERATE THE POINTS ON LAND USED TO DETERMINE INPUT LOCATIONS
    ## EITHER RIVER ESTUARIES
    ## OR MANGROVE LOCATIONS
    
    ### For simulations from the rivers (method 4 before)
    if (input_location == "river"){ 
      INPUT_PATH <- file.path(DATA_PATH, "river_data")
      
      cat("### READING DATA\n")
      tic(msg = "# READING DATA")
      # read data obtained from prep.river.data()
      rivers_IO <- readRDS(file.path(INPUT_PATH, "rivers_IO.rds"))
      
      # keep only discharge columns +
      # NEXT_DOWN (if == 0, means that it is the river segment on the coast)
      rivers_IO %>% dplyr::select(NEXT_DOWN, MAIN_RIV, ENDORHEIC, HYBAS_L12, dis_m3_pyr:dis_m3_pmx) -> rivers_IO_f
      
      toc()
      
      # cat("### FILTERING DATA WITH DISCHARGE THRESHOLD\n")
      # tic(msg = "# FILTERING DATA WITH DISCHARGE THRESHOLD")
      # if(thr_disch == "mean"){
      #   # keep only the segment with a mean yearly discharge >= the mean of all rivers
      #   rivers_IO_f %>% dplyr::filter(dis_m3_pyr >= mean(dis_m3_pyr)) %>%
      #     # NEXT_DOWN: id of the next downstream line segment (value 0 indicates a line with no downstream connection)
      #     # ENDORHEIC: indicator for endorheic basins (ie without surface flow connection to the ocean). 0 = not part of an endorheic bassin
      #     # cf. HydroRIVERS_TechDoc_v10.pdf
      #     dplyr::filter(NEXT_DOWN == 0 & ENDORHEIC == 0) -> rivers_IO_f
      #   thr_disch <- round(mean(rivers_IO_f$dis_m3_pyr),2)
      # } else if (is.numeric(thr_disch)){
      #   # keep only the segment with a mean yearly discharge >= the threshold
      #   rivers_IO_f %>% dplyr::filter(dis_m3_pyr >= thr_disch) %>%
      #     # NEXT_DOWN: id of the next downstream line segment (value 0 indicates a line with no downstream connection)
      #     # ENDORHEIC: indicator for endorheic basins (ie without surface flow connection to the ocean). 0 = not part of an endorheic bassin
      #     # cf. HydroRIVERS_TechDoc_v10.pdf
      #     dplyr::filter(NEXT_DOWN == 0 & ENDORHEIC == 0)-> rivers_IO_f
      # }
      # toc()
      
      # take the centroid of river segments
      #
      rivers_IO_f %>% st_centroid() -> entry_points
    
      
      ### For simulations from the mangroves (method 1(modified) & 2 before)  
    } else if (input_location == "mangrove"){
      
      cat("### READING DATA\n")
      tic(msg = "# READING DATA")
      INPUT_PATH <- file.path(DATA_PATH, "mangrove")
      
      coord_points <- readRDS(file = file.path(INPUT_PATH,"centroids_mangrove_no_chinese_sea.rds"))
      
      toc()
      
      cat("### SAMPLING DATA (10000 POINTS)\n")
      tic(msg = "# SAMPLING DATA (10000 POINTS)")
      ## SAMPLING 10 000 POINTS OUT OF coord_points
      set.seed(10)
      coord_points <- coord_points[sample(1:nrow(coord_points),10000),]
      
      entry_points <- st_as_sf(coord_points, coords = c("x","y"), crs = 4326)
      
    }
    
    ### 2. GET THE SEA POINTS
    ## EITHER AT N KM FROM COAST
    ## OR ON THE CURRENT PRODUCT MASK
    
    if (input_method == "kFromCoast"){
      
      cat("### DETERMINING CLOSEST COASTAL POINTS\n")
      tic(msg = "# DETERMINING CLOSEST COASTAL POINTS")
      
      # echantillonne la ligne de cote tous les 10km
      # besoin de changer la projection pour st_line_sample() fonctionne
      # besoin de la ligne precise, parce que sinon certaines arrivees par les iles
      # disparaissent du jeu de donnees(a cause du buffer dans la fct closest.point)
      
      load(file.path(RESOURCE_PATH, "coastline10.rda")) # highres coastline, downloaded at : https://github.com/ropensci/rnaturalearthhires/tree/master/data
      coastline10 <- st_as_sf(coastline10)
      coastline_points <- st_line_sample(st_transform(coastline10, 3857), density = 1/10000) %>%
        st_transform(crs = 4326) %>% # repasse a la projection initiale
        st_cast("POINT") # passe en "POINT" (plutot que MULTIPOINT)
      
      B_points <- list() #list the closest points to the entry points situated on the low resolution coastline
      pb <- progress_bar$new(total = dim(entry_points)[1])
      for(i in 1:dim(entry_points)[1]){
        ## do a buffer around the estuary point and keep only the coastline points contained in this buffer
        B_points[[i]] <- closest.point(entry_points[i,], coastline_points)
        
        pb$tick()
      }
      # bind the list into one sf object
      B_points <- bind_rows(B_points)
      
      toc()
      
      cat("### GET POINTS AT dist DEGREES FROM COAST\n")
      tic(msg = "# GET POINTS AT dist DEGREES FROM COAST")
      # recupere les points a dist degres de la cote, avec la fonction sea.point2
      C_points <- list()
      pb <- progress_bar$new(total = dim(B_points)[1])
      for (i in 1:dim(B_points)[1]){
        A <- st_coordinates(entry_points[i,])
        B <- st_coordinates(B_points[i,])
        C_points[[i]] <- sea.point2(A,B)
        C_points[[i]] <- bind_cols(C_points[[i]], st_drop_geometry(B_points[i,]))
        
        pb$tick()
      }
      input_points <- bind_rows(C_points)
      
      # world = st_as_sf(clgeo_Clean(getMap()))
      # Cinland <- st_contains(world, C_points, sparse = F)
      # C_points <- C_points[- which(apply(Cinland, 2, sum) == 1),] # supprime les points C qui sont sur la terre (Error2)
      
      toc()
      
    } else if (input_method == "onMask"){
      
      cat(paste("### GET CLOSEST POINT ON THE", curr_prod, "GRID\n"))
      tic(msg = paste("# GET CLOSEST POINT ON THE", curr_prod, "GRID"))
      cat("# GETTING PRODUCT MASK\n")
      sea_points <- get.current.mask(RESOURCE_PATH, curr_prod)
      
      
      cat("# GETTING CLOSEST POINTS\n")
      
      input_points <- list()
      pb <- progress_bar$new(total = dim(entry_points)[1])
      for (i in 1:dim(entry_points)[1]){
        #method1
        # input_points[[i]] <- sea_points[which.min(st_distance(entry_points[i,], sea_points)),] %>%
        #   st_as_sf() %>% # transform in sf object
        #   bind_cols(st_drop_geometry(entry_points[i,])) #add the columns of entry_points[i,]
        
        
        #method2 (limiting distance calculation using a buffer)
        #get the grid points contained in a buffer of 0.5 deg around the point
        #
        input_points[[i]] <- closest.point(entry_points[i,], sea_points)
        
        pb$tick()
        
      }
      input_points <- bind_rows(input_points)
      
      toc()
      
      
      
    }
  
  
  ### 3. GENERATE THE FILES TO BE SAVED
  
  
  input_coords <- round(st_coordinates(input_points), digits = 6) #get only the coordinates for the Ichthyop Input
  
  if (input_location == "river"){
    input_coords_id <- data.frame(cbind(input_points$MAIN_RIV), input_coords)
    names(input_coords_id) <- c("MAIN_RIV", "X", "Y")
  } else if (input_location == "mangrove"){
    input_coords_id <- input_coords
  }
  
  ## round the numbers to 6 digits, to have the exact same values in the .txt and in the ichthyop output for post-treatment
  # input_coords <- apply(input_coords, 2, function(x) format(round(x, 6), nsmall = 6))
  # input_coords_id[,c("X","Y")] <- apply(input_coords_id[,c("X","Y")], 2, function(x) format(round(x, 6), nsmall = 6))
  
  if (save==T){
    
    # create the name on the dir where files will be saved, depending on method and input location
    dir_name <- paste0(input_location, "_nlog_input_")
    if (input_method == "kFromCoast"){
      dir_name <- paste0(dir_name, dist*100,"k_from_coast")
    } else if (input_method == "onMask"){
      dir_name <- paste0(dir_name, curr_prod,"_mask")
    }
    # if (input_location == "river"){
    #   dir_name <- paste0(dir_name, "_thr_disch_",thr_disch,"m3s-1")
    # }
    
    cat("### SAVING INPUT LOCATIONS\n")
    dir_path <- file.path(OUTPUT_PATH, dir_name)
    try(dir.create(path = dir_path))
    #save points coordinates for input in Ichthyop
    # quote = F to save coordinates (which are characters) as numeric
    write.table(input_coords, file = file.path(dir_path, "input_icht.txt"),
                row.names = F,col.names = F, sep=" ")
    #save points coordinates with river id
    write.table(input_coords_id, file = file.path(dir_path, "IDs.txt"),
                row.names = F,col.names = F, sep=" ")
    #save sf object for post treatement after Ichthyop simulations
    write_sf(input_points, dsn = dir_path,
             layer = dir_name,
             driver = "ESRI Shapefile")
  }
  
  toc()
  
  if (return_format == "mat"){
    return(input_coords)
  } else if (return_format == "sf"){
    return(input_points)
  }
  
  
}
