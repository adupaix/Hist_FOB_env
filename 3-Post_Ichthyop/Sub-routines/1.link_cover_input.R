#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-28
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description :  Sub-routine to link the input points from an Ichthyop simulation with the forest cover
#'#*******************************************************************************************************************
#'@revision : 
#'#*******************************************************************************************************************
#'@comment: very long script, to run once. Will return a table with, for each input point, the number of
#' associated cover points. Takes the rivers and precipitations into account.
#'#*******************************************************************************************************************


#'@arguments
#'**********

buffer_size = 10^3 #in m

msg <- crayon::bold("\n1. Counting the number of cover points associated with each input point\n\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

if(!Exists$cover){

# get the cover files names
cover_files <- list.files(path = file.path(DATA_PATH,
                                           "forest_cover",
                                           paste0("forest_cover_", year)),
                          pattern = "shp")

#' read the input points
#' filter them to keep only the IO ones
#' add a column containing the surface of forest cover associated with each point
input_points <- read.table(file.path(sim_input_path, "IDs.txt"))
names(input_points) <- c("x","y", "id_curr")
input_points_sf <- st_as_sf(input_points,
                            coords = c("x","y"),
                            crs = 4326)
input_points <- keep.which.is.in.IO(RESOURCE_PATH, input_points_sf, # not really necessary ? The filter is already applied in 1-Input_Ichthyop
                                    buffer_size = 10^4,
                                    return_format = "df") %>%
  arrange(id_curr)
input_points$coastal_cover_surface_m2 <- 0


mouth_cover_fnames <- file.path(output_paths[1], paste0("link_mouths_",sub(".shp", "", cover_files),".txt"))

river_cover_fnames <- file.path(output_paths[1], paste0("link_rivers_",sub(".shp", "", cover_files),".txt"))

coastal_cover_fnames <- file.path(output_paths[1],
                                  sub(length(cover_files), "f", paste0("input_point_with_cover_nb_v", 1:length(cover_files), ".txt")))

# if any of the file counting the number of cover points per river is missing, we need the buffer around the rivers
if ( !all(file.exists(river_cover_fnames)) & !all(file.exists(coastal_cover_fnames))){
  # build buffer of 1km around the rivers
  msg <- "Generating buffer on rivers and on river mouths\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  # buffer on rivers
  rivers_filtered %>%
    st_transform(3857) %>%
    st_buffer(dist = buffer_size) %>%
    # st_transform(4326) %>%
    st_transform(4326) -> river_buffer
    # and fusion the polygons by rivers (one geometry per river instead of one per segment)
    # group_by(MAIN_RIV) %>%
    # summarise(.groups = "keep") %>%
    # ungroup() -> river_buffer
  
  #buffer on river mouths
  # embouchures %>%
  #   st_transform(3857) %>%
  #   st_buffer(dist = buffer_size) %>%
  #   st_transform(4326) -> mouth_buffer
}


for (k in 1:length(cover_files)){
  
  msg <- paste("\nCover file n",k,"/",length(cover_files),"\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  msg <- "1. Link river - cover\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  if (!file.exists(coastal_cover_fnames[k])){
    
    msg <- "  - Reading cover file\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    # read the cover file
    read_sf(file.path(DATA_PATH,
                      "forest_cover",
                      paste0("forest_cover_", year),
                      cover_files[k])) -> cover_df 
    # #select only the point and the id
    # dplyr::select(id, geometry) %>%
    # # transform it to lat/long coordinates
    # st_transform(4326) -> cover_df
    
    is_coastal_cover <- k %in% grep(pattern = "cote|coast", cover_files)
    
    # test if the forest cover shp is in the right format
    if(any(as.numeric(levels(as.factor(cover_df$couvert))) > 100) | any(as.numeric(levels(as.factor(cover_df$couvert))) < 1)){
      stop("Error: wrong forest cover file format (not a percentage)")
    }
    
    if (is_coastal_cover){
      msg <- "  - Filtering coastal cover points with the current product mask if needed\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
      
      cover_bbox <- st_bbox(cover_df)
      # read the bbox of the forcing product from the mask saved in the Resources folder
      forcing_bbox <- get.forcing.bbox(RESOURCE_PATH, forcing)
      #' crop the cover points df only if any of the points are outside the forcing product 
      #' if it's not the case, it would also work but it's useless and we'd loose time...
      cover_is_to_crop <- any(c(cover_bbox[1:2]<forcing_bbox[1:2],
                                cover_bbox[3:4]>forcing_bbox[3:4]))
      
      if (cover_is_to_crop){
        cover_df <- faster.st_crop.points(cover_df, forcing_bbox)
      }
    }
    
    msg <- "  - Getting the cover points inside the river buffer and the mouth buffer\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    #' get the points of cover which are inside the buffers
    #' return a list with for each point, the polygons inside which the point is
    is_within_river <- st_intersects(st_geometry(cover_df), st_geometry(river_buffer)) # for rivers
    
    # unlist the result, and keep only the first value (there shouldn't be any duplicates, but just in case...)
    unlist_is_within_river <- unlist(lapply(is_within_river, function(x) ifelse(length(x)==0, NA, x[1])))
    
    # test if some points are counted twice (if it happens, we need to determine the rule to follow)
    if((length(unlist_is_within_river) != length(is_within_river))){
      stop("Error: some points are associated with several rivers at the same time")
    }
    
    rm(is_within_river) ; invisible(gc())
    
    msg <- "  - Filling the cover df with the river ids\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    # create the column which will contain the river id
    cover_df %>% mutate(MAIN_RIV = NA,
                        HYRIV_ID = NA) -> cover_df
                        # is_within_mouth_buffer = NA) -> cover_df
    
    # fill in the river id
    cover_df$MAIN_RIV[which(!is.na(unlist_is_within_river))] <-
      river_buffer$MAIN_RIV[unlist_is_within_river[which(!is.na(unlist_is_within_river))]]
    
    cover_df$HYRIV_ID[which(!is.na(unlist_is_within_river))] <-
      river_buffer$HYRIV_ID[unlist_is_within_river[which(!is.na(unlist_is_within_river))]]
    
    # cover_df$is_within_mouth_buffer[which(!is.na(unlist_is_within_mouth))] <-
    #   mouth_buffer$MAIN_RIV[unlist_is_within_mouth[which(!is.na(unlist_is_within_mouth))]]
    
    
    msg <- "  - Saving the tables with the number of associated cover cells\n    for each river and for each river mouth\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    # get HYRIV_ID (river segment ids) corresponding to river ids associated with cover points
    #' @note that for each 90mx90m cell we have a float, corresponding to a percentage of forest cover (between 1 and 100)
    #' this float needs to be multiplied by 8100 to get a surface in m2
    as.data.frame(xtabs(couvert ~ HYRIV_ID, data = cover_df)) %>%
      # rename("MAIN_RIV" = "is_within_river_buffer") %>%
      mutate(HYRIV_ID = as.numeric(as.character(HYRIV_ID))) %>%
      right_join(link_HYRIV_MAINRIV, by = "HYRIV_ID") -> river_cover_summary
    names(river_cover_summary)[2] <- "river_cover_surface_m2"
    river_cover_summary$river_cover_surface_m2[which(is.na(river_cover_summary$river_cover_surface_m2))] <- 0
    river_cover_summary$river_cover_surface_m2 <- river_cover_summary$river_cover_surface_m2 * 8100
    
    write.table(river_cover_summary,
                file = river_cover_fnames[k],
                row.names = F)
    
    # write.table(mouth_cover_summary,
    #             file = mouth_cover_fnames[k],
    #             row.names = F)
    
  } else {
    
    msg <- "  - Reading existing river-cover and mouth-cover link tables\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    river_cover_summary <- read.table(river_cover_fnames[k])
    
    # mouth_cover_summary <- read.table(mouth_cover_fnames[k])
    
  }
  
  msg <- "2. Link input - coastal cover\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  if (!file.exists(coastal_cover_fnames[k])){
    
    msg <- "  - Filter: keep only coastal cover points\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    cover_df %>%
      dplyr::filter(is.na(MAIN_RIV) & is.na(HYRIV_ID)) -> coastal_cover
    
    rm(cover_df) ; invisible(gc())
    
    msg <- "  - Get input points associated with cover cells\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    sample_size <- 1000
    
    niter <- floor( dim(coastal_cover)[1] / sample_size )
    
    pb <- progress_bar$new(format = "[:bar] :percent | Cover points sample :current / :total",
                           total = niter+1
    )
    
    for (i in 1:niter){
      
      indexes <- ((i-1)*sample_size+1):(i*sample_size)
      
      cover_surface_per_points <- get.nb.cover.per.input(indexes, coastal_cover, input_points)
      
      input_points$coastal_cover_surface_m2[
        input_points$id_curr == as.numeric(names(cover_surface_per_points))] <-
        input_points$coastal_cover_surface_m2[ input_points$id_curr == as.numeric(names(cover_surface_per_points)) ] + cover_surface_per_points
      
      pb$tick()
      
    }
    
    indexes <- (niter*sample_size+1):(dim(coastal_cover)[1])
    
    cover_surface_per_points <- get.nb.cover.per.input(indexes, coastal_cover, input_points)
    
    input_points$nb_coastal_cover_points[
      input_points$id_curr == as.numeric(names(cover_surface_per_points))] <-
      input_points$nb_coastal_cover_points[ input_points$id_curr == as.numeric(names(cover_surface_per_points)) ] + cover_surface_per_points
    
    pb$tick()
    
    rm(coastal_cover) ; invisible(gc())
    
    write.table(input_points,
                file = coastal_cover_fnames[k])
    
  } else {
    
    msg <- "  - Reading existing coastal cover - release point table\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    input_points <- read.table(coastal_cover_fnames[k])
    
  }
  
}

#'********************************************************************************


msg <- "\n\n3. Get the length of coastline associated with each input point\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)


fname <- file.path(output_paths[1], "input_point_with_cover_and_coast_nb.txt")

if(!file.exists(fname)){
  
  # add a column to keep the number of coastal points (ie the length of the coast associated with the input point)
  input_points$nb_coastal_points <- 0
  
  #' Define the frame of study (same as the one used to generate the input points)
  # IO <- st_sf(data.frame("IO"),
  #             geometry = st_sfc(st_polygon(list(
  #               matrix(c(25,-35, 25,30, 140, 30, 140,-35, 25,-35),
  #                      ncol = 2,
  #                      byrow = T)))
  #             ),
  #             crs = 4326
  # )
  
  msg <- "  - Load coastline and sample points on it\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  #' get the cover files names from 2000 (the cover files from 2000 also contain the zeros,
  #' which allows to count the number of points and hence count the number of coastline points)
  coast_files <- list.files(path = file.path(DATA_PATH,
                                             "coast_cover_with_zeros"),
                            pattern = "shp$")
  
  for (k in 1:length(coast_files)){
    
    # read the coast file
    read_sf(file.path(DATA_PATH,
                      "coast_cover_with_zeros",
                      coast_files[k])) -> coast_df 
    # #select only the point and the id
    # dplyr::select(id, geometry) %>%
    # # transform it to lat/long coordinates
    # st_transform(4326) -> coast_df
    
    # test if the forest coast shp is in the right format
    if(!identical(as.numeric(0:9), as.numeric(levels(as.factor(coast_df$couvert))))){
      stop("Error: wrong coast file format (levels different from 0:9)")
    } else if(dim(coast_df)[1] != max(coast_df$id)){
      stop("Error: missing points in coast file format")
    }
    
    msg <- "  - Filtering coastal points with the current product mask if needed\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    coast_bbox <- st_bbox(coast_df)
    # read the bbox of the forcing product from the mask saved in the Resources folder
    forcing_bbox <- get.forcing.bbox(RESOURCE_PATH, forcing)
    #' crop the coast points df only if any of the points are outside the forcing product 
    #' if it's not the case, it would also work but it's useless and we'd loose time...
    coast_is_to_crop <- any(c(coast_bbox[1:2]<forcing_bbox[1:2],
                              coast_bbox[3:4]>forcing_bbox[3:4]))
    
    if (coast_is_to_crop){
      coast_df <- faster.st_crop.points(coast_df, forcing_bbox)
    }
    
    msg <- "  - Get input points associated with coastal cells\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    sample_size <- 2000
    
    niter <- floor( dim(coast_df)[1] / sample_size )
    
    pb <- progress_bar$new(format = "[:bar] :percent | Coastal points sample :current / :total",
                           total = niter+1
    )
    
    for (i in 1:niter){
      
      indexes <- ((i-1)*sample_size+1):(i*sample_size)
      
      cover_surface_per_points <- get.nb.cover.per.input(indexes, coast_df, input_points, count_cover = F)
      
      #'@test: result: fastest sample size is 2000?
      # indexes1 <- ((i-1)*500+1):(i*500)
      # indexes2 <- ((i-1)*1000+1):(i*1000)
      # indexes3 <- ((i-1)*1500+1):(i*1500)
      # indexes4 <- ((i-1)*1750+1):(i*1750)
      # indexes5 <- ((i-1)*2000+1):(i*2000)
      # microbenchmark(s500 = {cover_surface_per_points <- get.nb.cover.per.input(indexes1, coast_df, input_points, count_cover = F)},
      #                s1000 = {cover_surface_per_points <- get.nb.cover.per.input(indexes2, coast_df, input_points, count_cover = F)},
      #                s1500 = {cover_surface_per_points <- get.nb.cover.per.input(indexes3, coast_df, input_points, count_cover = F)},
      #                s1750 = {cover_surface_per_points <- get.nb.cover.per.input(indexes4, coast_df, input_points, count_cover = F)},
      #                s2000 = {cover_surface_per_points <- get.nb.cover.per.input(indexes5, coast_df, input_points, count_cover = F)})
      # 
      input_points$coastal_surface_m2[
        input_points$id_curr == as.numeric(names(cover_surface_per_points))] <-
        input_points$coastal_surface_m2[ input_points$id_curr == as.numeric(names(cover_surface_per_points)) ] + cover_surface_per_points
      
      pb$tick()
      
    }
    
    indexes <- (niter*sample_size+1):(dim(coast_df)[1])
    
    cover_surface_per_points <- get.nb.cover.per.input(indexes, coast_df, input_points, count_cover = F)
    
    input_points$coastal_surface_m2[
      input_points$id_curr == as.numeric(names(cover_surface_per_points))] <-
      input_points$coastal_surface_m2[ input_points$id_curr == as.numeric(names(cover_surface_per_points)) ] + cover_surface_per_points
    
    pb$tick()
    
    rm(coast_df) ; invisible(gc())
    
    write.table(input_points,
                file = fname)
    
  }
  
  
  
  
  
  #' load coastline
  # load(file.path(RESOURCE_PATH, "coastline10.rda")) # highres coastline, downloaded at : https://github.com/ropensci/rnaturalearthhires/tree/master/data (last accessed 2021-09-21)
  # coastline10 <- st_as_sf(coastline10)
  # 
  # coastline10 %>%
  #   # sample points on the coastline (every km)
  #   st_transform(3857) %>%
  #   st_line_sample(density = 10^(-3)) %>%
  #   st_transform(4326) %>%
  #   st_cast("MULTIPOINT") %>%
  #   st_cast("POINT") -> coastal_points
  # 
  # # create an sf object from the geometry deduced above
  # coastal_points <- st_sf(data.frame(id = seq(1, length(coastal_points), 1)),
  #                         geometry = coastal_points,
  #                         crs = 4326)
  # 
  # # keep only the input points which are in the area of interest
  # coastal_points <- keep.which.is.in.IO(RESOURCE_PATH, coastal_points,
  #                                       buffer_size = 5*10^4,
  #                                       return_format = "sf")
  # 
  # # coastal_points %>% st_transform(4326) -> coastal_points
  # 
  # msg <- "  - Get input points associated with coastal points\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  # 
  # sample_size <- 1000
  # niter <- floor( dim(coastal_points)[1] / sample_size )
  # 
  # pb <- progress_bar$new(format = "[:bar] :percent | Coastal points sample :current / :total",
  #                        total = niter+1
  # )
  # 
  # for (i in 1:niter){
  #   
  #   indexes <- ((i-1)*sample_size+1):(i*sample_size)
  #   
  #   n_coast_per_points <- get.nb.cover.per.input(indexes, coastal_points, input_points)
  #   
  #   for (k in 1:length(n_coast_per_points)){
  #     input_points$nb_coastal_points[
  #       input_points$id_curr == as.numeric(names(n_coast_per_points)[k])] <- 
  #       input_points$nb_coastal_points[ input_points$id_curr == as.numeric(names(n_coast_per_points)[k]) ] + n_coast_per_points[k]
  #   }
  #   
  #   pb$tick()
  #   
  # }
  # 
  # indexes <- (niter*sample_size+1):(dim(coastal_points)[1])
  # 
  # n_coast_per_points <- get.nb.cover.per.input(indexes, coastal_points, input_points)
  # 
  # for (k in 1:length(n_coast_per_points)){
  #   input_points$nb_coastal_points[
  #     input_points$id_curr == as.numeric(names(n_coast_per_points)[k])] <- 
  #     input_points$nb_coastal_points[ input_points$id_curr == as.numeric(names(n_coast_per_points)[k]) ] + n_coast_per_points[k]
  # }
  # 
  # pb$tick()
  # 
  # write.table(input_points,
  #             file = fname)
  
} else {
  
  msg <- "  - Reading existing table\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  input_points <- read.table(fname)
  
}

#'********************************************************************************

  msg <- "\n4. Merge coastal, river, river mouth and coastline length information\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

  link_riv_cov_files <- list.files(output_paths[1], pattern = "link_rivers_")
  # link_mouth_cov_files <- list.files(output_paths[1], pattern = "link_mouths_")
  
  river_cover_summary <- list()
  # n_cover_per_mouth <- list()

  for (i in 1:length(link_riv_cov_files)){
    river_cover_summary[[i]] <- read.table(file.path(output_paths[1], link_riv_cov_files[i]), header = T)
    # n_cover_per_mouth[[i]] <- read.table(file.path(output_paths[1], link_mouth_cov_files[i]), header = T)
  }

  bind_rows(river_cover_summary) %>%
    group_by(HYRIV_ID) %>%
    summarise(n = sum(river_cover_surface_m2), .groups = "keep") %>%
    ungroup() %>%
    right_join(link_HYRIV_MAINRIV, by = "HYRIV_ID") %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    rename("river_cover_surface_m2" = "n") %>%
    dplyr::arrange(MAIN_RIV, HYRIV_ID) -> river_cover_summary

  write.csv(river_cover_summary,
            file = Names$coverRiver,
            row.names = F)
  
  # bind_rows(n_cover_per_mouth) %>%
  #   group_by(MAIN_RIV) %>%
  #   summarise(n = sum(nb_mouth_cover_points), .groups = "keep") %>%
  #   ungroup() %>%
  #   rename("nb_mouth_cover_points" = "n") -> n_cover_per_mouth
  # 
  # write.csv(n_cover_per_mouth,
  #           file = Names$coverMouth,
  #           row.names = F)

  link_river_input <- read.table(file.path(sim_input_path, "Link_table.txt"), header = T)

  if (!all(river_cover_summary$MAIN_RIV %in% link_river_input$MAIN_RIV)){
    stop("Error: some rivers are not linked with the input points")
  }

  input_points %>%
    full_join(link_river_input, by = "id_curr") %>%
    arrange(id_curr) %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::select(-HYBAS_L12) %>%
    full_join(river_cover_summary, by = "MAIN_RIV") -> cover_global_summary
    # full_join(river_cover_summary, by = "MAIN_RIV") %>%
    # full_join(n_cover_per_mouth, by = "MAIN_RIV") -> cover_global_summary

  cover_global_summary$river_cover_surface_m2[which(is.na(cover_global_summary$river_cover_surface_m2))] <- 0
  # cover_global_summary$nb_mouth_cover_points[which(is.na(cover_global_summary$nb_mouth_cover_points))] <- 0
  
  cover_global_summary %>%
    dplyr::filter(MAIN_RIV == HYRIV_ID) %>% #keep only the river mouths
    group_by(id_curr) %>%
    summarise(nb_mouth_cover_points = sum(river_cover_surface_m2), .groups = "keep") -> mouth_cover_summary
  
  cover_global_summary %>%
    group_by(id_curr) %>%
    summarise(river_cover_surface_m2 = sum(river_cover_surface_m2),
              .groups = "keep") %>%
    # left_join(y = mouth_cover_summary, by = "id_curr") %>%
    left_join(y = input_points, by = "id_curr") %>%
    dplyr::mutate(total_cover_surface_m2 = river_cover_surface_m2 + coastal_cover_surface_m2) %>%
    # dplyr::mutate(nb_cover_points = nb_mouth_cover_points + river_cover_surface_m2 + nb_coastal_cover_points) %>%
    dplyr::select(id_curr, x, y,
                  river_cover_surface_m2,
                  # MAIN_RIV, HYRIV_ID,
                  # nb_mouth_cover_points,
                  coastal_cover_surface_m2,
                  total_cover_surface_m2,
                  coastal_surface_m2) %>%
    filter(!is.na(id_curr)) -> cover_surface_per_input

  write.csv(cover_surface_per_input,
            file = Names$coverGlobal,
            row.names = F)

  #' save a log
  sink(Names$log1, append = F)
  cat("Date & time :", format(Sys.time()), "\n")
  cat("\n  Year:", year)
  cat("\n  Forcing product:", forcing)
  cat("\n  Location of input points :", input_location)
  cat("\n  Method for input points :", input_method)
  sink()

} else {
  
  msg <- "Reading existing file\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
}

#'@output_for_next_subroutine
cover_surface_per_input <- read.csv(file = Names$coverGlobal)
cover_surface_per_river <- read.csv(file = Names$coverRiver)
cover_surface_per_mouth <- read.csv(file = Names$coverRiver) %>% dplyr::filter(MAIN_RIV == HYRIV_ID)


#' Do not delete
toKeep <- c(toKeep, "cover_surface_per_input", "n_cover_per_river", "n_cover_per_mouth")
