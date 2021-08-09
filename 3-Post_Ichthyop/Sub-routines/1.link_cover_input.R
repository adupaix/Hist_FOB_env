#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-28
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description :  Sub-routine to link the input points from an Ichthyop simulation with the forest cover
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************
#'@comment: very long script, to run once. Will return a table with, for each input point, the number of
#'associated forest points (one cell represents 900m2 of forest). Takes the rivers into account.
#'#*******************************************************************************************************************


#'@arguments
#'**********

buffer_size = 10^3 #in m

sim_output_path <- file.path(DATA_PATH, "Output_Ichthyop", sim_name)
sim_input_path <- file.path(DATA_PATH, "Input_Ichthyop", paste0(input_location, "_nlog_input_", forcing, "_", input_method))

msg <- crayon::bold("\n\n1. Counting the number of cover points associated with each input point\n\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

if(!nCoverExists){

# get the cover files names
cover_files <- list.files(path = file.path(DATA_PATH,
                                           "forest_cover",
                                           paste0("forest_cover_", year)),
                          pattern = "shp")

# read the input points to add a column containing the number of cover cells associated with each point
input_points <- read.table(file.path(sim_input_path, "IDs.txt"))
names(input_points) <- c("x","y", "id_curr")
input_points$nb_coastal_cover_points <- 0

# build buffer of 1km around the rivers
msg <- "Generating buffer\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

# add a buffer
rivers_filtered %>% 
  st_transform(3857) %>%
  st_buffer(dist = buffer_size) %>%
  st_transform(4326) %>%
  # and fusion the polygons by rivers (one geometry per river instead of one per segment)
  group_by(MAIN_RIV) %>%
  summarise(.groups = "keep") %>%
  ungroup() -> grouped_buffer


for (k in 1:length(cover_files)){
  
  msg <- crayon::bold("\nCover file n",k,"/",length(cover_files),"\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  msg <- "1. Link river - cover\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  river_cover_fname <- file.path(output_path_1, paste0("link_rivers_",sub(".shp", "", cover_files[k]),".txt"))
  
  if (!file.exists(river_cover_fname)){
    
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
    
    # test if the forest cover shp is in the right format
    if(!identical(as.numeric(1:9), as.numeric(levels(as.factor(cover_df$couvert))))){
      stop("Error: wrong forest cover file format (levels different from 1:9)")
    }
    
    msg <- "  - Getting the cover points inside the river buffer\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    #' get the points of cover which are inside the buffers
    #' return a list with for each point, the polygons inside which the point is
    is_within <- st_intersects(st_geometry(cover_df), st_geometry(grouped_buffer))
    
    
    # unlist the result, and keep only the first value (there shouldn't be any duplicates, but just in case...)
    unlist_is_within <- unlist(lapply(is_within, function(x) ifelse(length(x)==0, NA, x[1])))
    
    # test if some points are counted twice (if it happens, we need to determine the rule to follow)
    if(length(unlist_is_within) != length(is_within)){
      stop("Error: some points are associated with several rivers at the same time")
    }
    
    rm(is_within) ; invisible(gc())
    
    msg <- "  - Filling the cover df with the river ids\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    # create the column which will contain the river id
    cover_df %>% mutate(is_within_river_buffer = NA) -> cover_df
    
    # fill in the river id
    cover_df$is_within_river_buffer[which(!is.na(unlist_is_within))] <- grouped_buffer$MAIN_RIV[unlist_is_within[which(!is.na(unlist_is_within))]]
    
    
    msg <- "  - Saving the table with the number of associated cover cells for each river\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    # get MAIN_RIV ids corresponding to river ids associated with cover points
    #' @note that one cover cell is composed of nine 30mx30m cells, hence a float (and not an integer) is contained in the nb_river_cover_points column
    #' this float needs to be multiplied by 8100 to get a surface in m2
    as.data.frame(xtabs(couvert ~ is_within_river_buffer, data = cover_df)) %>%
      rename("MAIN_RIV" = "is_within_river_buffer") %>%
      mutate(MAIN_RIV = as.numeric(as.character(MAIN_RIV))) -> river_cover_summary
    names(river_cover_summary)[2] <- "nb_river_cover_points"
    
    # rivers_filtered %>%
    #   as.data.frame() %>%
    #   dplyr::select(HYRIV_ID, MAIN_RIV) %>%
    #   right_join(y = cover_df, by = c("MAIN_RIV" = "is_within_river_buffer")) %>%
    #   # get the number of cover points associated with each river mouth
    #   group_by(MAIN_RIV) %>%
    #   summarise(n_cover_points = n(), .groups = "keep") %>%
    #   dplyr::filter(!is.na(MAIN_RIV)) -> river_cover_summary
    
    write.table(river_cover_summary,
                file = river_cover_fname,
                row.names = F)
  } else {
    
    msg <- "  - Reading existing river-cover link table\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    river_cover_summary <- read.table(river_cover_fname, row.names = F)
  }
  
  
  msg <- "2. Link input - coastal cover\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  msg <- "  - Filter: keep only coastal cover points\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  cover_df %>%
    dplyr::filter(is.na(is_within_river_buffer)) -> coastal_cover
  
  rm(cover_df) ; invisible(gc())
  
  msg <- "  - Get input points associated with cover cells\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  sample_size <- 1000
  
  niter <- floor( dim(coastal_cover)[1] / sample_size )
  
  pb <- progress_bar$new(format = "[:bar] :percent | Cover points sample :current / :total",
                         total = niter+1
  )
  
  for (i in 1:niter){
    
    indexes <- ((i-1)*sample_size+1):(i*sample_size)
    
    n_cover_per_points <- get.nb.cover.per.input(indexes, coastal_cover, input_points)
    
    input_points$nb_coastal_cover_points[
      as.numeric(names(n_cover_per_points)) ] <- input_points$nb_coastal_cover_points[ as.numeric(names(n_cover_per_points)) ] + n_cover_per_points
    
    pb$tick()
    
  }
  
  indexes <- (niter*sample_size+1):(dim(coastal_cover)[1])
  
  n_cover_per_points <- get.nb.cover.per.input(indexes, coastal_cover, input_points)
  
  input_points$nb_coastal_cover_points[
    as.numeric(names(n_cover_per_points)) ] <- input_points$nb_coastal_cover_points[ as.numeric(names(n_cover_per_points)) ] + n_cover_per_points
  
  pb$tick()
  
  rm(coastal_cover) ; invisible(gc())
  
  file_name <- ifelse(k != length(cover_files),
                      paste0("input_point_with_cover_nb_v",k,".txt"),
                      "input_point_with_cover_nb_vf.txt")
  
  write.table(input_points,
              file = file.path(output_path_1, file_name))
  
}


msg <- "3. Merge coastal and river information\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

link_riv_cov_files <- list.files(output_path_1, pattern = "link_rivers_")

n_cover_per_river <- list()

for (i in 1:length(link_riv_cov_files)){
  n_cover_per_river[[i]] <- read.table(file.path(output_path_1, link_riv_cov_files[i]), header = T)
}

bind_rows(n_cover_per_river) %>%
  group_by(MAIN_RIV) %>%
  summarise(n = sum(nb_river_cover_points), .groups = "keep") %>%
  ungroup() %>%
  rename("nb_river_cover_points" = "n") -> n_cover_per_river

write.csv(n_cover_per_river,
            file.path(output_path_1, "n_cover_per_river.csv"),
            row.names = F)

link_river_input <- read.table(file.path(sim_input_path, "Link_table.txt"), header = T)

if (!all(n_cover_per_river$MAIN_RIV %in% link_river_input$MAIN_RIV)){
  stop("Error: some rivers are not linked with the input points")
}

input_points %>%
  full_join(link_river_input, by = "id_curr") %>%
  arrange(id_curr) %>%
  dplyr::select(-HYBAS_L12) %>%
  full_join(n_cover_per_river, by = "MAIN_RIV") -> cover_global_summary

cover_global_summary$nb_river_cover_points[which(is.na(cover_global_summary$nb_river_cover_points))] <- 0

cover_global_summary %>%
  group_by(id_curr) %>%
  summarise(nb_river_cover_points = sum(nb_river_cover_points), .groups = "keep") %>%
  left_join(y = input_points, by = "id_curr") %>%
  dplyr::mutate(nb_cover_points = nb_river_cover_points + nb_coastal_cover_points) %>%
  dplyr::select(id_curr, x, y, nb_river_cover_points, nb_coastal_cover_points, nb_cover_points) -> nb_cover_per_input

write.csv(nb_cover_per_input,
          file = file.path(output_path_1, "number_of_cover_points_per_input_point.csv"),
          row.names = F)

#' save a log
sink(logName1, append = F)
cat("Execution time :", format(Sys.time()))
sink()

} else {
  
  msg <- "Reading existing file\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  nb_cover_per_input <- read.csv(file = file.path(output_path_1, "number_of_cover_points_per_input_point.csv"))
  
}