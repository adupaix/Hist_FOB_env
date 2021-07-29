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
year = 2000 # year of the cover to be used

buffer_size = 10^3 #in m

sim_output_path <- file.path(DATA_PATH, "Output_Ichthyop", sim_name)
sim_input_path <- file.path(DATA_PATH, "Input_Ichthyop", paste0(input_location, "_nlog_input_", forcing, "_", input_method))


cat("\14")
cat(crayon::bold("Counting the number of cover points associated with each input point\n\n"))

# get the cover files names
cover_files <- list.files(path = file.path(DATA_PATH,
                                           "forest_cover",
                                           paste0("forest_cover_pts_", year)),
                          pattern = "shp")

# read the input points to add a column containing the number of cover cells associated with each point
input_points <- read.table(file.path(sim_input_path, "IDs.txt"))
names(input_points) <- c("x","y", "id_curr")
input_points$nb_coastal_cover_points <- 0

# read river
cat("  - Reading and filtering rivers file\n")
cat("      - Reading\n")

rivers_IO <- readRDS(file.path(DATA_PATH, "river_data", "rivers_IO.rds"))

cat("      - Filtering\n")

#' filter rivers
rivers_IO %>%
  # garde uniquement les embouchures
  # (identifiant de la portion de riviere = a l'identifiant principal de la riviere)
  dplyr::filter(HYRIV_ID == MAIN_RIV) %>%
  # garde uniquement les portions de riviere qui se jettent dans la mer
  dplyr::filter(ENDORHEIC == 0) %>%
  # garde uniquement les fleuves avec un debit maximal de plus d 100 m3 par seconde
  dplyr::filter(dis_m3_pmx >= thr_disch) -> embouchures

# keep only variables of interest
rivers_IO %>%
  dplyr::filter(MAIN_RIV %in% embouchures$HYRIV_ID) %>%
  dplyr::filter(dis_m3_pmx >= thr_disch) %>%
  dplyr::select(HYRIV_ID, #id de la portion de riviere
                NEXT_DOWN,#id de la portion en aval
                MAIN_RIV, # id de la portion qui se jette dans la mer
                LENGTH_KM, # longueur de la portion en km
                HYBAS_L12, # id du bassin versant,
                #pour faire le lien avec l'autre base de donnees
                dis_m3_pyr, # debit moyen en m3/s
                dis_m3_pmn, # debit minimal en m3/s
                dis_m3_pmx # debit maximal
  ) -> filtered

rm(embouchures) ; invisible(gc())

# build buffer of 1km around the rivers
cat("      - Generating buffer\n")

# add a buffer
filtered %>% 
  st_transform(3857) %>%
  st_buffer(dist = buffer_size) %>%
  st_transform(4326) %>%
  # and fusion the polygons by rivers (one geometry per river instead of one per segment)
  group_by(MAIN_RIV) %>%
  summarise(.groups = "keep") %>%
  ungroup() -> grouped_buffer


for (k in 1:length(cover_files)){
  
  cat(crayon::bold("\nCover file n",k,"/",length(cover_files),"\n"))
  
  cat("1. Link river - cover\n")
  
  cat("  - Reading cover file\n")
  
  # read the cover file
  read_sf(file.path(DATA_PATH,
                    "forest_cover",
                    paste0("forest_cover_pts_", year),
                    cover_files[k])) %>%
    #select only the point and the id
    dplyr::select(id, geometry) %>%
    # transform it to lat/long coordinates
    st_transform(4326) -> cover_df
  
  
  cat("  - Getting the cover points inside the river buffer\n")
  #' get the points of cover which are inside the buffers
  #' return a list with for each point, the polygons inside which the point is
  is_within <- st_intersects(st_geometry(cover_df), st_geometry(grouped_buffer))
  
  
  # unlist the result, and keep only the first value (there shouldn't be any duplicates, but just in case...)
  unlist_is_within <- unlist(lapply(is_within, function(x) ifelse(length(x)==0, NA, x[1])))
  
  # test if some points are counted twice (if it happens, need to determine the rule to follow)
  if(length(unlist_is_within) != length(is_within)){
    stop("Error: some points are associated with several rivers at the same time")
  }
  
  rm(is_within) ; invisible(gc())
  
  cat("  - Filling the cover df with the river ids\n")
  
  # create the column which will contain the river id
  cover_df %>% mutate(is_within_river_buffer = NA) -> cover_df
  
  # fill in the river id
  cover_df$is_within_river_buffer[which(!is.na(unlist_is_within))] <- grouped_buffer$MAIN_RIV[unlist_is_within[which(!is.na(unlist_is_within))]]
  
  
  cat("  - Saving the table with the number of associated cover cells for each river\n")
  # get MAIN_RIV ids corresponding to river ids associated with cover points
  # suppressWarnings is used because the as.numeric part introduces NAs (it's what we want, we don't care about the warning)
  suppressWarnings(
  as.data.frame(summary(as.factor(cover_df$is_within_river_buffer))) %>%
    tibble::rownames_to_column(var = "MAIN_RIV") %>% mutate(MAIN_RIV = as.numeric(MAIN_RIV)) %>%
    dplyr::filter(!is.na(MAIN_RIV)) -> river_cover_summary
  )
  names(river_cover_summary)[2] <- "nb_river_cover_points"
  
  # filtered %>%
  #   as.data.frame() %>%
  #   dplyr::select(HYRIV_ID, MAIN_RIV) %>%
  #   right_join(y = cover_df, by = c("MAIN_RIV" = "is_within_river_buffer")) %>%
  #   # get the number of cover points associated with each river mouth
  #   group_by(MAIN_RIV) %>%
  #   summarise(n_cover_points = n(), .groups = "keep") %>%
  #   dplyr::filter(!is.na(MAIN_RIV)) -> river_cover_summary
  
  write.table(river_cover_summary,
              file = file.path(OUTPUT_PATH, sim_name, paste0("link_rivers_",sub(".shp", "", cover_files[k]),".txt")),
              row.names = F)
  
  
  cat("2. Link input - coastal cover\n")
  
  cat("  - Filter: keep only coastal cover points")
  
  cover_df %>%
    dplyr::filter(is.na(is_within_river_buffer)) -> coastal_cover
  
  rm(cover_df) ; invisible(gc())
  
  ## calcul distance entre cover et input
  
  #'@sub_function
  #'***************
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
    
    # get the closest input point for each cover point
    n_cover_per_points <- summary( as.factor( input_points$id_curr[apply(dist_mat, 1, function(x) which(x == min(x)))] ))
    
    objects.list <- list("sub_coastal_cover","x_input","x_cover","y_input","y_cover","dist_mat")
    rm(list = objects.list) ; invisible(gc())
    
    return(n_cover_per_points)
    
  }
  #'***************
  
  cat("  - Get input points associated with cover cells")
  
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
              file = file.path(OUTPUT_PATH, sim_name, file_name))
  
}


cat("3. Merge coastal and river information\n")

link_riv_cov_files <- list.files(file.path(OUTPUT_PATH, sim_name), pattern = "link_rivers_")

n_cover_per_river <- list()

for (i in 1:length(link_riv_cov_files)){
  n_cover_per_river[[i]] <- read.table(file.path(OUTPUT_PATH, sim_name, link_riv_cov_files[i]), header = T)
}

bind_rows(n_cover_per_river) %>%
  group_by(MAIN_RIV) %>%
  summarise(n = sum(nb_river_cover_points), .groups = "keep") %>%
  ungroup() %>%
  rename("nb_river_cover_points" = "n") -> n_cover_per_river

write.table(n_cover_per_river,
            file.path(OUTPUT_PATH, sim_name, "n_cover_per_river.txt"),
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
          file = file.path(OUTPUT_PATH, sim_name, "number_of_cover_points_per_input_point.csv"),
          row.names = F)
