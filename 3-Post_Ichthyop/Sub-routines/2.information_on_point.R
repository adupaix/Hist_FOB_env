#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-21
#'@email : 
#'#*******************************************************************************************************************
#'@description :  For each release point, get the information for weighting (cover surface, precipitations and 
#'  associated rivers with discharge)
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


cat(bold("\n\n2. Getting information on input points\n"))
cat("  - Weighting method:",weight_method, "\n\n")

#' get the table containing the number of cover points associated with each river
n_cover_per_river <- read.csv(file.path(OUTPUT_PATH, sim_name, year, "1.nb_cover", "n_cover_per_river.csv"),
                              header = T)

#' Get the table linking river ids with input point ids
link_river_input <- read.table(file.path(sim_input_path, "Link_table.txt"), header = T)
#' keep only the rivers above the threshold in link_river_input
link_river_input %>% dplyr::filter(MAIN_RIV %in% embouchures$MAIN_RIV) -> link_river_input

sub_dirs <- list.files(sim_output_path)

non_null_points <- list()

pb <- progress_bar$new(format = "[:bar] :percent | Sub directory :current / :total",
                       total = length(sub_dirs))


for (i in 1:length(sub_dirs)){
  
  pb$tick()
  
  non_null_points[[i]] <- data.frame(matrix(ncol = 3))
  
  dir.create(file.path(output_path_2, sub_dirs[i]), recursive = T, showWarnings = F)
  
  rds_files <- list.files(file.path(sim_output_path, sub_dirs[i]), recursive = T, pattern = ".rds")
  rds_files <- grep("infos", rds_files, invert = T, value = T)
  
  # rds_files <- rds_files[1:5000]
  
  #'@arguments
  #'**********
  
  registerDoParallel(cores = nb_cores)
  
  
  non_null_points[[i]] <- foreach(k = 1:length(rds_files),
                               .combine = rbind) %dopar% {
                                 
                                 point <- list()
                                 
                                 point$id <- sub("/.*","",rds_files[k])
                                 
                                 fname <- sub(".*/","",rds_files[k])
                                 
                                 
                                 #'@comment trouver une solution ici pour accelerer (parallelisation?). La lecture du fichier prend ~0.016s, ce qui amene a 2h45 pour lire tous les fichiers
                                 #' ou sinon, lecture apres, sauvegarde juste les infos sur le point
                                 # point$data <- readRDS(file.path(sim_output_path, paste0("points_", num_sub_dir), point$id, fnames[i]))
                                 
                                 
                                 #'@get_information_on_release_point
                                 #'***********************************
                                 
                                 
                                 # get release coordinates
                                 point <- get.coords.release(sim_input_path,
                                                             point)
                                 
                                 # get release date
                                 
                                 point$release_date <- as.Date(sub("\\..*", "", sub(".*_", "", fname)))
                                 
                                 # get precipitations
                                 
                                 point <- get.precipitations(DATA_PATH, point)
                                 
                                 # get rivers and associated discharge + cover
                                 
                                 point <- get.associated.rivers(link_river_input, n_cover_per_river, embouchures, point)
                                 
                                 
                                 # get forest cover
                                 
                                 point <- get.number.of.cover.points(nb_cover_per_input, point)
                                 
                                 if (round(point$nb_cover_points) != round(point$nb_coastal_cover_points + sum(point$rivers$cover))){
                                   stop("Error: total number of cover points does not correspond to sum of coastal and river associated points")
                                 }
                                 
                                 
                                 # weight density map
                                 
                                 point <- add.weight(point, weight_method)
                                 
                                 
                                 # apply mortality
                                 
                                 # point <- apply.mortality(point, ltime, ltime_method, sd = ltime_sd)
                                 
                                 
                                 if (point$weight != 0){
                                   # fill in non_null_points
                                   non_null_point <- c(sub_dirs[i],
                                                       point$id,
                                                       # the date will be changed to character (hence keep the number of days since 1990-01-01, to choose the time origin)
                                                       as.numeric(difftime(point$release_date, as.Date("1990-01-01"), units = "days")))
                                   
                                   # Saving the point object
                                   outfile_name <- paste0(point$id, "_", point$release_date, "_infos.rds")
                                   
                                   out_dir <- file.path(output_path_2, sub_dirs[i], point$id)
                                   dir.create(out_dir, showWarnings = F)
                                   
                                   saveRDS(point, file.path(out_dir, outfile_name))
                                 } else {
                                   non_null_point <- c(NA,NA,NA)
                                 }
                                 
                                 non_null_point
                                 
                               }
  
  
  registerDoSEQ()
  
  # change format of non_null_points
  non_null_points[[i]] <- as.data.frame(non_null_points[[i]])
  names(non_null_points[[i]]) <- c("sub_dir", "point_id", "release_date")
  
  #' format release dates back to date
  non_null_points[[i]] %>% dplyr::filter(!is.na(release_date)) -> non_null_points[[i]]
  non_null_points[[i]]$release_date <- as.Date("1990-01-01")+ as.difftime(as.numeric(non_null_points[[i]]$release_date), units = "days")
  
  
}

non_null_points <- bind_rows(non_null_points)