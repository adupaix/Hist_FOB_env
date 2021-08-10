#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-21
#'@email : 
#'#*******************************************************************************************************************
#'@description :  For each release point, get the information for weighting (cover surface, precipitations and 
#'  associated rivers with discharge) and save a matrix with the weight associated with each 
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************
#'@todo:
#'#*******************************************************************************************************************


msg <- bold("\n\n2. Getting information on input points\n\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
msg <- paste(" Weighting method:",weight_method, "\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

if (!weightExists){
  
  sub_dirs <- list.files(sim_output_path)
  
  if (weight_method != 1){
    #' get the table containing the number of cover points associated with each river
    n_cover_per_river <- read.csv(file.path(OUTPUT_PATH, sim_name, year, "1.nb_cover", "n_cover_per_river.csv"),
                                  header = T)
    
    #' Get the table linking river ids with input point ids
    link_river_input <- read.table(file.path(sim_input_path, "Link_table.txt"), header = T)
    #' keep only the rivers above the threshold in link_river_input
    link_river_input %>% dplyr::filter(MAIN_RIV %in% embouchures$MAIN_RIV) -> link_river_input
    
    non_null_points <- list()
    
    # pb <- progress_bar$new(format = "[:bar] :percent | Sub directory :current / :total",
    #                        total = length(sub_dirs))
    
    
    for (i in 1:length(sub_dirs)){
      
      # pb$tick()
      
      non_null_points[[i]] <- data.frame(matrix(ncol = 4))
      
      dir.create(file.path(output_path_2, sub_dirs[i]), recursive = T, showWarnings = F)
      
      rds_files <- list.files(file.path(sim_output_path, sub_dirs[i]), recursive = T, pattern = ".rds")
      # rds_files <- grep("infos", rds_files, invert = T, value = T)
      
      # rds_files <- rds_files[1:5000]
      
      #'@arguments
      #'**********
      
      cat(lines.to.cat)
      cat("Sub_directory", i, "/", length(sub_dirs), " - ",sub_dirs[i], "\n")
      
      cl <- makeCluster(nb_cores)
      registerDoSNOW(cl)
      pb <- txtProgressBar(max = length(rds_files), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
      
      # registerDoParallel(cores = nb_cores)
      
      
      non_null_points[[i]] <- foreach(k = 1:length(rds_files),
                                      .combine = rbind,
                                      .packages = srcUsedPackages,
                                      .options.snow = opts) %dopar% {
                                        
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
                                                              as.numeric(difftime(point$release_date, as.Date("1990-01-01"), units = "days")),
                                                              as.numeric(point$weight))
                                          
                                          # Saving the point object
                                          outfile_name <- paste0(point$id, "_", point$release_date, "_infos.rds")
                                          
                                          out_dir <- file.path(output_path_2, sub_dirs[i], point$id)
                                          dir.create(out_dir, showWarnings = F)
                                          
                                          saveRDS(point, file.path(out_dir, outfile_name))
                                        } else {
                                          non_null_point <- rep(NA, 4)
                                        }
                                        
                                        non_null_point
                                        
                                      }
      
      
      close(pb)
      stopCluster(cl)
      registerDoSEQ()
      
      # change format of non_null_points
      non_null_points[[i]] <- as.data.frame(non_null_points[[i]])
      names(non_null_points[[i]]) <- c("sub_dir", "point_id", "release_date", "weight")
      
    }
    
    #' bind all the data frames
    non_null_points <- bind_rows(non_null_points)
    
    #' remove NAs
    non_null_points %>% dplyr::filter(!is.na(release_date)) -> non_null_points
    #' format release dates back to date
    non_null_points$release_date <- as.Date("1990-01-01")+ as.difftime(as.numeric(as.character(non_null_points$release_date)), units = "days")
    
  } else if (weight_method == 1){
    
    #' get the points ids
    point_ids <- c()
    
    for (i in 1:length(sub_dirs)){
      point_ids <- c(point_ids, list.files(file.path(sim_output_path, sub_dirs[i])))
    }
    
    #' get the release dates
    rds_files <- list.files(file.path(sim_output_path, sub_dirs[1], point_ids[1]), pattern = ".rds")
    
    release_dates <- c()
    for (i in 1:length(rds_files)){
      release_dates <- c(release_dates, as.Date(sub("\\..*", "", sub(".*_", "", rds_files[i]))))
    }
    
    sub_dirs <- rep(sub_dirs, each = n_points_per_dir*length(release_dates))
    sub_dirs <- sub_dirs[1:(length(point_ids)*length(release_dates))]
    
    non_null_points <- data.frame(cbind(sub_dir = sub_dirs,
                                        point_id = rep(point_ids, each = length(release_dates)),
                                        release_date = rep(release_dates, times = length(point_ids)),
                                        weight = 1))
    
    non_null_points$release_date <- as.Date("1970-01-01") +
      as.difftime(as.numeric(as.character(non_null_points$release_date)), units = "days")
    
  }
  
  #' change non_null_points to matrix with:
  #'   in lines the point_id
  #'   in columns the release_date
  non_null_points %>%
    tidyr::spread(release_date, weight) -> weight_per_points_matrix
  
  #' save non_null_points summary
  write.csv(non_null_points, file = file.path(output_path_2, "weight_per_points_summary.csv"), row.names = F)
  write.csv(weight_per_points_matrix, file = file.path(output_path_2, "weight_per_points_matrix.csv"), row.names = F)
  
  #' save a log
  sink(logName2, append = F)
  cat("Execution time :", format(Sys.time()))
  sink()
  
} else {
  
  msg <- "Reading existing file\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
}

non_null_points <- read.csv(file = file.path(output_path_2, "weight_per_points_summary.csv"),
                            colClasses = c("factor","character","Date","factor"))

weight_per_points_matrix <- read.csv(file = file.path(output_path_2, "weight_per_points_matrix.csv"),
                                     colClasses = c("character"))
