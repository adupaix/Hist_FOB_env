#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-21
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description :  For each release point, get the information for weighting (cover surface, precipitations and 
#'  associated rivers with discharge)
#'  and save a matrix with the weights associated with each point 
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************
#'@comment: long script, only needs to run once per simulation
#'#*******************************************************************************************************************


msg <- bold("\n\n2. Getting information on input points\n\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

if (!Exists$weight){
  
  sub_dirs <- list.dirs(sim_output_path, recursive=F, full.names=F)
  
  #' Get the table linking river ids with input point ids
  link_river_input <- read.table(file.path(sim_input_path, "Link_table.txt"), header = T)
  #' keep only the rivers above the threshold in link_river_input
  link_river_input %>% dplyr::filter(MAIN_RIV %in% embouchures$MAIN_RIV) -> link_river_input
  
  #' for each sub directory, weight_per_point will contain a data frame with all the weights associated
  #' with all the release point - release date pairs
  weight_per_points <- list()
  
  #' delete the file which contains the ichthyop output errors
  #' cannot delete it in the initialization - 0.Init - because it is geenrated only if !Exists$weight
  unlink(Names$error_ichthyop_outputs)
  
  # for each sub directory
  for (i in 1:length(sub_dirs)){
      
    #' create the output sub directory in the output folder 2
    dir.create(file.path(output_paths[2], sub_dirs[i]), recursive = T, showWarnings = F)
    
    #' get all the names of the rds files in the sub directory
    dens_files <- list.files(file.path(sim_output_path, sub_dirs[i]), recursive = T, pattern = ".nc")
    
    #'@!!! uncomment for script testing
    # dens_files <- dens_files[1:480]
    
    cat(lines.to.cat)
    cat("Sub_directory", i, "/", length(sub_dirs), " - ",sub_dirs[i], "\n")
    cat("Getting release dates from .nc files\n")
    
    #' get release dates
    #' and rename the ichthyop outputs so that they contain the date
    #' 
    #' At the same time, check if any of the Ichthyop outputs are empty
    #' if an .nc file is empty, save its name in "empty_ichthyop_outputs.txt"
    #' and delete it from the dens_files list of names
      release_dates <- rep(as.Date("1900-01-01"), length(dens_files))
      empty_files <- c()
      for (k in 1:length(dens_files)){
        if(grepl("density.nc", dens_files[k])){
          dens_nc <- try(ncdf4::nc_open(file.path(sim_output_path, sub_dirs[i], dens_files[k])))
          if (class(dens_nc) == "try-error"){
            empty_files <- c(empty_files, dens_files[k])
            add_to_log <- paste("\n\nWarning: some Ichthyop outputs were empty. Please see\n", Names$error_ichthyop_outputs, "\nto have the list of empty .nc files")
          } else {
            release_dates[k] <- release_dates[k] + min(as.difftime(ncdf4::ncvar_get(dens_nc, varid = "time"), units = "secs"), na.rm = T)
            ncdf4::nc_close(dens_nc)
            
            file.rename(from = file.path(sim_output_path, sub_dirs[i], dens_files[k]),
                        to = file.path(sim_output_path, sub_dirs[i], sub("density.nc", paste0("density_", release_dates[k],".nc"), dens_files[k])))
          }
        } else {
          release_dates[k] <- as.Date(sub(".nc","", sub(".*density_", "", dens_files[k])))
        }
        
      }
    
    #' update the names stored in dens_files
    dens_files <- list.files(file.path(sim_output_path, sub_dirs[i]), recursive = T, pattern = ".nc")
    dens_files <- dens_files[!dens_files %in% empty_files]
    release_dates <- release_dates[!release_dates == "1900-01-01"]
    
    #' save the name of the outputs with an error
    sink(Names$error_ichthyop_outputs, append = T)
    writeLines(file.path(sub_dirs[i], empty_files))
    sink()
    
    #selecting only the points which are in the area of interest
    # input_in_IO <- cover_surface_per_input$id_curr[(cover_surface_per_input$id_curr > (i-1) * n_points_per_dir) & (cover_surface_per_input$id_curr <= i * n_points_per_dir)]
    # input_in_IO <- unique(input_in_IO)
    # if (length(input_in_IO) != 0){
    #   
    #   input_in_IO <- formatC(input_in_IO, flag = "0", digits = 4)
    #   dens_files <- grep(pattern = paste(input_in_IO, collapse = "|"),
    #                     x = dens_files,
    #                     value = T)
      
    cat("Getting information on input points\n")
    
    if (!cluster){
      # set cluster for parallel run, and initialize progress bar
      cl <- makeCluster(nb_cores)
      registerDoSNOW(cl)
      pb <- txtProgressBar(max = length(dens_files), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
      
      #' for each rds file (each rds file contains the density matrices
      #' associated with a release at 1 release date from 1 release point)
      weight_per_points[[i]] <- foreach(k = 1:length(dens_files),
                                        .combine = rbind,
                                        .packages = srcUsedPackages,
                                        .options.snow = opts) %dopar% {
                                          
                                          ## open the precipitations netcdf file
                                          precip <- ncdf4::nc_open(filename = file.path(DATA_PATH,"precip.mon.mean.nc"))
                                          
                                          #' create a point "object"
                                          point <- list()
                                          
                                          #' get the id from the file name
                                          point$id <- sub("/.*","",dens_files[k])
                                          
                                          #' keep the file name
                                          fname <- sub(".*/","",dens_files[k])
                                          
                                          #'@get_information_on_release_point
                                          #'***********************************
                                          
                                          # get release coordinates
                                          point <- get.coords.release(sim_input_path,
                                                                      point)
                                          
                                          # get release date
                                          point$release_date <- release_dates[k]
                                          
                                          cheat = F
                                          if (year(point$release_date) == max(release_years)){cheat = T ; year(point$release_date) <- year(point$release_date) - 1}
                                          
                                          # get precipitations
                                          point <- get.precipitations(precip, point)
                                          
                                          # get forest cover
                                          point <- get.cover.surface(cover_surface_per_input, point)
                                          
                                          # get rivers and associated discharge
                                          point <- get.associated.rivers.and.precip(link_river_input, cover_surface_per_river, embouchures, point, precip)
                                          
                                          # get river mouths and associated discharge + cover
                                          # point <- get.associated.rivers(link_river_input, n_cover_per_mouth, embouchures, point, mouth = T)
                                          
                                          if (any(!is.na(point$rivers))){
                                            if (round(point$total_cover_surface_m2) != round(point$coastal_cover_surface_m2 +
                                                                                             sum(unlist(lapply(point$rivers$data, function(x) sum(x$river_cover_surface_m2)))))){
                                              stop("Error: total cover surface does not correspond to sum of coastal and river associated cover surfaces")
                                            }
                                          }
                                          
                                          #' get length of coastline associated with the point
                                          point <- get.coastline.length(cover_surface_per_input, point)
                                          
                                          #' get weights (returns a vector with the weight for all the weighting methods)
                                          weights <- get.weights(point)
                                          point$weights <- weights
                                          
                                          if (cheat == T){year(point$release_date) <- year(point$release_date) + 1}
                                          
                                          # fill in weight_per_points
                                          weight_per_point <- c(sub_dirs[i],
                                                                point$id,
                                                                # the date will be changed to character (hence keep the number of days since 1990-01-01, to choose the time origin)
                                                                as.numeric(difftime(point$release_date, as.Date("1990-01-01"), units = "days")),
                                                                as.numeric(weights))
                                          
                                          # Save the point object
                                          outfile_name <- paste0(point$id, "_", point$release_date, "_infos.rds")
                                          
                                          out_dir <- file.path(output_paths[2], sub_dirs[i], point$id)
                                          dir.create(out_dir, showWarnings = F)
                                          
                                          saveRDS(point, file.path(out_dir, outfile_name))
                                          
                                          #close the netcdf connection
                                          ncdf4::nc_close(precip)
                                          
                                          #' return the vector with c(sub_dir, point_id, release_date, all the weights)
                                          weight_per_point
                                          
                                        }
      
      #' stop parallel and close progress bar
      close(pb)
      stopCluster(cl)
      registerDoSEQ()
    } else {
      
      cl <- parallel::makeCluster(nb_cores)
      doParallel::registerDoParallel(cl)
      #' for each rds file (each rds file contains the density matrices
      #' associated with a release at 1 release date from 1 release point)
      weight_per_points[[i]] <- foreach(k = 1:length(dens_files),
                                        .combine = rbind,
                                        .packages = srcUsedPackages) %dopar% {
                                          
                                          ## open the precipitations netcdf file
                                          precip <- ncdf4::nc_open(filename = file.path(DATA_PATH,"precip.mon.mean.nc"))
                                          
                                          #' create a point "object"
                                          point <- list()
                                          
                                          #' get the id from the file name
                                          point$id <- sub("/.*","",dens_files[k])
                                          
                                          #' keep the file name
                                          fname <- sub(".*/","",dens_files[k])
                                          
                                          #'@get_information_on_release_point
                                          #'***********************************
                                          
                                          # get release coordinates
                                          point <- get.coords.release(sim_input_path,
                                                                      point)
                                          
                                          # get release date
                                          point$release_date <- release_dates[k]
                                          
                                          cheat = F
                                          if (year(point$release_date) == max(release_years)){cheat = T ; year(point$release_date) <- year(point$release_date) - 1}
                                          
                                          # get precipitations
                                          point <- get.precipitations(precip, point)
                                          
                                          # get forest cover
                                          point <- get.cover.surface(cover_surface_per_input, point)
                                          
                                          # get rivers and associated discharge
                                          point <- get.associated.rivers.and.precip(link_river_input, cover_surface_per_river, embouchures, point, precip)
                                          
                                          # get river mouths and associated discharge + cover
                                          # point <- get.associated.rivers(link_river_input, n_cover_per_mouth, embouchures, point, mouth = T)
                                          
                                          if (any(!is.na(point$rivers))){
                                            if (round(point$total_cover_surface_m2) != round(point$coastal_cover_surface_m2 +
                                                                                             sum(unlist(lapply(point$rivers$data, function(x) sum(x$river_cover_surface_m2)))))){
                                              stop("Error: total cover surface does not correspond to sum of coastal and river associated cover surfaces")
                                            }
                                          }
                                          
                                          #' get length of coastline associated with the point
                                          point <- get.coastline.length(cover_surface_per_input, point)
                                          
                                          #' get weights (returns a vector with the weight for all the weighting methods)
                                          weights <- get.weights(point)
                                          point$weights <- weights
                                          
                                          if (cheat == T){year(point$release_date) <- year(point$release_date) + 1}
                                          
                                          # fill in weight_per_points
                                          weight_per_point <- c(sub_dirs[i],
                                                                point$id,
                                                                # the date will be changed to character (hence keep the number of days since 1990-01-01, to choose the time origin)
                                                                as.numeric(difftime(point$release_date, as.Date("1990-01-01"), units = "days")),
                                                                as.numeric(weights))
                                          
                                          # Save the point object
                                          outfile_name <- paste0(point$id, "_", point$release_date, "_infos.rds")
                                          
                                          out_dir <- file.path(output_paths[2], sub_dirs[i], point$id)
                                          dir.create(out_dir, showWarnings = F)
                                          
                                          saveRDS(point, file.path(out_dir, outfile_name))
                                          
                                          #close the netcdf connection
                                          ncdf4::nc_close(precip)
                                          
                                          #' return the vector with c(sub_dir, point_id, release_date, all the weights)
                                          weight_per_point
                                          
                                        }
      
    }
      
    parallel::stopCluster(cl)
    
      # change format of weight_per_points
      weight_per_points[[i]] <- as.data.frame(weight_per_points[[i]])
      names(weight_per_points[[i]]) <- c("sub_dir", "point_id", "release_date", paste0("w", 1:n_weight_methods))
      
    # }
    
  }
    
  #' bind all the data frames
  weight_per_points <- bind_rows(weight_per_points)
    
  #' format release dates back to date
  weight_per_points$release_date <- as.Date("1990-01-01")+ as.difftime(as.numeric(as.character(weight_per_points$release_date)), units = "days")
  
  #' for each of the weighting methods
  wmethods <- paste0("w",1:n_weight_methods)
  for (i in 1:n_weight_methods){
    #' generate a matrix containing:
    #'     column 1: sub_dir names where the release point is saved into
    #'     column 2: point_id
    #'     following columns: for each release date, the weight associated with each release point
    weight_per_points %>% dplyr::select(-(grep(i, wmethods, value = T, invert = T))) %>%
      tidyr::spread(release_date, paste0("w", i)) -> weight_per_points_matrix
    
    #' save the matrix with the weight_method number in the name
    write.csv(weight_per_points_matrix, file = file.path(output_paths[2], paste0("weight_per_points_matrix_w",i,".csv")), row.names = F)  
  }
  
  #' save weight_per_points summary
  write.csv(weight_per_points, file = file.path(output_paths[2], "weight_per_points_summary.csv"), row.names = F)
  
  
  #' save a log
  sink(Names$log2, append = F)
  cat("Date & time :", format(Sys.time()), "\n")
  if (exists("add_to_log")){cat(add_to_log) ; rm(add_to_log)}
  sink()
  
} else {
  #' if all the weight matrices are generated, we just read the one of interest (after the if)
  
  msg <- "Reading existing file\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
}

# weight_per_points <- read.csv(file = file.path(output_paths[2], "weight_per_points_summary.csv"),
#                             colClasses = c("factor","character","Date",rep("factor", n_weight_methods)))

#'@output_for_next_subroutine
weight_per_points_matrix <- read.csv(file = file.path(output_paths[2], paste0("weight_per_points_matrix_w",weight_method,".csv")),
                                     colClasses = c("character"))

#' Do not delete
toKeep <- c(toKeep, "weight_per_points_matrix")
