#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-09
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description :  Apply weight and mortality to arrays
#'
#'used indices:
#'    @i : release date
#'    @k : release point
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************
#'@comment
#'#*******************************************************************************************************************

msg <- bold("\n\n3. Applying weight and mortality\n\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

if(!Exists$log3){
  
  # retrieve the point_ids and the corresponding sub directory
  sub_dirs <- weight_per_points_matrix$sub_dir
  points_id <- weight_per_points_matrix$point_id
  
  #'@!!!! uncomment for script testing
  # points_id <- points_id[1:100]
  
  # keep only the weights in the data frame
  weight_per_points_matrix %>% select(-point_id, -sub_dir) -> weight_per_points_matrix
  # change them to numeric
  for (i in 1:dim(weight_per_points_matrix)[2]){weight_per_points_matrix[,i] <- as.numeric(as.character(weight_per_points_matrix[,i]))}
  
  #' keep only the input points with non null weight at at least one release date
  input_to_keep <- apply(weight_per_points_matrix, 1, sum) != 0
  
  #' If any input_to_keep is NA, there is an error (no weight is associated to a point)
  #' However, if that point with no weight is in error_ichthyop_outputs, we do not take it into account
  #' it means that there is no associated weight because the Ichthyop output was empty and it has already be signaled 
  #' in the second routine
  na_points_id <- points_id[which(is.na(input_to_keep))]
  theres_an_error <- c()
  if (length(na_points_id) != 0){
    add_to_log <- paste("\n\nWarning: some Ichthyop outputs were empty. Please see\n", Names$error_ichthyop_outputs, "\nto have the list of empty .nc files")
    for (i in 1:length(na_points_id)){
      theres_an_error[i] <- !any(grepl(na_points_id[i], readLines(Names$error_ichthyop_outputs)))
    }
  }
  
  if(any(theres_an_error)){
    stop("Error: some points - for which the Ichthyop simulations worked - don't have any associated weight")
  } else if (all(input_to_keep, na.rm = T) == F){
    stop("Error: no point has any non null weight")
  }
  #' if no error was detected, remove the NAs and replace them by F
  input_to_keep[which(is.na(input_to_keep))] <- F
  
  weight_per_points_matrix <- weight_per_points_matrix[input_to_keep,]
  sub_dirs <- sub_dirs[input_to_keep]
  points_id <- points_id[input_to_keep]
  
  
  # for each of the release dates
  for (i in 1:dim(weight_per_points_matrix)[2]){
    
    # get the release date in the column name
    release_date.i <- as.Date(sub("X", "", gsub("\\.", "-", names(weight_per_points_matrix)[i])))
    
    # print message
    cat(lines.to.cat)
    cat("Release date", i, "/", dim(weight_per_points_matrix)[2], " - ",format(release_date.i), "\n")
    
    # name of the output generated in output folder 3
    fname <- file.path(output_paths[3], paste0(format(release_date.i),".rds"))
    
    if (file.exists(fname)){
      # if the output was already generated for this release date, prints a message and goes directly to the next iteration
      cat("Weight file already generated") ; Sys.sleep(0.2)
    } else {
      # get the weights associated with each release points at the given date
      weight.i <- as.numeric(as.character(weight_per_points_matrix[,i]))
      
      if (!cluster){
        # set cluster for parallel run, and initialize progress bar
        cl <- makeCluster(nb_cores)
        registerDoSNOW(cl)
        pb <- txtProgressBar(max = length(points_id), style = 3)
        progress <- function(n) setTxtProgressBar(pb, n)
        opts <- list(progress = progress)
        
        #' read, weight and sum all the arrays for the release date i
        array.i <- foreach(k = 1:length(points_id),
                           .combine = f.for.combining,
                           .packages = srcUsedPackages,
                           .options.snow = opts) %dopar% {
                             
                             dens_files <- list.files(file.path(sim_output_path, sub_dirs[k], points_id[k]))
                             nc_file_name.k <- grep(release_date.i, dens_files, value = T)
                             
                             nc.k <- ncdf4::nc_open(file.path(sim_output_path, sub_dirs[k], points_id[k], nc_file_name.k))
                             t <- ncdf4::ncvar_get(nc.k, varid = "time")
                             time_to_keep <- which(!is.na(t) & t<10^30)
                             t <- t[time_to_keep]
                             array.k <- ncdf4::ncvar_get(nc.k, varid = "density")[,,time_to_keep]
                             ncdf4::nc_close(nc.k)
                             # array.k <- readRDS(file.path(sim_output_path, sub_dirs[k], points_id[k], paste0(points_id[k], "_", release_date.i, ".rds")))
                             dimnames(array.k)[[3]] <- t
                             
                             array.k <- array.k * weight.i[k]
                             
                             array.k
                             
                           }
        
        #' stop parallel and close progress bar
        close(pb)
        stopCluster(cl)
        registerDoSEQ()
      } else {
        
        cl <- parallel::makeCluster(nb_cores)
        doParallel::registerDoParallel(cl)
        #' read, weight and sum all the arrays for the release date i
        array.i <- foreach(k = 1:length(points_id),
                           .combine = f.for.combining,
                           .packages = srcUsedPackages) %dopar% {
                             
                             dens_files <- list.files(file.path(sim_output_path, sub_dirs[k], points_id[k]))
                             nc_file_name.k <- grep(release_date.i, dens_files, value = T)
                             
                             nc.k <- ncdf4::nc_open(file.path(sim_output_path, sub_dirs[k], points_id[k], nc_file_name.k))
                             t <- ncdf4::ncvar_get(nc.k, varid = "time")
                             # time_to_keep <- which(!is.na(t) & t<10^30)
                             # t <- t[time_to_keep]
                             # array.k <- ncdf4::ncvar_get(nc.k, varid = "density")[,,time_to_keep]
                             array.k <- ncdf4::ncvar_get(nc.k, varid = "density")
                             ncdf4::nc_close(nc.k)
                             # array.k <- readRDS(file.path(sim_output_path, sub_dirs[k], points_id[k], paste0(points_id[k], "_", release_date.i, ".rds")))
                             dimnames(array.k)[[3]] <- t
                             
                             array.k <- array.k * weight.i[k]
                             
                             array.k
                             
                           }
      }
      
      parallel::stopCluster(cl)
      
      #' transform the dates in dimnames back to date format
      dimnames(array.i)[[3]] <- as.character(as.difftime(as.numeric(dimnames(array.i)[[3]]), units = "secs") + as.Date("1900-01-01"))
      
      #' apply mortality
      array.i <- apply.mortality(array.i, ltime, ltime_method, ltime_sd)
      
      #' save the results
      saveRDS(array.i, fname)
    }
    
  }
  
  #' save a log
  sink(Names$log3, append = F)
  cat("Date & time :", format(Sys.time()), "\n")
  cat("\n Number of generated .rds files:", length(list.files(output_paths[3], pattern = ".rds")))
  cat("\n\n  Weighting method :", paste(weight_informations[weight_method,], collapse = " - "))
  cat("\n\n  Life time method :", ltime_method)
  cat("\n  Mean life time :", ltime)
  if (ltime_method == 1){cat("\n  Life time standard deviation:", ltime_sd)}
  if (exists("add_to_log")){cat(add_to_log) ; rm(add_to_log)}
  sink()
  
  
} else {
  #' if log.txt exists (hence all the rds files were generated)
  
  msg <- "Files already generated\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  
}

#' @output_for_next_subroutine
weighted_arrays <- list.files(output_paths[3], pattern = ".rds")

