#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-09
#'@email : 
#'#*******************************************************************************************************************
#'@description :  Apply weight and mortality to arrays
#'
#'used indices:
#'    @i : release date
#'    @k : release point
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************
#'@todo: 
#'#*******************************************************************************************************************

msg <- bold("\n\n3. Applying weight and mortality\n\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

if(!log3Exists){
  
  sub_dirs <- weight_per_points_matrix$sub_dir
  points_id <- weight_per_points_matrix$point_id
  
  #'@!!!!
  # points_id <- points_id[1:100]
  
  weight_per_points_matrix %>% select(-point_id, -sub_dir) -> weight_per_points_matrix
  for (i in 1:dim(weight_per_points_matrix)[2]){weight_per_points_matrix[,i] <- as.numeric(weight_per_points_matrix[,i])}
  
  for (i in 1:dim(weight_per_points_matrix)[2]){
    
    release_date.i <- as.Date(sub("X", "", gsub("\\.", "-", names(weight_per_points_matrix)[i])))
    
    cat(lines.to.cat)
    cat("Release date", i, "/", dim(weight_per_points_matrix)[2], " - ",format(release_date.i), "\n")
    
    weight.i <- as.numeric(as.character(weight_per_points_matrix[,i]))
    # non_null_points %>% dplyr::filter(release_date == release_date.i) -> non_null_points.i
    
    
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
                         
                         array.k <- readRDS(file.path(sim_output_path, sub_dirs[k], points_id[k], paste0(points_id[k], "_", release_date.i, ".rds")))
                         
                         array.k <- array.k * weight.i[k]
                         
                         array.k
                         
                       }
    
    close(pb)
    stopCluster(cl)
    registerDoSEQ()
    
    #' apply mortality
    array.i <- apply.mortality(array.i, ltime, ltime_method, ltime_sd)
    
    #' save the results
    fname <- file.path(output_path_3, paste0(format(release_date.i),".rds"))
    saveRDS(array.i, fname)
    
  }
  
  weighted_arrays <- list.files(output_path_3)
  
  #' save a log
  sink(logName3, append = F)
  cat("Execution time :", format(Sys.time()))
  cat("\n Number of generated .rds files:", length(list.files(output_path_3))-1)
  sink()
  
  
} else {
  
  msg <- "Files already generated\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  weighted_arrays <- list.files(output_path_3)
  
}



