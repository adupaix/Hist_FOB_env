

  # 0.  Create names, paths and log file----
  #=========================================
cat("\14")
tic(msg = "### 1. read.ncs() function", quiet = F)
cat(crayon::bold("###### 1. Generating arrays from simulation results\n"))
start1 <- Sys.time()

  # 1. Calculate the global array (one matrix per timestep) ----
  #=============================================================
  
# If the global array or the mean global matrix were not built before, calculate it
if(!glob_array_exists){
    
    # a. Arguments and verification ----
    #-----------------------------------
  cat("\na. Generating the global array")
  NC_PATH <- file.path(DATA_PATH, sim_name)
  nc_files <- list.files(NC_PATH, pattern = "*.nc")
  
  # check if there are files corresponding to the simulation of interest
  if(length(nc_files)==0){
    sink(logName, append = T)
    cat("\n\n\n#####\nError: no available nc files for the given arguments\n#####")
    sink()
    stop("No available nc files for the given arguments")
  }
    
  # Getting sim_nb for the nc.to.Array function
  s_nb <- substr(nc_files, nchar(nc_files)-4, nchar(nc_files)-3)
  s_nb <- as.numeric(gsub("[^0-9]","",s_nb))
    
    ### b. Get information on rivers ----
    #-----------------------------------
  if (input_location == "river"){
    # récupérer le river_id
    # nom du dossier qui contient les positions d'input utilisees pour la simulation
    cat("\n  Recovering rivers data for filters and weight")
    dir_name <- "river_nlog_input_"
    if (input_method == "kFromCoast"){
      dir_name <- paste0(dir_name, dist,"k_from_coast")
    } else if (input_method == "onMask"){
      dir_name <- paste0(dir_name, forcing,"_mask")
    }
    
    # fichier txt qui contient les positions d'input et l'identifiant riviere correspondant
    river_id <- read.table(file.path(RESOURCE_PATH,"input_particles_txt", dir_name, "IDs.txt"))
    names(river_id) <- c("MAIN_RIV","init_longitude","init_latitude")
    
    # Read the river data.
    river_data <- read.river.data(RESOURCE_PATH, river_id, thr_disch)
    
  } else {
    river_id <- c()
    river_data <- c()
  }
  
    
  # c. Apply nc.to.Array to count the number of particles per cell per timestep -----
  #----------------------------------------------------------------------------------
  cat("\n  Counting the number of particle per cell for each timestep")
  arrays_per_sim <- parallel::mcmapply(nc.to.Array, nc_name = nc_files, sim_nb = s_nb,
                                       MoreArgs = list(NC_PATH = NC_PATH,
                                                       ltime = ltime,
                                                       ltime_method = ltime_method,
                                                       input_method = input_method,
                                                       dist = dist,
                                                       gsize = gsize,
                                                       river_id = river_id,
                                                       river_data = river_data,
                                                       weight_method = weight_method),
                                       SIMPLIFY = F,
                                       mc.cores = nb_cores)
    
  # d. Bind the results and aggregate per day ----
  #-----------------------------------------------
  cat("\n  Binding the results and aggregating per timestep")
  # bind the list of arrays into one global array
  glob_array <- abind::abind(arrays_per_sim, along = 3)
  rm(arrays_per_sim) ; invisible(gc())
    
  # arrange the array by time
  glob_array <- glob_array[,,sort(dimnames(glob_array)[[3]])]
    
  nm <- dimnames(glob_array)[[3]]
  
  # aggregate the array by timestamp, to have one data.frame per timestamp (instead of per timestamp per simulation)
  glob_array <- foreach(day.i = unique(nm),
                        .combine = function(x,y) abind::abind(x,y, along = 3)) %do% {
                          
                          array.i <- glob_array[,,which(dimnames(glob_array)[[3]] == day.i)]
                          apply(array.i, c(1,2), sum)
                          
                        }
  nm <- unique(nm)
  dimnames(glob_array)[[3]] <- nm
  
  # e. Remove the beginning and the end of the simulation ----
  #-----------------------------------------------------------
  # At the beginning, there is only one simulation running, after one month, there is 2, etc.
  # After ltime, particles from the 1st simulation are removed, at ltime + 1 month, particles from the second sim, etc
  # At the last timesteps, there are only particles from the last simulation
  # So, we keep only the matrices between t0 + ltime - 1 month & t(last) - ltime + 1 month
  t_between_pos <- as.numeric(lead(as.POSIXct(nm, format = "%Y-%m-%d_%H:%M:%S")) -
                                as.POSIXct(nm, format = "%Y-%m-%d_%H:%M:%S"), units = "days")[1]
  n_pos_to_remove <- (ltime - 30) / t_between_pos
    
  first_timestep <- n_pos_to_remove+1
  last_timestep <- dim(glob_array)[3] - n_pos_to_remove
    
  glob_array <- glob_array[,, first_timestep:last_timestep]
    
  # Save the global array
  saveRDS(glob_array, file = globalName)
  cat("\n\n Global array saved at :", globalName)
  sink(logName, append = T)
  cat("\n\nGlobal array saved at :\n", globalName)
  sink()

} else if (glob_array_exists){
  cat("\na. Global array already exists")
  cat("\nGlobal array used is at :\n", globalName)
  sink(logName, append = T)
  cat("\na. Global array already exists")
  cat("\n\n Global array used is at :\n", globalName)
  sink()
  
  glob_array <- readRDS(globalName)
  nm <- dimnames(glob_array)[[3]]
}
    
if (!mean_agg_array_exists){ 
  # 2. Build an array with the mean value for each time scale over the whole study period ----
  #===========================================================================================
  
  # for example, if time_scale = "month", returns an array with the mean value for each month (dim = c(nrow, ncol, 12))
  
  cat("\n\nb. Generating the mean aggregated array")
    
  nm <- dimnames(glob_array)[[3]]
  
  if(agg.time_scale == "day"){
    new_nm <- paste0(ifelse(nchar(month(nm)) == 2, month(nm), paste0("0",month(nm))),
                    "-",
                    ifelse(nchar(day(nm)) == 2, day(nm), paste0("0",day(nm))))
    
  } else if (agg.time_scale == "month"){
    new_nm <- paste(ifelse(nchar(month(nm)) == 2, month(nm), paste0("0",month(nm))))
    
  } else if (agg.time_scale == "quarter"){
    new_nm <- paste("Quarter", quarter(nm, fiscal_start = 11))
    
  } else if (agg.time_scale == "year"){
    new_nm <- rep("Global", dim(glob_array)[[3]])
  }
  
  dimnames(glob_array)[[3]] <- new_nm
  time_scale <- sort(unique(new_nm))
  
  registerDoParallel(cores = nb_cores)
  
  mean_agg_array <- foreach(time_scale.i = time_scale,
                       .combine = function(x,y) abind::abind(x,y, along = 3)) %dopar% {
                         
                         array.i <- glob_array[,,which(dimnames(glob_array)[[3]] == time_scale.i)]
                         apply(array.i, c(1,2), get("mean"))
                         
                       }
  
  registerDoSEQ()
  
  if (length(dim(mean_agg_array))==3){
    dimnames(mean_agg_array)[[3]] <- time_scale
  }
    
  # save the mean aggregated array
  saveRDS(mean_agg_array, file = meanAggName)
  cat("\n\n Mean aggregated array saved at :", meanAggName)
  sink(logName, append = T)
  cat("\n\nMean aggregated array saved at :\n", meanAggName)
  sink()
  
} else if (mean_agg_array_exists){
  cat("\n\nb. Mean aggregated array already exists")
  cat("\nMean aggregated array used is at :\n", meanAggName)
  sink(logName, append = T)
  cat("\nb. Mean aggregated array already exists")
  cat("\n\n Mean aggregated array used is at :\n", meanAggName)
  sink()
  
  mean_agg_array <- readRDS(meanAggName)
}  

  
glob_array <- readRDS(globalName)
nm <- dimnames(glob_array)[[3]]
  
# 3. Aggregate the global array per time_scale ----
#==================================================

# returns the value for each time_scale. For example, if time_scale = "quarter" returns a value for
# each quarter of each year

if (!agg_array_exists){
  
  cat("\n\nc. Generating the aggregated array")
  
  # aggregate the array by time_scale, to have one data.frame per time_scale (with the mean value)
  if(agg.time_scale == "day"){
    new_nm <- paste(day(nm), month(nm, label=T, abbr = F), year(nm))
    
  } else if (agg.time_scale == "month"){
    new_nm <- paste(month(nm, label=T, abbr = F),year(nm))
    
  } else if (agg.time_scale == "quarter"){
    new_nm <- paste(quarter(nm, with_year = T, fiscal_start = 11))
    
  } else if (agg.time_scale == "year"){
    new_nm <- year(nm)
  }
  
  dimnames(glob_array)[[3]] <- new_nm
  time_scale <- unique(new_nm)
  
  registerDoParallel(cores = nb_cores)
  
  agg_array <- foreach(time_scale.i = time_scale,
                       .combine = function(x,y) abind::abind(x,y, along = 3)) %dopar% {
                         
                         array.i <- glob_array[,,which(dimnames(glob_array)[[3]] == time_scale.i)]
                         apply(array.i, c(1,2), get("mean"))
                         
                       }
  
  registerDoSEQ()
  
  dimnames(agg_array)[[3]] <- time_scale
  
  # save the aggregated array
  saveRDS(agg_array, file = aggregateName)
  cat("\n\n Aggregated array saved at :", aggregateName, "\n\n")
  sink(logName, append = T)
  cat("\n\nAggregated array saved at :\n", aggregateName)
  sink()


} else if (agg_array_exists){
  
  cat("\n\nc. Aggregated array already exists")
  cat("\nAggregated array used is at :\n", aggregateName)
  sink(logName, append = T)
  cat("\nc. Aggregated array already exists")
  cat("\n\n Aggregated array used is at :\n", aggregateName)
  sink()
  
  agg_array <- readRDS(aggregateName)
  
}


# Update DoNotDeleteMe
DoNotDeleteMeToo <- c("glob_array", "agg_array", "mean_agg_array")
DoNotDeleteMe <- c(DoNotDeleteMe, DoNotDeleteMeToo, "DoNotDeleteMeToo")

# Save execution time in the log file
end1 <- Sys.time()
tps_exec <- end1 - start1
units(tps_exec) <- "mins"
tps_exec_mins <- trunc(tps_exec)
tps_exec_secs <- tps_exec - trunc(tps_exec)
units(tps_exec_secs) <- "secs"
tps_exec_secs <- round(tps_exec_secs, digits = 2)
sink(logName, append = T)
cat("\n\nExecution time :", as.character(tps_exec_mins), "minutes", as.character(tps_exec_secs), "seconds\n\n\n")
sink()


# print execution time
toc()


# Clear environment
if (agg.time_scale != 'day'){
  rm(list = ls()[!ls() %in% DoNotDeleteMe]) ; invisible(gc())
} else {
  rm(list = ls()[!ls() %in% DoNotDeleteMeToo]) ; invisible(gc())
}



