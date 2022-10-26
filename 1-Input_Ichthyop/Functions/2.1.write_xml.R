#!/usr/bin/env Rscript

# sous fontion de input.2.xml

#' @1
#' Generate a cfg file for the Ichthyop simulation from one point
#' @arguments:
#'  initial_time (POSIXct): vector of all the times of release
#'  tplate (chr): path to the template file which will be used to generate the cfg files
#'  sim_input_path (chr): path to the location of the current products which will be used in the Ichthyop simulation
#'  sim_output_path (chr): path to where the results of the Ichthyop simulation will be saved
#'  record_period (num): time between the saving of 2 positions of particles in the Ichthyop simulation
#'  dir_name.i (chr): sub-folder where the cfg file is to be saved
#'  i_chr (chr): number of the sub-folder where the cfg file will be saved
#'  
#'  long (num): longitude of the release point
#'  lat (num): latitude of the release point
#'  nb (chr): number of the point formatted as follow: "000xx" (5 digits)
write.cfg.xml <- function(initial_time,
                          tplate,
                          sim_input_path,
                          sim_output_path,
                          record_period,
                          dir_name.i,
                          i_chr,
                          
                          long, lat,
                          nb
                          ){
  
  #~ Name of the arguments in the xml file
  param_list <- c("transport_duration",
                  "initial_time",
                  "input_path",
                  "output_path",
                  "lon_stain",
                  "lat_stain",
                  "record_frequency")
  
  #~ Formatted values to replace in the template
  default_value <- c("0180 day\\(s\\) 00 hour\\(s\\) 00 minute\\(s\\)",
                     "year 1980 month 01 day 02 at 00:00",
                     "/home/adupaix/Documents/ichthyop-private/input/",
                     "/home/adupaix/Documents/ichthyop-private/output/PHILIN12.L75_river_onMask_d9/",
                     53,
                     -11.5,
                     12)
  
  value <- list(paste0(formatC(transport_duration, width = 4, flag = "0"),
                       " day\\(s\\) 00 hour\\(s\\) 00 minute\\(s\\)"),
                #' #' sample is used to shuffle the release dates. Because a lot of simulations will run at the same time, if the release dates
                #' #' are ordered, all simulations will read the same forcing product more or less at the same time. Shuffling the release dates
                #' #' should allow to gain time
                #' sample(paste0("year ",year(initial_time),
                #'               " month ",formatC(month(initial_time), width = 2, flag = "0"),
                #'               " day ",formatC(day(initial_time), width = 2, flag = "0"),
                #'               " at ",formatC(hour(initial_time), width = 2, flag = "0"),
                #'               ":",formatC(minute(initial_time), width = 2, flag = "0"))),
                paste0("year ",year(initial_time),
                       " month ",formatC(month(initial_time), width = 2, flag = "0"),
                       " day ",formatC(day(initial_time), width = 2, flag = "0"),
                       " at ",formatC(hour(initial_time), width = 2, flag = "0"),
                       ":",formatC(minute(initial_time), width = 2, flag = "0")),
                sim_input_path,
                file.path(sim_output_path, paste0("points_", i_chr), nb),
                long,
                lat,
                (record_period*24*3600)/2400)
  
  # for each parameter
  for (k in 1:length(param_list)){
    # get the line l where the parameter section starts
    l <- which(grepl(pattern = paste0("<key>",param_list[k]), tplate))
    m = l
    # get the following line where the value is entered (m)
    while (grepl("<value>", tplate[m]) == F) {
      m=m+1
    }
    
    # replace the default value in this line with the new value
    tplate[m] <- gsub(pattern = as.character(default_value[k]),
                      replacement = as.character(value[[k]][1]),
                      x = tplate[m])
    
    # if several values are to be entered for this parameter (initial_time)
    if (length(value[[k]])>1) {
      # insert a new line with the same formatting for each value
      for (n in 1:(length(value[[k]])-1)){
        tplate <- c(tplate[1:(m+n-1)],
                    gsub(pattern = as.character(value[[k]][1]),
                         replacement = as.character(value[[k]][n+1]),
                         x = tplate[m]),
                    tplate[(m+n):length(tplate)])
      }
    }
  }
  
  
  #~ Write tplate into a new cfg file
  cfg_name <- as.character(paste0("cfg_point_",nb,".xml"))
  try(file.remove(cfg_name), silent = T)
  file.create(file.path(dir_name.i, cfg_name))
  
  cfg <- file(file.path(dir_name.i, cfg_name), open = "w")
  writeLines(tplate, cfg)
  close(cfg)
  
  
}


#' @2
#' Generate the txt files which contain the commands launched by the mpi in the pbs job
#' @arguments:
#'   sim_input_path (chr): path to the directory where the cfg files and the current product are stored on the cluster
#'   cfg_path (chr): path where the jobs will be saved
#'   cfg_dir (chr): name of the directory which will contain the cfg files (format: cfgs_year1-year2)
#'   n_pbs_jobs (num): number of pbs jobs to generate
#'   n_mpi (num): vector which, for each generated job, contains the number of mpi asked
generate.command.list <- function(sim_input_path, cfg_path, cfg_dir, n_pbs_jobs, n_mpi, ichthyop_version){
  
  files <- list.files(cfg_path, pattern = "cfg_point", recursive = T)
  
  # save a log every 200 config files
  log <- rep("/dev/null", length(files))
  log[seq(1, length(log), 200)] <- paste0("sim", seq(1, length(log), 200), ".log")
  
  lignes <- paste0("java -jar ichthyop-private/target/ichthyop-",
                   ichthyop_version,
                   ".jar ",
                   sim_input_path,
                   "/",
                   cfg_dir,
                   "/",
                   files,
                   " >& ",
                   log)
  l = length(lignes)
  
  n_per_job <- c(1,
                 cumsum(round(l * n_mpi / sum(n_mpi))))
  n_per_job[length(n_per_job)] <- l+1
  
  for (i in 1:n_pbs_jobs){
    indexes = n_per_job[i]:(n_per_job[i+1]-1)
    
    file.create(file.path(cfg_path, paste0("commands_simu-",i,".txt")))
    cmds <- file(file.path(cfg_path, paste0("commands_simu-",i,".txt")), open = "w")
    writeLines(lignes[indexes], cmds)
    close(cmds)
  }
  
}


#' @3
#' Generate the jobs to run the Ichthyop simulations
#' @arguments:
#'   template (chr): list of the paths to the .pbs jobs templates ($pbs_cp and $pbs)
#'   sim_input_path (chr): path to the directory where the cfg files and the current product are stored on the cluster
#'   cfg_path (chr): path where the jobs will be saved
#'   cfg_dir (chr): name of the directory which will contain the cfg files (format: cfgs_year1-year2)
#'   release_year (num): year of the simulation
#'   n_pbs_jobs (num): number of pbs jobs to generate
#'   n_mpi (num): vector which, for each generated job, contains the number of mpi asked
#'   walltime (num): vector which, for each generated job, contains the walltime asked (in hours)
#'   curr_prod (chr): name of the forcing product which will be used in the Ichthyop simulation
#'   initial_time (POSIXct): vector of all the times of release
generate.jobs.pbs <- function(template, sim_input_path, cfg_path, cfg_dir,
                              n_pbs_jobs, n_mpi, walltime, curr_prod, initial_time,
                              transport_duration,
                              path_where_the_forcing_product_is_stored,
                              release_year){
  
  # 1. generate the job which copies ichthyop and the mpi to scratch
  pbs_text <- scan(template$pbs_cp, what = "", sep = "\n", quiet = T)
  
     # Copy the forcing product
  # put the name of the forcing product in the line to copy
  pbs_text <- sub("[forcing-product]", curr_prod, pbs_text, fixed = T)
  #change the path where the forcing product is copied FROM
  pbs_text <- sub("[path_where_the_forcing_product_is_stored]",
                  path_where_the_forcing_product_is_stored,
                  pbs_text, fixed = T)
  #change the path where the forcing product is copied TO
  pbs_text <- sub("[sim_input_path]", sim_input_path, pbs_text, fixed = T)
  # put the years in the line copying the config files
  pbs_text <- sub("[release_year]", release_year, pbs_text, fixed = T)
  
  # put the year 
  years <- seq(min(year(initial_time)),
               max(year(initial_time + as.difftime(transport_duration, units = "days"))))
  j <- grep("[year]", pbs_text, fixed=T)
  for (i in 1:length(years)){
    txt <- sub("[year]", years[i], pbs_text[j], fixed=T)
    pbs_text <- c(pbs_text[1:(j+i-1)],
                  txt,
                  pbs_text[(j+i):length(pbs_text)])
  }
  pbs_text <- pbs_text[-grep("[year]", pbs_text, fixed=T)]
  
  #add lines to launch the other jobs
  pbs_text <- append(pbs_text,
                     paste0("qsub ", path_where_the_forcing_product_is_stored,
                            "/",curr_prod,"/cfgs_",release_year,
                            "/sim_ichthyop-",release_year,"-",1:n_pbs_jobs,".pbs")
                     )
  
  # save the file
  file.create(file.path(cfg_path, paste0("sim_ichthyop-",release_year,"-0.pbs")))
  job <- file(file.path(cfg_path, paste0("sim_ichthyop-",release_year,"-0.pbs")), open = "w")
  writeLines(pbs_text, job)
  close(job)
  
  # 2. generate the other jobs
  tplate <- scan(template$pbs, what = "", sep = "\n", quiet = T)
  
  for (i in 1:n_pbs_jobs){
    
    pbs_text <- tplate
    
    # specify the number of mpi asked
    pbs_text[grep("mpi_10", pbs_text)] <- sub("10", n_mpi[i], pbs_text[grep("mpi_10", pbs_text)])
    
    #specify the time asked
    pbs_text[grep("walltime", pbs_text)] <- sub("20", walltime[i], pbs_text[grep("walltime", pbs_text)])
    
    #write the line lauching the mpi specifying the number of cores and the path to the command list
    pbs_text[grep("MPI_LAUNCH", pbs_text)] <- paste0("time $MPI_LAUNCH -np ", 28*n_mpi[i],
                                                     " ichthyop-mpi/ichthyopmpi ",
                                                     sim_input_path, "/", cfg_dir, "/commands_simu-", i, ".txt &> out-",
                                                     i ,".log")
    
    file.create(file.path(cfg_path, paste0("sim_ichthyop-",release_year,"-",i,".pbs")))
    job <- file(file.path(cfg_path, paste0("sim_ichthyop-",release_year,"-",i,".pbs")), open = "w")
    writeLines(pbs_text, job)
    close(job)
  }
}

