#!/usr/bin/env Rscript

# sous fontion de input.2.xml

write.cfg.xml <- function(initial_time,
                          tplate,
                          sim_input_path,
                          sim_output_path,
                          record_frequency,
                          dir_name.i,
                          
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
                paste0("year ",year(initial_time),
                       " month ",formatC(month(initial_time), width = 2, flag = "0"),
                       " day ",formatC(day(initial_time), width = 2, flag = "0"),
                       " at ",formatC(hour(initial_time), width = 2, flag = "0"),
                       ":",formatC(minute(initial_time), width = 2, flag = "0")),
                sim_input_path,
                file.path(sim_output_path, nb),
                long,
                lat,
                (record_frequency*24*3600)/1800)
  
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



