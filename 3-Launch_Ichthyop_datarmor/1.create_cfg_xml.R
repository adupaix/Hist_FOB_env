#!/usr/bin/env Rscript

library(lubridate)

#~ Read arguments
args = commandArgs(TRUE)
template = args[1]
long = args[2]
lat = args[3]
nb = args[4]
sim_input_path = args[5]
sim_output_path = args[6]

#~ In case the arguments are not provided properly
if (length(args) < 6){
  cat("-> Arguments are missing - Please run script again\n\n")
  cat("Expected arguments:\n  1. Template file name\n")
  cat("  2. Starting point longitude\n  3. Starting point latitude\n")
  cat("  4. Number of the point (in '000xx' format), to be saved in cfg file name\n  5. Path where the forcing product is save (input_path)\n")
  cat("  6. Path where the Ichthyop outputs are to be saved (output_path)\n")
  quit(save = "no")
}



#~ Fixed arguments
transport_duration = 100 #in days

first_release_date = "1980/01/02"
last_release_date = "1980/06/02"
release_frequency = 2 # in weeks
record_frequency = 7 #in days (interval between two recorded positions)


# generate the initial times from the start/end dates and the frequency
initial_time <- seq(as.POSIXct(paste(first_release_date, "00:00")),
                    as.POSIXct(paste(last_release_date, "00:00")),
                    as.difftime(release_frequency, units = "weeks"))

#~ Read the template file
tplate <- scan(template, what = "", sep = "\n", quiet = T)

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
for (i in 1:length(param_list)){
  # get the line j where the parameter section starts
  j <- which(grepl(pattern = paste0("<key>",param_list[i]), tplate))
  k = j
  # get the following line where the value is entered (k)
  while (grepl("<value>", tplate[k]) == F) {
    k=k+1
  }

  # replace the default value in this line with the new value
  tplate[k] <- gsub(pattern = as.character(default_value[i]),
                    replacement = as.character(value[[i]][1]),
                    x = tplate[k])

  # if several values are to be entered for this parameter (initial_time)
  if (length(value[[i]])>1) {
    # insert a new line with the same formatting for each value
    for (l in 1:(length(value[[i]])-1)){
      tplate <- c(tplate[1:(k+l-1)],
                  gsub(pattern = as.character(value[[i]][1]),
                       replacement = as.character(value[[i]][l+1]),
                       x = tplate[k]),
                  tplate[(k+l):length(tplate)])
    }
  }
}


#~ Write tplate into a new cfg file
cfg_name <- as.character(paste0("cfg_point_",nb,".xml"))
try(file.remove(cfg_name))
file.create(cfg_name)

cfg <- file(cfg_name, open = "w")
writeLines(tplate, cfg)
close(cfg)

#~ Empty the ichthyop output directory if exists
try(file.remove(file.path(sim_output_path, nb , list.files(file.path(sim_output_path, nb)))))

