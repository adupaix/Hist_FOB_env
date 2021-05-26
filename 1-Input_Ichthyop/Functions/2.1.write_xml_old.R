
# sous fontion de input.2.xml

write.cfg.xml <- function(template,
                          long, lat,
                          cfg_nb,
                          transport_duration = 180,
                          initial_time = c("1980/02/01 00:00",
                                           "1980/03/01 00:00",
                                           "1980/04/01 00:00"),
                          sim_input_path,
                          sim_output_path,
                          dir_path
                          ){
  
  
  #~ Read the template file
  tplate <- scan(template, what = "", sep = "\n", quiet = T)
  
  #~ Name of the arguments in the xml file
  param_list <- c("transport_duration",
                  "initial_time",
                  "input_path",
                  "output_path",
                  "lon_stain",
                  "lat_stain")
  
  #~ Formatted values to replace in the template
  default_value <- c("0180 day\\(s\\) 00 hour\\(s\\) 00 minute\\(s\\)",
                     "year 1980 month 01 day 02 at 00:00",
                     "/home/adupaix/Documents/ichthyop-private/input/",
                     "/home/adupaix/Documents/ichthyop-private/output/PHILIN12.L75_river_onMask_d9/",
                     53,
                     -11.5)
  
  #~ Format the given arguments, to be saved in the xml files
  initial_time <- as.POSIXct(initial_time)
  
  value <- list(paste0(formatC(transport_duration, width = 4, flag = "0"),
                       " day\\(s\\) 00 hour\\(s\\) 00 minute\\(s\\)"),
             paste0("year ",year(initial_time),
                   " month ",formatC(month(initial_time), width = 2, flag = "0"),
                   " day ",formatC(day(initial_time), width = 2, flag = "0"),
                   " at ",formatC(hour(initial_time), width = 2, flag = "0"),
                   ":",formatC(minute(initial_time), width = 2, flag = "0")),
             sim_input_path,
             sim_output_path,
             long,
             lat)
  
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
  cfg_name <- file.path(dir_path, "cfg",paste0("cfg_point_",
                                               formatC(cfg_nb, width = 5, flag = "0"),
                                               ".xml"))
  file.create(cfg_name)
  cfg <- file(file.path(cfg_name), open = "w")
  writeLines(tplate, cfg)
  close(cfg)
  
  
  
}



#'#***************************************************************************



write_zone_xml <- function(filename, template, df_points){
  
  filer <- file(filename, open = "r")
  filew <- file(template, open = "w")
  
  header <- readLines(filer, n = 46)
  writeLines(header, filew)
  
  writeLines("<zones>", filew)
  
  delta = 10^-4
  
  for (i in 1:dim(df_points)[1]){
    cat("  <zone>",
        paste0("    <key>",i,"</key>"),
        "    <type>release</type>",
        "    <enabled>true</enabled>",
        "    <color>[r=255,g=0,b=255]</color>",
        "    <polygon>",
        "      <point>",
        paste0("        <index>",0,"</index>"),
        paste0("        <lon>",df_points$x[i],"</lon>"),
        paste0("        <lat>",df_points$y[i],"</lat>"),
        "      </point>",
        paste0("        <index>",1,"</index>"),
        paste0("        <lon>",df_points$x[i]+delta,"</lon>"),
        paste0("        <lat>",df_points$y[i],"</lat>"),
        "      </point>",
        paste0("        <index>",2,"</index>"),
        paste0("        <lon>",df_points$x[i]+delta,"</lon>"),
        paste0("        <lat>",df_points$y[i]+delta,"</lat>"),
        "      </point>",
        paste0("        <index>",3,"</index>"),
        paste0("        <lon>",df_points$x[i],"</lon>"),
        paste0("        <lat>",df_points$y[i]+delta,"</lat>"),
        "      </point>",
        "    </polygon>",
        "    <bathy_mask>",
        "      <enabled>false</enabled>",
        "      <line_inshore>0.0</line_inshore>",
        "      <line_offshore>12000.0</line_offshore>",
        "    </bathy_mask>",
        "    <thickness>",
        "      <enabled>true</enabled>",
        "      <lower_depth>15.0</lower_depth>",
        "      <upper_depth>0.0</upper_depth>",
        "    </thickness>",
        "  </zone>",
        
        file = filew, sep="\n", append = T)
  }
  writeLines("</zones>", filew)
  
  close(filer)
  close(filew)
  
}