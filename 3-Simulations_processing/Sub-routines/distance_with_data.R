#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-08-13
#'@email : amael.dupaix@ird.fr
#'#*******************************************************************************************************************
#'@description :  This is the script calculating distance between observers data and Ichthyop simulation results
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

area_year_output_path <- file.path(OUTPUT_PATH, "Distance_to_data", AREAS, "csv_outputs", year)
try(dir.create(area_year_output_path, showWarnings = F, recursive = T))

processed_observers_file_path <- file.path(area_year_output_path, paste0("Obs_NLOG_",year,"_",effort_threshold,"d_of_obs.csv"))
distance_file_path <- file.path(area_year_output_path,
                                paste("distances_data_sim_", year, "_",effort_threshold,"d_of_obs.csv"))

if(!file.exists(distance_file_path) |
   RESET){
  if (!file.exists(processed_observers_file_path) | RESET){
    df <- list()
    cat("Calculating indicator of NLOG number from obs data\n")
    pb <- txtProgressBar(min = 0, max = 12, style = 3, width = 24)
    for (m in 1:12){
      setTxtProgressBar(pb, m)
      cat(paste(" ; Month:", m))
      rast <- map_occurence(Ob7 = data, DATA_PATH = file.path(DATA_PATH, "Observers_data"),
                            obj.type = "NLOG", year = year,
                            gsize = 2, month = m,
                            return_raster = T,
                            delete.low.effort = T,
                            data_preped = T,
                            eff.threshold = effort_threshold)
      df[[m]] <- as.data.frame(rast, xy = T)
      df[[m]]$month <- m
      names(df[[m]]) <- c("x","y","NLOGdata", "month")
      
      df[[m]]$NLOGdata[is.infinite(df[[m]]$NLOGdata)] <- NA
      
    }
    close(pb)
    
    df <- dplyr::bind_rows(df)
    df %>% dplyr::mutate(effort_threshold = effort_threshold) -> df
    
    write.csv2(df, file = processed_observers_file_path,
              row.names = FALSE)
  } else {
    cat("Reading existing processed obs file\n")
    df <- read.csv2(processed_observers_file_path)
  }
  
  
  ## Generate df of simulated NLOG density
  # open array
  plist <- list()
  
  cat("\nReading processed simulations and calculating distance\n")
  pb <- txtProgressBar(min = 0, max = 9, style = 3, width = 24)
  for (i in 1:9){
    setTxtProgressBar(pb, i)
    cat(paste(" ; Weight scenario:", i))
    monthly_array_name <- file.path(sim_output_path, year, paste0("w",i, "_ltime2-360_thr-disch100"), "4.maps_month", "aggregated_array_mean.rds")
    if (file.exists(monthly_array_name)){
      arr <- readRDS(monthly_array_name)
      arr <- arr[,,grep(as.character(year), dimnames(arr)[[3]])] #keep only the year of interest
      # arr <- apply(arr, c(1,2), mean) # calculate the yearly mean
      x <- seq(20, 140-1, 1) + 0.5
      y <- seq(-40, 40-1, 1) + 0.5
      # df_s <- data.frame(x = rep(x, times = length(y)),
      #                    y = rep(y, each = length(x)),
      #                    sim_nlog = as.vector(arr))
      df_s <- data.frame(x = rep(x, times = length(y)*dim(arr)[3]),
                         y = rep(rep(y, each = length(x)), times = dim(arr)[3]),
                         month = rep(dimnames(arr)[[3]], each = dim(arr)[1]*dim(arr)[2]),
                         sim_nlog = as.vector(arr))
      res = 2
      df_s %>% mutate(x_new = res*floor(x/res) + res/2,
                      y_new = res*floor(y/res) + res/2) %>%
        plyr::ddply(c("x_new","y_new","month"), function(x) mean(x$sim_nlog)) -> df_s
      
      names(df_s) <- c("x","y","month","NLOGsim")
      
      # merge data and simulations
      df_s$month <- as.numeric(sub(paste0(year,"-"), "", df_s$month))
      df_m <- merge(df, df_s, by = c("x","y", "month"))
      df_m %>% filter(!is.na(NLOGdata)) -> df_m #remove sim results out of data range
      
      # scale the NLOG values from the 2 columns, based on their respective maximum
      df_m$NLOGsim <- df_m$NLOGsim / max(df_m$NLOGsim, na.rm = T)
      df_m$NLOGdata <- df_m$NLOGdata / max(df_m$NLOGdata, na.rm = T)
      
      # calculate the individual cell distance
      df_m %>% dplyr::mutate(dist = (NLOGdata - NLOGsim)^2,
                             w = i) -> df_m
      
      if(i == 1){
        df_tot <- df_m
      } else {
        df_tot <- bind_rows(df_tot, df_m)
      }
      
      # plist[[i]] <- ggplot() +
      #   geom_sf() +
      #   coord_sf(xlim = c(35, 100), ylim = c(-25, 25), expand = FALSE, crs = st_crs(4326))+
      #   geom_tile(data=df_m, aes(x, y, fill=dist)) +
      #   scale_fill_gradientn(colors=c("black","blue","yellow","red"),
      #                        limits = c(0,1))+
      #   geom_polygon(data=world, aes(x=long, y=lat, group=group))+
      #   ggtitle(paste0("w",i," - d =", round(distance[i], digits = 10)))
      
    } else {
      stop("The requested simulations outputs do not exist")
    }
  }
  close(pb)
  # Adding area column
  cat("\nAdding area column\n")
  if (AREAS == "IO"){
    df_tot$area <- "Global"
    
  } else if (AREAS == "E_W"){
    df_tot %>% dplyr::mutate(area = if_else(x<80, "W", "E")) -> df_tot
    
  } else {
    df_tot <- add.area.column(df = df_tot,
                              area = my_areas)
  }
  
  write.csv2(df_tot, file = distance_file_path,
            row.names = F)
  
  distance <- ddply(df_tot, "w", summarise, sum(dist))
} else {
  cat("Reading existing file\n")
  df_tot <- read.csv2(distance_file_path)
  
}

