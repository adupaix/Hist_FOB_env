#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-13
#'@email : amael.dupaix@ird.fr
#'#*******************************************************************************************************************
#'@description :  This is the main script to calculate distance between observers data and Ichthyop simulation results
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

rm(list=ls()[!ls() %in% c("year","effort_threshold")])

STUDY_DIR <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/"

#' generate basic paths
WD <- file.path(STUDY_DIR,'3-Distance_with_data')
DATA_PATH <- file.path(STUDY_DIR,'0-Data')

OUTPUT_PATH <- file.path(WD,"Outputs")
FUNC_PATH <- file.path(WD,"Functions")
ROUT_PATH <- file.path(WD, "Sub-routines")
RESOURCE_PATH <- file.path(WD,"Resources")

#' @arguments
sim_output_path <- file.path(STUDY_DIR, "2-Post_Ichthyop/Outputs/nemo_river_allMask")

#' Delete the outputs to calculate anew (T) or not (F)
RESET = T

#' Year for which we want to do the comparison
year = 2008

#' Threshold used to filter the observers data, noted "T"
#' Cells with a number of days of observation < T are discarded 
effort_threshold = 0

## Generate df of NLOG observations
source(file.path(FUNC_PATH, "Maps", "1.Maps_obs.R"))

world <- map_data("world")

if (!file.exists(file.path(DATA_PATH, "Observers_data", paste0("Obs_NLOG_",year,"_",effort_threshold,"d_of_obs.csv"))) | RESET){
  data <- read.csv(file.path(DATA_PATH, "Observers_data", "all_operations_on_fobs_observe_fr_indian.csv"))
  
  rast <- map_occurence(Ob7 = data, DATA_PATH = file.path(DATA_PATH, "Observers_data"),
                        obj.type = "NLOG", year = year,
                        return_raster = T,
                        delete.low.effort = T,
                        eff.threshold = effort_threshold)
  df <- as.data.frame(rast, xy = T)
  names(df) <- c("x","y","NLOGdata")
  
  df$NLOGdata[is.infinite(df$NLOGdata)] <- NA
  df$NLOGdata <- df$NLOGdata / max(df$NLOGdata, na.rm = T)
  
  write.csv(df, file = file.path(DATA_PATH, "Observers_data", paste0("Obs_NLOG_",year,"_",effort_threshold,"d_of_obs.csv")))
} else {
  df <- read.csv(file.path(DATA_PATH, "Observers_data", paste0("Obs_NLOG_",year,"_",effort_threshold,"d_of_obs.csv")))
}


## Generate df of simulated NLOG density
# open array
plist <- list()
distance <- c()

for (i in 1:9){
  global_array_name <- file.path(sim_output_path, year, paste0("w",i, "_ltime2-360_thr-disch100"), "4.global_array.rds")
  if (file.exists(global_array_name)){
    arr <- readRDS(global_array_name)
    arr <- arr[,,grep(as.character(year), dimnames(arr)[[3]])] #keep only the year of interest
    arr <- apply(arr, c(1,2), mean) # calculate the yearly mean
    x <- seq(20, 140-1, 1) + 0.5
    y <- seq(-40, 40-1, 1) + 0.5
    df_s <- data.frame(x = rep(x, times = length(y)),
                       y = rep(y, each = length(x)),
                       sim_nlog = as.vector(arr))
    res = 2
    df_s %>% mutate(x_new = res*floor(x/res) + res/2,
                    y_new = res*floor(y/res) + res/2) %>%
      plyr::ddply(c("x_new","y_new"), function(x) mean(x$sim_nlog)) -> df_s
    
    names(df_s) <- c("x","y","NLOGsim")
    
    # merge data and simulations
    df_m <- merge(df, df_s, by = c("x","y"))
    df_m[which(is.na(df_m$NLOGdata)),"NLOGsim"] <- NA #remove sim results out of data range
    df_m %>% filter(!is.na(NLOGsim)) -> df_m
    df_m$NLOGsim <- df_m$NLOGsim / max(df_m$NLOGsim, na.rm = T)
    
    df_m %>% dplyr::mutate(dist = (NLOGdata - NLOGsim)^2,
                           w = i) -> df_m
    
    if(i == 1){
      df_tot <- df_m
    } else {
      df_tot <- bind_rows(df_tot, df_m)
    }
    
    distance[i] <- sum(df_m$dist)
    
    plist[[i]] <- ggplot() +
      geom_sf() +
      coord_sf(xlim = c(35, 100), ylim = c(-25, 25), expand = FALSE, crs = st_crs(4326))+
      geom_tile(data=df_m, aes(x, y, fill=dist)) +
      scale_fill_gradientn(colors=c("black","blue","yellow","red"),
                           limits = c(0,1))+
      geom_polygon(data=world, aes(x=long, y=lat, group=group))+
      ggtitle(paste0("w",i," - d =", round(distance[i], digits = 10)))
    
  }
}

for (i in 1:length(plist)){if(length(plist[[i]]) == 0){plist[[i]] <- plist[[i-1]]}}

fig <- ggpubr::ggarrange(plotlist = plist)

ggsave(file.path(OUTPUT_PATH, paste0("distance_maps",year,"_",effort_threshold,"d_of_obs.png")),
       width = 12, height = 8)

write.csv(df_tot, file.path(OUTPUT_PATH, paste("distances_data_sim_", year, "_",effort_threshold,"d_of_obs.csv")))








dfs <- list() ; year = 2018
for (i in 1:12){
  rast <- map_occurence(Ob7 = data, DATA_PATH = file.path(DATA_PATH, "Observers_data"),
                        obj.type = "NLOG", year = year, month = c(i),
                        return_raster = T,
                        delete.low.effort = T,
                        eff.threshold = effort_threshold)
  df <- as.data.frame(rast, xy = T)
  names(df) <- c("x","y","NLOGdata")
  
  df$NLOGdata[is.infinite(df$NLOGdata)] <- NA
  df$NLOGdata <- df$NLOGdata / max(df$NLOGdata, na.rm = T)
  
  dfs[[i]] <- df
}

maps <- list()
for (i in 1:12){
  maps[[i]] <- ggplot() +
    geom_sf() +
    coord_sf(xlim = c(35, 100), ylim = c(-25, 25), expand = FALSE, crs = st_crs(4326))+
    geom_tile(data=dfs[[i]], aes(x, y, fill=NLOGdata)) +
    scale_fill_gradientn(colors=c("black","blue","yellow","red"))+
    geom_polygon(data=world, aes(x=long, y=lat, group=group))+
    # ggtitle(paste0("w",i," - d =", round(distance[i], digits = 10)))
    ggtitle(i)
}
obs <- ggpubr::ggarrange(plotlist = maps)
obs
