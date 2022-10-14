#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-10-12
#'@email : amael.dupaix@ird.fr
#'#*******************************************************************************************************************
#'@description :  This is the main script to study NLOG numbers trends from Ichthyop simulations
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

rm(list=ls())

STUDY_DIR <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/"

#' generate basic paths
WD <- file.path(STUDY_DIR,'3-Simulations_processing')
DATA_PATH <- file.path(STUDY_DIR,'0-Data')

OUTPUT_PATH <- file.path(WD,"Outputs")
FUNC_PATH <- file.path(WD,"Functions")
ROUT_PATH <- file.path(WD, "Sub-routines")
RESOURCE_PATH <- file.path(WD,"Resources")

try(dir.create(file.path(OUTPUT_PATH, "NLOG_trends")))

#' Load libraries
source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("ggplot2","plyr","sf","ggspatial","rnaturalearth","rnaturalearthdata","rgeos",
                     "raster","rworldmap","shape", "dplyr","zoo")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)

#' @arguments
# Path to where the outputs of 2-Post_Ichthyop are stored
sim_output_path <- file.path(STUDY_DIR, "2-Post_Ichthyop/Outputs/nemo_river_allMask")

# number of days over which the moving average is performed for the global time series
n_days_average <- 7

#' 0. Read simulation outputs
#' 
for (w in 1:9){
  years <- as.numeric(list.dirs(sim_output_path, recursive = F, full.names = F))
  years <- years[years!=2018]
  
  x <- seq(20, 140-1, 1) + 0.5
  y <- seq(-40, 40-1, 1) + 0.5
  #' Read the shapefile of the world seas, to filter in the for loop (1.):
  #'          Flanders Marine Institute (2018). IHO Sea Areas, version 3.
  #'          Available online at https://www.marineregions.org/ https://doi.org/10.14284/323
  IO <- read_sf(file.path(DATA_PATH,"World_Seas_IHO_v3/World_Seas_IHO_v3.shp")) %>%
    dplyr::filter(NAME %in% c("Indian Ocean", "Laccadive Sea", "Bay of Bengal", "Arabian Sea", "Mozambique Channel"))
  
  list_dfs <- list()
  for (i in 1:length(years)){
    cat("     ",paste0(years[i]),"\n")
    global_array_name <- file.path(sim_output_path, years[i], paste0("w",w, "_ltime2-360_thr-disch100"), "4.global_array.rds")
    if (file.exists(global_array_name)){
      arr <- readRDS(global_array_name)
      arr <- arr[,,grep(as.character(years[i]), dimnames(arr)[[3]])] #keep only the year of interest
      # arr <- apply(arr, c(1,2), mean) # calculate the yearly mean
      df_s <- data.frame(x = rep(x, times = length(y)*dim(arr)[3]),
                         y = rep(rep(y, each = length(x)), times = dim(arr)[3]),
                         day = rep(dimnames(arr)[[3]], each = dim(arr)[1]*dim(arr)[2]),
                         sim_nlog = as.vector(arr))
    
      #' 1. Keep only the cells in the IO (exclude cells which are not in the IO polygons)
      pts <- st_as_sf(df_s, coords = c("x","y"), crs = st_crs(4326))
      df_inIO <- st_intersects(pts, IO, sparse = F)
      df_s <- df_s[apply(df_inIO, 1, any),]
    
      #' 2. Define areas
      #'                a. Eastern-Western IO (<=80° and >80°)
      df_s %>% dplyr::mutate(area = if_else(x<80, "W", "E")) -> df_s
      plyr::ddply(df_s, .variables = c("day","area"), function(x) sum(x$sim_nlog)) -> list_dfs[[i]]
      plyr::ddply(df_s, .variables = c("day"), function(x) sum(x$sim_nlog)) %>%
        dplyr::mutate(area = "IO") %>%
        dplyr::select(day, area, V1) -> df_sum
      list_dfs[[i]] <- dplyr::bind_rows(list_dfs[[i]], df_sum)
    }
  }
  
  #' 3. Long term trend: yearly mean number of NLOGs per area
  #'                     Use the mean value of 2000 as a reference point
  dplyr::bind_rows(list_dfs) -> yearly_mean
  yearly_mean %>% dplyr::mutate(year = lubridate::year(as.Date(day))) %>%
    plyr::ddply(c("year", "area"), summarise, sim_nlog = mean(V1)) %>%
    do({df <- .; df %>% dplyr::mutate(sim_nlog = sim_nlog / df[which(df$year == "2000" & df$area == "IO"),"sim_nlog"])}) -> yearly_mean
  
  p <- ggplot(yearly_mean, aes(x=as.factor(year), y = sim_nlog, group = area, color = area))+
    geom_point()+xlab("Date")+ylab("Indicator of the number of NLOGs")+
    scale_color_brewer("Area", palette = "Set1")+
    ggtitle(w)
  
  ggplot2::ggsave(file.path(OUTPUT_PATH, "NLOG_trends", paste0("Yearly_mean_w",w,".png")), p,
                  width = 10, height = 6)
  
  #' 4. Seasonal trend: global times series of daily values
  #'                    Use January the 1st 2000 as a reference point
  dplyr::bind_rows(list_dfs) -> global_ts
  global_ts %>% dplyr::mutate(sim_nlog = V1 / global_ts[which(global_ts$day == "2000-01-01" & global_ts$area == "IO"),"V1"],
                           year = lubridate::year(as.Date(day)),
                           area = factor(area, levels = c("IO", "W","E"))) %>%
    dplyr::select(-V1) %>%
    dplyr::arrange(area, year, day) -> global_ts
  
  #' Moving averaging over the global time series
  #'       average over "n_days_average" days, centered
  #'       use the zoo::rollmean function
  global_ts$av_sim_nlog <- NA
  for (i in 1:length(years)){
    for (j in 1:length(unique(global_ts$area))){
      global_ts[which(global_ts$year == years[i] & global_ts$area == unique(global_ts$area)[j]),
             "av_sim_nlog"] <- zoo::rollmean(
               global_ts[which(global_ts$year == years[i] & global_ts$area == unique(global_ts$area)[j]),
                      "sim_nlog"],
               k = n_days_average,
               fill = NA
               )
    }
  }
 
  p <- ggplot(data = global_ts, aes(x = as.Date(day), y = sim_nlog, color = area, group = area))+
    facet_wrap(~year, nrow = 1, scales = "free_x")+
    geom_point(size = 0.5, alpha = 0.2)+
    geom_line(aes(x = as.Date(day), y = av_sim_nlog, color = area, group = area))+
    xlab("Date")+ylab("Indicator of the number of NLOGs")+
    scale_color_brewer("Area", palette = "Set1")+
    ggtitle(w)+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          title = element_text(hjust = 0.5))

  ggplot2::ggsave(file.path(OUTPUT_PATH, "NLOG_trends", paste0("Evolution_NLOG_number_w",w,".png")), p,
        width = 10, height = 6)
  
  
}