#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-10-12
#'@email : amael.dupaix@ird.fr
#'#*******************************************************************************************************************
#'@description :  This is the main script to map NLOG numbers from Ichthyop simulations
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

# choose if take the NLOGs in the Burma Sea into account
# add_burma_sea <- T

# Weighting methods for which the maps are built
# weight_methods <- c(2,3,4,7,1)
weight_methods <- c(2,3,4,6,7,9)

#' @AREAS
#' IO: study of the global trend on the whole ocean basin
#' E_W: 2 zones, east and west
#' IOTC: same areas used in Dupaix et al. (2021) and Dagorn et al. (2013)
#' IHO: areas obtained from the shapefile of the world seas:
#'          Flanders Marine Institute (2018). IHO Sea Areas, version 3.
#'          Available online at https://www.marineregions.org/ https://doi.org/10.14284/323
#' myAreas: personalized areas
# AREAS <- c("IO", "E_W", "IOTC", "IHO", "myAreas")
AREAS <- "myAreas"

#' @do_not_change:
daily_saved_years <- c(2002,2004,2008,2012,2014:2019)

#' Create output path
NEW_OUTPUT_PATH <- file.path(OUTPUT_PATH, "Maps_density",
                             AREAS)
mapply(dir.create,
       file.path(NEW_OUTPUT_PATH,
                 c("Mean_density","SD_density",
                   "data_frames")),
       MoreArgs = list(recursive = T,
                       showWarnings = F))

#' Read necessary functions
source(file = file.path(FUNC_PATH, "readAreas.R"))
source(file = file.path(FUNC_PATH, "add.area.column.R"))


# Weights correspondance table
weight_informations <- data.frame(cbind(1:9,
                                        c("No weight",
                                          "CL",
                                          "CC",
                                          "RC",
                                          "River cover * mean river discharge + coastal cover",
                                          "CCp",
                                          "RCp",
                                          "Precipitations * (River cover * mean river discharge + coastal cover)",
                                          "R&CC"
                                        )))
weight_informations$X2 <- as.character(weight_informations$X2)


# Read the necessary areas
#' Read the shapefile of the world seas, to filter in the for loop (1.):
#'          Flanders Marine Institute (2018). IHO Sea Areas, version 3.
#'          Available online at https://www.marineregions.org/ https://doi.org/10.14284/323
world <- map_data("world")
IO <- read.IO.areas("IHO", DATA_PATH = DATA_PATH, add_burma_sea = T)
#' Read the areas selected with the AREAS argument. If AREAS not in 'IO' 'E_W'
if (AREAS %in% c("IOTC","IHO","myAreas")){
  my_areas <- read.IO.areas(AREAS, DATA_PATH = DATA_PATH,
                              add_burma_sea = T)
}
# List years with outputs
years <- as.numeric(list.dirs(sim_output_path, recursive = F, full.names = F))
years <- years[!is.na(years)]


# add areas surface
my_areas$surface_km2 <- as.vector(st_area(my_areas)) * 10**-6


#' 0. Read simulation outputs
#' 
for (w in weight_methods){
  message <- paste("\14 Weight method:",w,"\n===============\n    - Reading simulation outputs\n") ; cat(message)
  
  if (!file.exists(file.path(NEW_OUTPUT_PATH, "data_frames",
                            paste0("global_df_w", w, ".Rdata")))){
    x <- seq(20, 140-1, 1) + 0.5
    y <- seq(-40, 40-1, 1) + 0.5
    
    list_dfs <- list()
    for (i in 1:length(years)){
      cat(message, paste("     ",paste0(years[i]),"\n"))
      global_array_name <- file.path(sim_output_path, years[i], paste0("w",w, "_ltime2-360_thr-disch100"), "4.global_array.rds")
      if (file.exists(global_array_name)){
        arr <- readRDS(global_array_name)
        arr <- arr[,,grep(as.character(years[i]), dimnames(arr)[[3]])] #keep only the year of interest
        # arr <- apply(arr, c(1,2), mean) # calculate the yearly mean
        df_s <- data.frame(x = rep(x, times = length(y)*dim(arr)[3]),
                           y = rep(rep(y, each = length(x)), times = dim(arr)[3]),
                           day = rep(dimnames(arr)[[3]], each = dim(arr)[1]*dim(arr)[2]),
                           sim_nlog = as.vector(arr))
        
        df_s %>% dplyr::filter(!sim_nlog < max(sim_nlog)/10**4) -> df_s
        
        #' 1. Keep only the cells in the IO (exclude cells which are not in the IO polygons)
        
        pts <- st_as_sf(df_s, coords = c("x","y"), crs = st_crs(4326))
        df_inIO <- st_intersects(pts, IO, sparse = F)
        df_s <- df_s[apply(df_inIO, 1, any),]
        
        rm(pts, df_inIO)
        
        #' 2. Define areas
        if (AREAS == "IO"){
          df_s$area <- "Global"
          plyr::ddply(df_s, .variables = c("day","area"), function(x) sum(x$sim_nlog)) -> list_dfs[[i]]
          
          rm(df_s)
          
        } else {
          if (AREAS == "E_W"){
            df_s %>% dplyr::mutate(area = if_else(x<80, "W", "E")) -> df_s
          } else if (AREAS %in% c("IHO", "IOTC","myAreas")){
            df_s <- add.area.column(df_s, my_areas)
          }
          # filter the cells which are not in the defined areas
          df_s %>% dplyr::filter(!is.na(area)) -> df_s
          
          # plyr::ddply(df_s, .variables = c("day","area"), function(x) sum(x$sim_nlog)) -> list_dfs[[i]]
          # plyr::ddply(df_s, .variables = c("day"), function(x) sum(x$sim_nlog)) %>%
          #   dplyr::mutate(area = "Global") %>%
          #   dplyr::select(day, area, V1) -> df_sum
          list_dfs[[i]] <- df_s
          
          rm(df_s)
        }
        
        # Delete aberrant points
        # df_s <- list()
        # areas <- unique(list_dfs[[i]]$area)
        # for (j in 1:length(areas)){
        #   df <- list_dfs[[i]] %>% dplyr::filter(area == areas[j])
        #   df_s[[j]] <- df[-delete.aberrant(df$V1),]
        # }
        # list_dfs[[i]] <- bind_rows(df_s)
        
        
      }
    }
    
    save(list_dfs, file = file.path(WD, "temp", "list_dfs.Rdata"))
    load(file.path(WD, "temp", "list_dfs.Rdata"))
    
    dplyr::bind_rows(list_dfs) %>%
      dplyr::mutate(area = as.factor(area),
                    sim_nlog = sim_nlog / max(sim_nlog),
                    day = as.Date(day),
                    quarter = ceiling(lubridate::month(day)/3),
                    year = lubridate::year(day)) -> global_df
    
    save(global_df, file = file.path(NEW_OUTPUT_PATH, "data_frames",
                                     paste0("global_df_w", w, ".Rdata")))
  } else{
    load(file = file.path(NEW_OUTPUT_PATH, "data_frames",
                          paste0("global_df_w", w, ".Rdata")))
  }
  
  message <- c(message, "   - Summarizing simulation outputs\n") ; cat(message)
  
  plyr::ddply(.data = global_df,
              .variables = c("x","y", "quarter","year"),
              function(x) mean(x$sim_nlog)) %>%
    plyr::ddply(.variables = c("x","y","quarter"),
                function(x) mean(x$V1)) %>%
    dplyr::rename("sim_nlog" = "V1") -> quarterly_mean
  
  plyr::ddply(.data = global_df,
              .variables = c("x","y", "quarter"),
              function(x) sd(x$sim_nlog)) %>%
    dplyr::rename("sim_nlog" = "V1") -> quarterly_sd
  
  message <- c(message, "   - Building maps\n") ; cat(message)
  
  p <- ggplot()+
    geom_tile(data = quarterly_mean, aes(x=x,y=y, fill = sim_nlog, color = sim_nlog))+
    scale_fill_viridis_c("Simulated indicator\nof NLOG density", trans = "log10")+
    scale_color_viridis_c("Simulated indicator\nof NLOG density", trans = "log10")+
    facet_wrap(~quarter)+
    geom_sf(data = my_areas, fill = NA, color = "grey90")+
    coord_sf(xlim = c(20, 130), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
    geom_polygon(data=world, aes(x=long, y=lat, group=group))+
    ylab("Latitude")+xlab("Longitude")+
    ggtitle(weight_informations[w,2])+
    theme(panel.background = element_rect(fill = "grey90", color = "black"),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  ggplot2::ggsave(file.path(NEW_OUTPUT_PATH, "Mean_density",
                            paste0("Quarterly_density_w", w, ".png")),
                  p,
                  width = 12,
                  height = 8)
  saveRDS(p, file.path(NEW_OUTPUT_PATH, "Mean_density",
                       paste0("Quarterly_density_w", w, ".rds")))
  
  p <- ggplot()+
    geom_tile(data = quarterly_mean, aes(x=x,y=y, fill = sim_nlog, color = sim_nlog))+
    scale_fill_viridis_c("Simulated indicator\nof NLOG density", trans = "log10")+
    scale_color_viridis_c("Simulated indicator\nof NLOG density", trans = "log10")+
    geom_sf(data = my_areas, fill = NA, color = "grey90")+
    coord_sf(xlim = c(20, 130), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
    geom_polygon(data=world, aes(x=long, y=lat, group=group))+
    ylab("Latitude")+xlab("Longitude")+
    ggtitle(weight_informations[w,2])+
    theme(panel.background = element_rect(fill = "grey90", color = "black"),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  ggplot2::ggsave(file.path(NEW_OUTPUT_PATH, "Mean_density",
                            paste0("Density_w", w, ".png")),
                  p,
                  width = 12,
                  height = 8)
  saveRDS(p, file.path(NEW_OUTPUT_PATH, "Mean_density",
                       paste0("Density_w", w, ".rds")))
  
  p <- ggplot()+
    geom_tile(data = quarterly_sd, aes(x=x,y=y, fill = sim_nlog, color = sim_nlog))+
    scale_fill_viridis_c("Simulated indicator\nof NLOG density", trans = "log10")+
    scale_color_viridis_c("Simulated indicator\nof NLOG density", trans = "log10")+
    geom_sf(data = my_areas, fill = NA, color = "grey90")+
    coord_sf(xlim = c(20, 130), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
    geom_polygon(data=world, aes(x=long, y=lat, group=group))+
    ylab("Latitude")+xlab("Longitude")+
    ggtitle(weight_informations[w,2])+
    theme(panel.background = element_rect(fill = "grey90", color = "black"),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  ggplot2::ggsave(file.path(NEW_OUTPUT_PATH, "SD_density",
                            paste0("SD_density_w", w, ".png")),
                  p,
                  width = 12,
                  height = 8)
}

unlink(file.path(WD, "temp"), recursive = T)