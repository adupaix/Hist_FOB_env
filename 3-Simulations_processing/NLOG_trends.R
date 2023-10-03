#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2023-06-29
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
add_burma_sea <- T

# Weighting methods for which the trend is assessed
# weight_methods <- c(2,3,4,7,1)
weight_methods <- c(2,3,4,6,7,9)

#' @AREAS
#' IO: study of the global trend on the whole ocean bassin
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
NEW_OUTPUT_PATH <- file.path(OUTPUT_PATH, "NLOG_trends",
                             ifelse(add_burma_sea, "with_burma_sea", "without_burma_sea"),
                             AREAS)
invisible(try(mapply(
  dir.create,
  file.path(NEW_OUTPUT_PATH,
            c("Density_climato_panel-area_group-w",
              # "Yearly_mean_panel-w_group-area",
              # "Times_series_panel-w_group-area",
              # "Time_series_panel-w_nogroup",
              # "Density_climato_panel-w_group-area",
              # "Time_series_panel-area_group-w",
              # "Density_time_series_panel-w_group-area",
              # "Density_time_series_panel-area_group-w",
              "Density_anomaly_panel-area_group-w",
              "Slope_tests")),
  MoreArgs = list(recursive = T,
                  showWarnings = F))))

try(dir.create(file.path(WD, "temp")))

#' change language to english
invisible(Sys.setlocale("LC_ALL", "en_US.utf8"))

# Functions
# delete.under.threshold <- function(x, threshold){
#   diff <- anomaly.detection(x)
#   condition <- abs(diff) > threshold
#   for (i in 2:(length(diff)-2)){
#     if (condition[i]){
#       y <- x[-i]
#       return(c(i, delete.under.threshold(y, threshold)+1))
#     }
#   }
#   return(c())
# }
# get.threshold <- function(x){
#   diff <- anomaly.detection(x)
#   thr <- 2*sd(diff, na.rm = T)
#   return(thr)
# }
# anomaly.detection <- function(x){
#   return(lag(x) + x - lead(x) - lead(lead(x)))
#   return(lag(x) + x - lead(x) - lead(lead(x)))
# }
# delete.aberrant <- function(x){
#   thr <- get.threshold(x)
#   return(delete.under.threshold(x, thr))
# }
breaks.of <- function(x, step){
  mx <- step*ceiling(max(x)/step)
  mn <- step*floor(min(x)/step)
  return(seq(mn,mx,step))
}
source(file = file.path(FUNC_PATH, "readAreas.R"))
source(file = file.path(FUNC_PATH, "add.area.column.R"))
source(file = file.path(FUNC_PATH, "build.climato.R"))
source(file = file.path(FUNC_PATH, "correct.lm.pvalue.temp.autocorr.R"))

# Weights corresponding table
weight_informations <- data.frame(cbind(1:9,
                                        c("No weight",
                                          "Coastline length",
                                          "Coastal cover",
                                          "River cover * mean river discharge",
                                          "River cover * mean river discharge + coastal cover",
                                          "Coastal cover * precipitations",
                                          "River cover * mean river discharge * precipitations",
                                          "Precipitations * (River cover * mean river discharge + coastal cover)",
                                          "River cover + coastal cover"
                                        )))
weight_informations$X2 <- as.character(weight_informations$X2)


# Read the necessary areas
#' Read the shapefile of the world seas, to filter in the for loop (1.):
#'          Flanders Marine Institute (2018). IHO Sea Areas, version 3.
#'          Available online at https://www.marineregions.org/ https://doi.org/10.14284/323
IO <- read.IO.areas("IHO", DATA_PATH = DATA_PATH, add_burma_sea = add_burma_sea)
#' Read the areas selected with the AREAS argument. If AREAS not in 'IO' 'E_W'
if (AREAS %in% c("IOTC","IHO","myAreas")){
  my_areas <- read.IO.areas(AREAS, DATA_PATH = DATA_PATH,
                              add_burma_sea = add_burma_sea)
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
  
  if(!file.exists(file.path(NEW_OUTPUT_PATH, paste0("global_ts",w,".Rdata")))){
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
        
        # df_s %>% dplyr::filter(!sim_nlog < max(sim_nlog)/10**4) -> df_s
        
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
          
          plyr::ddply(df_s, .variables = c("day","area"), function(x) sum(x$sim_nlog)) -> list_dfs[[i]]
          plyr::ddply(df_s, .variables = c("day"), function(x) sum(x$sim_nlog)) %>%
            dplyr::mutate(area = "Global") %>%
            dplyr::select(day, area, V1) -> df_sum
          list_dfs[[i]] <- dplyr::bind_rows(list_dfs[[i]], df_sum)
          
          rm(df_sum, df_s)
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
    
    message <- c(message, "   - Building plots\n") ; cat(message)
    #' 3. Long term trend: yearly mean number of NLOGs per area
    #' *******************************************************
    #'                     Use the mean value of 2000 as a reference point
    # dplyr::bind_rows(list_dfs) %>%
    #   dplyr::mutate(area = as.factor(area)) %>%
    #   dplyr::mutate(year = lubridate::year(as.Date(day))) %>%
    #   plyr::ddply(c("year", "area"), summarise, sim_nlog = mean(V1)) %>%
    #   do({df <- .; df %>% dplyr::mutate(sim_nlog = sim_nlog / df[which(df$year == "2000" & df$area == "Global"),"sim_nlog"])}) -> yearly_mean
    # 
    # my_colors <- Set1.without.yellow(my_areas$NAME)
    # 
    # p <- ggplot(yearly_mean, aes(x=year, y = sim_nlog,
    #                              group = relevel(area, "Global"), color = relevel(area,"Global")))+
    #   geom_point()+xlab("Date")+ylab("Indicator of the number of NLOGs")+
    #   scale_color_manual("Area", values = c(my_colors, Global = "black"))+
    #   ggtitle(weight_informations[w,2])+
    #   theme(plot.title = element_text(hjust = 0.5))
    # 
    # ggplot2::ggsave(file.path(NEW_OUTPUT_PATH,
    #                           "Yearly_mean_panel-w_group-area",
    #                           paste0("Yearly_mean_w",w,".png")), p,
    #                 width = 6, height = 4)
    # 
    # rm(yearly_mean)
    
    
    dplyr::bind_rows(list_dfs) %>%
      dplyr::mutate(area = as.factor(area)) %>%
      dplyr::mutate(day = as.Date(day),
                    year = lubridate::year(day)) -> global_ts
    
    #' Moving averaging over the global time series
    #'       average over "n_days_average" days, centered
    #'       use the zoo::rollmean function
    #'       Keep the same number of values for year saved daily and weekly
    global_ts$sim_nlog_number <- NA
    list_ts <- list()
    for (i in 1:length(years)){
      if (years[i] %in% daily_saved_years){
        global_ts %>%
          dplyr::filter(year == years[i]) %>%
          group_by(area) %>%
          dplyr::mutate(sim_nlog_number = zoo::rollmean(V1, k = n_days_average, fill = NA)) %>%
          dplyr::mutate(sim_nlog_number = case_when(is.na(sim_nlog_number) ~ V1,
                                                !is.na(sim_nlog_number) ~ sim_nlog_number)) %>%
          ungroup() -> list_ts[[i]]
        
      } else {
        global_ts %>%
          dplyr::filter(year == years[i]) %>%
          dplyr::mutate(sim_nlog_number = V1) -> list_ts[[i]]
      }
      
      days_to_keep <- seq(from = as.Date(paste0(years[i],"-01-05")),
                          by = as.difftime(n_days_average, units = "days"),
                          length.out = round(365/n_days_average))
      list_ts[[i]] %>%
        dplyr::filter(day %in% days_to_keep) -> list_ts[[i]]
    }
    
    #' Bind the results
    global_ts <- dplyr::bind_rows(list_ts) %>%
      dplyr::select(-V1)
    
    #'  Calculate densities:
    #'  Divide the number values by the surface of the area
    left_join(global_ts, st_drop_geometry(my_areas),
              by = c("area" = "NAME")) %>%
      dplyr::mutate(surface_km2 = case_when(area == "Global" ~ sum(my_areas$surface_km2),
                                            area != "Global" ~ surface_km2)) %>%
      dplyr::mutate(sim_density = sim_nlog_number / surface_km2) -> global_ts
    
    #'  Divide the values by the standard deviation in the area
    global_ts %>%
      plyr::ddply(.variables = c("area"), function(x) sd(x$sim_nlog_number)) %>%
      dplyr::rename("sd" = "V1") -> standard_deviations_nb
    global_ts %>%
      plyr::ddply(.variables = c("area"), function(x) sd(x$sim_density)) %>%
      dplyr::rename("sd" = "V1") -> standard_deviations_dens
    
    dplyr::left_join(global_ts, standard_deviations_nb, by = "area") %>%
      dplyr::rename("sd_nb" = "sd") %>%
      dplyr::left_join(standard_deviations_dens, by = "area") %>%
      dplyr::rename("sd_dens" = "sd") %>%
      dplyr::mutate(sim_nlog_number = sim_nlog_number / sd_nb,
                    sim_density = sim_density / sd_dens) %>%
      dplyr::select(-sd_dens, -sd_nb) %>%
      dplyr::arrange(year, day, area) -> global_ts
    
    # save(global_ts, file = file.path(WD, "temp", paste0("global_ts",w,".Rdata")))
    # load(file.path(WD, "temp", paste0("global_ts",w,".Rdata")))
    
    # p <- ggplot(data = global_ts)+
    #   # facet_wrap(~year, nrow = 1, scales = "free_x")+
    #   # geom_point(aes(x = as.Date(day), y = sim_nlog,
    #   #                color = relevel(area, "Global"),
    #   #                group = relevel(area, "Global")),
    #   #            size = 0.5, alpha = 0.2)+
    #   geom_line(aes(x = as.Date(day), y = av_sim_nlog,
    #                 color = relevel(area, "Global"),
    #                 group = relevel(area, "Global")))+
    #   scale_y_continuous(breaks = breaks.of(global_ts$av_sim_nlog, 1))+
    #   xlab("Date")+ylab("Indicator of the number of NLOGs")+
    #   scale_color_manual("Area", values = c(my_colors, Global = "black"))+
    #   ggtitle(weight_informations[w,2])+
    #   theme(plot.title = element_text(hjust = 0.5))
    # 
    # global_ts %>%
    #   dplyr::filter(area == 'Global' & !is.na(av_sim_nlog)) -> toplot
    # ggplot(toplot)+
    #   # facet_wrap(~year, nrow = 1, scales = "free_x")+
    #   # geom_point(size = 0.5, alpha = 0.2)+
    #   geom_line(aes(x = as.Date(day), y = av_sim_nlog))+
    #   scale_y_continuous(breaks = breaks.of(toplot$av_sim_nlog, 0.25))+
    #   xlab("Date")+ylab("Indicator of the number of NLOGs")+
    #   # scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
    #   # scale_color_brewer("Area", palette = "Set1")+
    #   ggtitle(weight_informations[w,2])+
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    #         plot.title = element_text(hjust = 0.5)) -> p2
    
    # ggplot2::ggsave(file.path(NEW_OUTPUT_PATH,
    #                           "Times_series_panel-w_group-area",
    #                           paste0("Evolution_NLOG_number_areas_w",w,".png")), p,
    #                 width = 12, height = 6)
    
    # ggplot2::ggsave(file.path(NEW_OUTPUT_PATH,
    #                           "Time_series_panel-w_nogroup",
    #                           paste0("Evolution_NLOG_number_w",w,".png")), p2,
    #                 width = 12, height = 6)
    
    
    
      # build 2 variables:
      #                   - sim_density: density scaled to the maximum density
      #                   - sim_density_area: density scaled to the density in the area at the first day of simulation
      # dplyr::group_by(area) %>%
      # dplyr::mutate(sim_density_area = sim_density / sim_density[which(day == min(day))]) %>%
      # dplyr::ungroup() %>%
      # dplyr::mutate(sim_density = sim_density / max(sim_density)) -> global_ts
    
    # p_dens <- ggplot(data = global_ts)+
    #   # facet_wrap(~year, nrow = 1, scales = "free_x")+
    #   # geom_point(aes(x = as.Date(day), y = sim_nlog,
    #   #                color = relevel(area, "Global"),
    #   #                group = relevel(area, "Global")),
    #   #            size = 0.5, alpha = 0.2)+
    #   geom_line(aes(x = as.Date(day), y = sim_density,
    #                 color = relevel(area, "Global"),
    #                 group = relevel(area, "Global")))+
    #   scale_y_continuous(breaks = breaks.of(global_ts$sim_density, 0.25))+
    #   xlab("Date")+ylab("Indicator of the density of NLOGs")+
    #   scale_color_manual("Area", values = c(my_colors, Global = "black"))+
    #   ggtitle(weight_informations[w,2])+
    #   theme(plot.title = element_text(hjust = 0.5))
    # 
    # ggplot2::ggsave(file.path(NEW_OUTPUT_PATH,
    #                           "Density_time_series_panel-w_group-area",
    #                           paste0("Evolution_NLOG_density_w",w,".png")), p_dens,
    #                 width = 12, height = 6)
    
    #' 4.2. Seasonal trend @climato
    #' ******************************
    #' Calculate the daily average over all the years
    # climato <- build.climato(global_ts, years,
    #                          var_to_average = "sim_density",
    #                          duplicated_over = c("area"))
    
    # plot it
    # p3 <- ggplot(data = climato, aes(x = as.Date(day), y = sim_density,
    #                                  color = relevel(area, "Global"),
    #                                  group = relevel(area, "Global")))+
    #   # facet_wrap(~year, nrow = 1, scales = "free_x")+
    #   geom_line(linewidth = 0.5)+
    #   scale_x_date(date_labels = "%b", date_breaks = "1 month")+
    #   scale_y_continuous(breaks = breaks.of(climato$sim_density,
    #                                         step = 0.25))+
    #   xlab("Date")+ylab("Indicator of the density of NLOGs")+
    #   scale_color_manual("Area", values = c(my_colors, Global = "black"))+
    #   ggtitle(weight_informations[w,2])+
    #   theme(plot.title = element_text(hjust = 0.5),
    #         axis.text.x = element_text(angle = 45,
    #                                    hjust = 1,
    #                                    vjust = 1))
    # 
    # ggplot2::ggsave(file.path(NEW_OUTPUT_PATH,
    #                           "Density_climato_panel-w_group-area",
    #                           paste0("Climato_NLOG_density_w",w,".png")), p3,
    #                 width = 10, height = 6)
    
    save(global_ts, file = file.path(NEW_OUTPUT_PATH, paste0("global_ts",w,".Rdata")))
  }
}


liste <- list()
for (w in weight_methods){
  load(file = file.path(NEW_OUTPUT_PATH, paste0("global_ts",w,".Rdata")))
  liste[[w]] <- global_ts %>%
    dplyr::mutate(w = w)
}
global_daily_mean_df <- do.call("rbind", liste) ; rm(liste)

kept_scenarios <- c("CL","CC","RC","CCp","RCp","R&CC")
global_daily_mean_df %>%
  dplyr::filter(!w %in% c(1,5,8)) %>%
  dplyr::mutate(w = dplyr::case_when(w == 2 ~ "CL",
                                     w == 3 ~ "CC",
                                     w == 4 ~ "RC",
                                     w == 6 ~ "CCp",
                                     w == 7 ~ "RCp",
                                     w == 9 ~ "R&CC")) %>%
  dplyr::mutate(w = factor(w, levels = kept_scenarios)) -> global_daily_mean_df



#' Seasonal trend @density_by_area
#' ***********************************
my_colors <- Set1.without.yellow(level_names = levels(global_daily_mean_df$w))

# years <- unique(toplot$year)
new_df <- list()
slopes <- data.frame(area = c(NA),
                     weight = c(NA),
                     slope = c(NA),
                     corrected_p_value = c(NA))
k=1

for (a in 1:(dim(my_areas)[1]+1)){
  if (a != dim(my_areas)[1]+1){
    area.i <- as.character(my_areas$NAME[a])
  } else {
    area.i <- "Global"
  }
  global_daily_mean_df %>%
    dplyr::filter(area == area.i) -> toplot
  
  # ggplot(toplot)+
  #   # facet_wrap(~year, nrow = 1, scales = "free_x")+
  #   # geom_point(size = 0.5, alpha = 0.2)+
  #   geom_line(aes(x = as.Date(day), y = sim_nlog_number,
  #                 group = w, color = w))+
  #   # scale_y_continuous(breaks = breaks.of(toplot$sim_nlog, 0.25))+
  #   xlab("Date")+ylab("Indicator of the number of NLOGs")+
  #   # scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  #   scale_color_manual("Scenario", values = my_colors)+
  #   ggtitle(area.i)+
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  #         plot.title = element_text(hjust = 0.5)) -> p4
  # 
  # ggplot2::ggsave(file.path(NEW_OUTPUT_PATH,
  #                           "Time_series_panel-area_group-w",
  #                           paste0("NLOG_number_",
  #                                  gsub(" ", "_", area.i),
  #                                  ".png")),
  #                 p4, width = 10, height = 6)
  
  # ggplot(toplot,
  #        aes(x = as.Date(day), y = sim_density,
  #            group = w))+
  #   # facet_wrap(~year, nrow = 1, scales = "free_x")+
  #   # geom_point(size = 0.5, alpha = 0.2)+
  #   geom_line(aes(color = w))+
  #   # scale_y_continuous(breaks = breaks.of(toplot$sim_nlog, 0.25))+
  #   xlab("Date")+ylab("Indicator of the density of NLOGs")+
  #   # scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  #   scale_color_manual("Scenario", values = my_colors)+
  #   ggtitle(area.i)+
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  #         plot.title = element_text(hjust = 0.5)) -> p5
  # 
  # ggplot2::ggsave(file.path(NEW_OUTPUT_PATH,
  #                           "Density_time_series_panel-area_group-w",
  #                           paste0("NLOG_density_",
  #                                  gsub(" ", "_", area.i),
  #                                  ".png")),
  #                 p5, width = 10, height = 6)
  # 
  # ggplot2::ggsave(file.path(NEW_OUTPUT_PATH,
  #                           "Density_time_series_panel-area_group-w",
  #                           paste0("facet_NLOG_density_",
  #                                  gsub(" ", "_", area.i),
  #                                  ".png")),
  #                 p5+
  #                   geom_smooth(method = "lm", color = "grey40")+
  #                   facet_wrap(~w),
  #                 width = 10, height = 6)
  
  climato <- build.climato(df = toplot,
                           years = years,
                           var_to_average = "sim_density",
                           duplicated_over = "w") %>%
    dplyr::arrange(day) %>%
    dplyr::mutate(day_index = as.numeric(as.factor(day)))
    # dplyr::group_by(w) %>%
    # dplyr::mutate(sim_density_w = sim_density_area / sim_density_area[which(day == min(day))]) %>%
    # dplyr::ungroup()
  
  # Build seasonal variations
  climato %>%
    group_by(w) %>%
    dplyr::mutate(sim_density_normalized = sim_density / mean(sim_density)) %>%
    dplyr::ungroup() -> climato_toplot
  
  ggplot(climato_toplot)+
    # facet_wrap(~year, nrow = 1, scales = "free_x")+
    # geom_point(size = 0.5, alpha = 0.2)+
    geom_line(aes(x = as.Date(day), y = sim_density_normalized,
                  group = w, color = w))+
    # scale_y_continuous(breaks = breaks.of(toplot$sim_nlog, 0.25))+
    xlab("Date")+ylab("Indicator of the density of NLOGs")+
    # scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
    scale_color_manual("Scenario", values = my_colors)+
    scale_x_date(date_labels = "%b", date_breaks = "1 month")+
    ggtitle(area.i)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          plot.title = element_text(hjust = 0.5)) -> p6
  
  ggplot2::ggsave(file.path(NEW_OUTPUT_PATH,
                            "Density_climato_panel-area_group-w",
                            paste0("Climato_NLOG_density_",
                                   gsub(" ", "_", area.i),
                                   ".png")),
                  p6, width = 10, height = 6)
  saveRDS(p6, file.path(NEW_OUTPUT_PATH,
                        "Density_climato_panel-area_group-w",
                        paste0("Climato_NLOG_density_",
                               gsub(" ", "_", area.i),
                               ".rds")))
  
  # Build trend removing seasonal variations
  climato %>%
    dplyr::arrange(day_index) %>%
    dplyr::select(-day) -> climato
    # substract the climato obtained above
    toplot %>%
      dplyr::arrange(day) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(day_index = as.numeric(as.factor(day))) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(climato,
                       c("w", "day_index")) %>%
      dplyr::rename("sim_density" = "sim_density.x",
                    "sim_density_climato" = "sim_density.y") -> toplot
    
    

  # density_anomaly: anomaly density with regards to the mean seasonal pattern
  toplot <- toplot %>%
    dplyr::mutate(density_anomaly = sim_density - sim_density_climato)
  
  ggplot(toplot,
         aes(x = as.Date(day), y = density_anomaly,
             group = w))+
    facet_wrap(~w, nrow = 2)+
    # geom_point(size = 0.5, alpha = 0.2)+
    geom_line(aes(color = w))+
    # scale_y_continuous(breaks = breaks.of(toplot$sim_nlog, 0.25))+
    xlab("Date")+ylab("NLOG density anomaly")+
    # scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
    scale_color_manual("Scenario", values = my_colors)+
    ggtitle(area.i)+
    geom_smooth(method = "lm", color = "grey40")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          plot.title = element_text(hjust = 0.5)) -> p7
  
  ggplot2::ggsave(file.path(NEW_OUTPUT_PATH,
                            "Density_anomaly_panel-area_group-w",
                            paste0("Anomaly_NLOG_density_",
                                   gsub(" ", "_", area.i),
                                   ".png")),
                  p7, width = 10, height = 6)
  saveRDS(p7, file.path(NEW_OUTPUT_PATH,
                        "Density_anomaly_panel-area_group-w",
                        paste0("Anomaly_NLOG_density_",
                               gsub(" ", "_", area.i),
                               ".rds")))
  
  # test for slope significance in Global
  for (w.i in kept_scenarios){
    toplot %>%
      dplyr::filter(w == w.i) -> df_for_lm
    model <- lm(density_anomaly ~ day, data = df_for_lm)
    corrected_pvalue <- correct.pvalue.temporal.autocorr(model, return.lag_autocorr = T)
    sink(file.path(NEW_OUTPUT_PATH, "Slope_tests",
                   paste0(area.i, ".txt")),
         append = T)
    cat(w.i,"\n~~~~~~~~~\n")
    print(summary(model))
    cat("Corrected p-value of slope and Lag spatial autocorrelation: ",
        corrected_pvalue,
        "\n\n\n")
    sink()
    
    slopes[k,] <- c(area.i, w.i, model$coefficients[2], corrected_pvalue[1])
    k=k+1
  }
  

}


write.csv(slopes,
          file.path(NEW_OUTPUT_PATH, "Slope_tests",
                    "All_coefficients.csv"))

# library(tseries)
# df.ts <- ts(df_for_lm$sim_density, frequency = 52, start = c(2000,1))
# 
# kpss.test(df.ts, null = "Trend") # p.value < 0.05, the time series is not trend stationary, hence we decompose with "additive"
# 
# dec <- decompose(df.ts, type = c("additive"), filter = NULL)
# plot(dec)
# 
# model <- auto.arima(ts_data)
# summary(model)
# # try(unlink(file.path(WD, "temp"), recursive = T, force = T))
