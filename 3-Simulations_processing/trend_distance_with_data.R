rm(list = ls())

STUDY_DIR <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/"

#' generate basic paths
WD <- file.path(STUDY_DIR,'3-Simulations_processing')
DATA_PATH <- file.path(STUDY_DIR,'0-Data')

OUTPUT_PATH <- file.path(WD,"Outputs")
FUNC_PATH <- file.path(WD,"Functions")
ROUT_PATH <- file.path(WD, "Sub-routines")
RESOURCE_PATH <- file.path(WD,"Resources")

#' Source sub-functions
source(file.path(FUNC_PATH, "add.area.column.R"))
source(file.path(FUNC_PATH, "Maps", "1.Maps_obs.R"))
source(file.path(FUNC_PATH, "readAreas.R"))
source(file.path(FUNC_PATH, "install_libraries.R"))

#' Load used packages
srcUsedPackages <- c("ggplot2","plyr","sf","ggspatial","rnaturalearth","rnaturalearthdata","rgeos",
                     "raster","rworldmap","shape", "dplyr", "RColorBrewer")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)

DoNotDelete <- c("df_list")

df_list <- list()

#'***************
#' @arguments
#'***************

#' Path to where the processed Ichthyop simulations are stored
sim_output_path <- file.path(STUDY_DIR, "2-Post_Ichthyop/Outputs/nemo_river_allMask")

#' Delete the outputs to calculate anew (T) or not (F)
RESET = F

#' Year for which we want to do the comparison
#' After 2007, because no data is available before
#' The best is to consider 2014 onward
YEARS = 2014:2019

#' @AREAS
#' IO: study of the global trend on the whole ocean bassin
#' E_W: 2 zones, east and west
#' IOTC: same areas used in Dupaix et al. (2021) and Dagorn et al. (2013)
#' IHO: areas obtained from the shapefile of the world seas:
#'          Flanders Marine Institute (2018). IHO Sea Areas, version 3.
#'          Available online at https://www.marineregions.org/ https://doi.org/10.14284/323
#' myAreas: areas defined with M.Lengaigne and M.Capello
# AREAS <- c("IO", "E_W", "IOTC", "IHO", "myAreas")
AREAS = "myAreas"

#' Threshold used to filter the observers data, noted "T"
#' Cells with a number of days of observation < T are discarded 
EFFORT_THRESHOLD = c(0,6,10)

#' Read world polygons to build maps
world <- map_data("world")

#' Read observers data
cat("\14")
cat("Read raw observers data\n")
data <- read.csv(file.path(DATA_PATH, "Observers_data", "all_operations_on_fobs_observe_fr_indian.csv"))
data <- prep.obs(data)

#' Read area 
if(!AREAS %in% c("IO", "E_W")){
  cat("\nRead IO areas\n")
  my_areas <- read.IO.areas(area = AREAS,
                            DATA_PATH = DATA_PATH,
                            add_burma_sea = T)
}

for (year in YEARS){
  for (effort_threshold in EFFORT_THRESHOLD){
    cat('\14')
    cat(paste("Processing year:", year, "; T:",effort_threshold, "\n===========================\n\n"))
    source(file.path(ROUT_PATH, "distance_with_data.R"))
    
    df_list[[paste(year, effort_threshold, sep = "-")]] <- df_tot
  }
}
for (i in 1:length(df_list)){
  df_list[[i]]$year <- sub("-.*","",names(df_list)[i])
}

# bind all the distances for all the years
df <- dplyr::bind_rows(df_list)


cat("\n\nBuilding plots\n==============")
#' remove weights 1 5 and 8
#' and rename other weight scenarios as in the study
kept_scenarios <- c("CL","CC","RC","CCp","RCp","R&CC")
df %>%
  dplyr::filter(!w %in% c(1,5,8)) %>%
  dplyr::mutate(w = dplyr::case_when(w == 2 ~ "CL",
                                     w == 3 ~ "CC",
                                     w == 4 ~ "RC",
                                     w == 6 ~ "CCp",
                                     w == 7 ~ "RCp",
                                     w == 9 ~ "R&CC")) %>%
  dplyr::mutate(w = factor(w, levels = kept_scenarios)) -> df

#' @plot1
#' *****
# summarize them by month, to plot the time series
# (one value per weight scenario, per month-year, per T value)
plyr::ddply(df, .variables = c("month","effort_threshold","w","year"),
            function(x) sum(x$dist)/nrow(x)) -> toplot
toplot$date <- as.Date(paste(toplot$year, toplot$month, 1, sep = "-"))
toplot$effort_threshold <- factor(toplot$effort_threshold, levels = c("0","6","10"))

p1=ggplot(toplot,
       aes(x = date, y = V1, group = as.factor(w), color = as.factor(w)))+
  ggplot2::facet_wrap(~effort_threshold, scales = "free", nrow = 3)+
  scale_color_brewer("Weighting\nmethod", palette = "Set1")+
  geom_point(alpha=0.5)+geom_line(alpha=0.5)+
  ylab("Distance")

ggsave(file.path(OUTPUT_PATH,"Distance_to_data", AREAS, "distance_trend.png"),
       p1,width=10,height=6)

#' @plot2
#' *****
# summarize them by weight (one value per weight scenario, per T value)
# to plot the histogram
plyr::ddply(df, .variables = c("effort_threshold","w"),
            function(x) sum(x$dist)/nrow(x)) -> toplot2
tidyr::pivot_wider(data = toplot2,
                   names_from = "effort_threshold",
                   names_prefix = "T = ",
                   values_from = "V1") -> distance_summary

p2 <- ggplot(toplot2, aes(x = as.factor(w), y = V1,
                    group = as.factor(effort_threshold),
                    fill = as.factor(effort_threshold)))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_brewer("T", palette = "Set1")+
  xlab("Weighting method")+
  ylab("Sum of monthly distances")

ggsave(file.path(OUTPUT_PATH, "Distance_to_data", AREAS,"distance_sum.png"),
       p2, width = 6, height = 4)

#' @plot3
#' *****
## Summarize them by area, to plot the heat map
# one value per area, per weight scenario, per value of T
plyr::ddply(df, .variables = c("effort_threshold","w","area"),
            function(x) sum(x$dist)/nrow(x)) %>%
  dplyr::filter(!is.na(area)) %>%
  dplyr::mutate(effort_threshold = paste("T =", effort_threshold)) -> toplot3

plyr::ddply(toplot3, .variables = c("w", "effort_threshold"), function(x) sum(x$V1)) %>%
  dplyr::mutate(area = "Total") -> total

dplyr::bind_rows(toplot3, total) -> toplot3

#mise en forme
toplot3$effort_threshold <- factor(toplot3$effort_threshold,
                                   levels = c("T = 0", "T = 6", "T = 10"))
toplot3$area <- as.factor(toplot3$area)
toplot3$area <- relevel(toplot3$area, "Total")

p3 <- ggplot(toplot3)+
  geom_tile(aes(y = area, x = as.factor(w), fill = V1), color = "grey")+
  facet_wrap(~effort_threshold, ncol = 1)+
  scale_fill_viridis_c("Distance")+
  xlab("Weight scenario")+ylab("Area")

ggsave(file.path(OUTPUT_PATH, "Distance_to_data", AREAS, "distance_heatmap.png"),
       p3, width = 8, height = 10)

#' @plot4
#' *****
#' Plot measured vs simulated values
#' 
#'     By cells

obs_vs_predict_output <- file.path(OUTPUT_PATH, "Distance_to_data",
                                   AREAS, "observed_vs_predicted")
try(dir.create(obs_vs_predict_output, recursive = T, showWarnings = F))

my_colors <- Set1.without.yellow(my_areas$NAME)

for (eff in EFFORT_THRESHOLD){
  df  %>%
    dplyr::mutate(quarter = ceiling(month / 3)) %>%
    dplyr::filter(effort_threshold == eff) %>%
    ggplot()+
    geom_point(aes(x = NLOGdata, y = NLOGsim, color = as.factor(quarter)))+
    geom_abline(slope = 1, intercept = 0, color = "grey")+
    scale_color_brewer("Quarter", palette = "Set1")+
    scale_x_continuous(limits = c(0,1))+
    scale_y_continuous(limits = c(0,1))+
    facet_wrap(~w) +
    theme(panel.background = element_rect(fill = "white",
                                          colour = "black")) -> p4
  df %>%
    dplyr::filter(effort_threshold == eff) %>%
    ggplot()+
    geom_point(aes(x = NLOGdata, y = NLOGsim, color = area))+
    geom_abline(slope = 1, intercept = 0, color = "grey")+
    scale_color_manual("Area", values = my_colors)+
    scale_x_continuous(limits = c(0,1))+
    scale_y_continuous(limits = c(0,1))+
    facet_wrap(~w) +
    theme(panel.background = element_rect(fill = "white",
                                          colour = "black")) -> p5
  
  ggsave(file.path(obs_vs_predict_output,
                   paste0("byCell_T",eff,"_quarter.png")),
         p4, width = 10, height = 6)
  
  ggsave(file.path(obs_vs_predict_output,
                   paste0("byCell_T",eff,"_area.png")),
         p5, width = 10, height = 6)
  
  #'     By areas
  #'     one per year
  if (AREAS == "myAreas"){
    df %>%
      dplyr::mutate(quarter = ceiling(month / 3)) %>%
      plyr::ddply(.variables = c("year","quarter","area",
                                 "w", "effort_threshold"),
                  function(x) cbind(NLOGdata = mean(x$NLOGdata),
                                    NLOGsim = mean(x$NLOGsim))) %>%
      dplyr::filter(effort_threshold == eff,
                    area %in% c("Arabian Sea",
                                "Mozambique",
                                "SCTR",
                                "Somalia",
                                "Southern IO")) %>%
      ggplot()+
      geom_abline(slope = 1, intercept = 0, color = "grey")+
      geom_point(aes(x = NLOGdata,
                     y = NLOGsim,
                     color = as.factor(area),
                     shape = as.factor(quarter)))+
      scale_shape("Quarter")+
      scale_color_manual("Area", values = my_colors)+
      scale_x_continuous(limits = c(0,1))+
      scale_y_continuous(limits = c(0,1))+
      facet_wrap(~w)+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black")) -> p6
    
    ggsave(file.path(obs_vs_predict_output,
                     paste0("byAreas_T",eff,"_quarter.png")),
           p6, width = 10, height = 6)
    
#'     By areas
#'     climato

    # df_climato contains, for each cell, the mean monthly values (averaged between years)
    df_climato <- df %>%
      plyr::ddply(.variables = c("x","y","month",
                                 "w", "effort_threshold",
                                 "area"),
                  function(x) cbind(NLOGdata = mean(x$NLOGdata),
                                    NLOGsim = mean(x$NLOGsim)))
    
    df_climato %>%
      dplyr::mutate(quarter = ceiling(month / 3)) %>%
      plyr::ddply(.variables = c("quarter","area",
                                 "w", "effort_threshold"),
                  function(x) cbind(NLOGdata = mean(x$NLOGdata),
                                    NLOGsim = mean(x$NLOGsim))) %>%
      dplyr::filter(effort_threshold == eff,
                    area %in% c("Arabian Sea",
                                "Mozambique",
                                "SCTR",
                                "Somalia",
                                "Southern IO")) %>%
      ggplot()+
      geom_abline(slope = 1, intercept = 0, color = "grey")+
      geom_point(aes(x = NLOGdata,
                     y = NLOGsim,
                     color = as.factor(area),
                     shape = as.factor(quarter)))+
      scale_shape("Quarter")+
      scale_color_manual("Area", values = my_colors)+
      scale_x_continuous(limits = c(0,1))+
      scale_y_continuous(limits = c(0,1))+
      facet_wrap(~w)+
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black")) -> p7
    
    ggsave(file.path(obs_vs_predict_output,
                     paste0("climato_byAreas_T",eff,"_quarter.png")),
           p7, width = 10, height = 6)
  }
}  

eff = 10
df_climato %>%
  dplyr::mutate(quarter = ceiling(month / 3)) %>%
  plyr::ddply(.variables = c("quarter","area",
                             "w", "effort_threshold"),
              function(x) cbind(NLOGdata = mean(x$NLOGdata),
                                NLOGsim = mean(x$NLOGsim))) %>%
  dplyr::filter(effort_threshold == eff,
                area %in% c("Arabian Sea",
                            "Mozambique",
                            "SCTR",
                            "Somalia",
                            "Southern IO")) -> for_tests
for (wi in kept_scenarios){
  data <- for_tests %>%
    dplyr::filter(w == wi)
  sink(file.path(obs_vs_predict_output, "wilcox_tests.txt"),
       append = T)
  cat(wi, "\n")
  print(wilcox.test(data$NLOGsim, data$NLOGdata))
  sink()
}

