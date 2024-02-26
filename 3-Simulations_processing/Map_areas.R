#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-10-12
#'@email : amael.dupaix@ird.fr
#'#*******************************************************************************************************************
#'@description :  Script generating the map of the studied area
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
RESOURCE_PATH <- file.path(WD,"Resources")

NEW_OUTPUT_PATH <- file.path(OUTPUT_PATH, "Maps_areas")
try(dir.create(NEW_OUTPUT_PATH, showWarnings = F))

#' Load libraries
source(file.path(FUNC_PATH, "install_libraries.R"))

srcUsedPackages <- c("ggplot2", "sf","dplyr")

installAndLoad_packages(srcUsedPackages, loadPackages = TRUE)

#' load shapefiles
world <- map_data("world")
IO <- read_sf(file.path(DATA_PATH, "World_Seas_IHO_v3/World_Seas_IHO_v3.shp")) %>%
  dplyr::filter(NAME %in% c("Indian Ocean", "Laccadive Sea", "Bay of Bengal", "Arabian Sea", "Mozambique Channel", "Andaman or Burma Sea"))
  # dplyr::filter(NAME %in% c("Indian Ocean", "Laccadive Sea", "Bay of Bengal", "Arabian Sea", "Mozambique Channel"))
bbox <- read_sf(file.path(DATA_PATH, "OI.shp"))

source(file = file.path(FUNC_PATH, "readAreas.R"))

#' crop the IO with the used bbox
IO <- st_crop(IO, st_bbox(bbox))

map <- ggplot()+geom_sf(data = IO, fill = RColorBrewer::brewer.pal(6, "Blues")[3],
                        color = NA, alpha = 0.5)+
  coord_sf(xlim = c(20, 130), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
  # coord_sf(xlim = c(80, 120), ylim = c(-10, 30), expand = FALSE, crs = st_crs(4326))+
  geom_line(aes(x = c(80,80), y = c(-35,30)), color = "red", linewidth = 0.5, linetype = "dashed")+
  # geom_label(data = IO, aes(x = Longitude, y = Latitude, label = NAME))+
  geom_polygon(data=world, aes(x=long, y=lat, group=group))+
  geom_label(aes(x = c(60,100), y = c(-15,-15),
                 label = c("West","East")))+
  ylab("Latitude")+xlab("Longitude")+
  theme(panel.background = element_rect(fill = "grey90", color = "black"),
        panel.grid = element_blank())

ggsave(file.path(NEW_OUTPUT_PATH, "map_study_area_E_W.png"), map,
       height = 8, width = 12)

map <- ggplot()+
  geom_sf(data = IO, aes(fill = NAME), color = "grey40", alpha = 0.5)+
  coord_sf(xlim = c(20, 130), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
  scale_fill_brewer("Area", palette = "Blues", guide = "none")+
  # coord_sf(xlim = c(80, 120), ylim = c(-10, 30), expand = FALSE, crs = st_crs(4326))+
  # geom_line(aes(x = c(80,80), y = c(-35,30)), color = "red", size = 0.5, linetype = "dashed")+
  # geom_label(data = IO, aes(x = Longitude, y = Latitude, label = NAME))+
  geom_polygon(data=world, aes(x=long, y=lat, group=group))+
  geom_label(aes(x = c(40,80,88,60,100,75),
                 y = c(-20,-15,15,15,10,5),
                 label = IO$NAME))+
  ylab("Latitude")+xlab("Longitude")+
  theme(panel.background = element_rect(fill = "grey90", color = "black"),
        panel.grid = element_blank())

ggsave(file.path(NEW_OUTPUT_PATH, "map_study_area_IHO.png"), map,
       height = 8, width = 12)

IOTC <- generate.CTOI.areas()

map <- ggplot()+
  geom_sf(data = IOTC, aes(fill = NAME), color = "grey40", alpha = 0.5)+
  coord_sf(xlim = c(20, 130), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
  scale_fill_brewer("Area", palette = "Blues", guide = "none")+
  # coord_sf(xlim = c(80, 120), ylim = c(-10, 30), expand = FALSE, crs = st_crs(4326))+
  # geom_line(aes(x = c(80,80), y = c(-35,30)), color = "red", size = 0.5, linetype = "dashed")+
  # geom_label(data = IO, aes(x = Longitude, y = Latitude, label = NAME))+
  geom_polygon(data=world, aes(x=long, y=lat, group=group))+
  geom_label(aes(x = c(65,45,65,75,40),
                 y = c(7,-5,-5,-5,-20),
                 label = IOTC$NAME))+
  ylab("Latitude")+xlab("Longitude")+
  theme(panel.background = element_rect(fill = "grey90", color = "black"),
        panel.grid = element_blank())

ggsave(file.path(NEW_OUTPUT_PATH, "map_study_area_IOTC.png"), map,
       height = 8, width = 12)

my_areas <- generate.perso.areas(IHO_areas = IO)

map <- ggplot()+
  geom_sf(data = my_areas, aes(fill = NAME), color = "grey40", alpha = 0.5)+
  scale_fill_brewer(palette = "Blues", guide = "none")+
  coord_sf(xlim = c(20, 130), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
  # geom_label(data = IO, aes(x = Longitude, y = Latitude, label = NAME))+
  geom_polygon(data=world, aes(x=long, y=lat, group=group))+
  geom_label(aes(x = c(70,86,86,105,40,60,55,80),
                 y = c(15,15,-5,-5,-20,-5,7,-25),
                 label = my_areas$NAME))+
  ylab("Latitude")+xlab("Longitude")+
  theme(panel.background = element_rect(fill = "grey90", color = "black"),
        panel.grid = element_blank())

ggsave(file.path(NEW_OUTPUT_PATH, "map_study_area_myAreas.png"), map,
       height = 8, width = 12)
