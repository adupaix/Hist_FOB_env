#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2022-10-12
#'@email : amael.dupaix@ird.fr
#'#*******************************************************************************************************************
#'@description :  Plotting time series of forest cover and precipitations in the IO
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

rm(list = ls())


# OUTPUT_PATH <- "/home1/datawork/adupaix/Hist_FOB_env/Data_description/Outputs"
OUTPUT_PATH <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/Data_description/Outputs/precipitations"
dir.create(OUTPUT_PATH, showWarnings = F)

# path containing the forest cover data
# WD <- "/home1/datawork/adupaix/Hist_FOB_env/0-Data/forest_cover"
DATA_PATH <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/0-Data/"

library(ncdf4)
library(lubridate)
library(ggplot2)
library(sf)
library(plyr)
library(dplyr)
# library(tidyr)

years <- 2000:2019

precip <- ncdf4::nc_open(file.path(DATA_PATH, "precip.mon.mean.nc"))

#'@sub-function 3
#'***************
#' generate the precipitation array from the netcdf opened connection
get.precip.array <- function(precip){
  
  precip_array <- ncdf4::ncvar_get(precip, varid = "precip")
  
  time <- ncdf4::ncvar_get(precip, varid = "time")
  init_nc <- as.Date("1800-01-01")
  time <- as.difftime(time, units = "days") + init_nc
  
  lon <- ncdf4::ncvar_get(precip, varid = "lon")
  lat <- ncdf4::ncvar_get(precip, varid = "lat")
  
  dimnames(precip_array) <- list(lon, lat, as.character(time))
  
  rm(time, init_nc, lon, lat)
  
  return(precip_array)
  
}

precip_array <- get.precip.array(precip)

xlims <- c(20,130)
ylims <- c(-40,30)

lon_of_int <- which(as.numeric(dimnames(precip_array)[[1]]) >= xlims[1] &
                      as.numeric(dimnames(precip_array)[[1]]) <= xlims[2])
lat_of_int <- which(as.numeric(dimnames(precip_array)[[2]]) >= ylims[1] &
                      as.numeric(dimnames(precip_array)[[2]]) <= ylims[2])
date_of_int <- which(lubridate::year(as.Date(dimnames(precip_array)[[3]])) >= years[1] &
                       lubridate::year(as.Date(dimnames(precip_array)[[3]])) <= years[length(years)])

precip_array <- precip_array[lon_of_int,
                             lat_of_int,
                             date_of_int]

monthly_mean <- data.frame(month = as.Date(dimnames(precip_array)[[3]]),
                          precip_mean = apply(precip_array, 3, mean))

ggplot(data = monthly_mean,
       aes(x=month, y = precip_mean))+
  geom_point()+ geom_line(alpha = 0.5)+
  xlab("Year")+ylab("Mean precipitations in the Indian Ocean (mm/day)")+
  scale_y_continuous(limits = c(0,NA)) -> p

ggsave(file.path(OUTPUT_PATH, "mean_monthly_precipitations.png"), p,
       width = 8, height = 6)

precip_df <- as.data.frame.table(precip_array, stringsAsFactors = F)
names(precip_df) <- c("x","y","date","precip")
precip_df$x <- as.numeric(precip_df$x)
precip_df$y <- as.numeric(precip_df$y)
precip_df$date <- as.Date(precip_df$date)

precip_df %>%
  dplyr::mutate(month = lubridate::month(date)) %>%
  plyr::ddply(.variables = c("x","y","month"),
              function(x) mean(x$precip)) -> precip_climato

world <- map_data("world")

ggplot()+
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill = "grey80")+
  geom_tile(data = precip_climato,
            aes(x=x, y=y, fill = V1), alpha = 0.7)+
  facet_wrap(~month)+
  scale_fill_distiller("Mean\nprecipitations\n(mm/day)",
                       limits = c(0,NA),
                       palette = "Blues",
                       direction = 1)+
  coord_sf(xlim = c(20, 130), ylim = c(-40, 30), expand = FALSE, crs = st_crs(4326))+
  ylab("Latitude")+xlab("Longitude")+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) -> precip_map_climato

ggsave(file.path(OUTPUT_PATH, "maps_precipitations_climato.png"), precip_map_climato,
       width = 15, height = 8)

precip_df %>% dplyr::mutate(E_W = if_else(x > 70, "E", "W")) -> precip_df

plyr::ddply(precip_df, c("E_W","date"), function(x) mean(x$precip)) -> monthly_mean

ggplot(data = monthly_mean,
       aes(x=date, y = V1, color = E_W,
           group = E_W))+
  geom_point()+
  geom_line()+
  xlab("Year")+ylab("Mean precipitations in the Indian Ocean (mm/day)")+
  scale_y_continuous(limits = c(0,NA)) -> p

ggsave(file.path(OUTPUT_PATH, "mean_monthly_precipitations_EW.png"), p,
       width = 8, height = 6)

# r <- raster::raster(t(precip_array[,,1]), xmn = 20, xmx = 130, ymn = -40, ymx = 30)
# raster::crs(r) <- "+proj=longlat"
# raster::area(r) -> cell_area
# cell_area <- t(raster::as.matrix(cell_area))*10**6 # cell areas in km2
# 
# precip_array <- precip_array / 10**6 # change precipitations from mm/day to km/day
# 
# # calculate precipitation volume
# precip_volume <- precip_array
# for (i in 1:dim(precip_volume)[3]){precip_volume[,,i] <- precip_volume[,,i]*cell_area}
# 
# monthly_volumne <- data.frame(month = as.Date(dimnames(precip_volume)[[3]]),
#                           precip_sum = apply(precip_volume, 3, mean))
# 
# ggplot()+
#   geom_point(data = monthly_volumne,
#              aes(x=month, y = precip_sum))+
#   xlab("Year")+ylab("Mean precipitations volume in the Indian Ocean (km3)")+
#   scale_y_continuous(limits = c(0,NA))

