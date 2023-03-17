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
OUTPUT_PATH <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/Data_description/Outputs/forest_cover"
dir.create(OUTPUT_PATH, showWarnings = F)

# path containing the forest cover data
# WD <- "/home1/datawork/adupaix/Hist_FOB_env/0-Data/forest_cover"
WD <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/0-Data/forest_cover/"

setwd(dir = WD)

library(sf)
library(ggplot2)
library(plyr)
library(dplyr)

if (!file.exists(file.path(OUTPUT_PATH, "1degree_cell_sum_of_forest_cover.rds"))){
  years <- 2000:2020
  #' @testing (comment next line for whole run)
  # years <- 2016:2018
  
  file_pattern <- ".shp$"
  #' @testing (comment next line for whole run)
  # file_pattern <- "sample*.shp$"
  
  # contains the global sum of cover per year
  cover_sum <- list()
  # contains the sum of cover per 1 degree cell
  cover_df <- list()
  
  for (i in 1:length(years)){
    
    cat(years[i],"\n")
    files <- list.files(paste0("forest_cover_",years[i]), pattern = file_pattern, full.names = T)
    cover_sum[[i]] <- c(0)
    cover_df[[i]] <- data.frame(X = NA, Y = NA, cover = NA)
    
    for (f in files){
      sink(file = file.path(OUTPUT_PATH, "FOLLOW_UP"), append = F)
      cat(years[i],"\n  ",f)
      sink()
      
      cat(" ",f,"\n")
      cover <- read_sf(f)
      cover_df.temp <- data.frame(cbind(cover, st_coordinates(cover))) %>%
        dplyr::mutate(X = floor(X)+0.5,
                      Y = floor(Y)+0.5) %>%
        plyr::ddply(.variables = c("X","Y"), function(x){sum(x$couvert)})
      names(cover_df.temp) <- c("X","Y","cover")
      cover_df[[i]] <- bind_rows(cover_df[[i]],
                                 cover_df.temp)
      
      cover_sum[[i]] <- c(cover_sum[[i]], sum(cover$couvert))
      
      rm(cover_df.temp) ; invisible(gc())
    }
    cover_sum[[i]] <- sum(cover_sum[[i]])
    
    cover_df[[i]] %>% dplyr::mutate(year = years[i]) %>%
      dplyr::filter(!is.na(X)) -> cover_df[[i]]
  }
  
  cover_df <- dplyr::bind_rows(cover_df)
  saveRDS(cover_df, file.path(OUTPUT_PATH, "1degree_cell_sum_of_forest_cover.rds"))
  
  # General sum
  cover_sum_df <- data.frame(year = years, cover = unlist(cover_sum)) %>%
    dplyr::mutate(cover = cover * 90**2/10**6)
  
  saveRDS(cover_sum_df, file.path(OUTPUT_PATH, "yearly_sum_of_forest_cover.rds"))
  
  p <- ggplot(cover_sum_df, aes(x=year, y=cover))+
    geom_point()+
    scale_y_continuous("Total associated surface of forest cover (in km2)",
                       labels = function(x) format(x, scientific = TRUE,
                                                   digits = 3))+
    scale_x_continuous("Year")
  
  ggsave(file.path(OUTPUT_PATH, "yearly_sum_of_forest_cover.png"), p,
         width = 8, height = 12)
  
} else {
  cover_df <- readRDS(file.path(OUTPUT_PATH, "1degree_cell_sum_of_forest_cover.rds"))
  cover_sum_df <- readRDS(file.path(OUTPUT_PATH, "yearly_sum_of_forest_cover.rds"))
}

# Maps of loss
cover_df %>%
  plyr::ddply(.variables = c("X","Y","year"), function(x) sum(x$cover)) %>%
  dplyr::rename("cover"="V1") %>%
  dplyr::filter(year != 2020) -> cover_df

cover_df %>%
  dplyr::filter(year == 2000) %>%
  dplyr::arrange(X, Y) -> cover_ref

cover_df %>%
  tidyr::pivot_wider(id_cols = c("X","Y"),
                     names_from = "year",
                     values_from = "cover",
                     names_prefix = "y") %>%
  dplyr::arrange(X,Y) %>%
  dplyr::mutate_at(.vars = paste0("y",2000:2019),
                   .funs = function(x) 100 - (x / cover_ref$cover)*100) %>%
  tidyr::pivot_longer(cols = paste0("y",2000:2019),
                      names_to = "year",
                      values_to = "percent_loss") %>%
  dplyr::mutate(year = as.numeric(sub("y","",year))) -> percent_loss

world <- map_data("world")

ggplot()+
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill = "grey80")+
  geom_tile(data = percent_loss,
            aes(x=X, y=Y, fill = percent_loss))+
  facet_wrap(~year)+
  scale_fill_distiller(limits = c(0,50),
                       palette = "OrRd",
                       direction = 1,
                       na.value = "black")+
  coord_sf(xlim = c(20, 130), ylim = c(-40, 40), expand = FALSE, crs = st_crs(4326))+
  ylab("Latitude")+xlab("Longitude")+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank()) -> map_percent_loss

ggsave(file.path(OUTPUT_PATH, "maps_percent_cover_loss.png"), map_percent_loss,
       width = 15, height = 10)

# Loss trend
cover_sum_init <- cover_sum_df[which(cover_sum_df$year == 2000),"cover"]
cover_sum_df %>%
  dplyr::mutate(percent_init = 100 * cover / cover_sum_init) %>%
  dplyr::filter(year != 2020) -> cover_sum_df

p <- ggplot(cover_sum_df, aes(x=year, y=percent_init))+
  geom_point()+
  scale_y_continuous("Percentage of the cover in 2000")+
  scale_x_continuous("Year")

ggsave(file.path(OUTPUT_PATH, "yearly_percent_of_2000_forest_cover.png"), p,
       width = 8, height = 6)