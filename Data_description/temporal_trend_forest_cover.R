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

# OUTPUT_PATH <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/Data_description/Outputs"
OUTPUT_PATH <- "/home1/datawork/adupaix/Hist_FOB_env/Data_description/Outputs"
dir.create(OUTPUT_PATH, showWarnings = F)

# path containing the forest cover data
WD <- "/home1/datawork/adupaix/Hist_FOB_env/0-Data/forest_cover"
#' @testing (comment next line for whole run)
# WD <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/0-Data/forest_cover/"

setwd(dir = WD)

library(sf)
library(ggplot2)
library(plyr)
library(dplyr)

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