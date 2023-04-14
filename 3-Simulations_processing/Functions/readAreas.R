#' Read different sets of IO areas
#' 
#' area: one of "IOTC" "IHO" "myAreas"
#' DATA_PATH: only needed if area = "IHO"
#' add_burma_sea: only needed if area = "IHO"
read.IO.areas <- function(area, DATA_PATH = "", add_burma_sea = T){
  if (area == "IHO"){
    return(read.IHO(DATA_PATH = DATA_PATH,
                    add_burma_sea = add_burma_sea))
  } else if (area == "IOTC"){
    return(generate.CTOI.areas())
  } else if (area == "myAreas"){
    return(generate.perso.areas(IHO_areas = read.IHO(DATA_PATH = DATA_PATH,
                                                     add_burma_sea = add_burma_sea)))
  }
}



# Function to generate an sf object with the polygons of the IOTC areas (used in Dagorn et al. 2013 & Dupaix et al. 2021)

generate.CTOI.areas <- function(){
  
  Som <- matrix(nrow = 5, ncol = 2)
  Som[1,] <- c(50,12)
  Som[2,] <- c(80,12)
  Som[3,] <- c(80,0)
  Som[4,] <- c(40,0)
  Som[5,] <- Som[1,]
  Som <- st_polygon(list(Som))
  
  NWSey <- matrix(nrow = 7, ncol = 2)
  NWSey[1,] <- c(35,0)
  NWSey[2,] <- c(58,0)
  NWSey[3,] <- c(58,-7)
  NWSey[4,] <- c(49,-7)
  NWSey[5,] <- c(49,-10)
  NWSey[6,] <- c(35,-10)
  NWSey[7,] <- NWSey[1,]
  NWSey <- st_polygon(list(NWSey))
  
  SESey <- matrix(nrow = 7, ncol = 2)
  SESey[1,] <- c(58,0)
  SESey[2,] <- c(70,0)
  SESey[3,] <- c(70,-12)
  SESey[4,] <- c(49,-12)
  SESey[5,] <- c(49,-7)
  SESey[6,] <- c(58,-7)
  SESey[7,] <- SESey[1,]
  SESey <- st_polygon(list(SESey))
  
  Chag <- matrix(nrow = 5, ncol = 2)
  Chag[1,] <- c(70,0)
  Chag[2,] <- c(80,0)
  Chag[3,] <- c(80,-12)
  Chag[4,] <- c(70,-12)
  Chag[5,] <- Chag[1,]
  Chag <- st_polygon(list(Chag))
  
  Moz <- matrix(nrow = 6, ncol = 2)
  Moz[1,] <- c(30,-10)
  Moz[2,] <- c(49,-10)
  Moz[3,] <- c(49,-12)
  Moz[4,] <- c(46,-30)
  Moz[5,] <- c(30,-30)
  Moz[6,] <- Moz[1,]
  Moz <- st_polygon(list(Moz))
  
  CTOI_area <- list(Som, NWSey, SESey, Chag, Moz)
  
  CTOI_area <- st_sfc(geom = CTOI_area,
                      crs = 4326)
  
  
  areas <- st_as_sf(data.frame(NAME = c("Somalia", "NW Seychelles", "SE Seychelles", "Chagos", "Mozambique")),
                    geom = CTOI_area) %>%
    dplyr::arrange(NAME)
  
  return(areas)
}


#' Function to read the IHO areas
read.IHO <- function(DATA_PATH, add_burma_sea = T){
  #' Read the shapefile of the world seas, to filter in the for loop (1.):
  #'          Flanders Marine Institute (2018). IHO Sea Areas, version 3.
  #'          Available online at https://www.marineregions.org/ https://doi.org/10.14284/323
  names_IO <- c("Indian Ocean", "Laccadive Sea", "Bay of Bengal", "Arabian Sea", "Mozambique Channel")
  if (add_burma_sea){names_IO <- c(names_IO, "Andaman or Burma Sea")}
  IO <- read_sf(file.path(DATA_PATH,"World_Seas_IHO_v3/World_Seas_IHO_v3.shp")) %>%
    dplyr::filter(NAME %in% names_IO) %>%
    dplyr::arrange(NAME)
  
  return(IO)
}


# Function to generate personalized areas
generate.perso.areas <- function(IHO_areas){
  
  Som <- matrix(nrow = 5, ncol = 2)
  Som[1,] <- c(50,11.5)
  Som[2,] <- c(60,11.5)
  Som[3,] <- c(60,0)
  Som[4,] <- c(40,0)
  Som[5,] <- Som[1,]
  Som <- st_polygon(list(Som))
  
  Sey <- matrix(nrow = 5, ncol = 2)
  Sey[1,] <- c(35,0)
  Sey[2,] <- c(77,0)
  Sey[3,] <- c(77,-12)
  Sey[4,] <- c(35,-12)
  Sey[5,] <- Sey[1,]
  Sey <- st_polygon(list(Sey))
  
  Moz <- matrix(nrow = 6, ncol = 2)
  Moz[1,] <- c(30,-12)
  Moz[2,] <- c(49,-12)
  Moz[3,] <- c(49,-16)
  Moz[4,] <- c(45,-25.5)
  Moz[5,] <- c(30,-25.5)
  Moz[6,] <- Moz[1,]
  Moz <- st_polygon(list(Moz))
  
  E_IO <- matrix(nrow = 5, ncol = 2)
  E_IO[1,] <- c(77,0)
  E_IO[2,] <- c(95,0)
  E_IO[3,] <- c(95,-12)
  E_IO[4,] <- c(77,-12)
  E_IO[5,] <- E_IO[1,]
  E_IO <- st_polygon(list(E_IO))
  
  AS <- matrix(nrow = 5, ncol = 2)
  AS[1,] <- c(60,0)
  AS[2,] <- c(77,0)
  AS[3,] <- c(77,30)
  AS[4,] <- c(60,30)
  AS[5,] <- AS[1,]
  AS <- st_polygon(list(AS))
  
  BB <- matrix(nrow = 5, ncol = 2)
  BB[1,] <- c(77,0)
  BB[2,] <- c(77,30)
  BB[3,] <- c(95,30)
  BB[4,] <- c(95,0)
  BB[5,] <- BB[1,]
  BB <- st_polygon(list(BB))
  
  IND <- matrix(nrow = 5, ncol = 2)
  IND[1,] <- c(95,10)
  IND[2,] <- c(130,10)
  IND[3,] <- c(130,-12)
  IND[4,] <- c(95,-12)
  IND[5,] <- IND[1,]
  IND <- st_polygon(list(IND))
  
  S_IO <- matrix(nrow = 5, ncol = 2)
  S_IO[1,] <- c(25,-35)
  S_IO[2,] <- c(25,-12)
  S_IO[3,] <- c(130,-12)
  S_IO[4,] <- c(130,-35)
  S_IO[5,] <- S_IO[1,]
  S_IO <- st_polygon(list(S_IO))
  
  CTOI_area <- list(Som, Sey, Moz, E_IO, AS, BB, IND, S_IO)
  
  CTOI_area <- st_sfc(geom = CTOI_area,
                      crs = 4326)
  
  
  areas <- st_as_sf(data.frame(NAME = c("Somalia",
                                        "SCTR",
                                        "Mozambique",
                                        "Eastern IO",
                                        "Arabian Sea",
                                        "Bay of Bengal",
                                        "Indonesia",
                                        "Southern IO")),
                    geometry = CTOI_area)
  
  # Complete areas
  
  # complete arabian sea
  areas$geometry[areas$NAME == "Arabian Sea"] <- st_union(areas$geometry[areas$NAME == "Arabian Sea"],
                                                          IHO_areas$geometry[IHO_areas$NAME == "Arabian Sea"]) %>%
    st_difference(areas$geometry[areas$NAME == "Somalia"]) %>%
    st_difference(areas$geometry[areas$NAME == "SCTR"])
  
  # complete Bay of Bengal
  areas$geometry[areas$NAME == "Bay of Bengal"] <- st_union(areas$geometry[areas$NAME == "Bay of Bengal"],
                                                          IHO_areas$geometry[IHO_areas$NAME == "Bay of Bengal"]) %>%
    st_union(IHO_areas$geometry[IHO_areas$NAME == "Andaman or Burma Sea"])
  
  # Crop Indonesia
  areas$geometry[areas$NAME == "Indonesia"] <- st_intersection(areas$geometry[areas$NAME == "Indonesia"],
                                                               IHO_areas$geometry[IHO_areas$NAME == "Indian Ocean"])
  
  # Crop southern IO
  areas$geometry[areas$NAME == "Southern IO"] <- st_intersection(areas$geometry[areas$NAME == "Southern IO"],
                                                                 IHO_areas$geometry[IHO_areas$NAME == "Indian Ocean"]) %>%
    st_union(IHO_areas$geometry[IHO_areas$NAME == "Mozambique Channel"]) %>%
    st_difference(areas$geometry[areas$NAME == "Mozambique"]) %>%
    st_difference(areas$geometry[areas$NAME == "SCTR"])
  
  # order levels of NAME (so that colors are the same on all figures)
  areas %>% dplyr::arrange(NAME) -> areas
  areas$NAME <- factor(areas$NAME, levels = sort(levels(areas$NAME)))
    
  return(areas)
}


color.for.areas <- function(my_areas){
  cols <- RColorBrewer::brewer.pal(9, "Set1")
  names(cols) <- my_areas$NAME
  cols <- cols[!is.na(names(cols))]
  return(cols)
}
