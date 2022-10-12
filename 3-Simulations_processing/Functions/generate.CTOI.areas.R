
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
  
  
  areas <- st_as_sf(data.frame(name = c("Somalia", "NW Seychelles", "SE Seychelles", "Chagos", "Mozambique")),
                    geom = CTOI_area)
  
  return(areas)
}
