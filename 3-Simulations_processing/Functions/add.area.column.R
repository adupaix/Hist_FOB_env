add.area.column <- function(df, area){
  pts <- st_as_sf(df, coords = c("x","y"), crs = st_crs(4326))
  df_inArea <- st_intersects(pts, area)
  
  df_inArea <- lapply(df_inArea, function(x){
    if(length(x) == 0){
      x <- NA
    } else {
      x <- x
    }
    return(x)
    }
    )
  
  df_inArea <- unlist(df_inArea)
  df$area <- area$NAME[df_inArea]
  
  rm(pts, df_inArea) ; invisible(gc())
  
  return(df)
}
