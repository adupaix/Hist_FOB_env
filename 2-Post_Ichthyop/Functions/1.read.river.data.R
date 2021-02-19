
## This function reads the rivers data
# and returns a data frame, with prepared filters and weights

read.river.data <- function(RESOURCE_PATH, river_id, thr_disch){
  
  ### ADD HERE for filters or weights on rivers data ###
  rivers_IO <- readRDS(file.path(RESOURCE_PATH, "river_data","rivers_IO.rds"))
  # and keep only the estuaries segments of the rivers used to generate input points
  rivers_IO %>% dplyr::filter(MAIN_RIV %in% river_id$MAIN_RIV) %>%
    dplyr::filter(NEXT_DOWN == 0) %>%
    # transform as a data frame
    st_drop_geometry() %>%
    # keep the variables of interest (more details in RiverATLAS_Catalog_v10.pdf)
    dplyr::select(MAIN_RIV, # river id
                  dis_m3_pyr, # annual mean river discharge (in m3/s) at the reach pour point
                  # dis_m3_pmx, # annual maximum river discharge (in m3/s) at the reach pour point
                  ria_ha_usu, # total surface in hectares of the river basin in the total watershed upstream of the reach pour point
                  for_pc_use # percentage of forest cover in the total watershed upstream of the reach pour point
                  ) %>%
    # create a new variable containing the extent in km2 of the forest cover, in the total watershed upstream of the reach pour point
    dplyr::mutate(for_km_usu = 0.01 * ria_ha_usu * for_pc_use) -> river_data
  
  # add a column which will allow to filter the particles according to the discharge
  if (!is.null(thr_disch)) {
    river_data %>% mutate(is_bellow_thr = ifelse(dis_m3_pyr <= thr_disch, TRUE, FALSE)) -> river_data
  } else {
    river_data %>% mutate(is_bellow_thr =  FALSE) -> river_data
  }
  
  rm(rivers_IO) ; invisible(gc())
  
  return(river_data)
}