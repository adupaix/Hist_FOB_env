
#######################################################################################
#                          GENERATES THE TITLE OF THE GGPLOT                          #
#######################################################################################
# ARGUMENTS:                                                                          #
# coeff_var (log) : if F, returns the map (or raster) of the mean value of FOB per    #
#                    cell                                                             #
#                   if T, return the map (or raster) of the variation coefficient     #
#                     (standard deviation / mean) of FOB per cell                     #
# simulation: (logical) if T, takes data from ichthyop simulations using              #
#                             get.sim.data()                                          #
# year: (vector) the years we're interested in                                        #
# ltime (num): ltime utilise lors du traitement post ichthyop                         #
# fob_type: (chr) "NLOG" or "FAD", or "ALOG" (canNOT plot for all objects)            #
# grid: (logical) if T, the plot titles are ready for a figure with several panels    #
# trimestre: (logical) if T, returns a list of 4 ggplots, one for each trimester      #
#                      if F, returns one ggplot of the whole year                     #
#                                                                                     #
# OUTPUT:                                                                             #
#######################################################################################


plot.title <- function(coeff_var, simulation, year, ltime, fob_type,
                       grid, trimestre){
  
  # title of the ggplot
  if (coeff_var == F){
    title <- "Mean number of"
  } else if (coeff_var == T){
    title <- "Variation coefficient of the number of"
  }
  
  if (simulation == F){
    title <- paste(title, fob_type, "\n obtained from the trajectories\nYears :",
                   year[1], "to", year[length(year)])
  } else if (simulation == T){
    title <- paste(title, "NLOG \n obtained from the trajectories\nYears :",
                   year[1], "to", year[length(year)], "\nLife time:", ltime)
  }
  
  if (grid == T){
    if (simulation == F){
      title <- paste(fob_type)
    } else {
      title <- paste("Life time :", ltime)
    }
  }
  
  if (trimestre == T){
    trim_text <- c("Dec. to Feb.", "March to May", "June to Aug.", "Sept. to Nov.")
    title <- paste(title, "\nMonths:",trim_text)
  }
  
  
  #title of the colour legend
  if(coeff_var == F){
    col_title <- "Mean number\n of"
  }else if (coeff_var == T){
    col_title <- "Variation coefficient\n of the number\n of"
  }
  
  if(simulation == F){
    col_title <- paste(col_title, "buoys associated\nwith", fob_type)
  } else {
    col_title <- paste(col_title, "simulated", fob_type)
  }
  
  
  titre <- list(title, col_title)
  names(titre) <- c("title","col_title")
  
  return (titre)
  
}


#######################################################################################
#                          GENERATES THE GGPLOT                                       #
#######################################################################################
# ARGUMENTS:                                                                          #
# df (data.frame): obtained from the raster containing occurence data                 #
# titre (chr): obtained with plot.title() above, title of the ggplot                  #
# col_title (chr): obtained with plot.title() above, title of the colour legend       #
# log_color_scale (log): color scale linear or with log10 transformation              #
# color_scale_pos (chr): position of the color scale, either inside the map, outside  #
#                        or no color scale                                            #
#                                                                                     #
# OUTPUT: ggplot                                                                      #
#######################################################################################

build.ggplot <- function(df, titre, col_title, log_color_scale, fixed_scale_max,
                         color_scale_pos){
  
  # get the max of the color scale
  if (fixed_scale_max != 0){
    max = fixed_scale_max
  } else{
    max = max(df$occ[is.na(df$occ) == FALSE &
                       is.infinite(df$occ) == FALSE])
  }
  
  if (log_color_scale == T){
    min = min(df$occ[is.na(df$occ) == FALSE &
                       is.infinite(df$occ) == FALSE &
                       df$occ != 0])
  }
  
  p <- ggplot() +
    geom_sf() +
    coord_sf(
      xlim = c(35, 120),
      ylim = c(-30, 25),
      expand = FALSE,
      crs = st_crs(4326)
    ) +
    geom_raster(data = df, aes(x, y, fill = occ)) +
    borders(fill = "grey30", colour = "grey30")
  
  ### Echelle de couleur
  # transformation log
  if (log_color_scale == T){
    p <- p + scale_fill_gradientn(
      trans = "log10",
      colors = c("gray80", "blue", "yellow", "red"),
      name = col_title,
      limits = c(min, max))
  #sans transformation log
  } else {
    p <- p + scale_fill_gradientn(
      colors = c("gray80", "blue", "yellow", "red"),
      name = col_title,
      limits = c(0,max))
  }
  
  
  
  p <- p + labs(title = titre) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_text(hjust = 0.5)) +
    # echelle distance
    annotation_scale(location = "bl", width_hint = 0.5) +
    # fleche nord
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      pad_x = unit(0.75, "in"),
      pad_y = unit(0.5, "in"),
      style = north_arrow_fancy_orienteering
    )
  
  
  ### Position de la legend de couleur
  # pas de legende
  if (color_scale_pos == "null"){
    p <- p + guides(fill = F)
    
  # legend a l'interieur de la carte
  } else if (color_scale_pos == "in_panel"){
    # legende a l interieur
    p <- p + theme(
      panel.border = element_rect(colour = "black", fill = NA),
      legend.position = c(1, 0),
      legend.justification = c(1, 0),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black"
      ),
      legend.title.align = .5
    )
    
    # legende a l exterieur de la carte
  } else if (color_scale_pos == "out_panel"){
    p <- p + theme(
      panel.border = element_rect(colour = "black", fill = NA),
      # legend.position = c(1, 0),
      # legend.justification = c(1, 0),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black"
      ),
      legend.title.align = .5
    )
  }
    
    
  
  return(p)
  
}



###########################################################################################
#                          ADD INPUT POINTS FROM THE TXT FILE                             #
#                              TO THE OCCURENCE MAP FROM SIM                              #
###########################################################################################
# ARGUMENTS:                                                                              #
# p: (ggplot) ggplot obtained from plot.traj.occurence()                                  #
# DATA_PATH: (chr) path to the data directory                                             #
# input_method (num): method used to generate input txt file for in ICHTHYOP simulation   #
# dist (num): distance specified in the input_method above                                #
#                                                                                         #
# OUTPUT: ggplot with points added in the input locations (one or one for each trimester) #
###########################################################################################

add.input.occurence <- function(p, DATA_PATH, input_method = 2, dist = 100){
  
  if (input_method==1){
    filename <- paste0("mangrove_nlog_input_",dist,"k.txt")
  } else if (input_method==2){
    filename <- paste0("mangrove_nlog_input_",dist,"k_from_coast.txt")
  }
  
  coords <- read.table(file = file.path(DATA_PATH,"input_particles_txt",filename))
  coords$id <- "Input"
  names(coords)<-c("longitude","latitude","Input")
  
  p <- p + geom_point(data = coords, aes(x= longitude, y = latitude, colour=Input), colour = "forestgreen", size = 0.5)
  
  return(p)
}


###########################################################################################
#                      ADD INPUT POINTS FROM THE SHP OF MANGROVE OR RIVERS                #
#                              TO THE OCCURENCE MAP FROM SIM                              #
###########################################################################################
# ARGUMENTS:                                                                              #
# p: (ggplot) ggplot obtained from plot.traj.occurence()                                  #
# DATA_PATH: (chr) path to the data directory                                             #
# input_location (chr): either river or mangrove                                          #
# thr_disch (num): disharge threshold used to filter rivers of input                      #
#                                                                                         #
# OUTPUT: ggplot with points added in the input locations (one or one for each trimester) #
###########################################################################################

add.gen.input.location <- function(p, DATA_PATH, input_location = "mangrove", thr_disch = 20){
  
  if (input_location=="mangrove"){
    
    filename <- "mangrove/centroids_mangrove_no_chinese_sea.rds"
    coords <- readRDS(file = file.path(DATA_PATH,filename))
    set.seed(10)
    coords <- coords[sample(1:nrow(coords),10000),]
    
  } else if (input_location=="river"){
    
    filename <-"river_data/rivers_IO.rds"
    
    coords <- readRDS(file.path(DATA_PATH, filename))
    
    coords %>% dplyr::select(NEXT_DOWN, MAIN_RIV, ENDORHEIC, HYBAS_L12, dis_m3_pyr:dis_m3_pmx) %>%
      dplyr::filter(dis_m3_pyr >= thr_disch) %>%
      dplyr::filter(NEXT_DOWN == 0 & ENDORHEIC == 0) %>%
      st_centroid() %>% st_coordinates() %>%
      as.data.frame()-> coords
    
  }
  
  
  names(coords)<-c("longitude","latitude")
  
  p <- p + geom_point(data = coords, aes(x= longitude, y = latitude), colour = "forestgreen", size = 0.5)
  
  return(p)
}


