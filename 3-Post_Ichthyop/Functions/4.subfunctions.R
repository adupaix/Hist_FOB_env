#######################################################################################
#                     CREATE AN EMPTY RASTER IN THE INDIAN OCEAN                      #
#######################################################################################
# ARGUMENTS:                                                                          #
# gsize (num): size of the grid cells, in degree (either 1, 2, or 5)                  #
#######################################################################################


create.raster <- function(gsize){
  
  r <- raster(
    res = gsize,
    xmn = 20,
    xmx = 140,
    ymn = -40,
    ymx = 40
  )
  
  r[] <- 0
  names(r) <- "occ"
  
  return(r)
}


#######################################################################################
#               FROM ONE MATRIX OF THE ARRAY RETURNS A GGPLOT                         #
#######################################################################################
# ARGUMENTS:                                                                          #
#                                                                                     #
# OUTPUT: ggplot                                                                      #
#######################################################################################

matrix.to.ggplot <- function(array.i, dimname.i,
                             log_color_scale, fixed_scale_max,
                             color_scale_pos, area_to_map){

  r.i <- raster(array.i)
  extent(r.i) <- extent(area_to_map)
  
  # df from the raster
  df.i <- as.data.frame(r.i, xy = TRUE)

  ### PLOT
  p <- build.ggplot(df = df.i, titre = dimname.i, col_title = "Mean number\nof particles",
                    log_color_scale, fixed_scale_max, color_scale_pos,
                    area_to_map)

  return(p)

}


#######################################################################################
#                          GENERATES THE GGPLOT                                       #
#######################################################################################
# ARGUMENTS:                                                                          #
# df (data.frame): obtained from the raster containing one time_scale of the array    #
# titre (chr): title of the ggplot                                                    #
# col_title (chr): title of the colour legend                                         #
# log_color_scale (log): color scale linear or with log10 transformation              #
# color_scale_pos (chr): position of the color scale, either inside the map, outside  #
#                        or no color scale                                            #
#                                                                                     #
# OUTPUT: ggplot                                                                      #
#######################################################################################

build.ggplot <- function(df, titre, col_title, log_color_scale, fixed_scale_max,
                         color_scale_pos, area_to_map){

  # get the max of the color scale
  if (fixed_scale_max != 0){
    max = fixed_scale_max
  } else{
    max = max(df$layer[is.na(df$layer) == FALSE &
                         is.infinite(df$layer) == FALSE])
  }

  if (log_color_scale == T){
    min = min(df$layer[is.na(df$layer) == FALSE &
                         is.infinite(df$layer) == FALSE &
                         df$layer != 0])
  }
  
  xlimits <- area_to_map[1:2]
  ylimits <- area_to_map[3:4]

  p <- ggplot() +
    geom_sf() +
    coord_sf(
      xlim = xlimits,
      ylim = ylimits,
      expand = FALSE,
      crs = st_crs(4326)
    ) +
    geom_raster(data = df, aes(x, y, fill = layer)) +
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
      pad_x = unit(0.25, "in"),
      pad_y = unit(0.25, "in"),
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
