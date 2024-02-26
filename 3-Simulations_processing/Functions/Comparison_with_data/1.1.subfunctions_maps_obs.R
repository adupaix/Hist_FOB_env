
## SELECTION DES DONNEES DES MOIS D'INTERET

subset.month <- function(data, month){
  data$observation_month <- as.numeric(strftime(data$observation_date, "%m"))
  data <- subset(data, data$observation_month %in% month)
  
  return(data)
}


## REMPLACE LES VALEURS DES CELLULES AVEC MOINS DE 6 JOURS D OBSERVATION PAR DES NA

del.low.eff <- function(rast, eff_obs, gsize, eff.threshold){
  for (i in 1:length(rast@data@values)) {
    if (eff_obs@data@values[i]<eff.threshold){
      rast@data@values[i]<-NA
    }
  }
  return(rast)
}


## COMPTE DES OCCURENCES PAR CELLULE DE RASTER
fill.raster <- function(data, rast){
  for (i in 1:dim(data)[1]) {
    coord<-c(data$longitude[i],data$latitude[i])
    rast[cellFromXY(rast,coord)]<-rast[cellFromXY(rast,coord)]+1
  }
  return(rast)
}


#######################################################################################
#                     CREATE AN EMPTY RASTER IN THE INDIAN OCEAN                      #
#######################################################################################
# ARGUMENTS:                                                                          #
# gsize (num): size of the grid cells, in degree                                      #
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


### GENERATE PLOT TITLE FOR map_occurence()

generate.title.occ <- function(obj.type, eff, month, year){
  
  titre <- "Nombre de"
  echelle <- paste(obj.type)
  
  if(obj.type != "all"){
    titre <- paste(titre, obj.type)
  } else if (obj.type == "all"){
    titre <- paste(titre, "FOB")
  }
  
  if(eff == "T"){
    titre <- paste(titre, "par jour d'observation")
    echelle = paste(obj.type,"/ day")
  } else if(eff == "plot"){
    titre <- "Effort d'observation"
    echelle = "Effort (day)"
  }
  
  if(any(year==0)){
    titre <- paste(titre, "\nPool toutes annees")
  } else {
    titre <- paste(titre, "\nAnnee :", paste(year, collapse = ","))
  }
  
  if(!(is.null(month))){
    titre <- paste(titre, "\nMois :", paste(month, collapse = ","))
  }
  
  l <- list(titre = titre, echelle = echelle)
  
  return(l)
}


mise.en.forme.ggplot <- function(p){
  p <- p + xlab("Longitude") +
    ylab("Latitude") +
    # echelle distance
    annotation_scale(location = "bl", width_hint = 0.5) +
    # fleche nord
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    # legende a l interieur
    theme(panel.border = element_rect(colour = "black", fill=NA),
          legend.position = c(1,0),
          legend.justification = c(1,0),
          legend.background = element_rect(fill="white", linetype = "solid", colour = "black"),
          legend.title.align = .5)
  
}



