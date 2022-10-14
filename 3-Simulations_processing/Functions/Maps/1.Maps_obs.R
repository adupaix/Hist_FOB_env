#########################################################################################
                              #   MAPS FUNCTIONS  #
#########################################################################################
#                             FUNCTIONS                                                 #
#=======================================================================================#
# 1. map_ratio(Ob7, year, log.type, gsize) : cartes des ratios LOG/FAD                  #
# 2. map_density(Ob7, year, obj.type) : carte de densite d'objets par type              #
# 3. map_occurence(Ob7, year, obj.type, gsize) : carte du nb d'occurrence d'objet       #
# 4. map_traj_bal(data_buoy, year, obj.type, n) : carte avec n trajectoires de          #
#               balises, associees avec des obj.type, de l'annee year                   #
#

# @Ob7: observers data, containing all the operations on objects

###FONCTIONS NECESSAIRES
source(file.path(FUNC_PATH,'Prep_obs.R'))
source(file.path(FUNC_PATH, 'Maps','1.1.subfunctions_maps_obs.R'))

#======================================================================================#
#                               CARTE DES RATIOS LOG/FAD ou FAD/LOG                    #
#======================================================================================#
#  ARGUMENTS:                                                                          #
# Ob7 (data.frame) : donnees observateur brut                                          #
# DATA_PATH (chr) : chemin vers le dossier contenant les donnees                       #
# year (num) : annees dont on veut la carte. c() pour un pool des annees               #
# log.type (chr) : choix entre "ALOG" et "NLOG"                                        #
# month (vector) : mois dont on veut la carte. c() pool tous les mois                  #
# gsize (num) : choix entre 1, 2 et 5, pour des cellules de 1dx1d, 2dx2d, 5dx5d        #
# data_preped (log) : Ob7 fourni est brut (F) ou c'est le res de prep.obs() (T)        #
# invert_ratio (log) : if T plots the ratio FAD/LOG                                    #
#                      if F plots the ratio LOG/FAD                                    #
#                                                                                      #
# return: ggplot                                                                       #
#--------------------------------------------------------------------------------------#

map_ratio_discrete <- function(Ob7, DATA_PATH, year = c(), log.type = "NLOG", month = c(),
                               gsize = 2, data_preped=F, invert_ratio = T, return_raster = F){
  
  ### 1. PREPARATION DES DONNEES ----
  #------------------------------------------------------------------------------  
  if (data_preped == FALSE){
    Ob7 <- prep.obs(Ob7)
  }
  
  # ttob7 <- "all_vessel_activities_observe_fr_indian.rds"
  ttob7 <- read.csv(file = file.path(DATA_PATH, "all_vessel_activities_observe_fr_indian.csv"))
  
  ### 2. SELECTION DONNEES D'INTERET ----
  #------------------------------------------------------------------------------   
  ## A. SELECTION COLONNES
  data<-data.frame(Ob7$year,Ob7$latitude,Ob7$longitude,Ob7$obj_conv,Ob7$observation_date)
  names(data)<-c("year","latitude","longitude","fob_type","observation_date")
  
  ttob7 <- data.frame(ttob7$year,ttob7$vessel_name,ttob7$observation_date,
                      ttob7$latitude,ttob7$longitude)
  names(ttob7) <- c("year","vessel_name","observation_date","latitude","longitude")
  
  ## B. EXTRACTION DES DONNEES DE L'ANNEE ----
  if (!is.null(year)){
    data<-data[data$year %in% year,]
    ttob7 <- ttob7[ttob7$year %in% year,]
  }
  
  ## C. SUPPRIME LES LIGNES CONTENANT NULL, FOB, LOG ou des COORDONNEES NULLES-----
  data<-data[data$fob_type!="NULL",]
  data<-data[data$fob_type!="FOB",]
  data<-data[data$fob_type!="LOG",]
  data<-data[data$latitude!=0 | data$longitude!=0,]
  ttob7<-ttob7[ttob7$latitude!=0 | ttob7$longitude!=0,]
  
  ## D. SELECTION DES DONNEES DU TRIMESTRE ----
  if (!is.null(month)){

    data <- subset.month(data, month)
    ttob7 <- subset.month(ttob7, month)

  }
  
  ## E. TRI POUR GARDER UNE OBS PAR J DANS TTES OBS ----
  ttob7$id_unique <- paste(ttob7$vessel_name,ttob7$observation_date)
  ttob7 <- ttob7[!duplicated(ttob7$id_unique),]
  
  ## F. NOMBRE TOTAL D'OBJET-----
  n.fad <- dim(data[data$fob_type=="FAD",])[1]
  if (log.type=="ALOG"){
    n.log <- dim(data[data$fob_type=="ALOG",])[1]
  } else {
    n.log <- dim(data[data$fob_type=="NLOG",])[1]
  }
  ### 3. CREATION DES RASTERS-----
  #---------------------------------------------------------------------------------------
  ## A. creation d'un raster, avec les bonnes coordonnees, et dont la grille contient des 0-----
  r <- create.raster(gsize)
  r_fad <- create.raster(gsize)
  eff_obs <- create.raster(gsize)
  
  ## B. COMPTE DES OCCURENCES PAR CELLULE-----
  # i. FAD-----
  fad<-data[data$fob_type=="FAD",]
  
  r_fad <- fill.raster(fad, r_fad)
  
  # ii. LOG-----
  if(log.type=="ALOG"){
    alog<-data[data$fob_type=="ALOG",]
    
    r <- fill.raster(alog, r)
    
  }else if(log.type=="NLOG"){
    nlog<-data[data$fob_type=="NLOG",]
    
    r <- fill.raster(nlog, r)
    
  }
  
  # iii. Effort ----
  eff_obs <- fill.raster(ttob7, eff_obs)
  
  ## C. REMPLACE LES VALEURS DES CELLULES AVEC MOINS DE 10 JOURS D OBSERVATION PAR DES NA ----
  r <- del.low.eff(r, eff_obs, gsize)
  r_fad <- del.low.eff(r_fad, eff_obs, gsize)
  
  ## D. CALCUL DU RATIO-----
  if (invert_ratio == T){
    r[] <- r_fad[]/r[]
  } else {
    r[]<-r[]/r_fad[]
  }
  names(r)<-"ratio"
  
  if(return_raster == T){
    return(r)
  }
  
  
  ### 3. CREATION DU GGPLOT-----
  #--------------------------------------------------------------------------
  world <- map_data("world")
  df<-as.data.frame(r, xy=TRUE)
  
  
  ## A. TITRE-----
  if (invert_ratio == F){
    if (is.null(year)){
      titre<- paste("Ratio de", log.type, "/ FAD - pool toutes annees")
    }else{
      titre<- paste("Ratio de", log.type, "/ FAD \nAnnee :", paste(year,collapse=","))
    }
    fill_lab <- paste(log.type,"/ FAD")
  } else {
    if (is.null(year)){
      titre<- paste("Ratio de FAD /",log.type,"- pool toutes annees")
    }else{
      titre<- paste("Ratio de FAD /",log.type,"\nAnnee :", paste(year,collapse=","))
    }
    fill_lab <- paste("FAD /",log.type)
  }
  
  
  ## B. ECHELLE DE COULEURS (DISCRETE) ----
  # On discretise/categorise les valeurs de ratio
  if(invert_ratio == F){
    # pas qu'on prend si on considere le ratio LOG/FAD
    steps <- c(0, 0.02, 0.05, 0.1, 0.2, 0.5, 1)
    lev <- c("0-0.02","0.02-0.05","0.05-0.1","0.1-0.2","0.2-0.5","0.5-1",paste0("1-",round(max(df$ratio, na.rm=T),2)))
  } else{
    # pas qu'on prend si on considere le ratio FAD/LOG
    steps <- c(0, 1, 2, 5, 10, 20, 50)
    lev <- c("0-1","1-2","2-5","5-10","10-20","20-50",">50",paste("No",log.type))
  }
  
  #Rajoute une colonne a df, qu'on va completer avec les valeurs des categories
  df$discrete <- as.character(NA)
  
  for (i in 1:(length(steps)-1)){ # pour chaque pas
    df %>% mutate(discrete = case_when( #on rempli la colonne discrete avec une chaine de charactere
      #par exemple, si on en est au pas = 0.02, on remplace le NA de la colonne discrete par "0.02-0.05" si ratio est compris dans [0.02,0.05]
      ratio >= steps[i] & ratio < steps[i+1] & is.na(discrete) ~ paste0(steps[i],"-",steps[i+1]),
      !is.na(discrete) ~ discrete
    )) -> df
  }
  
  if (invert_ratio == F){
    df %>%
      mutate(discrete = case_when( # dans le cas où le ratio est superieur au dernier pas, on rempli par e.g. "1-1.36"
        ratio >= max(steps) & is.na(discrete) ~ paste0(max(steps),"-",round(max(df$ratio, na.rm=T),2)),
        !is.na(discrete) ~ discrete
      )) -> df
  } else {
    df %>%
      mutate(discrete = case_when( # dans le cas où le ratio est superieur au dernier pas, on rempli par e.g. "1-1.36"
        ratio >= max(steps) & is.na(discrete) ~ paste0(">",max(steps)),
        !is.na(discrete) ~ discrete
      )) %>%
      mutate(discrete = case_when(
        is.infinite(ratio) ~ paste("No",log.type),
        !is.na(discrete) ~ discrete
      ))-> df
  }
  
  
  # lev <- levels(as.factor(df$discrete)) # on enregistre l'ensemble des chr obtenus au dessus
  
  ## Tri des niveaux du facteur "discrete"
  ordre <- as.data.frame(cbind(
    # cree une colonne dans laquelle on mettra la borne inferieure de l'intervalle
    ord = NA,
    # mets les niveaux de la colonne discrete en parallele
    lev = lev
  ))
  ordre$ord <- as.character(ordre$ord) #ord cree en facteur, on le passe en charactere pour le completer
  for (i in 1:dim(ordre)[1]){
    if (length(unlist(strsplit(lev[i],"-"))) == 2){ # si lev est de la forme "x-y", on garde "x" dans ord
      ordre$ord[i] <- unlist(strsplit(lev[i],"-"))[1]
    } else if (lev[i] == paste("No",log.type)){ # si on a une valeur infinie ie lev de la forme "No log", on garde "Inf"
      ordre$ord[i] <- Inf
    } else if (length(unlist(strsplit(lev[i],">"))) == 2){ # si lev est de la forme ">y", on garde "y" dans ord
      ordre$ord[i] <- unlist(strsplit(lev[i],">"))[2]
    }
  }
  ordre$ord <- as.numeric(as.character(ordre$ord))
  ordre <- arrange(ordre, desc(ord)) #mets les niveaux dans le bon ordre
  
  lev <- ordre$lev # niveaux dans le bon ordre
  
  # passe discrete en facteur avec les niveaux dans le bon ordre
  df %>% mutate(discrete = factor(discrete, levels = lev)) -> df
  
  ## C. GGPLOT-----
  
  p<- ggplot() +
    geom_sf() +
    coord_sf(xlim = c(35, 100), ylim = c(-25, 25), expand = FALSE, crs = st_crs(4326))+
    geom_raster(data=df, aes(x, y, fill=discrete)) +
    # scale_fill_hue(h = c(150,375), l = 30, c = 150, #p5
    #                # direction = -1, # si invert_ratio= = T
    #                na.value = "grey50")+
    scale_fill_brewer(palette = "OrRd",
                      direction = -1, #si invert_ratio == T
                      na.value = "grey50",
                      drop = F)+ #p2
    # scale_fill_manual(values = rainbow(length(lev))[length(lev):1], #p1
    # scale_fill_manual(values = rainbow(length(lev)), # si invert_ratio == T
    # na.value = "grey50")+
    # scale_fill_brewer(palette = "YlOrRd",
    #                   # direction = -1, # si invert_ratio == T
    #                   na.value = "grey50")+ #p3
    # scale_fill_brewer(palette = "Set1",
    #                   direction = -1, # si invert_ratio == F
    #                   na.value = "grey50")+ #p4
    geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
    labs(title = titre,
         subtitle = paste("n FAD = ",n.fad," ; n", log.type," = ", n.log),
         fill=fill_lab)
  
  p <- mise.en.forme.ggplot(p)
  
  return(p)
}

#======================================================================================#
#                         CARTE DU NOMBRE D'OCCURENCES DE FOB                      342 #
#======================================================================================#
#  ARGUMENTS:                                                                          #
# Ob7 (data.frame) : donnees observateur BRUT                                          #
# DATA_PATH (chr) : path to data directory                                             #
# year (num) : annee dont on veut la carte. 0 pour un pool des annees                  #
# obj.type (chr) : choix entre "ALOG", "NLOG", "FAD" et "all" pour pool                #
# gsize (num) : choix entre 1, 2 et 5, pour des cellules de 1dx1d, 2dx2d ou 5dx5d      #
# month (vector num) : pour representer seulement certains mois trimestre. Si c(), pas #
#                   de mois choisi                                                     #
# n (logi) : si n = TRUE, retourne le nombre total d'objet observe sur la periode      #
#             selectionnee. Ne plot pas la carte.                                      #
# eff (chr) : si "T", pond?re le nombre d'observations par le nb de jour dans la zone, #
#             si "F", ne prend pas en compte l'effort                                  #
#             si "plot", fait une carte avec le nombre de jour passe par case          #
# fixed.scale.max (num) default 0, the fill scale is not fixed. If different from 0    #
#                       specify the maximum value of the desired scale                 #
# linear.scale (log) if T, colour of the filling scale are evenly spaced               #
#                    if F, they bring up contrast between smaller values               #
# del.low.eff (log): if T, the cells with a low observation effort are replaced by NAs #
#                    if F, all the cells are kept                                      #
# eff.threshold (num): minimum number of observation days par cell, if del.low.eff is T#
#                                                                                      #
# return: ggplot                                                                       #
#--------------------------------------------------------------------------------------#

### PROBLEME AVEC L EFFORT D OBSERVATION, PAS TRES PRECIS. JE PRENDS UNE OBSERVATION
### PAR JOUR, CE QUI EST UNE GROSSE APPROXIMATION DU TEMPS PASSE PAR ZONE

map_occurence <- function(Ob7, DATA_PATH, year = 0, obj.type = "all", gsize = 2, month = c(),
                          return_raster = F,
                          data_preped = FALSE, eff = "T", linear.scale=T, fixed.scale.max=0,
                          delete.low.effort = T, eff.threshold = 6){
  
  ### 1. PREPARATION DES DONNEES ----
  #------------------------------------------------------------------------------  
  if (data_preped == FALSE){
    Ob7 <- prep.obs(Ob7)
  }
  
  # @ttob7 <- "all_vessel_activities_observe_fr_indian.rds"
  ttob7 <- read.csv(file = file.path(DATA_PATH, "all_vessel_activities_observe_fr_indian.csv"))
  
  ### 2. SELECTION DONNEES D'INTERET ----
  #------------------------------------------------------------------------------   
  ## A. SELECTION COLONNES
  data<-data.frame(Ob7$year,Ob7$latitude,Ob7$longitude,Ob7$obj_conv,Ob7$observation_date)
  names(data)<-c("year","latitude","longitude","fob_type","observation_date")
  
  ttob7 <- data.frame(ttob7$year,ttob7$vessel_name,ttob7$observation_date,
                      ttob7$latitude,ttob7$longitude)
  names(ttob7) <- c("year","vessel_name","observation_date","latitude","longitude")
  
  ## B. EXTRACTION DES DONNEES DE L'ANNEE ----
  if (any(year!=0)){
    data<-data[data$year %in% year,]
    ttob7 <- ttob7[ttob7$year %in% year,]
  }
  
  ## C. SELECTION DU TYPE D'OBJET CHOISI SI obj.type DIFFERENT DE "all" ----
  ##    SI obj.type == "all", NETTOIE LES DONNEES
  if(obj.type!="all"){
    data<-subset(data, data$fob_type == obj.type)
  }else{
    data<-data[data$fob_type!="NULL",]
    data<-data[data$fob_type!="FOB",]
    data<-data[data$fob_type!="LOG",]
  }
  
  ## D. SUPPRIME LES LIGNES CONTENANT des COORDONNEES NULLES ----
  data<-data[data$latitude!=0 | data$longitude!=0,]
  ttob7<-ttob7[ttob7$latitude!=0 | ttob7$longitude!=0,]
  
  ## E. SELECTION DES DONNEES DU TRIMESTRE ----
  if (!is.null(month)){
    
    data <- subset.month(data, month)
    ttob7 <- subset.month(ttob7, month)
    
  }
  
  ## F. TRI POUR GARDER UNE OBS PAR J DANS TTES OBS ----
  ttob7$id_unique <- paste(ttob7$vessel_name,ttob7$observation_date)
  ttob7 <- ttob7[!duplicated(ttob7$id_unique),]
  
  ## G. NOMBRE TOTAL D'OBJETS ----
  nb<-dim(data)[1]
  if (eff=="plot"){
    nb <-dim(ttob7)[1]
  }
  
  ### 3. CREATION DES RASTER A TRACER ----
  #------------------------------------------------------------------------------
  ## A. creation des rasters ----
  ## avec les bonnes coordonnees, et dont la grille contient des 0
  r <- create.raster(gsize)
  eff_obs <- create.raster(gsize)
  
  ## B. COMPTE DES OCCURENCES ET DE L EFFORT PAR CELLULE ----
  r <- fill.raster(data, r)
  eff_obs <- fill.raster(ttob7, eff_obs)
  
  
  ## C. CALCUL DU RATIO OCC/EFF ----
  if (eff == "T"){
    r[]<-r[]/eff_obs[]
  }else if (eff == "plot"){
    r[]<-eff_obs[]
  }
  names(r)<-"occ_over_eff"
  
  ## D. REMPLACE LES VALEURS DES CELLULES AVEC MOINS DE eff.threshold JOURS D OBSERVATION PAR DES NA ----
  if (delete.low.effort){
    r <- del.low.eff(r, eff_obs, gsize, eff.threshold)
  }
  
  if (return_raster == T){
    return(r)
  }
  
  ### 4. CREATION DU GGPLOT ----
  #-----------------------------------------------------------------------
  world <- map_data("world")
  df<-as.data.frame(r, xy=TRUE)
 
  ## A. TITRE ----
  
  title <- generate.title.occ(obj.type, eff, month, year)
  
    ## B. SCALE ----
  if (linear.scale==FALSE){ # SCALE WITH CHANGE OF COLOUR CONCENTRATED IN THE LOWER VALUES
    
    if(fixed.scale.max!=0){ # FIX THE MAX OF THE SCALE
      max=fixed.scale.max
    }else {
      max=max(df$occ[is.na(df$occ)==FALSE & is.infinite(df$occ)==FALSE])
    }
    
    mid1=max/3
    mid2=2*max/3
    
    if (eff=="F"){
      labs <- c(0,round(2*max/10),
                round(mid1),round(mid2),
                round(max))
      brks <- c(0,round(2*max/10),round(mid1),round(mid2),round(max))
    } else {
      labs <- c(0,round(2*max/10,2),
                round(mid1,2),round(mid2,2),
                round(max,2))
      brks <- c(0,2*max/10,mid1,mid2,max)
    }
    vals <- c(0,0.1,0.2,1)
    lims <- c(0,max)
    
  } else { # SCALE WITH THE CHANGE OF COLOUR UNIFORM OVER THE WHOLE SCALE
    if(fixed.scale.max!=0){ # FIX THE MAX OF THE SCALE
      max=fixed.scale.max
    }else {
      max=max(df$occ[is.na(df$occ)==FALSE & is.infinite(df$occ)==FALSE])
    }
    
    vals = c(0,1/3,2/3,1)
    brks = waiver()
    labs = waiver()
    lims = c(0,max)
  }
  
   ## C. GGPLOT ----
  p<- ggplot() +
    geom_sf() +
    coord_sf(xlim = c(35, 100), ylim = c(-25, 25), expand = FALSE, crs = st_crs(4326))+
    geom_raster(data=df, aes(x, y, fill=occ_over_eff)) +
    scale_fill_gradientn(colors=c("gray80","blue","yellow","red"),
                         breaks = brks,
                         values = vals,
                         labels=labs,
                         limits=lims)+
    geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
    labs(title = title$titre,
         subtitle = paste("n = ",nb),
         fill=title$echelle)
  
  p <- mise.en.forme.ggplot(p)
  
  return(p)
}


#======================================================================================#
#                         CARTE OPERATION SUR BALISES                                  #
#======================================================================================#
#  ARGUMENTS:                                                                          #
# Ob7 (data.frame) : donnees observateur                                               #
# DATA_PATH (chr) : path to data directory                                             #
# year (num) : annee dont on veut la carte. c() pour un pool des annees                #
# operation (num) : operation realisee sur la balise (code de la base observe)         #
#                   1 : visite                                                         #
#                   2 : recuperation                                                   #
#                   3 : mise a l'eau                                                   #
# obj.type (chr) : vecteur avec choix entre "ALOG", "NLOG", "FAD"                      #
# gsize (num) : choix entre 1, 2 et 5, pour des cellules de 1dx1d, 2dx2d ou 5dx5d      #
# month (vector num) : pour representer seulement certains mois trimestre. Si c(), pas #
#                   de mois choisi                                                     #
# data_preped (logical) : if F, use prep.obs function on the input data                #
# eff (logical) : si T, pond?re le nombre d'observations par le nb de jour dans la zone#
#             si F, ne prend pas en compte l'effort                                    #
#                                                                                      #
# return: ggplot                                                                       #
#--------------------------------------------------------------------------------------#


map_buoy_operation <- function(Ob7, DATA_PATH, year = c(), operation = 3, obj.type = c("NLOG","FAD"), gsize = 2, month = c(),
                                data_preped = FALSE, eff = F, linear.scale=T, fixed.scale.max=0){
  
  ### 1. PREPARATION DES DONNEES ----
  #------------------------------------------------------------------------------  
  if (data_preped == FALSE){
    Ob7 <- prep.obs(Ob7)
  }
  
  ttob7 <- read.csv(file = file.path(DATA_PATH, "all_vessel_activities_observe_fr_indian.csv"))
  
  ### 2. SELECTION DONNEES D'INTERET ----
  #------------------------------------------------------------------------------   
  ## A. SELECTION COLONNES
  data<-data.frame(Ob7$year,Ob7$latitude,Ob7$longitude,Ob7$obj_conv,Ob7$observation_date,
                   Ob7$operation_on_buoy,Ob7$operation_on_buoy_code)
  names(data)<-c("year","latitude","longitude","fob_type","observation_date",
                 "operation_on_buoy","operation_on_buoy_code")
  
  data <- data[data$operation_on_buoy_code==operation,]

  ttob7 <- data.frame(ttob7$year,ttob7$vessel_name,ttob7$observation_date,
                      ttob7$latitude,ttob7$longitude)
  names(ttob7) <- c("year","vessel_name","observation_date","latitude","longitude")
  
  ## B. EXTRACTION DES DONNEES DE L'ANNEE ----
  if (!is.null(year)){
    data<-data[data$year %in% year,]
    ttob7 <- ttob7[ttob7$year %in% year,]
  }
  #  if (dim(data)[1]==0){
  #    #sortir de la fonction et dire (pas de donnees pour cette annee)
  #  }
  
  ## C. SELECTION DU TYPE D'OBJET CHOISI ----
  data<-subset(data, data$fob_type %in% obj.type)
  
  ## D. SUPPRIME LES LIGNES CONTENANT des COORDONNEES NULLES ----
  data<-data[data$latitude!=0 | data$longitude!=0,]
  ttob7<-ttob7[ttob7$latitude!=0 | ttob7$longitude!=0,]
  
  ## E. SELECTION DES DONNEES DU TRIMESTRE ----
  if (!is.null(month)){
    data <- subset.month(data, month)
    ttob7 <- subset.month(ttob7, month)
  }
  
  ## F. TRI POUR GARDER UNE OBS PAR J DANS TTES OBS ----
  ttob7$id_unique <- paste(ttob7$vessel_name,ttob7$observation_date)
  ttob7 <- ttob7[!duplicated(ttob7$id_unique),]
  
  ## G. NOMBRE TOTAL D'OBJETS ----
  nb<-dim(data)[1]
  
  ### 3. CREATION DES RASTER A TRACER ----
  #------------------------------------------------------------------------------
  ## A. creation des rasters ----
  ## avec les bonnes coordonnees, et dont la grille contient des 0
  r <- create.raster(gsize)
  eff_obs <- create.raster(gsize)
  
  ## B. COMPTE DES OCCURENCES ET DE L EFFORT PAR CELLULE ----
  r <- fill.raster(data, r)
  eff_obs <- fill.raster(ttob7, eff_obs)
  
  ## C. CALCUL DU RATIO OCC/EFF
  if (eff == T){
    r[]<-r[]/eff_obs[]
  }
  names(r)<-"occ_over_eff"
  
  ## D. REMPLACE LES VALEURS DES CELLULES AVEC MOINS DE 10 JOURS D OBSERVATION PAR DES NA ----
  r <- del.low.eff(r, eff_obs, gsize)
  
  ### 4. CREATION DU GGPLOT ----
  #-----------------------------------------------------------------------
  world <- map_data("world")
  df<-as.data.frame(r, xy=TRUE)
  
  ## A. TITRE ----
  
  echelle <- "Buoys"
  titre <- "Nombre de balises"
  
  if (!is.null(obj.type)){
    titre <- paste(titre, 'associees aux', paste(obj.type))
  }
  
  
  if (operation == 1){
    echelle <- paste(echelle, "visited\n")
    titre <- paste(titre, "visitees")
  } else if (operation == 2){
    echelle <- paste(echelle, "recuperated\n")
    titre <- paste(titre, "recuperees")
  } else if (operation == 3){
    echelle <- paste(echelle, "deployed\n")
    titre <- paste(titre, "deployees")
  }
  
  if (!is.null(obj.type)){
    echelle <- paste(echelle, 'on', paste(obj.type))
  }
  
  
  if (eff == T){
    echelle <- paste(echelle, "/ day")
    titre <-  paste(titre, "\npar jour d'observation")
  }
  
  if (is.null(year)){
    titre <-  paste(titre, "\nPool toutes annees")
  } else if (!is.null(year)){
    titre <- paste(titre, "\nEn", paste(year))
  }
  
  
  # a. If eff ----
  # if (eff == T){
  #   echelle = paste("Buoy deployment on\n",obj.type,"/day")
  #   if (is.null(month)){
  #     if (is.null(year)){
  #       if (obj.type!="all"){
  #         titre<- paste("Nombre de balises deployees sur", obj.type, "\npar jour d'observation \n Pool toutes annees")
  #       }else{
  #         titre<- "Nombre de balises deployees sur FOB \npar jour d'observation \n Pool toutes annees"
  #       }
  #     }else{
  #       if (obj.type!="all"){
  #         titre<- paste("Nombre de balises deployees sur", obj.type, "\npar jour d'observation en", year)
  #       }else{
  #         titre<- paste("Nombre de balises deployees sur FOB \npar jour d'observation en", year)
  #       }
  #     }
  #   }else{
  #     if (is.null(year)){
  #       if (obj.type!="all"){
  #         titre<- paste0("Nombre de balises deployees sur ", obj.type,"\npar jour d'observation \n Pool toutes annees \n Mois : ", paste(month,collapse=","))
  #       }else{
  #         titre<- paste0("Nombre de balises deployees sur FOB \npar jour d'observation \n Pool toutes annees \n Mois : ", paste(month,collapse=","))
  #       }
  #     }else{
  #       if (obj.type!="all"){
  #         titre<- paste0("Nombre de balises deployees sur ", obj.type,"\npar jour d'observation\n Annee : ", year,"\n Mois : ", paste(month,collapse=","))
  #       }else{
  #         titre<- paste0("Nombre de balises deployees sur FOB \npar jour d'observation \n Annee :", year,"\n Mois : ", paste(month,collapse=","))
  #       }
  #     }
  #   }
  # }
  # 
  # # b. If eff FALSE ----
  # if (eff == F){
  #   echelle = paste("Buoy deployment on\n",obj.type)
  #   if (is.null(month)){
  #     if (is.null(year)){
  #       if (obj.type!="all"){
  #         titre<- paste("Nombre de balises deployees sur", obj.type, "\n Pool toutes annees")
  #       }else{
  #         titre<- "Nombre de balises deployees sur FOB \n Pool toutes annees"
  #       }
  #     }else{
  #       if (obj.type!="all"){
  #         titre<- paste("Nombre de balises deployees sur", obj.type, "en", year)
  #       }else{
  #         titre<- paste("Nombre de balises deployees sur FOB en", year)
  #       }
  #     }
  #   }else{
  #     if (is.null(year)){
  #       if (obj.type!="all"){
  #         titre<- paste0("Nombre de balises deployees sur ", obj.type,"\n Pool toutes annees \n Mois : ", paste(month,collapse=","))
  #       }else{
  #         titre<- paste0("Nombre de balises deployees sur FOB\n Pool toutes annees \n Mois : ", paste(month,collapse=","))
  #       }
  #     }else{
  #       if (obj.type!="all"){
  #         titre<- paste0("Nombre de balises deployees sur ", obj.type,"\n Annee : ", year,"\n Mois : ", paste(month,collapse=","))
  #       }else{
  #         titre<- paste0("Nombre de balises deployees sur FOB\n Annee :", year,"\n Mois : ", paste(month,collapse=","))
  #       }
  #     }
  #   }
  # }
  
  
  
  ## B. ECHELLE DE COULEUR ----
  if (linear.scale==FALSE){ # SCALE WITH CHANGE OF COLOUR CONCENTRATED IN THE LOWER VALUES
    
    if(fixed.scale.max!=0){ # FIX THE MAX OF THE SCALE
      max=fixed.scale.max
    }else {
      max=max(df$occ_over_eff[is.na(df$occ_over_eff)==FALSE & is.infinite(df$occ_over_eff)==FALSE])
    }
    
    mid1=max/3
    mid2=2*max/3
    
    if (eff==F){
      labs <- c(0,round(2*max/10),
                round(mid1),round(mid2),
                round(max))
      brks <- c(0,round(2*max/10),round(mid1),round(mid2),round(max))
    } else {
      labs <- c(0,round(2*max/10,2),
                round(mid1,2),round(mid2,2),
                round(max,2))
      brks <- c(0,2*max/10,mid1,mid2,max)
    }
    vals <- c(0,0.1,0.2,1)
    lims <- c(0,max)
    
  } else { # SCALE WITH THE CHANGE OF COLOUR UNIFORM OVER THE WHOLE SCALE
    if(fixed.scale.max!=0){ # FIX THE MAX OF THE SCALE
      max=fixed.scale.max
    }else {
      max=max(df$occ_over_eff[is.na(df$occ_over_eff)==FALSE & is.infinite(df$occ_over_eff)==FALSE])
    }
    
    vals = c(0,1/3,2/3,1)
    brks = waiver()
    labs = waiver()
    lims = c(0,max)
  }
  
  ## C. GGPLOT ----
  
  p<- ggplot() +
    geom_sf() +
    coord_sf(xlim = c(35, 100), ylim = c(-25, 25), expand = FALSE, crs = st_crs(4326))+
    geom_raster(data=df, aes(x, y, fill=occ_over_eff)) +
    scale_fill_gradientn(colors=c("gray80","blue","yellow","red"),
                         breaks = brks,
                         values = vals,
                         labels=labs,
                         limits=lims)+
    geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
    labs(title = titre,
         subtitle = paste("n = ",nb),
         fill=echelle)
  
  p <- mise.en.forme.ggplot(p)
  
  return(p)
}

