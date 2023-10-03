###############################################################################################
#                              #   PREPARATION DONNEES OBSERVATEUR  #                         #
###############################################################################################
#                               Ob7 : data.frame avec donnees obs brutes                      #
#                                                                                             #
#        return : data.frame avec donnees preparees. 3 elements:                              #
#             1. Deduction obj.type quand absent                                              #
#             2. conversion type d'objet pour ne garder que                                   #
#                     FAD, NLOG, ALOG, LOG, FOB et NULL                                       #
#             3. suppression des doublons ac id fob identique                                 #
#                                                                                             #
###############################################################################################

prep.obs <- function (Ob7){
  require(tictoc)
  tic(msg = "Preparation of observers data", quiet = F)
  
  cat("Starting fob.null function \n")
  Ob7 <- fob.null(Ob7)
  #return(Ob7)
  
  cat("Starting conver.fob function \n")
  Ob7 <- conver.fob(Ob7) #Ameliorer pour accelerer
  
  #return(Ob7)
  cat("Starting doubl.obs function \n")
  Ob7 <- doubl.obs(Ob7)
  
  toc()
  return(Ob7)
  
}


#===================================================================================#
#                           DEDUCTION TYPE OBJ QUAND NULL                           #
#===================================================================================#
# Ob7 : (data.frame) donnees obs brutes                                             #
#                                                                                   #
# return : (data.frame) donnees completees                                          #
#                                                                                   #
#===================================================================================#

fob.null <- function(Ob7){
  
  ### 1. FOB type = NULL
  ###====================
    # Beaucoup de ligne du jeu de donnees contiennent un obj_conv == ""
    # Dans la majorite des cas, ce fob_type peut etre deduit des donnees 
    # fob_type_when_arriving et de l activite sur objet realisee
  
    #On cree une colonne qui contient les infos d objet de fob_type_when_arriving
    # qu on va completer avec celles de fob_type_when_leaving
  Ob7<-cbind(Ob7, obj_conv=Ob7$fob_type_when_arriving)
  
  data <- Ob7[Ob7$obj_conv == "",]
  data_rest <- Ob7[Ob7$obj_conv != "",]
  
  lev_of_reference <- c("","AFAD","ALOG","ANLOG","DFAD","FAD","FALOG","FOB","HALOG","LOG","NLOG","VNLOG")
  
  if (length(levels(data$obj_conv)) != length(lev_of_reference)){
    levels(data$obj_conv)<-lev_of_reference
  }

  data %>% dplyr::filter(operation_on_object_code %in% c(1,3,4,6,7,8,9)) -> data_unknown
  data %>% dplyr::filter(!operation_on_object_code %in% c(1,3,4,6,7,8,9)) -> data
  #   ## 1.a. OPERATION OBJECT = 2
  #     # Pour les observations où le code de l'operation sur objet est egal a 2 (Visit)
  #     # on complete les donnees de obj_conv avec celles de
  #     # fob_type_when_arriving
  # 
  # data[data$operation_on_object_code==2, "obj_conv"] <-
  #   data[data$operation_on_object_code==2, "fob_type_when_arriving"]
  # 
  #   ## 1.b. OPERATION OBJECT = 1
  #   ## ATTENTION, VALABLE QUE DANS LE CAS PRECIS DE MON JEU DE DONNEES
  #   ##----------------------------------------------------------------
  #     # Il y a une unique observation avec FOB type NULL et Operation objet = 1 (Deploiement)
  #     # Le fob_comment contient 'Bambous et filets'. Il s'agit donc d'un DFAD
  # 
  # data[data$operation_on_object_code==1, "obj_conv"]<-"DFAD"
  # 
    # ## 1.c. OPERATION OBJECT = 4 ou 7
    #   # L operation realisee est soit Removal, soit Sunk
    #   # Il n y a alors plus d objet en partant, mais il y a bien observation d un objet
    #   
    # data[data$operation_on_object_code == 4, "obj_conv"] <-
    #   data[data$operation_on_object_code == 4, "fob_type_when_arriving"]
    # data[data$operation_on_object_code == 7, "obj_conv"] <-
    #   data[data$operation_on_object_code == 7, "fob_type_when_arriving"]
    # 
    ## 1.d. NA RESTANTS
      # On utilise fob_comment pour completer la colonne obj_conv
      # si le commentaire contient 'radeau' ou 'DCP' on met DFAD
      # pour tout autre commentaire, on supprime les lignes
  
      # on test ensuite si le commentaire contient 'pas de radeau' ou 'pas de DCP'
      # et on supprime les lignes correspondantes
  
  data$fob_comment <- as.character(data$fob_comment)
  # 
  # narest <- data[data$obj_conv == "",]
  # data_non_na <- data[data$obj_conv!="",]
  
  if (dim(data)[1] != 0){
  # l<-c()
    for (i in 1:dim(data)[1]){
      
      comment <- tolower(data$fob_comment[i])
      
      if (grepl(pattern = "radeau|dfad|dcp", x = comment)){
        data$obj_conv[i]<-"DFAD"
      } else if (grepl(pattern = "naturel|tronc|bois|bille|renforcement", x = comment)){
        data$obj_conv[i]<-"NLOG"
      }
      # } else {
      #   l <- c(l,i)
      # }
      
      # if (grepl(pattern = "pas de radeau|pas de dcp", x = comment)){
      #   l <- c(l, i)
      # }
    }
  
  # data<-data[-l,]
  
  }
  
  ### RETOUR DONNEES COMPLETEES
  
  Ob7<-rbind(data_rest, data_unknown, data)
  
  return(Ob7)
}

#======================================================================================#
#                               CONVERSION FOB TYPES                                   #
#                               LAISSE CINQ CATEGORIES                                 #
#                            FAD, ALOG, NLOG, LOG, FOB                                 #
#======================================================================================#
#  ARGUMENTS:                                                                          #
# Ob7 (data.frame) : donnees observateurs                                              #
#                     ATTENTION la colonne contenant le type d'objet doit s'appeler    #
#                     "obj_conv"                                                       #
#                                                                                      #
#return: Ob7 avec obj_conv modifie                                                     #
#--------------------------------------------------------------------------------------#

conver.fob <- function (Ob7){
  
  ###MATRICE DE CONVERSION pour garder 6 types d'objet: FAD, ALOG, NLOG, FOB, LOG, NULL
  conv<-data.frame(c("AFAD","ALOG","ANLOG","DFAD","FAD","FALOG","FOB","HALOG", "LOG","NLOG","","VNLOG"),
                   c("FAD","ALOG","NLOG","FAD","FAD", "ALOG","FOB","ALOG", "LOG","NLOG","NULL","NLOG"))
  names(conv)<-c("tt.obj","conversion")
  
  conv <- conv[as.character(conv[,1]) %in% levels(Ob7$obj_conv),]
  
  ###REMPLI OBJ_CONV AVEC TYPE D OBJET CONVERTI
  Ob7$num_ligne <- as.numeric(rownames(Ob7))
  
  Ob7 <- merge(Ob7, conv, by.x = "obj_conv",
                by.y = "tt.obj", all = FALSE, sort = FALSE)
  
  Ob7 <- Ob7[order(Ob7$num_ligne),]
  rownames(Ob7) = as.character(Ob7$num_ligne)
  
  Ob7 <- subset(Ob7, select = -c(obj_conv,num_ligne))
  names(Ob7)[dim(Ob7)[2]] = "obj_conv"
  
  return(Ob7)
}

#======================================================================================#
#                               SUPRESSION DES DOUBLONS                                #
#                               GARDE UNE SEULE OBS POUR                               #
#                             CELLES AVEC UN fob_id IDENTIQUE                          #
#======================================================================================#
#  ARGUMENTS:                                                                          #
# Ob7 (data.frame) : donnees observateurs                                              #
#                                                                                      #
# return: Ob7 avec doublons supprimes                                                  #
#--------------------------------------------------------------------------------------#

doubl.obs <- function (Ob7){
  
  data<-Ob7
  
  data$fob_id <- as.character(data$fob_id)
  
  dupli_fobid <- data.frame(table(data$fob_id))
  dupli_fobid <- dupli_fobid[dupli_fobid$Freq > 1, ]
  
  data_d <- subset(data, data$fob_id %in% dupli_fobid$Var1)
  data_nd <- subset(data, !(data$fob_id %in% dupli_fobid$Var1))
  
  data_d <- data_d[!duplicated(data_d[,"fob_id"]),]
  
  data <- rbind(data_d, data_nd)
  
  return(data)
}


#=======================================================================================#
#                              SUPPRESSION DES DONNEES                                  #
#                           QUI NE CORRESPONDENT PAS A UNE                              #
#                                RENCONTRE ALEATOIRE                                    #
#=======================================================================================#
# ARGUMENTS:                                                                            #
# preped_Ob7 (data.frame) : donnees observateur preparee avec prep.obs                  #
#                                                                                       #
# return : data.frame avec les observations non aleatoires d objets supprimees          #
#=======================================================================================#

random.encounter <- function(preped_Ob7){

  data<-preped_Ob7
  
  ## 1. Suppression des mise a l eau de FAD ----
  data <- data[data$operation_on_object_code != 1,]
  
  ## 2. Suppression des visites/recuperations de balise appartenant au navire ou a son armement ----
  data<-data[(data$operation_on_buoy_code!=1 | data$buoy_ownership!="Ce navire ou cet armement")
      &(data$operation_on_buoy_code!=2 | data$buoy_ownership!="Ce navire ou cet armement"),]
  
  
  return(data)
}

