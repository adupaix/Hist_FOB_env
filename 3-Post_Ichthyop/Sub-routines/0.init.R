#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-11
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description :  Initialize the analysis (functions, paths, file names, log files)
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


#' Load functions used in the following sub-routines:
source(file.path(FUNC_PATH, "1.subfunctions.R"))
source(file.path(FUNC_PATH,'2.subfunctions.R'))
source(file.path(FUNC_PATH,'3.subfunctions.R'))
source(file.path(FUNC_PATH,'4.subfunctions.R'))

#' Do not delete
toKeep <- c("toKeep", ls())

# Create the name of the simulation from the arguments (function in Functions/1.subfunctions)
sim_name <- generate.sim_name(forcing,
                                input_location,
                                input_method,
                                dist,
                                bouncing)
  
#' Create output folders
#'     Names
output_paths <- c()
output_paths[1] <- file.path(OUTPUT_PATH, sim_name, year, "1.nb_cover")
output_paths[2] <- file.path(OUTPUT_PATH, sim_name, year, "2.info_on_points")
output_path_3_more <- file.path(OUTPUT_PATH, sim_name, year, paste0("w",weight_method,
                                                                    "_ltime",ltime_method,"-",ltime,
                                                                    ifelse(ltime_method == 1, paste0("-sd",ltime_sd), ""),
                                                                    ifelse(is.null(thr_disch), "_no-thr-disch", paste0("_thr-disch",thr_disch))))
output_paths[3] <- file.path(output_path_3_more, "3.arrays_by_release_date")
output_paths[4] <- file.path(output_path_3_more, paste0("4.maps_",agg.time_scale))
#'     Create
dir.create(output_paths[1], recursive = T, showWarnings = F)
dir.create(output_paths[2], recursive = T, showWarnings = F)
dir.create(output_paths[3], recursive = T, showWarnings = F)
dir.create(output_paths[4], recursive = T, showWarnings = F)


# create paths used to read data from the simulation and from the 1-Input_Ichthyop folder
sim_output_path <- file.path(DATA_PATH, "Output_Ichthyop", sim_name, year)
sim_input_path <- file.path(DATA_PATH, "Input_Ichthyop", paste0(input_location, "_nlog_input_", forcing, "_", input_method))


# If RESET is T, delete all the output files
if (RESET[1] == T){
  try(unlink(list.files(output_paths[1], full.names = T),
             recursive = T),
      silent = T)
}
if (RESET[2] == T){
  try(unlink(list.files(output_paths[2], full.names = T),
             recursive = T),
      silent = T)
}
if (RESET[3] == T){
  try(unlink(list.files(output_paths[3], full.names = T),
             recursive = T),
      silent = T)
}
if (RESET[4] == T){
  try(unlink(list.files(output_paths[4], full.names = T),
             recursive = T),
      silent = T)
  try(unlink(file.path(output_path_3_more, "4.global_array.rds")),
      silent = T)
}


#' Print messages
lines.to.cat <- c()
msg <- "\14" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
msg <- bold("0. Initializing\n\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)


#' Information on weighting methods
#'     Will be used in logs (log.txt)
#'     and to know how many weighting method are considered
weight_informations <- data.frame(cbind(1:9,
                                        c("No weight",
                                          "Weight proportional to associated coastline length",
                                          "Weight proportional to coastal forest cover surface",
                                          "Weight proportional to forest cover surface of associated river basins multiplied by mean river discharge",
                                          "Weight proportional to forest cover surface (multiplied by river discharge for associated rivers",
                                          "Weight proportional to coastal forest cover multiplied by the precipitations at the release point",
                                          "Weight proportional to forest cover of associated rivers multiplied by the precipitations at the release point",
                                          "Weight proportional to forest cover surface multiplied by precipitations at release point",
                                          "Weight proportional to forest cover surface"
                                          )))
weight_informations$X2 <- as.character(weight_informations$X2)
n_weight_methods <- dim(weight_informations)[1]


#' Create output files names
Names <- list()
#'       Sub-routine 1
Names$log1 <- file.path(output_paths[1], "log.txt")
Names$coverRiver <- file.path(output_paths[1], "n_cover_per_river.csv")
Names$coverMouth <- file.path(output_paths[1], "n_cover_per_mouth.csv")
Names$coverGlobal <- file.path(output_paths[1], "number_of_cover_points_per_input_point.csv")
#'       Sub-routine 2
Names$log2 <- file.path(output_paths[2], "log.txt")
Names$weightInput <- c(file.path(output_paths[2], "weight_per_points_summary.csv"),
                 file.path(output_paths[2], paste0("weight_per_points_matrix_w",weight_method,".csv")))
#'       Sub-routine 3
Names$log3 <- file.path(output_paths[3], "log.txt")
#'       Sub-routine 4
Names$log4 <- file.path(output_paths[4], "log.txt")
Names$globArray <- file.path(output_path_3_more, "4.global_array.rds")
Names$aggArray <- file.path(output_paths[4], paste0("aggregated_array_",agg.function,".rds"))
Names$plotList <- file.path(output_paths[4], paste0("plot_list_", agg.function,
                                                      ifelse(log_color_scale == T, "_log_","_"),
                                                      paste(area_to_map, collapse = ""),
                                                      ".rds"))
Names$pngMaps <- file.path(output_paths[4], paste0(ifelse(log_color_scale == T, "log_",""),
                                                     agg.function,
                                                     "_", paste(area_to_map, collapse = ""),
                                                     "_w",weight_method,
                                                     "_ltime",ltime_method,"-",ltime,
                                                     ifelse(ltime_method == 1, paste0("-sd",ltime_sd), ""),
                                                     ifelse(is.null(thr_disch), "_no-thr-disch", paste0("_thr-disch",thr_disch)),
                                                     "_", agg.time_scale,"ly_maps.png"))

rm(output_path_3_more)

# Logical to know if output files exist
Exists <- list()
Exists$cover <- all(file.exists(c(Names$coverGlobal, Names$coverRiver)))
Exists$weight <- all(file.exists(Names$weightInput))
Exists$log3 <- file.exists(Names$log3)
Exists$globArray <- file.exists(Names$globArray)
Exists$aggArray <- file.exists(Names$aggArray)
Exists$maps <- file.exists(Names$pngMaps)

# For parallel:
# On Windows, or if don't want to parralelize, set cores number to 1
if (.Platform$OS.type == "windows" | as.logical(Parallel[1]) == F) {
  nb_cores = 1
} else { #use a fraction of the available cores
  nb_cores = trunc(detectCores() * as.numeric(Parallel[2]))
}

#'@read_rivers
#'**********
#' The 2 first sub routines need to use data on rivers
#' If none of these 2 subroutines need to be run, we do not
#' read the rivers data
if (!Exists$cover | !Exists$weight){
  
  msg <- "Reading and filtering rivers file\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  msg <- "    - Reading\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  rivers_IO <- readRDS(file.path(DATA_PATH, "river_data", "rivers_IO.rds"))
  
  msg <- "    - Filtering\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  #' filter rivers
  rivers_IO %>%
    # garde uniquement les embouchures
    # (identifiant de la portion de riviere = a l'identifiant principal de la riviere)
    dplyr::filter(HYRIV_ID == MAIN_RIV) %>%
    # garde uniquement les portions de riviere qui se jettent dans la mer
    dplyr::filter(ENDORHEIC == 0) %>%
    # garde uniquement les fleuves avec un debit maximal de plus d 100 m3 par seconde
    dplyr::filter(dis_m3_pmx >= thr_disch)%>%
    dplyr::select(HYRIV_ID, #id de la portion de riviere
                  NEXT_DOWN,#id de la portion en aval
                  MAIN_RIV, # id de la portion qui se jette dans la mer
                  LENGTH_KM, # longueur de la portion en km
                  HYBAS_L12, # id du bassin versant,
                  #pour faire le lien avec l'autre base de donnees
                  dis_m3_pyr, # debit moyen en m3/s
                  dis_m3_pmn, # debit minimal en m3/s
                  dis_m3_pmx # debit maximal
    ) -> embouchures
  
  # keep only variables of interest
  rivers_IO %>%
    dplyr::filter(MAIN_RIV %in% embouchures$HYRIV_ID) %>%
    # dplyr::filter(NEXT_DOWN != 0) %>% #do not keep the river mouths (considered separately)
    dplyr::filter(dis_m3_pmx >= thr_disch) %>%
    dplyr::select(HYRIV_ID, #id de la portion de riviere
                  NEXT_DOWN,#id de la portion en aval
                  MAIN_RIV, # id de la portion qui se jette dans la mer
                  LENGTH_KM, # longueur de la portion en km
                  HYBAS_L12, # id du bassin versant,
                  #pour faire le lien avec l'autre base de donnees
                  dis_m3_pyr, # debit moyen en m3/s
                  dis_m3_pmn, # debit minimal en m3/s
                  dis_m3_pmx # debit maximal
    ) -> rivers_filtered
  
  st_drop_geometry(rivers_filtered) %>%
    dplyr::select(MAIN_RIV, HYRIV_ID) -> link_HYRIV_MAINRIV
  
}

