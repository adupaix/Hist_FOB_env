#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-02-24
#'@email : 
#'#*******************************************************************************************************************
#'@description :  Initialize the analysis (functions, paths, file names, log file)
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


# Load functions used in the following sub-routines:

# source(file.path(FUNC_PATH, "1.nc.to.Array.R"))
# source(file.path(FUNC_PATH, "1.read.river.data.R"))
source(file.path(FUNC_PATH, "1.subfunctions.R"))
source(file.path(FUNC_PATH,'2.subfunctions.R'))
source(file.path(FUNC_PATH,'3.subfunctions.R'))
source(file.path(FUNC_PATH,'4.subfunctions.R'))

# Create the name of the simulation from the arguments
sim_name <- generate.sim_name(forcing,
                                input_location,
                                input_method,
                                dist,
                                bouncing)
  
# Create output folder
output_path_1 <- file.path(OUTPUT_PATH, sim_name, year, "1.nb_cover")
output_path_2_more <- file.path(OUTPUT_PATH, sim_name, year, paste0("w",weight_method,
                                                                    "_ltime",ltime_method,"-",ltime,"-sd",ltime_sd,
                                                                    ifelse(is.null(thr_disch), "_no-thr-disch", paste0("_thr-disch",thr_disch))))
output_path_2 <- file.path(output_path_2_more, "2.info_on_points")
output_path_3 <- file.path(output_path_2_more, "3.arrays_by_release_date")
output_path_4 <- file.path(output_path_2_more, paste0("4.maps_",agg.time_scale))
dir.create(output_path_1, recursive = T, showWarnings = F)
dir.create(output_path_2, recursive = T, showWarnings = F)
dir.create(output_path_3, recursive = T, showWarnings = F)
dir.create(output_path_4, recursive = T, showWarnings = F)

# If RESET is T, delete all the output files
if (RESET[1] == T){
  try(unlink(list.files(output_path_1, full.names = T),
             recursive = T),
      silent = T)
} else if (RESET[2] == T){
  try(unlink(list.files(output_path_2, full.names = T),
             recursive = T),
      silent = T)
} else if (RESET[3] == T){
  try(unlink(list.files(output_path_3, full.names = T),
             recursive = T),
      silent = T)
} else if (RESET[4] == T){
  try(unlink(list.files(output_path_4, full.names = T),
             recursive = T),
      silent = T)
}

lines.to.cat <- c()

msg <- "\14" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

#'@read_rivers
#'**********

msg <- bold("0. Initializing:\n\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
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
  dplyr::filter(dis_m3_pmx >= thr_disch) -> embouchures

# keep only variables of interest
rivers_IO %>%
  dplyr::filter(MAIN_RIV %in% embouchures$HYRIV_ID) %>%
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

#' Create output files names
logName1 <- file.path(output_path_1, "log.txt")
nCoverPoints <- file.path(output_path_1, "number_of_cover_points_per_input_point.csv")

logName2 <- file.path(output_path_2, "log.txt")
weightInput <- c(file.path(output_path_2, "weight_per_points_matrix.csv"),
                 file.path(output_path_2, "weight_per_points_matrix.csv"))

logName3 <- file.path(output_path_3, "log.txt")

logName4 <- file.path(output_path_4, "log.txt")
plotListName <- file.path(output_path_4, "plot_list.rds")
pngMapsName <- file.path(output_path_4, paste0(agg.time_scale,"ly_maps.png"))

# Logical to know if output files exist
nCoverExists <- file.exists(nCoverPoints)
weightExists <- all(file.exists(weightInput))
log3Exists <- file.exists(logName3)
log4Exists <- file.exists(logName4)

# For parallel study:
# On Windows, or if don't want to parralelize, set cores number to 1
if (.Platform$OS.type == "windows" | as.logical(Parallel[1]) == F) {
  nb_cores = 1
} else { #use a fraction of the available cores
  nb_cores = trunc(detectCores() * as.numeric(Parallel[2]))
}

# # Do not delete
# DoNotDeleteMe <- c("DoNotDeleteMe", ls())
# 
# # Delete the output files already created if RESET = T
# if (any(RESET) & any(filesExist)){
#   
#   # if RESET[1] = T, delete the files which exist generated by the two sub_routines 
#   if(RESET[1]){
#     invisible(file.remove(c(logName, globalName, meanAggName, aggregateName,
#                             plotListName, meanPlotListName, mapName)[filesExist] ))
#   }
#   # if RESET[2] = T, delete the files generated by the second sub_routine which exist
#   if (RESET[2]){
#     invisible(file.remove(c(plotListName, meanPlotListName, mapName)[filesExist[5:7]]))
#   }
# }
# 
# # Logical to know if files exist (to know if they need to be calculated)
# glob_array_exists <- file.exists(globalName)
# mean_agg_array_exists <- file.exists(meanAggName)
# agg_array_exists <- file.exists(aggregateName)
# 
# # Create log file
# sink(logName, append = T)
#   
# cat("#### FROM SIMUALTION RESULTS TO MAPS ####\n=========================================\n Date & Time:",format(Sys.time()),"\n\n")
# cat("1. GENERATING ARRAY FROM SIMULATION RESULTS")
# cat("\n Execution time :", format(Sys.time()))
# cat("\n\n### SIMULATION CHARACTERISTICS")
# cat(paste("\n Forcing product :", forcing))
# cat(paste("\n Location of input points :", input_location))
# cat(paste("\n Method for input points :", input_method))
# 
# if (input_method == "kFromCoast"){
#   cat(paste0("\n Distance of input from coast : ", dist, "km"))
# }
# cat(paste0("\n Dispersion coefficient : 10^-", dispersion, " m^2/s^3"))
# cat(paste0("\n Bouncing on coast : ", bouncing, "\n\n"))
# 
# cat("### ARGUMENTS USED")
# cat(paste("\n Life time :", ltime, "days"))
# cat(paste("\n Life time method :", ltime_method))
# cat(paste("\n Weighting method :", weight_method))
# cat(paste("\n Grid cell size :", gsize, "degrees"))
# cat(paste("\n Aggregating time scale :", agg.time_scale))
# cat(paste("\n Discharge threshold :", ifelse(is.null(thr_disch), "NULL", paste(thr_disch, "m3/s"))))
# cat(paste("\n Using parallelization :", as.logical(Parallel[1])))
# cat(paste("\n Number of cores used :", ifelse(Parallel[1]==F, 1, trunc(detectCores() * Parallel[2]))))
# 
# sink()
