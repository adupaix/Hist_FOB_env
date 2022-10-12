#WORKING DIRECTORY
WD <- "path to the working directory"


DATA_PATH <- file.path(WD,"Data")
FUNC_PATH <- file.path(WD,"Functions")
MAP_PATH <- file.path(FUNC_PATH, "Maps")

source(file.path(MAP_PATH,'2.1.subfunctions.count.traj.R'), echo=FALSE) #sub functions used to build a raster with the counts of emissions
source(file.path(MAP_PATH,'2.1.subfunctions.ggplot.R'), echo=FALSE) # sub functions used to built the map and add input points for simulations
source(file.path(FUNC_PATH,'subfunctions.read.data.R'), echo=FALSE) # sub function used to read datasets or to filter data initially

library(lubridate)
library(ggplot2)
library(grid)
library(raster)
library(tictoc)
library(sf)
library(crayon)
library(scales)
library("ggspatial")#superposer les cartes et legende
library(plyr)
library(dplyr)


#####################################################################################
#                     GENERATE DENSITY MAPS FROM THE TRAJECTORIES                   #
#                         uses stat_density_2d (kernel density)                     #
#####################################################################################
# ARGUMENTS:                                                                        #
# DATA_PATH: (chr) path to the data directory                                       #
# year: (vector) the years we're interested in                                      #
# fob_type: (chr) "NLOG" or "FAD", or "ALOG"                                        #
# grid: (logical) if T, the plot titles are ready for a figure with several panels  #
#                                                                                   #
# OUTPUT: list containing 4 ggplots (one for each semester)                         #
#####################################################################################


plot.traj.density <- function(DATA_PATH, year, fob_type="NLOG",grid = F){
  
  # get the reconstructed data
  data = get.reg.GPS.data(DATA_PATH,year)
  
  data_trim <- list()
  
  trim = list(c(12,1,2),c(3,4,5),c(6,7,8),c(9,10,11))
  
  # take 4 datasets (one per trimester)
  for(i in 1:4){
    data_trim[[i]]=data[data$fob_type==fob_type & month(data$time) %in% trim[[i]],]
  }
  
  xlim = c(30, 105)
  ylim = c(-30, 25)
  
  p=list()
  # generate the title
  for (i in 1:4){
    if(grid==F){
      titre = paste("Density of trajectories of",fob_type,
                    "\n years :",year[1],"to",year[length(year)],
                    "\n months :",paste(trim[[i]],collapse = ";"))
    } else {
      titre = paste(fob_type)
    }
    
    #generate the ggplots
    p[[i]]=ggplot(data = data_trim[[i]], aes(x=longitude,y=latitude))+
      stat_density_2d(aes(fill=..level..), geom="polygon")+
      borders(fill="grey75", colour="grey75")+
      coord_map(xlim=xlim, ylim=ylim)+
      ggtitle(titre)+
      xlab("Longitude")+
      ylab("Latitude")+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_fill_gradientn(colours = c("forestgreen","yellow","red"))
  }
  
  
  return(p)
}


#####################################################################################
#                     GENERATE OCCURENCE MAPS FROM THE TRAJECTORIES                 #
#                         counts the number of emissions from                       #
#                           the reconstructed trajectories                          #
#####################################################################################
# GENERAL ARGUMENTS:                                                                #
# DATA_PATH: (chr) path to the data directory                                       #
# year: (vector) the years we're interested in                                      #
# fob_type: (chr) "NLOG" or "FAD", or "ALOG" (canNOT plot for all objects)          #
# gsize: (numeric) 1, 2 or 5; size in degree of the cells                           #
# trimestre: (logical) if T, returns a list of 4 ggplots, one for each trimester    #
#                      if F, returns one ggplot of the whole year                   #
# return_raster (logical) if T, does not return the plot but returns the raster     #
# coeff_var (log) : if F, returns the map (or raster) of the mean value of FOB per  #
#                    cell                                                           #
#                   if T, return the map (or raster) of the variation coefficient   #
#                     (standard deviation / mean) of FOB per cell                   #
#                                                                                   #
# ARGUMENTS ABOUT THE PLOT:                                                         #
# fixed_scale_max (num) default 0, the fill scale is not fixed. If different from 0 #
#                       specify the maximum value of the desired scale              #
# grid: (logical) if T, the plot titles are ready for a figure with several panels  #
# log_color_scale:   if T, colour of the filling scale are evenly spaced            #
#                    if F, they bring up contrast between smaller values            #
#                                                                                   #
# ARGUMENTS ABOUT THE SIMULATION:                                                   #
# simulation: (logical) if T, takes data from ichthyop simulations using            #
#                             get.sim.data()                                        #
# other arguments : simulation parameters, cf. read.ncs() in 8.Post-ichthyop dir    #
#                                                                                   #
# FILTERS AND WEIGHTS ON THE SIM RESULTS:                                           #
# weight_method (num) : method used to add a weight to particles                    #
#                             1. equal weight to every particles                    #
#                             2. weight depending on mean yearly river discharge    #
# thr_disch (chr or num): threshold of river discharge.                             #
#                          keep only the rivers with discharge >= to thr_disch      #
#                          (value in m3/s)                                          #
#                                                                                   #
# per_year (log) : if T, build the raster year per year and then sum (to avoid      # 
#                  filling up the computer RAM when working on simulations)         #
#                  if F, builds only one raster (allows to take years with few data #
#                  available into account, when working on data)                    #
#                                                                                   #
#                                                                                   #
# OUTPUT: ggplots (one or one for each trimester)                                   #
#####################################################################################

plot.traj.occurence <-
  function(DATA_PATH,
           year = 2011:2018,
           fob_type = "NLOG",
           gsize = 2,
           trimestre = F,
           return_raster = F,
           coeff_var = F,
           # arguments about the plot
           fixed_scale_max = 0,
           grid = F,
           log_color_scale = F,
           color_scale_pos = c("in_panel","null","out_panel"),
           # arguments about simulations
           simulation = F,
           forcing = c("oscar","globcurrent","nemo15m","nemo"),
           ltime = 180,
           ltime_method = 2,
           timestep = 6,
           input_location = c("river","mangrove"),
           input_method = c("onMask","kFromCoast"),
           dist = 100,
           dispersion = 9,
           bouncing = F,
           # arguments for filters and weights on the simulations results
           weight_method = 1,
           thr_disch = 0,
           # per year or not
           per_year = simulation) {
    
    
    
    tic.clear()
    cat("### STARTING PLOT TRAJ OCC FUNCTION ###\n###\n")
    tic(msg = "### END PLOT TRAJ OCC FUNCTION ###", quiet = FALSE)
    
    #### GET timestamp and simulation name
    
    ## timestamp: time between two observations
    # not the same depending if the data is from simulation or from buoys
    if (simulation == T) {
      timestamp = 12
      # if it's a simulation, timestamp = 12
      # and we generate the name of the simulation results
      
      sim_name <- generate.sim_name(forcing,
                                    input_location,
                                    input_method,
                                    dist,
                                    timestep,
                                    dispersion,
                                    bouncing,
                                    ltime,
                                    ltime_method)
      
    } else {
      sim_name <- ""
      # if it's buoys data, timestamp is unregular but trajectories have been reconstructed with a 6 hours timestamp
      timestamp = 6
    }
    
    
    # PER YEAR ----
    if (per_year == T) {
      
      ## CANNOT CALCULATE VARIATION COEFFICIENT PER YEAR
      # (could but cannot get one global value from yearly values afterward)
      if (coeff_var == T){
        cat("Error: the global variation coefficient cannot be calculated on data subsets of each year\n")
        return("Error: the global variation coefficient cannot be calculated on data subsets of each year")
      }
      
      df_year <- as.data.frame(year)
      
      ### APPLY THE SUBFUNCTION READING AND COUNTING GPS POSITIONS
      ### ON EACH YEAR INDEPENDENTLY
      rasts <- apply(df_year, 1, read.and.count.traj,
                     # other arguments given to the subfunction
                     simulation = simulation, DATA_PATH = DATA_PATH,
                     trimestre = trimestre, sim_name = sim_name,
                     fob_type = fob_type, gsize = gsize, coeff_var = F,
                     timestamp = timestamp, weight_method = weight_method,
                     thr_disch = thr_disch)
      
      ### AND SUM THE RESULTS
      if (trimestre==F){
        r <- rasts[[1]]
        
        if(length(rasts)>1){
          for (i in 2:length(rasts)) {
            r[] <- r[] + rasts[[i]][]
          }
        }
      } else if (trimestre == T) {
        r <- list()
        for (k in 1:length(rasts[[1]])) {
          r[[k]] <- rasts[[1]][[k]]
          
          if (length(rasts) > 1) {
            for (i in 2:length(rasts)) {
              r[[k]][] <- r[[k]][] + rasts[[i]][[k]][]
            }
          }
        }
      }
      
      rm(rasts)
      gc()
      
      names(r) <- "occ"
      
    } else if (per_year == F) {
      
      ### APPLY THE SUBFUNCTION READING AND COUNTING GPS POSITIONS
      r <- read.and.count.traj(year,
                               simulation, DATA_PATH, sim_name, trimestre,
                               fob_type, gsize, coeff_var, timestamp)
      
      names(r) <- "occ"
      
    }
    
    if (return_raster == T) {
      return(r)
    }
    
    
    
    
    # BUILD THE CORRESPONDING PLOTS ====
    tic(msg = "## BUILDING PLOT(S)", quiet = FALSE)
    
    world <- map_data("world")
    
    # TITLE
    titre <- plot.title(coeff_var, simulation, year, ltime, fob_type,
                        grid, trimestre)
    
    # i. Only one plot for the whole year ----
    if (trimestre == F){
      
      # df from the raster
      df <- as.data.frame(r, xy = TRUE)
      
      ### PLOT
      p <- build.ggplot(df, titre$title, titre$col_title, log_color_scale, fixed_scale_max, color_scale_pos)
      
      
      # ii. One plot per trimester ----
    } else if (trimestre == TRUE) {
      p <- list()
      
      for (i in 1:4) {
        
        # df from the raster
        df <- as.data.frame(r[[i]], xy = TRUE)
        
        ### PLOT
        p[[i]] <- build.ggplot(df, titre$title[i], titre$col_title, log_color_scale, fixed_scale_max, color_scale_pos)
        
      }
    }
    
    toc()
    
    toc()
    return(p)
  }


#####################################################################################
#               GENERATE A MAP OF THE LOCALIZATION OF BUOYS DESACTIVATION           #
#####################################################################################
# ARGUMENTS:                                                                        #
# DATA_PATH: (chr) path to the data directory                                       #
# year: (vector) the years we're interested in                                      #
# gsize: (numeric) 1, 2 or 5; size in degree of the cells                           #
# month (numeric) : the months we're interested in, if c() every months are pooled  #
# fixed_scale_max (num) default 0, the fill scale is not fixed. If different from 0 #
#                       specify the maximum value of the desired scale              #
# log_color_scale:   if T, colour of the filling scale are evenly spaced            #
#                    if F, they bring up contrast between smaller values            #
#                                                                                   #
# OUTPUT: ggplots                                                                   #
#####################################################################################



plot.desactivation.buoy <- function(DATA_PATH, year = 2010:2018, gsize = 2, month = c(),
                                    log_color_scale = T, fixed_scale_max = 0){
  
  # READING DATA
  cat("### READING DATA")
  tic(msg = "## READING DATA", quiet = FALSE)
  data <- get.init.GPS.data(DATA_PATH, year)
  toc()
  
  # KEEPING ONLY THE MONTH OF INTEREST
  if(!is.null(month)){
    data %>% dplyr::filter(month(position_date) %in% month) -> data
  }
  
  cat("### KEEPING ONLY THE LAST EMISSION BEFORE DESACTIVATION")
  tic(msg = "## KEEPING ONLY THE LAST EMISSION BEFORE DESACTIVATION", quiet = FALSE)
  # KEEPING ONLY THE LAST EMISSION BEFORE DESACTIVATION
  # create an id with buoy id and trajId
  data %>% mutate(buoy_traj_id = paste0(buoy_id,"_",trajId)) %>%
    # keep only emissions on the desactivation day
    dplyr::group_by(buoy_traj_id) %>% dplyr::filter(as.Date(position_date) == as.Date(desactivation_date)) %>%
    # keep only the last emission of the ones on the last day
    filter(position_date == max(position_date)) %>% dplyr::ungroup() -> data
  toc()
  
  nb <- dim(data)[1]
  
  # CREATE THE RASTER
  r <- create.raster(gsize = gsize)
  
  # COUNT THE NUMBER OF DESACTIVATION PER CELL
  tic(msg = "## COUNTING EMISSION PER CELL", quiet = FALSE)
  r <- total.nb.per.cell(data, r)
  toc()
  
  
  # BUILD THE GGPLOT
  tic(msg = "## BUILDING PLOT", quiet = FALSE)
  
  world <- map_data("world")
  df <- as.data.frame(r, xy = TRUE)
  
  p <- build.ggplot(df,
                    titre = "Number of buoys\ndesactivations",
                    col_title = "Number of\ndesactivation",
                    log_color_scale = log_color_scale,
                    fixed_scale_max = fixed_scale_max)
  
  p <- p + ggtitle(label = "Number of buoys\ndesactivations",
                   subtitle = paste("n buoys :", nb))
  
  toc()
  
  return(p)
}
