
MAP_PATH <- file.path(getwd(), "1.Maps", "Functions")
DISP_PATH <- file.path(getwd(), "4.Dispersion", "Functions")
DATA_PATH <- file.path(getwd(), "0.Data")

source(file.path(MAP_PATH, "Maps.R"))
source(file.path(DISP_PATH, "0.plot_traj.R"))


#======================================================================================#
#                    CARTE DES RATIOS NLOG obs/ NLOG sim                               #
#======================================================================================#
#  ARGUMENTS:                                                                          #
# Ob7 (data.frame) : donnees observateur brut                                          #
# DATA_PATH (chr) : chemin vers le dossier contenant les donnees                       #
# year (num) : annees dont on veut la carte. c() pour un pool des annees               #
# gsize (num) : choix entre 1, 2 et 5, pour des cellules de 1dx1d, 2dx2d, etc.         #
# fixed_scale_max (num) default 0, the fill scale is not fixed. If different from 0    #
#                       specify the maximum value of the desired scale                 #
# grid: (logical) if T, the plot titles are ready for a figure with several panels     #
# log_color_scale:   if T, colour of the filling scale are evenly spaced               #
#                    if F, they bring up contrast between smaller values               #
#                                                                                      #
# simulation: (logical) if T, takes data from ichthyop simulations using               #
#                             get.sim.data()                                           #
# other arguments : simulation parameters, cf. read.ncs() in 8.Post-ichthyop dir       #
#                                                                                      #
# return: ggplot                                                                       #
#--------------------------------------------------------------------------------------#


plot.ratio.obs.sim <-
  function(Ob7, DATA_PATH,
           year = 2011:2015,
           # fob_type = "NLOG",
           gsize = 2,
           data_preped = F,
           # trimestre = F,
           # return_raster = F,
           # coeff_var = F,
           # arguments about the plot
           fixed_scale_max = 0,
           log_color_scale = T,
           color_scale_pos = "in_panel",
           # arguments about simulations
           # simulation = F,
           forcing = c("oscar","globcurrent","nemo","nemo15m"),
           ltime = 180,
           ltime_method = 2,
           timestep = 6,
           input_location = c("river","mangrove"),
           input_method = c("onMask","kFromCoast"),
           dist = 100,
           dispersion = 9,
           bouncing = F,
           weight_method = 1,
           thr_disch = 10){
    
    r_obs <- map_occurence(Ob7, DATA_PATH, year, obj.type = "FAD",
                           gsize = gsize, data_preped = data_preped,
                           return_raster = T)
    
    r_sim <- plot.traj.occurence(DATA_PATH, year, fob_type = "NLOG",
                                 gsize = gsize, trimestre = F,
                                 return_raster = T, simulation = T,
                                 forcing = forcing, ltime = ltime,
                                 ltime_method = ltime_method, timestep = timestep,
                                 input_location = input_location,
                                 input_method = input_method, dist = dist,
                                 dispersion = dispersion, bouncing = bouncing,
                                 weight_method = weight_method, thr_disch = thr_disch)
    
    r <- create.raster(gsize)
    
    r[] <- r_obs[] / r_sim[]
    
    # BUILD THE CORRESPONDING PLOTS ====
    world <- map_data("world")
    
    # TITLE
    titre <- list( title = paste(forcing),
                   col_title = "NLOG / \nSimulated NLOG")
    
    # df from the raster
    df <- as.data.frame(r, xy = TRUE)
      
    ### PLOT
    p <- build.ggplot(df, titre$title, titre$col_title, log_color_scale, fixed_scale_max, color_scale_pos)
    
    
    
    return(p)
    
    
  }



