
#############################################################
#############################################################
#       SUB-FUNCTIONS USED AFTER input.nlog()               #
#             TO PLOT SOME RESULTS                          #
#############################################################
#############################################################


#################################################################################
#           PLOT OF THE BEACHING EVENTS DURING THE ICHTHYOP SIMULATION          #
#################################################################################
# ARGUMENTS :                                                                   #
# output (nc) : ncdf of the ichthyop output opened with the open.nc() function  #
# dist (num or chr) : for the title of the plot, size of the buffer used to     #
#                       infer the input points (method=1) or distance to the    #
#                       coast (method=2)                                        #
#                                                                               #
# OUTPUT : ggplot                                                               #
#################################################################################

plot.beaching <- function(output, dist=10, method=1){
  mort <- data.frame(var.get.nc(output,"mortality"))
  
  duration <- att.get.nc(output,"NC_GLOBAL","app.time.transport_duration")
  duration <- as.numeric(strsplit(duration," ")[[1]][1])
  
  x<-data.frame()
  for (i in 1:dim(mort)[2]){
    x=data.frame(rbind(x,summary(factor(mort[,i],levels = c(-99,0,4)))))
  }
  
  names(x)<-c("not_sea","alive","beached")
  nb_particles <- apply(x,1,sum)[1]
  nb_at_sea <- x$alive[dim(x)[1]]
  
  x <- 100*x/apply(x,1,sum)
  
  save_inter = round(duration / dim(x)[1], digits=1)
  
  if (duration > 360) {
    x <- x[1:((360/save_inter)+1),]
  }
  
  x$days <- seq(0,360,save_inter)
  
  ts <- as.numeric(att.get.nc(output, variable = "NC_GLOBAL", attribute = "app.time.time_step"))/3600
  
  if (method==1){
    titre <- paste(dist,"km buffer\n",ts,"h time step")
  } else if (method==2){
    titre <- paste("Points at",dist,"km from the coast\n",ts,"h time step")
  } else{
    return("Error in the method chosen, choose between 1 and 2")
  }
  
  
  p=ggplot()+
    geom_line(data=x, aes(x = days, y = beached, colour="Beached"))+
    geom_line(data=x, aes(x = days, y = alive, colour="At sea"))+
    geom_line(data=x, aes(x = days, y = not_sea, colour="Not released"))+
    geom_hline(yintercept = 100, linetype=2)+
    geom_hline(yintercept = x$alive[dim(x)[1]], colour="red", linetype=2)+
    labs(title = titre, colour = "Particle state")+
    xlab("Days of simulation")+ ylab("% of particles")+
    theme(plot.title = element_text(hjust = 0.5, face="bold"))+
    annotate("text", x = 0, y = 100,
             hjust=0, vjust=-0.2,colour="blue",
             label = paste(nb_particles, "particles released"))+
    annotate("text", x = 0, y = x$alive[dim(x)[1]],
             hjust=0,vjust=-1, label = paste(nb_at_sea, "particles at sea"),
             colour="red")
  return(p)
}


#################################################################################
#                   PLOTS A MAP OF THE INPUT POINTS OF NLOG                     #
#                       WITH THE SPECIFIED BUFFER SIZE                          #
#################################################################################
# ARGUMENTS :                                                                   #
#  OUTPUT_PATH (chr) path to the output directory                               #
#  method (num) : from 1 to 4                                                   #
#  km (num or chr) : size of the buffer, for method 1; distance from coast, for #
#                    method 2 and 4                                             #
#  thr_disch (num) : for method 4 only. The threshold of discharge used to      #
#                    filter rivers                                              #
#                                                                               #
# OUTPUT : ggplot                                                               #
#################################################################################


plot.nlog.input <- function(OUTPUT_PATH, method = 2, km = 100, thr_disch = 20){
  world <- map_data("world")
  
  if (method == 1){
    filepath = file.path(OUTPUT_PATH, paste0("mangrove_nlog_input_",km,"k.txt"))
    titre <- paste(km, "km buffer from mangroves")
  }else if (method == 2){
    filepath = file.path(OUTPUT_PATH, paste0('mangrove_nlog_input_',km,"k_from_coast.txt"))
    titre <- paste(km, "km from coast from mangroves")
  }else if (method == 3){
    filepath = file.path(OUTPUT_PATH, "nlog_input_reverse_simulations.txt")
    titre <- "NLOG observations for reverse simulations"
  }else if (method == 4){
    filepath = file.path(OUTPUT_PATH, paste0('river_nlog_input_dist',km,"k_thr_disch_",thr_disch,"m3s-1.txt"))
    titre <- paste(km, "km from coast from rivers\n Discharge threshold:",thr_disch)
  }
  
  coords = read.table(file = filepath, header = F, sep=" ")
  names(coords)<-c("x","y")
  
  p = ggplot(data = coords, aes(x=x, y=y))+ geom_point(col="green")+
    coord_sf(xlim = c(20, 150), ylim = c(-40, 40), expand = FALSE)+
    geom_polygon(data=world, aes(x=long, y=lat, group=group))+
    xlab("Longitude")+ ylab("Latitude")+
    labs(title = titre)+
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

