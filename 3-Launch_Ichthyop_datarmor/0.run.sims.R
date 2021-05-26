#!/usr/bin/env Rscript

library(lubridate)

points <- read.table("/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/1-Input_Ichthyop/Outputs/river_nlog_input_PHILIN12.L75_allMask/IDs.txt",
                     header = T, sep = " ")

template <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/template_cfg.xml"
sim_input_path <- "/home/adupaix/Documents/ichthyop-private/input"
sim_output_path <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/ichthyop-output"
rds_output_path <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/rds-output"

# size of the matrix cells to save ichthyop simulations
gsize <- 0.5

#~ Fixed arguments
transport_duration = 180 #in days

first_release_date = "1980/01/02"
last_release_date = "1981/12/02"
release_frequency = 14 # in days
record_frequency = 5 #in days (interval between two recorded positions)

# get the current location
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
this.dir <- dirname(script.name)
cfg_dir <- file.path(this.dir, "cfgs")
dir.create(cfg_dir)

n_cfg_per_dir = 560

n_dirs <- floor(dim(points)[1]/n_cfg_per_dir)

n_iter <- dim(points)[1]
pb <- txtProgressBar(min = 0, 
                     max = n_iter,
                     style = 3)

for (i in 1:n_dirs){
  
  t <- paste(rep(0, 2), collapse = "")
  i_chr <- as.character(i)
  i_chr <- paste0(substr(t, 1, nchar(t)-nchar(i_chr)), i_chr)
  
  dir_name.i <- file.path(cfg_dir, paste0("cfgs_", i_chr))
  
  dir.create(dir_name.i)
  
  for (j in 1:n_cfg_per_dir){
    long <- points[n_cfg_per_dir*(i-1)+j,1]
    lat <- points[n_cfg_per_dir*(i-1)+j,2]
    cfg_nb <- points[n_cfg_per_dir*(i-1)+j,3]
    
    #~ Transform cfg_nb into "000xx" format
    t <- paste(rep(0, 5), collapse = "")
    cfg_nb <- as.character(cfg_nb)
    nb <- paste0(substr(t, 1, nchar(t)-nchar(cfg_nb)), cfg_nb)
    
    # #~ Create the xml cfg for ichthyop
    source(file.path(this.dir, "1.create_cfg_xml.R"))
    
    setTxtProgressBar(pb, n_cfg_per_dir*(i-1)+j)
  }
  
  
  
  # 
  # # ~ Run ichthyop
  # torun <- paste0('java -jar /home/adupaix/Documents/ichthyop-private/target/ichthyop-3.3.8.jar cfg_point_', nb, '.xml')
  # # torun <- paste0(torun, ' >> log_sim_',nb,'.txt')
  # 
  # system(torun)

  #~ Transform ichthyop outputs into arrays
  # source(file.path(this.dir,"2.read.ncs.R"))
  
}


i=n_dirs+1
t <- paste(rep(0, 2), collapse = "")
i_chr <- as.character(i)
i_chr <- paste0(substr(t, 1, nchar(t)-nchar(i_chr)), i_chr)

dir_name.i <- file.path(cfg_dir, paste0("cfgs_", i_chr))

dir.create(dir_name.i)

for (j in 1:(n_iter - n_dirs*n_cfg_per_dir)){
  long <- points[n_cfg_per_dir*(i-1)+j,1]
  lat <- points[n_cfg_per_dir*(i-1)+j,2]
  cfg_nb <- points[n_cfg_per_dir*(i-1)+j,3]
  
  #~ Transform cfg_nb into "000xx" format
  t <- paste(rep(0, 5), collapse = "")
  cfg_nb <- as.character(cfg_nb)
  nb <- paste0(substr(t, 1, nchar(t)-nchar(cfg_nb)), cfg_nb)
  
  # #~ Create the xml cfg for ichthyop
  source(file.path(this.dir, "1.create_cfg_xml.R"))
  
  setTxtProgressBar(pb, n_cfg_per_dir*(i-1)+j)
  
}

close(pb)
