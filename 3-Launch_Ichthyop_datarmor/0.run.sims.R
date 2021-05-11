#!/usr/bin/env Rscript

points <- read.table("/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/test_IDs.txt",
                     header = F, sep = " ")

template <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/template_cfg.xml"
sim_input_path <- "/home/adupaix/Documents/ichthyop-private/input"
sim_output_path <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/ichthyop-output"
rds_output_path <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/rds-output"

gsize <- 0.5

# get the current location
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
this.dir <- dirname(script.name)

for (i in 1){
  
  long <- points[i,1]
  lat <- points[i,2]
  cfg_nb <- points[i,3]
  
  #~ Transform cfg_nb into "000xx" format
  t <- paste(rep(0, 5), collapse = "")
  cfg_nb <- as.character(cfg_nb)
  nb <- paste0(substr(t, 1, nchar(t)-nchar(cfg_nb)), cfg_nb)
  
  #~ Create the xml cfg for ichthyop
  source(file.path(this.dir, "1.create_cfg_xml.R"))
  
  # ~ Run ichthyop
  torun <- paste0('java -jar /home/adupaix/Documents/ichthyop-private/target/ichthyop-3.3.8.jar cfg_point_', nb, '.xml')
  torun <- paste0(torun, ' >> log_sim_',nb,'.txt')
  
  system(torun)

  #~ Transform ichthyop outputs into arrays
  source(file.path(this.dir,"2.read.ncs.R"))
  
}