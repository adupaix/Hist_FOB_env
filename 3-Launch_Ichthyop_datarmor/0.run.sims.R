#!/usr/bin/env Rscript

points <- read.table("/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/test_IDs.txt",
                     header = F, sep = " ")

template <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/template_cfg.xml"
sim_input_path <- "/home/adupaix/Documents/ichthyop-private/input"
sim_output_path <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/ichthyop-output"
rds_output_path <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Launch_Ichthyop_datarmor/rds-output"

gsize <- 0.5

for (i in 1){
  
  long <- points[i,1]
  lat <- points[i,2]
  cfg_nb <- points[i,3]
  
  #~ Transform cfg_nb into "000xx" format
  t <- paste(rep(0, 5), collapse = "")
  cfg_nb <- as.character(cfg_nb)
  nb <- paste0(substr(t, 1, nchar(t)-nchar(cfg_nb)), cfg_nb)
  
  #~ Create the xml cfg for ichthyop
  torun1 <- paste('Rscript 1.create_cfg_xml.R', template, long, lat, nb, sim_input_path, sim_output_path)

  system(torun1)


  # ~ Run ichthyop
  torun2 <- paste0('java -jar /home/adupaix/Documents/ichthyop-private/target/ichthyop-3.3.8.jar cfg_point_', nb, '.xml')

  system(torun2)

  #~ Transform ichthyop outputs into arrays
  torun3 <- paste('Rscript 2.read.ncs.R', nb, gsize, sim_output_path, rds_output_path)

  system(torun3)
  
}