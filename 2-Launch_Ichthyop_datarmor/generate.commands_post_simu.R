#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-03
#'@email : 
#'#*******************************************************************************************************************
#'@description :  Generate commands_post_simu.txt file(s)
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

tplate_line <- "Rscript ichth_to_rds.R points_"

n_cfg_per_dir = 28*8*5 # number of .xml file per directory
n_cfg_total = 12789

n_pbs_jobs = 1 #number of .pbs jobs to run in the cluster (to generate the command files)

dirs <- formatC(rep(1:12, each = n_cfg_per_dir), flag = "0", digits = 1)
sub_dirs <- formatC(1:n_cfg_total, flag = "0", digits = 4)

lignes <- paste0(tplate_line, dirs, "/", sub_dirs)[1:n_cfg_total]

file.create(file.path(getwd(), "2-Launch_Ichthyop_datarmor", "commands_post_simu.txt"))
f <- file(file.path(getwd(), "2-Launch_Ichthyop_datarmor", "commands_post_simu.txt"), open = "w")
writeLines(lignes, f)
close(f)
