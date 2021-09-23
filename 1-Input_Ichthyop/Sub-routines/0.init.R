#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-19
#'@email : amael.dupaix@ens-lyon.fr
#'#*******************************************************************************************************************
#'@description :  Script initializing the study generating input points for Ichthyop simulations 
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

cat("\14")
cat(crayon::bold("###### Generating input points for Ichthyop simulations\n\n"))

cat(crayon::bold("0. Initialization"))

#' Source subfunctions
source(file.path(FUNC_PATH,'1.1.subfunctions.pre.R'))
source(file.path(FUNC_PATH,'1.2.subfunctions.general.R'))
source(file.path(FUNC_PATH,'1.3.subfunctions.post.R'))
source(file.path(FUNC_PATH,'2.1.write_xml.R'))

# Get the paths to the templates
Template <- c()
Template$xml <- file.path(RESOURCE_PATH, paste0("template_cfg_",curr_prod,".xml"))
Template$pbs <- file.path(RESOURCE_PATH, "template_ichthyop_job.pbs")
Template$pbs_post <- file.path(RESOURCE_PATH, "template_post_ichthyop_job.pbs")

if(!all(unlist(lapply(Template, file.exists)))){
  stop("Error: some templates are missing")
}

## Create output paths
# subroutine 1
# create the name on the dir where files will be saved, depending on method and input location
dir_name <- paste0(input_location, "_nlog_input_")
if (input_method == "kFromCoast"){
  dir_name <- paste0(dir_name, dist*100,"k_from_coast")
} else if (input_method == "onMask"){
  dir_name <- paste0(dir_name, curr_prod,"_mask")
} else if (input_method == "allMask"){
  dir_name <- paste0(dir_name, curr_prod, "_allMask")
}

output_path1 <- file.path(OUTPUT_PATH, dir_name)
dir.create(path = output_path1, showWarnings = F)

#Create the directory to save the cfg files (output path of subroutine 2)
cfg_dir <- paste0("cfgs_",first_release_year,"-",last_release_year)
output_path2 <- file.path(output_path1, cfg_dir)
dir.create(path = output_path2, showWarnings = F)



### Output names
# create the names of the outputs (of subroutine 1), to check if they exist
Names <- c()
Names$output_1 <- c("IDs.txt", "input_icht.txt", paste0(dir_name,c(".shp",
                                                                   ".dbf",
                                                                   ".prj",
                                                                   ".shx")))
if (input_method == "allMask"){ Names$output_1 <- c(Names$output_1, "Link_table.txt")}

#' outputs of subroutine 2
#' @!! only check the existence of the pbs jobs (not the xml cfg files)
Names$output_2.1 <- paste0("list_commands",1:n_pbs_jobs,".txt")
Names$output_2.2 <- paste0("sim_ichthyop-",last_release_year,"-",1:n_pbs_jobs,".pbs")
Names$output_2.3 <- c(paste0("post-simu-ichthyop-",last_release_year,".pbs"),
                      paste0("commands_post_simu-",last_release_year,".txt"))



# delete outputs if RESET == T
if (RESET[1] == T){
  try(unlink(file.path(output_path1, Names$output_1)), silent = T)
}
if (RESET[2] == T){
  try(unlink(list.files(output_path2, full.names = T), recursive = T))
}



###Check in outputs exist
Exist <- c()
Exist$output_1 <- file.exists(file.path(output_path1, Names$output_1))
Exist$output_2 <- file.exists(c(file.path(output_path2, Names$output_2.1),
                                file.path(output_path2, Names$output_2.2),
                                file.path(output_path2, Names$output_2.3)))


#' add the year to the sim_output_path (path where the Ichthyop output will be save on the cluster)
sim_output_path <- file.path(sim_output_path, curr_prod, last_release_year)
#' add the forcing product name to the sim_input_path
sim_input_path <- file.path(sim_input_path, curr_prod)

#list objects to keep at the end
toKeep <- c(ls(), "toKeep")

# checks
if (length(n_mpi) == 1){
  n_mpi <- rep(n_mpi, n_pbs_jobs)
} else if (length(n_mpi) != n_pbs_jobs){
  stop("Error: n_mpi has the wrong length")
}

if (any(n_mpi > 20 | n_mpi < 1)){
  stop("Error: wrong n_mpi numbers")
}