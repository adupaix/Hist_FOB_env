
cat("\14")
cat(crayon::bold("###### Generating input points for Ichthyop simulations\n\n"))

cat(crayon::bold("0. Initialization"))

#' Source subfunctions
source(file.path(FUNC_PATH,'1.1.subfunctions.pre.R'))
source(file.path(FUNC_PATH,'1.2.subfunctions.general.R'))
source(file.path(FUNC_PATH,'1.3.subfunctions.post.R'))
source(file.path(FUNC_PATH,'2.1.write_xml.R'))

# Get the path to the templates
Template <- c()
Template$xml <- file.path(RESOURCE_PATH, paste0("template_cfg_",curr_prod,".xml"))
Template$pbs <- file.path(RESOURCE_PATH, "template_job.pbs")

if(!all(unlist(lapply(Template, file.exists)))){
  stop("Error: some templates are missing")
}

# create the name on the dir where files will be saved, depending on method and input location
dir_name <- paste0(input_location, "_nlog_input_")
if (input_method == "kFromCoast"){
  dir_name <- paste0(dir_name, dist*100,"k_from_coast")
} else if (input_method == "onMask"){
  dir_name <- paste0(dir_name, curr_prod,"_mask")
} else if (input_method == "allMask"){
  dir_name <- paste0(dir_name, curr_prod, "_allMask")
}

dir_path <- file.path(OUTPUT_PATH, dir_name)
try(dir.create(path = dir_path))

# create the names of the outputs (of subroutine 1), to check if they exist
files_to_check <- c("IDs.txt", "input_icht.txt", paste0(dir_name,".shp"))
if (input_method == "allMask"){ files_to_check <- c(files_to_check, "Link_table.txt")}

#Create the directory to save the cfg files (output path of subroutine 2)
cfg_dir <- paste0("cfgs_",first_release_year,"-",last_release_year)
cfg_path <- file.path(OUTPUT_PATH, dir_name, cfg_dir)
dir.create(file.path(dir_path, cfg_dir), showWarnings = F)

# delete outputs if reset == T
if (reset == T){
  try(unlink(file.path(dir_path, files_to_check)), silent = T)
}

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