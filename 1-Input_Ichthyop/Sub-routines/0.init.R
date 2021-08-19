
cat("\14")
cat(crayon::bold("###### Generating input points for Ichthyop simulations\n\n"))

cat("0. Initialization")

#' Source subfunctions
source(file.path(FUNC_PATH,'1.1.subfunctions.pre.R'))
source(file.path(FUNC_PATH,'1.2.subfunctions.general.R'))
source(file.path(FUNC_PATH,'1.3.subfunctions.post.R'))


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

# create the names of the outputs, to check if they exist
files_to_check <- c("IDs.txt", "input_icht.txt", paste0(dir_name,".shp"))
if (input_method == "allMask"){ files_to_check <- c(files_to_check, "Link_table.txt")}

# delete outputs if reset == T
if (reset == T){
  try(unlink(file.path(dir_path, files_to_check)), silent = T)
}