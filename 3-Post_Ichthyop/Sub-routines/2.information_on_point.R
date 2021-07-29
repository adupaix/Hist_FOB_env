#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-21
#'@email : 
#'#*******************************************************************************************************************
#'@description :  Reads the rds files from the simulation from one point and weight it according to the chosen
#'  method
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


#'@arguments
#'**********

sim_output_path <- file.path(DATA_PATH, "Output_Ichthyop", sim_name)

point <- list()

point$id <- "00101"

year <- 2000

n_points_per_dir = 28*8*5

#'

num_sub_dir <- floor((as.numeric(point$id)-1)/ n_points_per_dir) + 1
num_sub_dir <- paste0(substr("00", 1, nchar("00")-nchar(num_sub_dir)),
                      num_sub_dir)

fnames <- list.files(file.path(sim_output_path,
                               paste0("points_", num_sub_dir),
                               point$id),
                     pattern = "rds")

link_table <- read.link_cover_input(OUTPUT_PATH, sim_name)

n_cover_per_river <- read.table(file.path(OUTPUT_PATH, sim_name, "n_cover_per_river.txt"),
                                header = T)

# read rds file
i=1
data <- readRDS(file.path(sim_output_path, paste0("points_", num_sub_dir), point$id, fnames[i]))

sim_input_path <- file.path(DATA_PATH, "Input_Ichthyop", paste0(input_location, "_nlog_input_", forcing, "_", input_method))


#'@get_information_on_release_point
#'***********************************


# get release coordinates
point <- get.coords.release(sim_input_path,
                            point)

# get release date

point$release_date <- as.POSIXct(dimnames(data)[[3]][1])

# get precipitations

point <- get.precipitations(DATA_PATH, point)

# get rivers and associated discharge + cover

point <- get.associated.rivers(sim_input_path, n_cover_per_river, filtered, point)


# get forest cover

point <- get.number.of.cover.points(link_table, point)

if (point$nb_river_cover_points != sum(point$rivers$cover)){
  stop("Error")
}


#'@weight_density_maps
#'********************

add.weight <- function(weight_method, data, point){
  
  # method 1 : don't apply any weight
  if (weight_method == 1){
    weight <- 1
    
    # method 2 : apply a weight depending on the water discharge of the river
  }  else if (weight_method==2){
    weight <- sum(point$rivers$dis_m3_pyr)
    
    # method 3: apply a weight depending on the surface of cover associated with the input point
  } else if (weight_method == 3){
    weight <- point$nb_cover_points
    
    # method 4: apply a weight depending on the area covered by forest in the river basins
  } else if (weight_method == 4){
    weight <- point$nb_coastal_cover_points + sum(point$rivers$cover * point$rivers$dis_m3_pyr)
  }
  
  return(data*weight)
}
