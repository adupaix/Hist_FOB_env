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

point$id <- "00001"

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

# read rds file
data <- readRDS(file.path(sim_output_path, paste0("points_", num_sub_dir), point$id, fnames[1]))

sim_input_path <- file.path(DATA_PATH, "Input_Ichthyop", paste0(input_location, "_nlog_input_", forcing, "_", input_method))


# read cover file (to put above, once only for all the points)
get.cover.shp <- function(DATA_PATH, year){
  
  cover_path <- file.path(DATA_PATH, "forest_cover")
  cover_files <- list.files(cover_path)
  
  # get all the years available in the forest_cover dir
  y_availables <- as.numeric(gsub("\\D+", "", cover_files))
  
  # get the year which is the closest to the year of interest
  file_of_int <- which(abs(year - y_availables) == min(abs(year - y_availables)))
  
  path <- file.path(cover_path, cover_files[file_of_int])
  
  # read the shp
  cover <- read_sf(file.path(path, list.files(path, pattern = "shp")))
  
  cover %>% st_transform(crs = 4326) -> cover
  
  return(cover)
}


# cover <- get.cover.shp(DATA_PATH, year)


#'@get_information_on_release_point
#'***********************************


# get.coords.release(sim_name, point)

get.coords.release <- function(sim_input_path,
                               point){
  
  # get the coordinates of the point
  coords <- read.table(file.path(sim_input_path, "IDs.txt"))
  
  point$x <- coords[coords$V3 == as.numeric(point$id),1]
  point$y <- coords[coords$V3 == as.numeric(point$id),2]
  
  # get the coordinates of the points which are at less than 1/4° of the point
  dists <- sqrt((point$x-coords[,1])^2 + (point$y-coords[,2])^2)
  df <- coords[which(dists < 1/4 & coords$V3 != as.numeric(point$id)),]
  names(df) <- c("x","y","id")
  point$close_points <- df
  
  return(point)
}

point <- get.coords.release(sim_input_path,
                            point)

# get release date

point$release_date <- as.POSIXct(dimnames(data)[[3]][1])

# get precipitations (point_coords, release_date)

get.precipitations <- function(DATA_PATH,
                               point
                               ){
  
  x = point$x
  y = point$y
  
  ## open the netcdf file
  precip <- open.nc(con = file.path(DATA_PATH,"precip.mon.mean.nc"))
  
  # select the interval of interest
  time <- var.get.nc(precip, "time")
  init_nc <- as.Date("1800-01-01")
  time <- as.difftime(time, units = "days") + init_nc
  time_of_int <- which(paste0(year(time),"-",month(time)) == paste0(year(point$release_date),"-",month(point$release_date)))
  
  
  lon <- var.get.nc(precip, "lon")
  lon_of_int <- which(abs(lon - x) == min(abs(lon - x)))
  
  lat <- var.get.nc(precip, "lat")
  lat_of_int <- which(abs(lat - y) == min(abs(lat - y)))
  
  of_int <- c(lon_of_int, lat_of_int, time_of_int)
  
  # get the precipitation value
  point$precip <- var.get.nc(precip, "precip", start = of_int, count = rep(1,3))
  
  # close the netcdf file
  close.nc(precip)
  
  return(point)
}

point <- get.precipitations(DATA_PATH, point)

# get associated rivers (point)

get.associated.rivers <- function(sim_input_path,
                                  point){
  
  rivers <- read.table(file.path(sim_input_path, "Link_table.txt"), header = T)
  
  point$rivers <- (rivers %>% filter(id_curr == as.numeric(point$id)))$MAIN_RIV
  
  return(point)
}

point <- get.associated.rivers(sim_input_path, point)


# get forest surface (release_date, point_coords, point_associated_rivers, forest_cover)


