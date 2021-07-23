#!/usr/bin/Rscript

#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-07-19
#'@email : 
#'#*******************************************************************************************************************
#'@description :  In case an error occurred in the datarmor job (typically required time exceeded)
#' Script to generate a new command_list.txt (named 'command_list2.txt')
#' containing only the commands of the simulations that were not performed
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

#'@arguments
#' path where the ichthyop outputs are saved:
path = "/home1/scratch/adupaix/ichthyop-output"

#' path where the cfgs files, the current products and the list of bash command lines are saved
sim_input_path <- "/home1/datawork/adupaix/input-ichthyop"

#' number of release dates in a typical simulation
#' (compared with the number of files in a sub directory)
nsim <- 48

#' total number of points in the simulation
#' (= total number of subdirectories)
npoints = 12789

#'@run
# get the dirs named points_01, points_02, etc.
dirs = list.dirs(path, recursive = F, full.names = F)

#' will allow to store the performed simulations
sim_faite <- c()

for (i in 1:length(dirs)){
  
  # get the dirs named 00001, 00002, etc.
  subdirs <- list.dirs(file.path(path, dirs[i]), recursive = F, full.names = F)
  
  for (j in 1:length(subdirs)){
    
    # number of files in points_01/00001, etc
    nfiles <- length(list.files(file.path(path, dirs[i], subdirs[j])))
    
    if (nfiles == nsim){
      # if there are enough files in the folder, we save it as performed
      sim_faite <- c(sim_faite, subdirs[j])
    } else {
      # else, we delete the files which are in the folder
      unlink(file.path(path, dirs[i], subdirs[j]), recursive = T)
    }
        
    
  }
  
}

#' get the number of the points which were not performed
sim_points <- formatC(1:npoints, digits = 4, flag = "0")
sim_a_faire <- sim_points[!sim_points %in% sim_faite]

#' save them in a txt file
write.table(as.matrix(sim_a_faire), file.path(sim_input_path, "sim_a_faire.txt"))

#' read the commands
lignes <- readLines(file.path(sim_input_path, "cfgs/list_commands.txt"))

#' for each "non-performed" point, get the corresponding bash command line
n=1000
iter <- round(length(sim_a_faire)/n)
new_lignes <- c()
for (i in 1:iter){
  new_lignes <- c(new_lignes,
                  unique( grep(paste(sim_a_faire[(n*(i-1)+1):(n*i)],collapse="|"), lignes, value = T) ))
}
new_lignes <- c(new_lignes,
                unique( grep(paste(sim_a_faire[(n*(iter)+1):length(sim_a_faire)],collapse="|"), lignes, value = T) ))

#' save the command lines list in 'list_commands2.txt'
file.create("/home1/datahome/adupaix/list_commands2.txt")
cmds <- file("/home1/datahome/adupaix/list_commands2.txt", open = "w")
writeLines(new_lignes, cmds)
close(cmds)

