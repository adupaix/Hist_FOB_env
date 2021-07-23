#!/usr/bin/Rscript

# Determination des simulations qui restent a faire tourner

path = "/home1/scratch/adupaix/ichthyop-output"
sim_input_path <- "/home1/datawork/adupaix/input-ichthyop"

# path <- '/home/adupaix/test'
nsim <- 48 #remplacer par 48

npoints = 12789
# npoints = 10*50

# points_01, points_02, etc.
dirs = list.dirs(path, recursive = F, full.names = F)

sim_faite <- c()

for (i in 1:length(dirs)){
  
  # 00001, 00002, etc.
  subdirs <- list.dirs(file.path(path, dirs[i]), recursive = F, full.names = F)
  
  for (j in 1:length(subdirs)){
    
    # number of files in points_01/00001, etc
    nfiles <- length(list.files(file.path(path, dirs[i], subdirs[j])))
    
    if (nfiles == nsim){
      # s'il y a assez de fichiers dans le dossier, on l'enregistre comme etant fait
      sim_faite <- c(sim_faite, subdirs[j])
    } else {
      # sinon on supprime le sous dossier et ce qu'il contient
      unlink(file.path(path, dirs[i], subdirs[j]), recursive = T)
    }
        
    
  }
  
}

sim_points <- formatC(1:npoints, digits = 4, flag = "0")
sim_a_faire <- sim_points[!sim_points %in% sim_faite]

write.table(as.matrix(sim_a_faire), file.path(sim_input_path, "sim_a_faire.txt"))

lignes <- readLines("/home1/datawork/adupaix/input-ichthyop/cfgs/list_commands.txt")

n=1000
iter <- round(length(sim_a_faire)/n)
new_lignes <- c()
for (i in 1:iter){
  new_lignes <- c(new_lignes,
                  unique( grep(paste(sim_a_faire[(n*(i-1)+1):(n*i)],collapse="|"), lignes, value = T) ))
}
new_lignes <- c(new_lignes,
                unique( grep(paste(sim_a_faire[(n*(iter)+1):length(sim_a_faire)],collapse="|"), lignes, value = T) ))


file.create("/home1/datahome/adupaix/list_commands2.txt")
cmds <- file("/home1/datahome/adupaix/list_commands2.txt", open = "w")
writeLines(new_lignes, cmds)
close(cmds)

