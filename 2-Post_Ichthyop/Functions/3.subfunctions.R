#'@sub-function 1
#'***************
#' apply the mortality on the density map (modify array.k)

apply.mortality <- function(array.k, ltime, ltime_method, sd = 30){
  
  timestep <- as.POSIXct(dimnames(array.k)[[3]][2]) - as.POSIXct(dimnames(array.k)[[3]][1])
  
  total_time_sim <- dim(array.k)[3]
  
  if (ltime_method == 1){
    
    x <- seq(1, total_time_sim, 1)
    
    prop_alive <- round( 1 - cumsum(dnorm(x, ltime/as.numeric(timestep), sd/as.numeric(timestep))), digits = 4 )
    
  } else if (ltime_method == 2){
    
    x <- seq(1, total_time_sim, 1)
    
    prop_alive <- round( 1 - cumsum(dpois(x, ltime/as.numeric(timestep))), digits = 4 )
    
  } else if (ltime_method == 3){
    
    prop_alive <- c( rep(1, ltime/as.numeric(timestep)), rep(0, (total_time_sim-ltime)/as.numeric(timestep)))
    
  }
  
  # multiply the matrix of each timestep by the proportion of particles alive
  array.k <- sweep(array.k, 3, prop_alive, "*")
  
  # delete dates when no particles are left, to gain space
  has_particles_left <- apply(array.k, 3, sum) != 0
  array.k <- array.k[,,which(has_particles_left)]
  
  return(array.k)
}

#'@sub-function 2
#'***************
#' function to combine arrays in the foreach loop
#'   Sums the two 3D matrices term by term
#'   checking if their 3rd dimension has the same length
#'   If its not the case (eg. dim(x)[3] > dim(y)[3])
#'   complete the smallest matrix (in that case y) with zeros

f.for.combining <- function(x,y){
  
  if(!identical(dim(x),dim(y))){
    if (dim(x)[3] > dim(y)[3]){
      x2 <- x
      y2 <- abind::abind(y, array(0, dim = c(dim(y)[1:2],dim(x)[3]-dim(y)[3])))
    } else {
      x2 <- y
      y2 <- abind::abind(x, array(0, dim = c(dim(x)[1:2],dim(y)[3]-dim(x)[3])))
    }
  } else {
    x2 <- x
    y2 <- y
  }
  
  return(sweep(x2, 1:3, y2, "+"))
}
