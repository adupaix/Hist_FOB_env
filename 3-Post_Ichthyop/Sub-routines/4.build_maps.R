#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-09
#'@email : 
#'#*******************************************************************************************************************
#'@description :  Build maps from weighted arrays
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************
#'@todo: 
#'#*******************************************************************************************************************

msg <- bold("\n\n4. Building maps\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
msg <- paste0("Time scale of aggregation: ", agg.time_scale, "\n\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)

if (!log4Exists){
  
  msg <- "    - Reading weighted arrays\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  # cl <- makeCluster(nb_cores)
  # registerDoSNOW(cl)
  pb <- txtProgressBar(max = length(weighted_arrays), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  glob_array <- foreach(i = 1:length(weighted_arrays),
                        .packages = srcUsedPackages,
                        .options.snow = opts,
                        .combine = function(x,y) abind::abind(x,y, along = 3)) %do% {
                          
                          array.i <- readRDS(file.path(output_path_3, weighted_arrays[i]))
                        }
  
  close(pb)
  # stopCluster(cl)
  # registerDoSEQ()
  
  # arrange the array by time
  glob_array <- glob_array[,,order(dimnames(glob_array)[[3]])]
  
  nm <- dimnames(glob_array)[[3]]
  
  # aggregate the array by timestamp, to have one data.frame per timestamp (instead of per timestamp per simulation)
  glob_array <- foreach(day.i = unique(nm),
                        .combine = function(x,y) abind::abind(x,y, along = 3)) %do% {
                          
                          array.i <- glob_array[,,which(dimnames(glob_array)[[3]] == day.i)]
                          apply(array.i, c(1,2), sum)
                          
                        }
  nm <- unique(nm)
  dimnames(glob_array)[[3]] <- nm
  
  msg <- "    - Generating the mean aggregated array\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  if (agg.time_scale == "month"){
    new_nm <- paste0(year(nm),
                     "-",
                     ifelse(nchar(month(nm)) == 2, month(nm), paste0("0",month(nm)))
    )
    
  } else if (agg.time_scale == "quarter"){
    new_nm <- sub("\\.", "\\.Q", quarter(nm, fiscal_start = 11, with_year = T))
    
  } else if (agg.time_scale == "year"){
    new_nm <- year(nm)
  }
  
  dimnames(glob_array)[[3]] <- new_nm
  nm_time_scale <- sort(unique(new_nm))
  
  # cl <- makeCluster(nb_cores)
  # registerDoSNOW(cl)
  pb <- txtProgressBar(max = length(weighted_arrays), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  mean_agg_array <- foreach(time_scale.i = nm_time_scale,
                            .combine = function(x,y) abind::abind(x,y, along = 3)) %dopar% {
                              
                              array.i <- glob_array[,,which(dimnames(glob_array)[[3]] == time_scale.i)]
                              apply(array.i, c(1,2), get("mean"))
                              
                            }
  
  close(pb)
  # stopCluster(cl)
  # registerDoSEQ()
  
  if (length(dim(mean_agg_array))==3){
    dimnames(mean_agg_array)[[3]] <- nm_time_scale
  }
  
  
  #' selecting only the year of interest
  
  msg <- paste0("    - Selecting the year of interest : ",year,"\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  is_of_year_of_interest <- grepl(year, dimnames(mean_agg_array)[[3]])
  mean_agg_array <- mean_agg_array[,,is_of_year_of_interest]
  
  msg <- "    - Building maps\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  mean.ggplot.list <- list()
  
  time_scale <- 1:dim(mean_agg_array)[3]
  
  if (common_scale_max == T){
    fixed_scale_max <- ceiling(max(mean_agg_array))
  } else{
    fixed_scale_max <- 0
  }
  
  # for each time_scale, we build a map and save it in the list
  for (i in time_scale){
    
    array.i <- mean_agg_array[,,i]
    dimname.i <- dimnames(mean_agg_array)[[3]][i]
    
    mean.ggplot.list[[i]] <- matrix.to.ggplot(array.i, dimname.i, gsize,
                                              log_color_scale, fixed_scale_max,
                                              color_scale_pos)
  }
  
  msg <- "    - Saving maps\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  saveRDS(mean.ggplot.list, plotListName)
  cat("\n\n List of maps saved in:", plotListName, "\n")
  
  # save an image of the mean maps
  l = length(mean.ggplot.list)
  n_col <- ceiling(sqrt(l))
  n_row <- ceiling(l / n_col)
  p <- ggarrange(plotlist = mean.ggplot.list,
                 ncol = n_col, nrow = n_row,
                 align = "hv", labels = "AUTO",
                 common.legend = common_scale_max,
                 legend = "right")
  
  ggsave(pngMapsName, p, width = ifelse(common_scale_max, 180*n_col + 20, 180*n_col),
         height = 120*n_row, units = "mm")
  
  #' save a log
  sink(logName4, append = F)
  cat("Execution time :", format(Sys.time()))
  sink()
  
  
} else {
  
  msg <- "Maps already exist\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
}



