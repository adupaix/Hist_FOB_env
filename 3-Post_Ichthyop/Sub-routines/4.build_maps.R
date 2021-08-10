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
  
  if (!globArrayExists){
    msg <- "    - Reading weighted arrays and suming them into a global array\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    glob_array <- foreach(i = 1:length(weighted_arrays),
                          .packages = srcUsedPackages,
                          .combine = function(x,y) abind::abind(x,y, along = 3)) %do% {
                            
                            array.i <- readRDS(file.path(output_path_3, weighted_arrays[i]))
                          }
    
    
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
    
    saveRDS(glob_array, file = globArrayName)
    
  } else {
    
    msg <- "    - Reading global array from file\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    glob_array <- readRDS(globArrayName)
    
    nm <- dimnames(glob_array)[[3]]
    
  }
  
  
  if (!aggArrayExists){
    
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
    
    mean_agg_array <- foreach(time_scale.i = nm_time_scale,
                              .combine = function(x,y) abind::abind(x,y, along = 3)) %do% {
                                
                                array.i <- glob_array[,,which(dimnames(glob_array)[[3]] == time_scale.i)]
                                apply(array.i, c(1,2), get("mean"))
                                
                              }
    
    
    if (length(dim(mean_agg_array))==3){
      dimnames(mean_agg_array)[[3]] <- nm_time_scale
    }
    
    saveRDS(mean_agg_array, file = aggArrayName)
    
  } else {
    
    msg <- "    - Reading aggregated array from file\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
    
    mean_agg_array <- readRDS(aggArrayName)
    
  }
  
  #' selecting only the year of interest
  
  msg <- paste0("    - Selecting the year of interest : ",year,"\n") ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  is_of_year_of_interest <- grepl(year, dimnames(mean_agg_array)[[3]])
  nm <- dimnames(mean_agg_array)[[3]][is_of_year_of_interest]
  mean_agg_array <- mean_agg_array[,,is_of_year_of_interest]
  
  msg <- "    - Building maps\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  mean.ggplot.list <- list()
  
  time_scale <- ifelse(length(dim(mean_agg_array)) == 3, 1:dim(mean_agg_array)[3], 1)
  
  if (common_scale_max == T){
    fixed_scale_max <- ceiling(max(mean_agg_array))
  } else{
    fixed_scale_max <- 0
  }
  
  # for each time_scale, we build a map and save it in the list
  if (length(time_scale)>1){
    for (i in time_scale){
      
      array.i <- mean_agg_array[,,i]
      dimname.i <- nm[i]
      
      mean.ggplot.list[[i]] <- matrix.to.ggplot(array.i, dimname.i, gsize,
                                                log_color_scale, fixed_scale_max,
                                                color_scale_pos)
    }
  } else if (time_scale == 1){
    array.i <- mean_agg_array
    dimname.i <- nm
    
    mean.ggplot.list <- matrix.to.ggplot(array.i, dimname.i, gsize,
                                         log_color_scale, fixed_scale_max,
                                         color_scale_pos)
    
  }
  
  msg <- "    - Saving maps\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
  saveRDS(mean.ggplot.list, plotListName)
  cat("\n\n List of maps saved in:", plotListName, "\n")
  
  # save an image of the mean maps
  if (length(time_scale)>1){
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
  } else if (time_scale == 1){
    ggsave(pngMapsName, mean.ggplot.list, width = ifelse(common_scale_max, 200, 180),
           height = 120, units = "mm")
  }
  
  
  #' save a log
  sink(logName4, append = F)
  cat("Date & time :", format(Sys.time()), "\n")
  cat("\n  Time scale of aggregation:", agg.time_scale)
  sink()
  
  
} else {
  
  msg <- "Maps already exist\n" ; cat(msg) ; lines.to.cat <- c(lines.to.cat, msg)
  
}



