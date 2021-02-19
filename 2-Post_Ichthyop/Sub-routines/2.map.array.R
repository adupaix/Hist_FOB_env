
# Verbose
tic(msg = "### 2. map.array() function", quiet = F)
cat(crayon::bold("\n\n###### 2. Building maps from the arrays\n"))
start1 <- Sys.time()

# Update log file
sink(logName, append = T)
cat("2. BUILDING MAPS FROM ARRAY")
cat("\n Execution time :", format(Sys.time()))
cat("\n\n### ARGUMENTS USED")
cat(paste("\n log_color_scale :", log_color_scale))
cat(paste("\n common_scale_max :", common_scale_max))
cat(paste("\n color_scale_pos :", color_scale_pos))
sink()
  
  
# if the list of maps and the global map were already generated
# we only load them
if (file.exists(plotListName) & file.exists(mapName)){
  cat("\n Maps list already built and saved in:", plotListName)
  cat("\n Mean maps list already built and saved in:", meanPlotListName, "\n")
  ggplot.list <- readRDS(plotListName)
  mean.ggplot.list <- readRDS(meanPlotListName)
    
} else{
  
  if (!file.exists(aggregateName)){
    stop("Error: array does not exist")
    
  } else {
      
    cat("\n Building a map for each time scale")
    
    ggplot.list <- list()
    
    agg_array <- readRDS(aggregateName)
    time_scale <- 1:dim(agg_array)[3]
    
    if (common_scale_max == T){
      fixed_scale_max <- ceiling(max(agg_array))
    } else{
      fixed_scale_max <- 0
    }
    
    # for each time_scale, we build a map and save it in the list
    for (i in time_scale){
        
      array.i <- agg_array[,,i]
      dimname.i <- dimnames(agg_array)[[3]][i]
        
      ggplot.list[[i]] <- matrix.to.ggplot(array.i, dimname.i, gsize,
                                             log_color_scale, fixed_scale_max,
                                             color_scale_pos)
    }
      
      
  }
    
    
  if (!file.exists(meanAggName)){
    stop("Error: matrix does not exist")
    
  } else {
    
    cat("\n Building a map with the mean result for each time scale over the whole study period")
    
    mean.ggplot.list <- list()
    
    mean_agg_array <- readRDS(meanAggName)
      
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
  }

  
# save the list of ggplots
saveRDS(ggplot.list, plotListName)
cat("\n\n List of maps saved in:", plotListName, "\n")
sink(logName, append = T)
cat("\n\nList of maps saved in:\n", plotListName)
sink()

# save the list of mean ggplots
saveRDS(mean.ggplot.list, meanPlotListName)
cat("\n List of mean maps saved in:", meanPlotListName, "\n\n")
sink(logName, append = T)
cat("\n\nList of mean maps saved in:\n", meanPlotListName)
sink()

# save an image of the mean maps
l = length(mean.ggplot.list)
n_col <- ceiling(sqrt(l))
n_row <- ceiling(l / n_col)
p <- ggarrange(plotlist = mean.ggplot.list,
                ncol = n_col, nrow = n_row,
                align = "hv", labels = "AUTO",
                common.legend = common_scale_max,
               legend = "right")
            
ggsave(mapName, p, width = ifelse(common_scale_max, 180*n_col + 20, 180*n_col),
       height = 120*n_row, units = "mm")

}


# Update dont delete
DoNotDeleteMeToo <- c(DoNotDeleteMeToo, "mean.ggplot.list")

# Save execution time in the log file
end1 <- Sys.time()
tps_exec <- end1 - start1
units(tps_exec) <- "mins"
tps_exec_mins <- trunc(tps_exec)
tps_exec_secs <- tps_exec - trunc(tps_exec)
units(tps_exec_secs) <- "secs"
tps_exec_secs <- round(tps_exec_secs, digits = 2)

sink(logName, append = T)
cat("\n\nExecution time :", as.character(tps_exec_mins), "minutes", as.character(tps_exec_secs), "seconds\n\n\n")
sink()


#print execution time
toc()
  

#clear environment
rm(list = ls()[!ls() %in% DoNotDeleteMeToo]) ; invisible(gc())



