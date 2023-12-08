#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2023-10-03
#'@email : amael.dupaix@ird.fr
#'#*******************************************************************************************************************
#'@description :  Subfunctions used specifically in trend_distance_with_data.R
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************



build.SimVsData.plot <- function(df, color_var,
                                 shape_var = NA,
                                 colors = my_colors,
                                 scale_by_max = SCALE_BY_MAX){
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  if(is.na(shape_var)){
    p <- ggplot(df)+
      geom_point(aes(x = NLOGdata, y = NLOGsim, color = !!ensym(color_var)))
  } else{
    p <- ggplot(df)+
      geom_point(aes(x = NLOGdata, y = NLOGsim,
                     color = !!ensym(color_var),
                     shape = !!ensym(shape_var)))+
      scale_shape(firstup(shape_var))
  }
  
  p <- p +
    scale_color_manual(firstup(color_var), values = colors)+
    facet_wrap(~w, scales = "free") +
    theme(panel.background = element_rect(fill = "white",
                                          colour = "black"))+
    scale_y_continuous("Simulated NLOG abundance index")+
    scale_x_continuous("Number of NLOG observations per day of observation")
 
  if (scale_by_max){
    p <- p +
      geom_abline(slope = 1, intercept = 0, color = "grey")+
      scale_y_continuous("Simulated NLOG abundance index", limits = c(0,1))+
      scale_x_continuous("Number of NLOG observations per day of observation", limits = c(0,1))
  }
  
  return(p)
}


overall_p <- function(my_model){
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

label.lm.for.ggplot <- function(model){
  a <- round(summary(model)$r.squared,digits = 2)
  b <- format(model$coefficients[1], digits = 2,
              scientific = T)
  c <- round(model$coefficients[2], digits = 2)
  d = format(overall_p(model), digits = 3,
             scientific = T)
  return(
    paste0("r.sq = ",a,"\n",
           # "y = ",b," + ",c,"x\n",
           "p = ",d)
  )
}
