#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2023-06-29
#'@email : amael.dupaix@ird.fr
#'#*******************************************************************************************************************
#'@description :  Function to correct for temporal autocorrelation in the calculation of the p.value of the slope
#'                when fitting a linear model Y ~ time
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

#model: output from lm(Y ~ time)
correct.pvalue.temporal.autocorr <- function(model,
                                             cor_threshold = 0.2,
                                             n_spl_for_t_comparison = 10**7,
                                             return.lag_autocorr = F){
  Y_name <- all.vars(model$call$formula)[1]
  
  # regarde l'autocorrelation temporelle, pour corriger le calcul de la p.value du slope du lm
  j=1
  corr_lag <- cor(model$model[-seq(1,j),Y_name],
                  lag(model$model[,Y_name], n = j)[-seq(1,j)])
  while(corr_lag > cor_threshold){
    j=j+1
    corr_lag <- cor(model$model[-seq(1,j),Y_name],
                    lag(model$model[,Y_name], n = j)[-seq(1,j)])
  }
  lag_autocorr <- j
  
  # recalcule la standard error en prenant le nombre de degres de liberte divise par
  # lag_autocorr
  S <- coef(summary(model))[2,2]
  S_corrected <- S * sqrt(lag_autocorr)
  
  # recalcule la t value to do the t test
  t_corrected <- coef(summary(model))[2,1] / S_corrected
  
  # check if t_corrected is significantly inferior from T extracted from the t distribution
  # ie if the slope coefficient is significantly different from zero
  n_spl = n_spl_for_t_comparison
  t_to_compare <- rt(n = n_spl, df = model$df.residual / lag_autocorr)
  p.value <- sum(t_corrected < t_to_compare) / n_spl
  
  if(return.lag_autocorr){
    return(c(p.value, lag_autocorr))
  }
  
  return(p.value)
}
