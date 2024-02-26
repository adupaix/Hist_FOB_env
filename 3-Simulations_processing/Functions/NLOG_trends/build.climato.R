
build.climato <- function(df, years,
                          var_to_average = "av_sim_nlog",
                          duplicated_over = c("area")){
  df %>% dplyr::mutate(month_day = format(day, "%m-%d")) -> df
  
  # first year as a ref
  list_for_climato <- list()
  list_for_climato[[1]] <- df %>% dplyr::filter(year == years[1])
  ref_days <- unique(list_for_climato[[1]]$month_day)
  is_leap_ref <- is.leap(years[1])
  
  for (i in 2:length(years)){
    df %>%
      dplyr::filter(year == years[i]) -> list_for_climato[[i]]
    if (years[i] %in% daily_saved_years){
      list_for_climato[[i]] %>%
        dplyr::filter(month_day %in% ref_days) -> list_for_climato[[i]]
    } else if (is_leap_ref){
      if (is.leap(years[i])) {
        #nothing to do
      } else if (!is.leap(years[i])){
        list_for_climato[[i]] %>%
          dplyr::mutate(day = case_when(day <= as.Date(paste0(years[i], "-02-28")) ~ day,
                                        day > as.Date(paste0(years[i], "-02-28")) ~ day-1)) %>%
          dplyr::mutate(month_day = format(day, "%m-%d"))-> list_for_climato[[i]]
      } else if (!is_leap_ref){
        
        if (is.leap(years[i])){
          list_for_climato[[i]] %>%
            dplyr::mutate(day = case_when(day <= as.Date(paste0(years[i], "-02-28")) ~ day,
                                          day > as.Date(paste0(years[i], "-02-28")) ~ day+1)) %>%
            dplyr::mutate(month_day = format(day, "%m-%d"))-> list_for_climato[[i]]
        } else if(!is.leap(years[i])){
          #nothing to do
        }
        
      }
    } 
  }
  
  df_for_climato <- dplyr::bind_rows(list_for_climato)
  
  plyr::ddply(df_for_climato, .variables = c(duplicated_over, "month_day"), function(x) mean(x[,var_to_average])) %>%
    dplyr::mutate(day = as.Date(paste0("2000-", month_day))) -> df
  
  names(df)[which(names(df) == "V1")] <- var_to_average
  
  return(df)
}

is.leap <- function(yr) {
  return((yr %% 4 == 0 & yr %% 100 != 0) | (yr %% 400 == 0))
}



