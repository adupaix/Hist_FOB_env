run_from_other_script <- ""

RESET=F
DoNotDelete <- c("df_list")

df_list <- list()

years = 2014:2018

for (year in years){
  for (effort_threshold in c(0,6,10)){
    source("/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/3-Simulations_processing/distance_with_data.R")
    
    df_list[[paste(year, effort_threshold)]] <- df_tot
  }
}
for (i in 1:length(df_list)){
  df_list[[i]]$year <- sub(" .*","",names(df_list)[i])
}
df <- dplyr::bind_rows(df_list)
head(df)
plyr::ddply(df, .variables = c("month","effort_threshold","w","year"),
            function(x) sum(x$dist)) -> toplot
toplot$date <- as.Date(paste(toplot$year, toplot$month, 1, sep = "-"))
toplot$effort_threshold <- factor(toplot$effort_threshold, levels = c("0","6","10"))
p1=ggplot(toplot %>% dplyr::filter(w %in% c(2,3,4,6,7,9)),
       aes(x = date, y = V1, group = as.factor(w), color = as.factor(w)))+
  ggplot2::facet_wrap(~effort_threshold, scales = "free", nrow = 3)+
  scale_color_brewer("Weighting\nmethod", palette = "Set1")+
  geom_point(alpha=0.5)+geom_line(alpha=0.5)+
  ylab("Distance")
ggsave(file.path(OUTPUT_PATH,"Distance_to_data/distance_trend.png"),p1,width=10,height=6)

plyr::ddply(df, .variables = c("effort_threshold","w"),
            function(x) sum(x$dist)) -> toplot2
  tidyr::pivot_wider(data = toplot2,
                     names_from = "effort_threshold",
                     names_prefix = "T = ",
                     values_from = "V1") -> distance_summary
p2 <- ggplot(toplot2, aes(x = as.factor(w), y = V1,
                    group = as.factor(effort_threshold),
                    fill = as.factor(effort_threshold)))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_brewer("T", palette = "Set1")+
  xlab("Weighting method")+
  ylab("Sum of monthly distances")

ggsave(file.path(OUTPUT_PATH, "Distance_to_data/distance_sum.png"), p2,
       width = 6, height = 4)

# cor.test(distance_summary$`T = 6`, distance_summary$`T = 10`,
#          method = "spearman")
