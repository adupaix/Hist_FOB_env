#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2023-10-02
#'@email : amael.dupaix@ird.fr
#'#*******************************************************************************************************************
#'@description :  Plotting time series of the input in different scenarios
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

rm(list = ls())


# OUTPUT_PATH <- "/home1/datawork/adupaix/Hist_FOB_env/Data_description/Outputs"
OUTPUT_PATH <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/Data_description/Outputs/scenarios"
dir.create(OUTPUT_PATH, showWarnings = F)

# path containing the matrices of weight
WD <- "/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/"

FUNC_PATH <- file.path(WD, "3-Simulations_processing/Functions")
DATA_PATH <- file.path(WD, "2-Post_Ichthyop/Outputs/nemo_river_allMask")

library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

source(file = file.path(FUNC_PATH, "readAreas.R"))

years <- 2000:2019

read.input.scenarios <- function(year.i, DATA_PATH){
  # open matrix
  data <- read.csv(file = file.path(DATA_PATH, year.i,
                                    "2.info_on_points",
                                    "weight_per_points_summary.csv"))
  # summarize
  data %>%
    group_by(release_date) %>%
    dplyr::summarize(across(w1:w9, sum)) %>%
    ungroup() %>%
    dplyr::mutate(release_date = as.Date(release_date)) %>%
    dplyr::filter(lubridate::year(release_date) == year.i) -> data
  
  return(data)
}

full_data <- lapply(X = years, FUN = read.input.scenarios,
               DATA_PATH = DATA_PATH) %>%
  bind_rows()

names(full_data) <- c("release_date",
  "w1", "CL", "CC", "RC",
  "w5", "CCp", "RCp", "w8",
  "R&CC")

kept_scenarios <- c("CL","CC","RC","CCp","RCp","R&CC")

full_data %>% dplyr::select(release_date,
                            CL, CC, RC, CCp, RCp, "R&CC") %>%
  tidyr::pivot_longer(-release_date,
                      names_to = "w",
                      values_to = "weight") %>%
  dplyr::mutate(w = factor(w, levels = kept_scenarios)) %>%
  dplyr::group_by(w) %>%
  dplyr::mutate(weight = 100 * weight / weight[which(release_date == min(release_date))]) %>%
  ungroup() -> toplot

my_colors <- Set1.without.yellow(level_names = levels(toplot$w))

# as.data.frame(my_colors) %>%
#   tibble::rownames_to_column(var = "w") %>%
#   right_join(toplot, by = "w") -> toplot

p <- ggplot(toplot)+
  geom_line(aes(x = release_date,
                y = weight,
                color = w))+
  scale_y_continuous(limits = c(0,NA))+
  scale_color_manual("Scenario", values = my_colors)+
  facet_wrap(~w, scales = "free")+
  ylab("Total input at the scale of the Indian Ocean\n(% of input on 2000-01-01)")+
  xlab("Date")

ggsave(filename = file.path(OUTPUT_PATH, "input_per_scenario.png"),
       p, width = 10, height = 6)
