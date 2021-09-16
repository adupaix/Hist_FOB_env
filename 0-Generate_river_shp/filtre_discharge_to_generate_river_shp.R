
library(dplyr)
library(sf)

filter_dis = 10 # en m3/s (seuil utilise)

## rivers_IO.rds contient une premiere extraction
# des donnees de rivieres, avec uniquement les rivieres
# du pourtour de l'ocean indien
rivers_IO <- readRDS("/home/adupaix/Documents/These/Axe_1/Hist_FOB_env/0-Data/river_data/rivers_IO.rds")

rivers_IO %>%
  # garde uniquement les embouchures
  # (identifiant de la portion de riviere = a l'identifiant principal de la riviere)
  filter(HYRIV_ID == MAIN_RIV) %>%
  # garde uniquement les portions de riviere qui se jettent dans la mer
  filter(ENDORHEIC == 0) %>%
  # garde uniquement les fleuves avec un debit maximal de plus d 100 m3 par seconde
  filter(dis_m3_pmx >= filter_dis) -> embouchures

rivers_IO %>%
  filter(MAIN_RIV %in% embouchures$HYRIV_ID) %>%
  filter(dis_m3_pmx >= filter_dis) -> filtered

# Ecriture du shapefile: (commente car long)
# st_write(obj = filtered, paste0("4-Generate_river_shp/rivers_IO_dismx_",filter_dis,"m3s.shp"))

filtered %>% dplyr::select(HYRIV_ID, #id de la portion de riviere
                           NEXT_DOWN,#id de la portion en aval
                           MAIN_RIV, # id de la portion qui se jette dans la mer
                           LENGTH_KM, # longeur de la portion en km
                           HYBAS_L12, # id du bassin versant,
                           #pour faire le lien avec l'autre base de donnees
                           dis_m3_pyr, # debit moyen en m3/s
                           dis_m3_pmn, # debit minimal en m3/s
                           dis_m3_pmx # debit maximal
) -> filtered

# Ecriture du shapefile avec quelques variables seulement:
st_write(obj = filtered, paste0("0-Generate_river_shp/Outputs/", filter_dis,"m3s/rivers_IO_dismx_",filter_dis,"m3s_selected_vars.shp"))
