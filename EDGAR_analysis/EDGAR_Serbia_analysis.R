library(tidyverse)
library(sf)
library(readxl)
library(ggpubr)
library(ggfortify)
library(here)
library(knitr)
library(kableExtra)
library(DT)
library(mapview)
library(rgdal)
library(SerbianCyrLat)
library(stringr)
library(classInt)
library(viridis)
library(gridExtra)
library(readr)
library(mapedit)



RS_grid_0.1_spated <- st_read(dsn = "EDGAR_analysis/RS_Grid_0.1_RF2015_bez_KiM.gpkg")
RS_grid_0.1_cent <- st_read(dsn = "CEIP_Grid_0.1/CEIP/tacke_ccentroidi_2015.gpkg")

# RS_grid_0.1_spated_n <- RS_grid_0.1_spated %>%
#   st_join(RS_grid_0.1_cent, join = st_contains) %>%
#   group_by(ID.x) %>%
#   dplyr::summarize(N = n()) %>%
#   dplyr::rename(ID = ID.x)
# 
# summary(RS_grid_0.1_spated_n)
# mapview(RS_grid_0.1_spated_n, zcol = "pt_count") + mapview(RS_grid_0.1_cent)


RS_grid_0.1_spated$N_pt <- lengths(st_intersects(RS_grid_0.1_spated, RS_grid_0.1_cent))
summary(RS_grid_0.1_spated$N_pt)



edg_NH3 <- read_delim(file = "D:/air-15/Edgar/v50_NH3_2015/v50_NH3_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_NH3_mov <- edg_NH3 %>% dplyr::mutate(lat = (lat - 0.05), 
                                         lon = (lon + 0.05))
sf_edg_NH3_mov <- st_as_sf(edg_NH3_mov, 
                           coords = c("lon", "lat"), 
                           crs = 4326)

RS_grid_0.1_spated <- st_join(RS_grid_0.1_spated, sf_edg_NH3_mov, join = st_contains) 

RS_grid_0.1_spated %<>% dplyr::rename(NH3_edgar = `emission 2015 (tons)`) 

mapview(RS_grid_0.1_spated, zcol = "NH3")

# gde je nula tu nula i u edgaru i sada sracunati cetvrtine celija
RS_grid_0.1_spated %<>% dplyr::mutate(NH3_edgar_sa_0 = case_when((NH3 == 0 | NOx == 0 | SO2 == 0 | PM2_5 == 0 | PM10 == 0 | NMVOC == 0) ~ 0, 
                                                           (NH3 != 0 & NOx != 0 & SO2 != 0 & PM2_5 != 0 & PM10 != 0 & NMVOC != 0) ~ NH3_edgar))


RS_grid_0.1_spated %<>% dplyr::mutate(NH3_diff = NH3_edgar_sa_0 - NH3) 
summary(RS_grid_0.1_spated$NH3_diff)

mean(RS_grid_0.1_spated$NH3_diff)
mapview(RS_grid_0.1_spated , zcol = "NH3_diff")
