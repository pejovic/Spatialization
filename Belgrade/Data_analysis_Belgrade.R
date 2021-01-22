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
# Belgrade

sf_data <- st_read(dsn = "Products/Sum_up_by_cell_by_pollutant.gpkg")

opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)
unique(sf_opstine$NAME_1)

sf_opstine_bg <- sf_opstine %>% dplyr::filter(NAME_1 == "Grad Beograd")

sf_data_beograd <- sf_data[sf_opstine_bg, ] 



# -------------------------------------------------------------------------------------------------------------
# EDGAR database
# -------------------------------------------------------------------------------------------------------------

edg_NH3 <- read_delim(file = "D:/air-15/Edgar/v50_NH3_2015/v50_NH3_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_NMVOC <- read_delim(file = "D:/air-15/Edgar/v50_NMVOC_2015/v50_NMVOC_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_NOx <- read_delim(file = "D:/air-15/Edgar/v50_NOx_2015/v50_NOx_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_PM2.5 <- read_delim(file = "D:/air-15/Edgar/v50_PM2.5_2015/v50_PM2.5_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_PM10 <- read_delim(file = "D:/air-15/Edgar/v50_PM10_2015/v50_PM10_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_SO2 <- read_delim(file = "D:/air-15/Edgar/v50_SO2_2015/v50_SO2_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")

sf_NH3 <- st_as_sf(edg_NH3, coords = c("lon", "lat"), crs = 4326)
sf_NMVOC <- st_as_sf(edg_NMVOC, coords = c("lon", "lat"), crs = 4326)
sf_NOx <- st_as_sf(edg_NOx, coords = c("lon", "lat"), crs = 4326)
sf_PM2.5 <- st_as_sf(edg_PM2.5, coords = c("lon", "lat"), crs = 4326)
sf_PM10 <- st_as_sf(edg_PM10, coords = c("lon", "lat"), crs = 4326)
sf_SO2 <- st_as_sf(edg_SO2, coords = c("lon", "lat"), crs = 4326)

sf_NH3_bg <- sf_NH3[sf_opstine_bg, ]

emep_grid <- st_read(dsn = "D:/Projekti/Spatialisation/OutputExamples-20191028T160417Z-001/OutputExamples/Sum-up and examples/1. Prior information/EMEP grid for Serbia/EMEP_GRID_01x01DEG_RS.shp")

mapview(sf_NH3_bg, zcol = "emission 2015 (tons)") + mapview(sf_data_beograd) + mapview(emep_grid)



edg_NH3_mov <- edg_NH3 %>% dplyr::mutate(lat = (lat - 0.05), 
                                         lon = (lon + 0.05))


sf_edg_NH3_mov <- st_as_sf(edg_NH3_mov, 
                           coords = c("lon", "lat"), 
                           crs = 4326)


sf_edg_NH3_mov_bg <- sf_edg_NH3_mov[sf_opstine_bg, ]

mapview(sf_edg_NH3_mov_bg, zcol = "emission 2015 (tons)") + mapview(sf_data_beograd) + mapview(emep_grid)



