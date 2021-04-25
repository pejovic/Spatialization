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
library(ggspatial)
library(data.table)

opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)
unique(sf_opstine$NAME_1)

sf_opstine_bg <- sf_opstine %>% dplyr::filter(NAME_1 == "Grad Beograd")
sf_opstine_bo <- sf_opstine %>% dplyr::filter(NAME_2 == "Bor")
sf_opstine_ns <- sf_opstine %>% dplyr::filter(NAME_2 == "Novi Sad")
sf_opstine_pa <- sf_opstine %>% dplyr::filter(NAME_2 == "Pančevo")
sf_opstine_sd <- sf_opstine %>% dplyr::filter(NAME_2 == "Smederevo")
sf_opstine_uz <- sf_opstine %>% dplyr::filter(NAME_2 == "Užice")



st_write(sf_opstine_bg, dsn = "Cities/Boundary_shapefiles/Belgrade.shp", layer = "Belgrade")
st_write(sf_opstine_bo, dsn = "Cities/Boundary_shapefiles/Bor.shp", layer = "Bor")
st_write(sf_opstine_ns, dsn = "Cities/Boundary_shapefiles/Novi Sad.shp", layer = "Novi Sad")
st_write(sf_opstine_pa, dsn = "Cities/Boundary_shapefiles/Pancevo.shp", layer = "Pancevo")
st_write(sf_opstine_sd, dsn = "Cities/Boundary_shapefiles/Smederevo.shp", layer = "Smederevo")
st_write(sf_opstine_uz, dsn = "Cities/Boundary_shapefiles/Uzice.shp", layer = "Uzice")









# Podaci po gradovima

vars <- c("av_NOx", "av_SO2", "av_PM10", "av_PM2.5", "av_NMVOC", "av_NH3")
sf_data_2015 <- st_read(dsn = "Products/Sum_up_by_cell_by_pollutant_2015.gpkg")
sf_data_2030 <- st_read(dsn = "2030/Products_2030/Sum_up_by_cell_by_pollutant_2030.gpkg")
sf_data_2030_A <- st_read(dsn = "2030_WAM_A/Products_2030_WAM_A/Sum_up_by_cell_by_pollutant_2030_WAM_A.gpkg")
sf_data_2030_B <- st_read(dsn = "2030_WAM_B/Products_2030_WAM_B/Sum_up_by_cell_by_pollutant_2030_WAM_B.gpkg")

data_lista <- list(sf_data_2015, sf_data_2030, sf_data_2030_A, sf_data_2030_B)

nazivi <- c("Belgrade", "Bor", "Novi Sad", "Pancevo", "Smederevo", "Uzice")

gradovi <- list(sf_opstine_bg, sf_opstine_bo, sf_opstine_ns, sf_opstine_pa, sf_opstine_sd, sf_opstine_uz)

f_nazivi <- c("2015", "WEM_2030", "WAM_A_2030", "WAM_B_2030")

for(i in 1:length(data_lista)){
  for(j in 1:length(nazivi)){
    dat_city_y <- data_lista[[i]][gradovi[[j]], ]
    st_write(dat_city_y, dsn = paste("Cities/Data_per_city_geopackages/", f_nazivi[i],"/Data_",nazivi[j],"_",f_nazivi[i],".gpkg", sep = ""), layer = paste(nazivi[j],"_",f_nazivi[i], sep = ""))
  }
}
















