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





clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)

# Urban areas
#sf_clc18_urb <- subset(sf_clc18, CODE_18 == "111" | CODE_18 == "112") %>% # CLC urban zones
#  st_transform(crs = "+init=epsg:32634")


# Old rural areas
# sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)
# sf_rur <- st_read(dsn = "Products/rural_areas.gpkg", layer = "rural_areas")


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# New rural areas
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_clc18_rur <- subset(sf_clc18, CODE_18 == "211" | CODE_18 == "221" | CODE_18 == "222" | CODE_18 == "231" | CODE_18 == "242" | CODE_18 == "243") %>% # CLC rural zones
  st_transform(crs = "+init=epsg:32634")
unique(sf_clc18_rur$CODE_18)
mapview(sf_clc18_rur, zcol = "CODE_18")

# st_write(sf_clc18_rur, dsn="Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg", layer='rural_areas_new')


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Industrial sites 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_clc18_industrial <- subset(sf_clc18, CODE_18 == "121") %>% 
  st_transform(crs = "+init=epsg:32634")
# st_write(sf_clc18_industrial, dsn="Version_2_update/Spatialization/Proxy_data_new/industrial_and_commercial.gpkg", layer='ind_and_comm')


osm_commercial <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/OSM_commercial_Serbia.gpkg") %>%
  select(osm_id, fclass)

#st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))

sf_industrial.int <- st_difference(sf_clc18_industrial, osm_commercial)

mapview(sf_industrial.int) + mapview(osm_commercial)

st_write(sf_industrial.int, dsn="Version_2_update/Spatialization/Proxy_data_new/industrial_int.gpkg", layer='industrial')



