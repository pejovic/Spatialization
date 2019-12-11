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


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Read all data files
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::

data.spat <- list.files('C:/Users/pbursac/Desktop/data/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/pbursac/Desktop/data/",data.spat[i], sep = ""))
}

sf_data <- data.spat.list[[1]]
for(i in 2:length(data.spat)){                                             
  sf_data <- st_join(sf_data, data.spat.list[[i]], join = st_equals) %>% 
    group_by(ID.x) %>%
    summarize(NOx = sum(NOx.x, NOx.y),
              SO2 = sum(SO2.x, SO2.y),
              PM10 = sum(PM10.x, PM10.y),
              PM2.5 = sum(PM2.5.x, PM2.5.y),
              NMVOC = sum(NMVOC.x, NMVOC.y),
              NH3 = sum(NH3.x + NH3.y)) %>%
    mutate(ID = ID.x) %>%
    select(ID, NOx, SO2, PM10, PM2.5, NMVOC, NH3)
  print(paste("NOx:",sum(sf_data$NOx))) 
  print(paste("SO2:",sum(sf_data$SO2)))
  print(paste("PM10:",sum(sf_data$PM10)))
  print(paste("PM2.5:",sum(sf_data$PM2.5)))
  print(paste("NMVOC:",sum(sf_data$NMVOC)))
  print(paste("NH3:",sum(sf_data$NH3)))
}

sf_data

mapview(sf_data, zcol = "NMVOC")

###################################################################
# d <- st_join(sf_data, data.spat.list[[2]], join = st_equals) %>% 
#   group_by(ID.x) %>%
#   summarize(NOx = sum(NOx.x, NOx.y),
#             SO2 = sum(SO2.x, SO2.y),
#             PM10 = sum(PM10.x, PM10.y),
#             PM2.5 = sum(PM2.5.x, PM2.5.y),
#             NMVOC = sum(NMVOC.x, NMVOC.y),
#             NH3 = sum(NH3.x + NH3.y)) %>%
#   mutate(ID = ID.x) %>%
#   select(ID, NOx, SO2, PM10, PM2.5, NMVOC, NH3)
# 
# mapview(d, zcol = "SO2") + mapview(sf_data, zcol = "SO2") + mapview(data.spat.list[[2]], zcol = "SO2")
###################################################################
