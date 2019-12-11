#' ---
#' title: "Pollutant inventory spatialization"
#' author:
#'    - "Milan Kilibarda"
#'    - "Dragutin Protic"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output:   
#'    html_document:
#'      keep_md: true
#'      theme: "simplex"
#'      highlight: tango
#'      toc: true
#'      toc_depth: 5
#'      toc_float: true
#'      fig_caption: yes
#' ---
#' 
#+ include = FALSE 
# <img src="Logo-Trans.png" align="center" alt="logo" width="2000" height = "3000" style = "border: none; float: right;">
#' This document provides the maps regarding the spatialization of pollutation inventory.
#' 
#' 
#+ include = TRUE, echo = FALSE, results = 'hide', warning = FALSE, message = FALSE
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
#+ include = FALSE 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Read all data files
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# data.spat <- list.files('C:/Users/pbursac/Desktop/data/')
# 
# data.spat.list <- list()                                                   
# for(i in 1:length(data.spat)){                                             
#   data.spat.list[[i]] <- st_read(paste("C:/Users/pbursac/Desktop/data/",data.spat[i], sep = ""))
# }
# 
# sf_data <- data.spat.list[[1]]
# for(i in 2:length(data.spat)){                                             
#   sf_data <- st_join(sf_data, data.spat.list[[i]], join = st_equals) %>% 
#     group_by(ID.x) %>%
#     summarize(NOx = sum(NOx.x, NOx.y),
#               SO2 = sum(SO2.x, SO2.y),
#               PM10 = sum(PM10.x, PM10.y),
#               PM2.5 = sum(PM2.5.x, PM2.5.y),
#               NMVOC = sum(NMVOC.x, NMVOC.y),
#               NH3 = sum(NH3.x + NH3.y)) %>%
#     mutate(ID = ID.x) %>%
#     select(ID, NOx, SO2, PM10, PM2.5, NMVOC, NH3)
#   print(paste("NOx:",sum(sf_data$NOx))) 
#   print(paste("SO2:",sum(sf_data$SO2)))
#   print(paste("PM10:",sum(sf_data$PM10)))
#   print(paste("PM2.5:",sum(sf_data$PM2.5)))
#   print(paste("NMVOC:",sum(sf_data$NMVOC)))
#   print(paste("NH3:",sum(sf_data$NH3)))
# }
# 
# sf_data
# 
# mapview(sf_data, zcol = "NMVOC")
# st_write(sf_data, dsn="Products/sf_data.gpkg", layer='sf_data')
#+ include = FALSE 
sf_data <- st_read(dsn = "Products/sf_data.gpkg", layer = "sf_data")
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

#+ include = FALSE 
classes.NOx <- classIntervals(sf_data$NOx, n = 40, style = "jenks")
classes.SO2 <- classIntervals(sf_data$SO2, n = 40, style = "jenks")
classes.PM10 <- classIntervals(sf_data$PM10, n = 40, style = "jenks")
classes.PM2.5 <- classIntervals(sf_data$PM2.5, n = 40, style = "jenks")
classes.NMVOC <- classIntervals(sf_data$NMVOC, n = 40, style = "jenks")
classes.NH3 <- classIntervals(sf_data$NH3, n = 40, style = "jenks")

sf_data <- sf_data %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T)
         )

pal1 <- viridisLite::viridis(40)
pal2 <- viridisLite::cividis(40)
pal3 <- viridisLite::inferno(40)
pal4 <- viridisLite::magma(40)
pal5 <- viridisLite::plasma(40)
pal6 <- viridisLite::viridis(40)

#+ include = FALSE 
a<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

b<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal2,
                    name = "SO2") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

c<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal3,
                    name = "PM10") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

d<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal4,
                    name = "PM2.5") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

e<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal5,
                    name = "NMVOC") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

f<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal6,
                    name = "NH3") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

#grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)
#mapview(sf_data, zcol = "percent_class_NH3", alpha.regions = 80, col.regions = pal6, query.type = c("mousemove"), query.digits = 2)
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
a
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
b
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
c
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
d
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
e
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
f