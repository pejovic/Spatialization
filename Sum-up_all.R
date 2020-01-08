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
st_write(sf_data, dsn="Products/Sum_up_by_cell_by_pollutant.gpkg", layer='Sum_up_by_cell_by_pollutant')
writexl::write_xlsx(sf_data %>% st_drop_geometry(), "Products/Sum_up_by_cell_by_pollutant.xlsx")

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
classes.NOx <- classIntervals(sf_data$NOx, n = 40, style = "fisher")
classes.SO2 <- classIntervals(sf_data$SO2, n = 40, style = "fisher")
classes.PM10 <- classIntervals(sf_data$PM10, n = 40, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data$PM2.5, n = 40, style = "fisher")
classes.NMVOC <- classIntervals(sf_data$NMVOC, n = 40, style = "fisher")
classes.NH3 <- classIntervals(sf_data$NH3, n = 40, style = "fisher")

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
        #legend.position = "None", ###################### legend
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
        #legend.position = "None", ###################### legend
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
        #legend.position = "None", ###################### legend
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
        #legend.position = "None", ###################### legend
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
        #legend.position = "None", ###################### legend
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
        #legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)
# out.width="100%"
mapview(sf_data, zcol = "percent_class_NH3", alpha.regions = 30, col.regions = pal6, query.type = c("mousemove"), query.digits = 2)
#' ## Map - NOx
#+ echo = FALSE, result = TRUE, eval = TRUE, fig.width = 10, fig.height = 10, fig.align='center'
a
#' ## Map - SO2
#+ echo = FALSE, result = TRUE, eval = TRUE, fig.width = 10, fig.height = 10, fig.align='center'
b
#' ## Map - PM10
#+ echo = FALSE, result = TRUE, eval = TRUE, fig.width = 10, fig.height = 10, fig.align='center'
c
#' ## Map - PM2.5
#+ echo = FALSE, result = TRUE, eval = TRUE, fig.width = 10, fig.height = 10, fig.align='center'
d
#' ## Map - NMVOC
#+ echo = FALSE, result = TRUE, eval = TRUE, fig.width = 10, fig.height = 10, fig.align='center'
e
#' ## Map - NH3
#+ echo = FALSE, result = TRUE, eval = TRUE, fig.width = 10, fig.height = 10, fig.align='center'
f

#+ echo = FALSE, result = TRUE, eval = TRUE, fig.width = 10, fig.height = 10, fig.align='center'
mapview(sf_data, zcol = "percent_class_NOx", col.regions = pal1, query.type = c("mousemove"), query.digits = 2) + 
  mapview(sf_data, zcol = "percent_class_SO2", col.regions = pal2, query.type = c("mousemove"), query.digits = 2) + 
  mapview(sf_data, zcol = "percent_class_PM10", col.regions = pal3, query.type = c("mousemove"), query.digits = 2) + 
  mapview(sf_data, zcol = "percent_class_PM2.5", col.regions = pal4, query.type = c("mousemove"), query.digits = 2) + 
  mapview(sf_data, zcol = "percent_class_NMVOC", col.regions = pal5, query.type = c("mousemove"), query.digits = 2) + 
  mapview(sf_data, zcol = "percent_class_NH3", col.regions = pal6, query.type = c("mousemove"), query.digits = 2) 


###########################################################################################



# Temporal profiles all in one
data.tprofiles <- list.files('Hourly_emissions/Products/')

                                          
data.temp.all <- readxl::read_xlsx(path = "Hourly_emissions/Products/TemporalProfiles_All_in_one.xlsx")

data.temp.list <- list()                                                   
for(i in 1:length(data.tprofiles)){                                             
  data.temp.list[[i]] <- readxl::read_xlsx(path = paste("Hourly_emissions/Products/",data.tprofiles[i], sep = ""))
}




NOx.temp <- data.temp.all %>% dplyr::select(ends_with("NOx")) %>% rowwise() %>%
  do( (.) %>% as.data.frame %>% mutate(NOx_temp = sum(.)) ) %>%
  ungroup() %>% mutate(Time = data.temp.all$Time) %>% dplyr::select(Time, NOx_temp)

NOx.temp$Time <- data.temp.all$Time
sum(NOx.temp$NOx_temp)


length(data.temp.list[[8]])


temporal_Profiles <- data.temp.list[[1]]$Time %>%
  cbind(
    data.temp.list[[1]][,2:85],
    data.temp.list[[2]][,2:37],
    data.temp.list[[3]][,2:25],
    data.temp.list[[4]][,2:55],
    data.temp.list[[5]][,2:79],
    data.temp.list[[6]][,2:25],
    data.temp.list[[7]][,2:145],
    data.temp.list[[8]][,2:25]
  ) %>%
  as_data_frame()


writexl::write_xlsx(temporal_Profiles, "Hourly_emissions/TemporalProfiles_by_pollutant_and_sub-categories.xlsx")


