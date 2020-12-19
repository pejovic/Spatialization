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
library(ggspatial)

my_theme <- function(base_size = 10, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 12),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fffcfc"),
      strip.background = element_rect(fill = "#820000", color = "#820000", size =0.5),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey30", fill = NA, size = 0.5)
    )
}

theme_set(my_theme())
mycolors=c("#f32440","#2185ef","#d421ef")



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
#sf_data <- st_read(dsn = "Products/sf_data.gpkg", layer = "sf_data")
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

sf_data <- st_read(dsn = "Products/Sum_up_by_cell_by_pollutant.gpkg")





#+ include = FALSE 
classes.NOx <- classIntervals(sf_data$NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data$SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data$PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data$PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data$NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data$NH3, n = 12, style = "fisher")

sf_data <- sf_data %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T,dig.lab=5),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T,dig.lab=5),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=5),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=5),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T,dig.lab=5)
         )

pal1 <- viridisLite::viridis(12, direction = -1)
# pal2 <- viridisLite::cividis(40)
# pal3 <- viridisLite::inferno(40)
# pal4 <- viridisLite::magma(40)
# pal5 <- viridisLite::plasma(40)
# pal6 <- viridisLite::viridis(40)

opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)
unique(sf_opstine$NAME_2)


#+ include = FALSE 
a<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

a
ggsave(plot = a, 
       filename = "Maps/FINAL2015/Serbia_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



b<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

b
ggsave(plot = b, 
       filename = "Maps/FINAL2015/Serbia_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

PM10<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

PM10
ggsave(plot = PM10, 
       filename = "Maps/FINAL2015/Serbia_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

d<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

d
ggsave(plot = d, 
       filename = "Maps/FINAL2015/Serbia_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



e<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

e
ggsave(plot = e, 
       filename = "Maps/FINAL2015/Serbia_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

f<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

f
ggsave(plot = f, 
       filename = "Maps/FINAL2015/Serbia_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)


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

###########################################################################################

# zoomed maps per city
# Belgrade, Bor, Novi Sad, Pančevo, Smederevo, Užice

# Belgrade

sf_data <- st_read(dsn = "Products/Sum_up_by_cell_by_pollutant.gpkg")

opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)
unique(sf_opstine$NAME_1)

sf_opstine_bg <- sf_opstine %>% dplyr::filter(NAME_1 == "Grad Beograd")

sf_data_beograd <- sf_data[sf_opstine_bg, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )

pal1 <- viridisLite::viridis(12, direction = -1)


#+ include = FALSE 
a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

a.bg
ggsave(plot = a.bg, 
       filename = "Maps/FINAL2015/Zoomed_maps/Belgrade_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



b.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

b.bg
ggsave(plot = b.bg, 
       filename = "Maps/FINAL2015/Zoomed_maps/Belgrade_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

c.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

c.bg
ggsave(plot = c.bg, 
       filename = "Maps/FINAL2015/Zoomed_maps/Belgrade_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

d.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

d.bg
ggsave(plot = d.bg, 
       filename = "Maps/FINAL2015/Zoomed_maps/Belgrade_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



e.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

e.bg
ggsave(plot = e.bg, 
       filename = "Maps/FINAL2015/Zoomed_maps/Belgrade_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

f.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

f.bg
ggsave(plot = f.bg, 
       filename = "Maps/FINAL2015/Zoomed_maps/Belgrade_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps/FINAL2015/Zoomed_maps/Beograd.jpg", 
       width = 40, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Bor
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
unique(sf_opstine$NAME_2)
sf_opstine_bo <- sf_opstine %>% dplyr::filter(NAME_2 == "Bor")

sf_data_bo <- sf_data[sf_opstine_bo, ] 

bo_classes.NOx <- classIntervals(sf_data_bo$NOx, n = 12, style = "fisher")
bo_classes.SO2 <- classIntervals(sf_data_bo$SO2, n = 12, style = "fisher")
bo_classes.PM10 <- classIntervals(sf_data_bo$PM10, n = 12, style = "fisher")
bo_classes.PM2.5 <- classIntervals(sf_data_bo$PM2.5, n = 12, style = "fisher")
bo_classes.NMVOC <- classIntervals(sf_data_bo$NMVOC, n = 12, style = "fisher")
bo_classes.NH3 <- classIntervals(sf_data_bo$NH3, n = 12, style = "fisher")

sf_data_bo <- sf_data_bo %>%
  mutate(percent_class_NOx = cut(NOx, bo_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(SO2, bo_classes.SO2$brks, include.lowest = T, dig.lab=7),
         percent_class_PM10 = cut(PM10, bo_classes.PM10$brks, include.lowest = T, dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, bo_classes.PM2.5$brks, include.lowest = T, dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, bo_classes.NMVOC$brks, include.lowest = T, dig.lab=7),
         percent_class_NH3 = cut(NH3, bo_classes.NH3$brks, include.lowest = T, dig.lab=7)
  )

pal1 <- viridisLite::viridis(12, direction = -1)


#+ include = FALSE 
a.bo<-ggplot() +
  geom_sf(data = sf_data_bo,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bo, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

a.bo
ggsave(plot = a.bo, 
       filename = "Maps/FINAL2015/Zoomed_maps/Bor_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



b.bo<-ggplot() +
  geom_sf(data = sf_data_bo,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bo, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

b.bo
ggsave(plot = b.bo, 
       filename = "Maps/FINAL2015/Zoomed_maps/Bor_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

c.bo<-ggplot() +
  geom_sf(data = sf_data_bo,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bo, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

c.bo
ggsave(plot = c.bo, 
       filename = "Maps/FINAL2015/Zoomed_maps/Bor_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

d.bo<-ggplot() +
  geom_sf(data = sf_data_bo,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bo, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

d.bo
ggsave(plot = d.bo, 
       filename = "Maps/FINAL2015/Zoomed_maps/Bor_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



e.bo<-ggplot() +
  geom_sf(data = sf_data_bo,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bo, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

e.bo
ggsave(plot = e.bo, 
       filename = "Maps/FINAL2015/Zoomed_maps/Bor_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

f.bo<-ggplot() +
  geom_sf(data = sf_data_bo,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_bo, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

f.bo
ggsave(plot = f.bo, 
       filename = "Maps/FINAL2015/Zoomed_maps/Bor_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)
bo <- grid.arrange(a.bo, b.bo, c.bo, d.bo, e.bo, f.bo, ncol = 2)


ggsave(plot = bo, 
       filename = "Maps/FINAL2015/Zoomed_maps/Bor.jpg", 
       width = 40, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Novi Sad
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
unique(sf_opstine$NAME_1)
sf_opstine_ns <- sf_opstine %>% dplyr::filter(NAME_2 == "Novi Sad")

sf_data_ns <- sf_data[sf_opstine_ns, ] 

ns_classes.NOx <- classIntervals(sf_data_ns$NOx, n = 12, style = "fisher")
ns_classes.SO2 <- classIntervals(sf_data_ns$SO2, n = 12, style = "fisher")
ns_classes.PM10 <- classIntervals(sf_data_ns$PM10, n = 12, style = "fisher")
ns_classes.PM2.5 <- classIntervals(sf_data_ns$PM2.5, n = 12, style = "fisher")
ns_classes.NMVOC <- classIntervals(sf_data_ns$NMVOC, n = 12, style = "fisher")
ns_classes.NH3 <- classIntervals(sf_data_ns$NH3, n = 12, style = "fisher")

sf_data_ns <- sf_data_ns %>%
  mutate(percent_class_NOx = cut(NOx, ns_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(SO2, ns_classes.SO2$brks, include.lowest = T, dig.lab=7),
         percent_class_PM10 = cut(PM10, ns_classes.PM10$brks, include.lowest = T, dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, ns_classes.PM2.5$brks, include.lowest = T, dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, ns_classes.NMVOC$brks, include.lowest = T, dig.lab=7),
         percent_class_NH3 = cut(NH3, ns_classes.NH3$brks, include.lowest = T, dig.lab=7)
  )

pal1 <- viridisLite::viridis(12, direction = -1)


#+ include = FALSE 
a.ns<-ggplot() +
  geom_sf(data = sf_data_ns,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_ns, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

a.ns
ggsave(plot = a.ns, 
       filename = "Maps/FINAL2015/Zoomed_maps/Novi Sad_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



b.ns<-ggplot() +
  geom_sf(data = sf_data_ns,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_ns, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

b.ns
ggsave(plot = b.ns, 
       filename = "Maps/FINAL2015/Zoomed_maps/Novi Sad_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

c.ns<-ggplot() +
  geom_sf(data = sf_data_ns,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_ns, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

c.ns
ggsave(plot = c.ns, 
       filename = "Maps/FINAL2015/Zoomed_maps/Novi Sad_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

d.ns<-ggplot() +
  geom_sf(data = sf_data_ns,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_ns, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

d.ns
ggsave(plot = d.ns, 
       filename = "Maps/FINAL2015/Zoomed_maps/Novi Sad_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



e.ns<-ggplot() +
  geom_sf(data = sf_data_ns,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_ns, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

e.ns
ggsave(plot = e.ns, 
       filename = "Maps/FINAL2015/Zoomed_maps/Novi Sad_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

f.ns<-ggplot() +
  geom_sf(data = sf_data_ns,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_ns, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

f.ns
ggsave(plot = f.ns, 
       filename = "Maps/FINAL2015/Zoomed_maps/Novi Sad_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)
ns <- grid.arrange(a.ns, b.ns, c.ns, d.ns, e.ns, f.ns, ncol = 2)

ggsave(plot = ns, 
       filename = "Maps/FINAL2015/Zoomed_maps/NoviSad.jpg", 
       width = 40, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Pančevo
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
unique(sf_opstine$NAME_1)
sf_opstine_pa <- sf_opstine %>% dplyr::filter(NAME_2 == "Pančevo")

sf_data_pa <- sf_data[sf_opstine_pa, ] 
Sys.setlocale(locale = 'Serbian (Latin)')
pa_classes.NOx <- classIntervals(sf_data_pa$NOx, n = 12, style = "fisher")
pa_classes.SO2 <- classIntervals(sf_data_pa$SO2, n = 12, style = "fisher")
pa_classes.PM10 <- classIntervals(sf_data_pa$PM10, n = 12, style = "fisher")
pa_classes.PM2.5 <- classIntervals(sf_data_pa$PM2.5, n = 12, style = "fisher")
pa_classes.NMVOC <- classIntervals(sf_data_pa$NMVOC, n = 12, style = "fisher")
pa_classes.NH3 <- classIntervals(sf_data_pa$NH3, n = 12, style = "fisher")

sf_data_pa <- sf_data_pa %>%
  mutate(percent_class_NOx = cut(NOx, pa_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(SO2, pa_classes.SO2$brks, include.lowest = T, dig.lab=7),
         percent_class_PM10 = cut(PM10, pa_classes.PM10$brks, include.lowest = T, dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, pa_classes.PM2.5$brks, include.lowest = T, dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, pa_classes.NMVOC$brks, include.lowest = T, dig.lab=7),
         percent_class_NH3 = cut(NH3, pa_classes.NH3$brks, include.lowest = T, dig.lab=7)
  )

pal1 <- viridisLite::viridis(12, direction = -1)


#+ include = FALSE 
a.pa<-ggplot() +
  geom_sf(data = sf_data_pa,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_pa, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

a.pa
ggsave(plot = a.pa, 
       filename = "Maps/FINAL2015/Zoomed_maps/Pancevo_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



b.pa<-ggplot() +
  geom_sf(data = sf_data_pa,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_pa, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

b.pa
ggsave(plot = b.pa, 
       filename = "Maps/FINAL2015/Zoomed_maps/Pancevo_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

c.pa<-ggplot() +
  geom_sf(data = sf_data_pa,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_pa, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

c.pa
ggsave(plot = c.pa, 
       filename = "Maps/FINAL2015/Zoomed_maps/Pancevo_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

d.pa<-ggplot() +
  geom_sf(data = sf_data_pa,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_pa, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

d.pa
ggsave(plot = d.pa, 
       filename = "Maps/FINAL2015/Zoomed_maps/Pancevo_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



e.pa<-ggplot() +
  geom_sf(data = sf_data_pa,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_pa, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

e.pa
ggsave(plot = e.pa, 
       filename = "Maps/FINAL2015/Zoomed_maps/Pancevo_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

f.pa<-ggplot() +
  geom_sf(data = sf_data_pa,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_pa, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

f.pa
ggsave(plot = f.pa, 
       filename = "Maps/FINAL2015/Zoomed_maps/Pancevo_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)
pa <- grid.arrange(a.pa, b.pa, c.pa, d.pa, e.pa, f.pa, ncol = 2)

ggsave(plot = pa, 
       filename = "Maps/FINAL2015/Zoomed_maps/Pancevo.jpg", 
       width = 40, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Smederevo
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
unique(sf_opstine$NAME_1)
sf_opstine_sd <- sf_opstine %>% dplyr::filter(NAME_2 == "Smederevo")

sf_data_sd <- sf_data[sf_opstine_sd, ] 
Sys.setlocale(locale = 'Serbian (Latin)')
sd_classes.NOx <- classIntervals(sf_data_sd$NOx, n = 12, style = "fisher")
sd_classes.SO2 <- classIntervals(sf_data_sd$SO2, n = 12, style = "fisher")
sd_classes.PM10 <- classIntervals(sf_data_sd$PM10, n = 12, style = "fisher")
sd_classes.PM2.5 <- classIntervals(sf_data_sd$PM2.5, n = 12, style = "fisher")
sd_classes.NMVOC <- classIntervals(sf_data_sd$NMVOC, n = 12, style = "fisher")
sd_classes.NH3 <- classIntervals(sf_data_sd$NH3, n = 12, style = "fisher")

sf_data_sd <- sf_data_sd %>%
  mutate(percent_class_NOx = cut(NOx, sd_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(SO2, sd_classes.SO2$brks, include.lowest = T, dig.lab=7),
         percent_class_PM10 = cut(PM10, sd_classes.PM10$brks, include.lowest = T, dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, sd_classes.PM2.5$brks, include.lowest = T, dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, sd_classes.NMVOC$brks, include.lowest = T, dig.lab=7),
         percent_class_NH3 = cut(NH3, sd_classes.NH3$brks, include.lowest = T, dig.lab=7)
  )

pal1 <- viridisLite::viridis(12, direction = -1)


#+ include = FALSE 
a.sd<-ggplot() +
  geom_sf(data = sf_data_sd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_sd, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

a.sd
ggsave(plot = a.sd, 
       filename = "Maps/FINAL2015/Zoomed_maps/Smederevo_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



b.sd<-ggplot() +
  geom_sf(data = sf_data_sd,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_sd, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

b.sd
ggsave(plot = b.sd, 
       filename = "Maps/FINAL2015/Zoomed_maps/Smederevo_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

c.sd<-ggplot() +
  geom_sf(data = sf_data_sd,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_sd, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

c.sd
ggsave(plot = c.sd, 
       filename = "Maps/FINAL2015/Zoomed_maps/Smederevo_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

d.sd<-ggplot() +
  geom_sf(data = sf_data_sd,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_sd, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

d.sd
ggsave(plot = d.sd, 
       filename = "Maps/FINAL2015/Zoomed_maps/Smederevo_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



e.sd<-ggplot() +
  geom_sf(data = sf_data_sd,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_sd, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

e.sd
ggsave(plot = e.sd, 
       filename = "Maps/FINAL2015/Zoomed_maps/Smederevo_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

f.sd<-ggplot() +
  geom_sf(data = sf_data_sd,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_sd, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

f.sd
ggsave(plot = f.sd, 
       filename = "Maps/FINAL2015/Zoomed_maps/Smederevo_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)
sd <- grid.arrange(a.sd, b.sd, c.sd, d.sd, e.sd, f.sd, ncol = 2)

ggsave(plot = sd, 
       filename = "Maps/FINAL2015/Zoomed_maps/Smederevo.jpg", 
       width = 40, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Užice
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
unique(sf_opstine$NAME_1)
sf_opstine_uz <- sf_opstine %>% dplyr::filter(NAME_2 == "Užice")

sf_data_uz <- sf_data[sf_opstine_uz, ] 
Sys.setlocale(locale = 'Serbian (Latin)')
uz_classes.NOx <- classIntervals(sf_data_uz$NOx, n = 12, style = "fisher")
uz_classes.SO2 <- classIntervals(sf_data_uz$SO2, n = 12, style = "fisher")
uz_classes.PM10 <- classIntervals(sf_data_uz$PM10, n = 12, style = "fisher")
uz_classes.PM2.5 <- classIntervals(sf_data_uz$PM2.5, n = 12, style = "fisher")
uz_classes.NMVOC <- classIntervals(sf_data_uz$NMVOC, n = 12, style = "fisher")
uz_classes.NH3 <- classIntervals(sf_data_uz$NH3, n = 12, style = "fisher")

sf_data_uz <- sf_data_uz %>%
  mutate(percent_class_NOx = cut(NOx, uz_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(SO2, uz_classes.SO2$brks, include.lowest = T, dig.lab=7),
         percent_class_PM10 = cut(PM10, uz_classes.PM10$brks, include.lowest = T, dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, uz_classes.PM2.5$brks, include.lowest = T, dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, uz_classes.NMVOC$brks, include.lowest = T, dig.lab=7),
         percent_class_NH3 = cut(NH3, uz_classes.NH3$brks, include.lowest = T, dig.lab=7)
  )

pal1 <- viridisLite::viridis(12, direction = -1)


#+ include = FALSE 
a.uz<-ggplot() +
  geom_sf(data = sf_data_uz,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_uz, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

a.uz
ggsave(plot = a.uz, 
       filename = "Maps/FINAL2015/Zoomed_maps/Uzice_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



b.uz<-ggplot() +
  geom_sf(data = sf_data_uz,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_uz, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

b.uz
ggsave(plot = b.uz, 
       filename = "Maps/FINAL2015/Zoomed_maps/Uzice_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

c.uz<-ggplot() +
  geom_sf(data = sf_data_uz,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_uz, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

c.uz
ggsave(plot = c.uz, 
       filename = "Maps/FINAL2015/Zoomed_maps/Uzice_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

d.uz<-ggplot() +
  geom_sf(data = sf_data_uz,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_uz, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

d.uz
ggsave(plot = d.uz, 
       filename = "Maps/FINAL2015/Zoomed_maps/Uzice_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



e.uz<-ggplot() +
  geom_sf(data = sf_data_uz,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_uz, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

e.uz
ggsave(plot = e.uz, 
       filename = "Maps/FINAL2015/Zoomed_maps/Uzice_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

f.uz<-ggplot() +
  geom_sf(data = sf_data_uz,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2020)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_uz, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

f.uz
ggsave(plot = f.uz, 
       filename = "Maps/FINAL2015/Zoomed_maps/Uzice_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)
aaa <- grid.arrange(a.uz, b.uz, c.uz, d.uz, e.uz, f.uz, ncol = 2)
ggsave(plot = aaa, 
       filename = "Maps/FINAL2015/Zoomed_maps/Uzice.jpg", 
       width = 40, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



sf_opstine_bg$NAME_1[2:17] <- NA
library(ggspatial)
library(ggsflabel)
# Opstine
sf_opstine_sel <- rbind(sf_opstine_bo, sf_opstine_ns, sf_opstine_pa, sf_opstine_sd, sf_opstine_uz)
map_cities <- ggplot() +
  geom_sf(data = sf_opstine_sel, fill = "red") +
  geom_sf(data = sf_opstine_bg, fill = "red")+
  #scale_fill_manual(name = "Cities")+
  labs(x = NULL, y = NULL,
       title = "Municipalities with specific air quality plans",
       subtitle = "Territory of the Republic of Serbia",
       caption = "UBFCE (2020)",
       fill = "Cities") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  geom_sf_label_repel(data = sf_opstine_sel, aes(label = NAME_2), nudge_x = 0.3, nudge_y = 0.3, seed = 10)+
  geom_sf_label_repel(data = sf_opstine_bg, aes(label = NAME_1))
map_cities

ggsave(plot = map_cities, 
       filename = "Maps/FINAL2015/Zoomed_maps/cities.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)
