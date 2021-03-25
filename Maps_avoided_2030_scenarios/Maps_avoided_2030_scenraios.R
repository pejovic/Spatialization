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




# -------------------------------------------------------------------------------------------------------------------------------
# Data
# -------------------------------------------------------------------------------------------------------------------------------

# It would be indeed very helpful for us if you can 
# provide maps presenting the 
# emissions avoided from WEM to WAM A and then from WAM A to WAM B.

vars <- c("av_NOx", "av_SO2", "av_PM10", "av_PM2.5", "av_NMVOC", "av_NH3")

sf_data_2030 <- st_read(dsn = "2030/Products_2030/Sum_up_by_cell_by_pollutant_2030.gpkg")
sf_data_2030_A <- st_read(dsn = "2030_WAM_A/Products_2030_WAM_A/Sum_up_by_cell_by_pollutant_2030_WAM_A.gpkg")
sf_data_2030_B <- st_read(dsn = "2030_WAM_B/Products_2030_WAM_B/Sum_up_by_cell_by_pollutant_2030_WAM_B.gpkg")


ems_av_2030_2030A <- sf_data_2030 %>%
  dplyr::mutate(av_NOx = NOx - sf_data_2030_A$NOx,
                av_SO2 = SO2 - sf_data_2030_A$SO2,
                av_PM10 = PM10 - sf_data_2030_A$PM10,
                av_PM2.5 = PM2.5 - sf_data_2030_A$PM2.5,
                av_NMVOC = NMVOC - sf_data_2030_A$NMVOC,
                av_NH3 = NH3 - sf_data_2030_A$NH3) %>%
  dplyr::select(ID, vars)


ems_av_2030A_2030B <- sf_data_2030_A %>%
  dplyr::mutate(av_NOx = NOx - sf_data_2030_B$NOx,
                av_SO2 = SO2 - sf_data_2030_B$SO2,
                av_PM10 = PM10 - sf_data_2030_B$PM10,
                av_PM2.5 = PM2.5 - sf_data_2030_B$PM2.5,
                av_NMVOC = NMVOC - sf_data_2030_B$NMVOC,
                av_NH3 = NH3 - sf_data_2030_B$NH3) %>%
  dplyr::select(ID, vars)





# -------------------------------------------------------------------------------------------------------------------------------









# -------------------------------------------------------------------------------------------------------------------------------
# Maps
# -------------------------------------------------------------------------------------------------------------------------------


# emissions avoided from WEM to WAM A

sf_data <- ems_av_2030_2030A

classes.NOx <- classIntervals(sf_data$av_NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data$av_SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data$av_PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data$av_PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data$av_NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data$av_NH3, n = 12, style = "fisher")

sf_data <- sf_data %>%
  mutate(percent_class_NOx = cut(av_NOx, classes.NOx$brks, include.lowest = T,dig.lab=5),
         percent_class_SO2 = cut(av_SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, classes.PM10$brks, include.lowest = T,dig.lab=5),
         percent_class_PM2.5 = cut(av_PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=5),
         percent_class_NMVOC = cut(av_NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=5),
         percent_class_NH3 = cut(av_NH3, classes.NH3$brks, include.lowest = T,dig.lab=5)
  )


#sf_data$percent_class_NH3 <- "[0,0]"


pal1 <- viridisLite::magma(12, direction = -1)


opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)
unique(sf_opstine$NAME_2)


a<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WEM-WAM_A_2030/Serbia_2030_WEM-WAM_A_NOx.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WEM-WAM_A_2030/Serbia_2030_WEM-WAM_A_SO2.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WEM-WAM_A_2030/Serbia_2030_WEM-WAM_A_PM10.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WEM-WAM_A_2030/Serbia_2030_WEM-WAM_A_PM2.5.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WEM-WAM_A_2030/Serbia_2030_WEM-WAM_A_NMVOC.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WEM-WAM_A_2030/Serbia_2030_WEM-WAM_A_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)





# emissions avoided from WAM A to WAM B


sf_data <- ems_av_2030A_2030B

classes.NOx <- classIntervals(sf_data$av_NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data$av_SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data$av_PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data$av_PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data$av_NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data$av_NH3, n = 12, style = "fisher")

sf_data <- sf_data %>%
  mutate(percent_class_NOx = cut(av_NOx, classes.NOx$brks, include.lowest = T,dig.lab=5),
         percent_class_SO2 = cut(av_SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, classes.PM10$brks, include.lowest = T,dig.lab=5),
         percent_class_PM2.5 = cut(av_PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=5),
         percent_class_NMVOC = cut(av_NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=5),
         percent_class_NH3 = cut(av_NH3, classes.NH3$brks, include.lowest = T,dig.lab=5)
  )


#sf_data$percent_class_NH3 <- "[0,0]"


pal1 <- viridisLite::magma(12, direction = -1)


opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)
unique(sf_opstine$NAME_2)


a<-ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WAM_A-WAM_B_2030/Serbia_2030_WAM_A-WAM_B_NOx.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WAM_A-WAM_B_2030/Serbia_2030_WAM_A-WAM_B_SO2.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WAM_A-WAM_B_2030/Serbia_2030_WAM_A-WAM_B_PM10.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WAM_A-WAM_B_2030/Serbia_2030_WAM_A-WAM_B_PM2.5.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WAM_A-WAM_B_2030/Serbia_2030_WAM_A-WAM_B_NMVOC.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_WAM_A-WAM_B_2030/Serbia_2030_WAM_A-WAM_B_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)








# -------------------------------------------------------------------------------------------------------------------------------





# -------------------------------------------------------------------------------------------------------------------------------
# Maps per cities
# -------------------------------------------------------------------------------------------------------------------------------
ems_av_2030_2030A
ems_av_2030A_2030B


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

pal1 <- viridisLite::magma(12, direction = -1)



#plot_function_city <- function(podaci = podaci, e.element = e.element, putanja = "", naziv = ""){

#}

# problem u menjanju imena promenljivih iz data.frame-a





# BG 1
sf_data_beograd <- ems_av_2030_2030A[sf_opstine_bg, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WEM-WAM_A_2030/Belgrade_2030_WEM-WAM_A_2030_NOx.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WEM-WAM_A_2030/Belgrade_2030_WEM-WAM_A_2030_SO2.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WEM-WAM_A_2030/Belgrade_2030_WEM-WAM_A_2030_PM10.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WEM-WAM_A_2030/Belgrade_2030_WEM-WAM_A_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WEM-WAM_A_2030/Belgrade_2030_WEM-WAM_A_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WEM-WAM_A_2030/Belgrade_2030_WEM-WAM_A_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WEM-WAM_A_2030/Belgrade.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)





# BG 2
sf_data_beograd <- ems_av_2030A_2030B[sf_opstine_bg, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WAM_A-WAM_B_2030/Belgrade_2030_WEM-WAM_A_2030_NOx.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WAM_A-WAM_B_2030/Belgrade_2030_WAM_A-WAM_B_2030_2030_SO2.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WAM_A-WAM_B_2030/Belgrade_2030_WAM_A-WAM_B_2030_2030_PM10.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WAM_A-WAM_B_2030/Belgrade_2030_WAM_A-WAM_B_2030_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WAM_A-WAM_B_2030/Belgrade_2030_WAM_A-WAM_B_2030_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2021)") +
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
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WAM_A-WAM_B_2030/Belgrade_2030_WAM_A-WAM_B_2030_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Belgrade/Maps_WAM_A-WAM_B_2030/Belgrade.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)




# BOR 1

sf_opstine_bo

sf_data_beograd <- ems_av_2030_2030A[sf_opstine_bo, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#a.bg
ggsave(plot = a.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WEM-WAM_A_2030/Bor_2030_WEM-WAM_A_2030_NOx.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#b.bg
ggsave(plot = b.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WEM-WAM_A_2030/Bor_2030_WEM-WAM_A_2030_SO2.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#c.bg
ggsave(plot = c.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WEM-WAM_A_2030/Bor_2030_WEM-WAM_A_2030_PM10.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#d.bg
ggsave(plot = d.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WEM-WAM_A_2030/Bor_2030_WEM-WAM_A_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#e.bg
ggsave(plot = e.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WEM-WAM_A_2030/Bor_2030_WEM-WAM_A_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#f.bg
ggsave(plot = f.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WEM-WAM_A_2030/Bor_2030_WEM-WAM_A_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WEM-WAM_A_2030/Bor_WEM-WAM_A_2030.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)











# BOR 2
sf_data_beograd <- ems_av_2030A_2030B[sf_opstine_bo, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#a.bg
ggsave(plot = a.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WAM_A-WAM_B_2030/Bor_2030_WAM_A-WAM_B_2030_NOx.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#b.bg
ggsave(plot = b.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WAM_A-WAM_B_2030/Bor_2030_WAM_A-WAM_B_2030_2030_SO2.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#c.bg
ggsave(plot = c.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WAM_A-WAM_B_2030/Bor_2030_WAM_A-WAM_B_2030_2030_PM10.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#d.bg
ggsave(plot = d.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WAM_A-WAM_B_2030/Bor_2030_WAM_A-WAM_B_2030_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#e.bg
ggsave(plot = e.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WAM_A-WAM_B_2030/Bor_2030_WAM_A-WAM_B_2030_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Bor",
       caption = "UBFCE (2021)") +
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

#f.bg
ggsave(plot = f.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WAM_A-WAM_B_2030/Bor_2030_WAM_A-WAM_B_2030_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Bor/Maps_WAM_A-WAM_B_2030/Bor_WAM_A-WAM_B_2030.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)




# NS 1

sf_opstine_ns

sf_data_beograd <- ems_av_2030_2030A[sf_opstine_ns, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#a.bg
ggsave(plot = a.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WEM-WAM_A_2030/Novi Sad_2030_WEM-WAM_A_2030_NOx.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#b.bg
ggsave(plot = b.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WEM-WAM_A_2030/Novi Sad_2030_WEM-WAM_A_2030_SO2.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#c.bg
ggsave(plot = c.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WEM-WAM_A_2030/Novi Sad_2030_WEM-WAM_A_2030_PM10.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#d.bg
ggsave(plot = d.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WEM-WAM_A_2030/Novi Sad_2030_WEM-WAM_A_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#e.bg
ggsave(plot = e.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WEM-WAM_A_2030/Novi Sad_2030_WEM-WAM_A_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#f.bg
ggsave(plot = f.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WEM-WAM_A_2030/Novi Sad_2030_WEM-WAM_A_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WEM-WAM_A_2030/Novi Sad_WEM-WAM_A_2030.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)











# NS 2
sf_data_beograd <- ems_av_2030A_2030B[sf_opstine_ns, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#a.bg
ggsave(plot = a.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WAM_A-WAM_B_2030/Novi Sad_2030_WAM_A-WAM_B_2030_NOx.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#b.bg
ggsave(plot = b.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WAM_A-WAM_B_2030/Novi Sad_2030_WAM_A-WAM_B_2030_2030_SO2.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#c.bg
ggsave(plot = c.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WAM_A-WAM_B_2030/Novi Sad_2030_WAM_A-WAM_B_2030_2030_PM10.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#d.bg
ggsave(plot = d.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WAM_A-WAM_B_2030/Novi Sad_2030_WAM_A-WAM_B_2030_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#e.bg
ggsave(plot = e.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WAM_A-WAM_B_2030/Novi Sad_2030_WAM_A-WAM_B_2030_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Novi Sad",
       caption = "UBFCE (2021)") +
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

#f.bg
ggsave(plot = f.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WAM_A-WAM_B_2030/Novi Sad_2030_WAM_A-WAM_B_2030_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Novi Sad/Maps_WAM_A-WAM_B_2030/Novi Sad_WAM_A-WAM_B_2030.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)




# PA 1

sf_opstine_pa

sf_data_beograd <- ems_av_2030_2030A[sf_opstine_pa, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#a.bg
ggsave(plot = a.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WEM-WAM_A_2030/Pancevo_2030_WEM-WAM_A_2030_NOx.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#b.bg
ggsave(plot = b.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WEM-WAM_A_2030/Pancevo_2030_WEM-WAM_A_2030_SO2.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#c.bg
ggsave(plot = c.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WEM-WAM_A_2030/Pancevo_2030_WEM-WAM_A_2030_PM10.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#d.bg
ggsave(plot = d.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WEM-WAM_A_2030/Pancevo_2030_WEM-WAM_A_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#e.bg
ggsave(plot = e.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WEM-WAM_A_2030/Pancevo_2030_WEM-WAM_A_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#f.bg
ggsave(plot = f.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WEM-WAM_A_2030/Pancevo_2030_WEM-WAM_A_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WEM-WAM_A_2030/Pancevo_WEM-WAM_A_2030.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)











# PA 2
sf_data_beograd <- ems_av_2030A_2030B[sf_opstine_pa, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#a.bg
ggsave(plot = a.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WAM_A-WAM_B_2030/Pancevo_2030_WAM_A-WAM_B_2030_NOx.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#b.bg
ggsave(plot = b.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WAM_A-WAM_B_2030/Pancevo_2030_WAM_A-WAM_B_2030_2030_SO2.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#c.bg
ggsave(plot = c.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WAM_A-WAM_B_2030/Pancevo_2030_WAM_A-WAM_B_2030_2030_PM10.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#d.bg
ggsave(plot = d.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WAM_A-WAM_B_2030/Pancevo_2030_WAM_A-WAM_B_2030_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#e.bg
ggsave(plot = e.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WAM_A-WAM_B_2030/Pancevo_2030_WAM_A-WAM_B_2030_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo",
       caption = "UBFCE (2021)") +
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

#f.bg
ggsave(plot = f.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WAM_A-WAM_B_2030/Pancevo_2030_WAM_A-WAM_B_2030_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Pancevo/Maps_WAM_A-WAM_B_2030/Pancevo_WAM_A-WAM_B_2030.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)





# SD 1

sf_opstine_sd

sf_data_beograd <- ems_av_2030_2030A[sf_opstine_sd, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#a.bg
ggsave(plot = a.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WEM-WAM_A_2030/Smederevo_2030_WEM-WAM_A_2030_NOx.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#b.bg
ggsave(plot = b.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WEM-WAM_A_2030/Smederevo_2030_WEM-WAM_A_2030_SO2.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#c.bg
ggsave(plot = c.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WEM-WAM_A_2030/Smederevo_2030_WEM-WAM_A_2030_PM10.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#d.bg
ggsave(plot = d.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WEM-WAM_A_2030/Smederevo_2030_WEM-WAM_A_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#e.bg
ggsave(plot = e.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WEM-WAM_A_2030/Smederevo_2030_WEM-WAM_A_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#f.bg
ggsave(plot = f.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WEM-WAM_A_2030/Smederevo_2030_WEM-WAM_A_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WEM-WAM_A_2030/Smederevo_WEM-WAM_A_2030.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)











# SD 2
sf_data_beograd <- ems_av_2030A_2030B[sf_opstine_sd, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#a.bg
ggsave(plot = a.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WAM_A-WAM_B_2030/Smederevo_2030_WAM_A-WAM_B_2030_NOx.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#b.bg
ggsave(plot = b.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WAM_A-WAM_B_2030/Smederevo_2030_WAM_A-WAM_B_2030_2030_SO2.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#c.bg
ggsave(plot = c.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WAM_A-WAM_B_2030/Smederevo_2030_WAM_A-WAM_B_2030_2030_PM10.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#d.bg
ggsave(plot = d.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WAM_A-WAM_B_2030/Smederevo_2030_WAM_A-WAM_B_2030_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#e.bg
ggsave(plot = e.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WAM_A-WAM_B_2030/Smederevo_2030_WAM_A-WAM_B_2030_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Smederevo",
       caption = "UBFCE (2021)") +
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

#f.bg
ggsave(plot = f.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WAM_A-WAM_B_2030/Smederevo_2030_WAM_A-WAM_B_2030_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Smederevo/Maps_WAM_A-WAM_B_2030/Smederevo_WAM_A-WAM_B_2030.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



# UE 1

sf_opstine_uz

sf_data_beograd <- ems_av_2030_2030A[sf_opstine_uz, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#a.bg
ggsave(plot = a.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WEM-WAM_A_2030/Uzice_2030_WEM-WAM_A_2030_NOx.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#b.bg
ggsave(plot = b.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WEM-WAM_A_2030/Uzice_2030_WEM-WAM_A_2030_SO2.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#c.bg
ggsave(plot = c.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WEM-WAM_A_2030/Uzice_2030_WEM-WAM_A_2030_PM10.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#d.bg
ggsave(plot = d.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WEM-WAM_A_2030/Uzice_2030_WEM-WAM_A_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#e.bg
ggsave(plot = e.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WEM-WAM_A_2030/Uzice_2030_WEM-WAM_A_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WEM to WAM A (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#f.bg
ggsave(plot = f.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WEM-WAM_A_2030/Uzice_2030_WEM-WAM_A_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WEM-WAM_A_2030/Uzice_WEM-WAM_A_2030.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)











# UE 2
sf_data_beograd <- ems_av_2030A_2030B[sf_opstine_uz, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$av_NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$av_SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$av_PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$av_PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$av_NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$av_NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(av_NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(av_SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(av_PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(av_PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(av_NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(av_NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )


a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NOx",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#a.bg
ggsave(plot = a.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WAM_A-WAM_B_2030/Uzice_2030_WAM_A-WAM_B_2030_NOx.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - SO2",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#b.bg
ggsave(plot = b.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WAM_A-WAM_B_2030/Uzice_2030_WAM_A-WAM_B_2030_2030_SO2.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM10",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#c.bg
ggsave(plot = c.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WAM_A-WAM_B_2030/Uzice_2030_WAM_A-WAM_B_2030_2030_PM10.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - PM2.5",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#d.bg
ggsave(plot = d.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WAM_A-WAM_B_2030/Uzice_2030_WAM_A-WAM_B_2030_2030_PM2.5.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NMVOC",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#e.bg
ggsave(plot = e.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WAM_A-WAM_B_2030/Uzice_2030_WAM_A-WAM_B_2030_2030_NMVOC.jpg", 
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
       title = "Emissions avoided from WAM A to WAM B (2030 Projeted Year) - NH3",
       subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Užice",
       caption = "UBFCE (2021)") +
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

#f.bg
ggsave(plot = f.bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WAM_A-WAM_B_2030/Uzice_2030_WAM_A-WAM_B_2030_2030_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Maps_avoided_2030_scenarios/Maps_per_cities/Uzice/Maps_WAM_A-WAM_B_2030/Uzice_WAM_A-WAM_B_2030.jpg", 
       width = 50, 
       height = 40, 
       units = "cm", 
       device = "jpeg",
       dpi=600)



# -------------------------------------------------------------------------------------------------------------------------------



