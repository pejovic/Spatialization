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
library(ggsflabel)
library(ggspatial)



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Maps by category
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

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


opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_energy <- st_read(dsn = "2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/1A1 - Energy_2030_WAM_B.gpkg")

classes.NOx <- classIntervals(sf_data_energy$NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data_energy$SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data_energy$PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_energy$PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_energy$NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data_energy$NH3, n = 12, style = "fisher")

sf_data_energy <- sf_data_energy %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T,dig.lab=7),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T,dig.lab=7)
  )

pal1 <- viridisLite::inferno(12, direction = -1)
pal2 <- viridisLite::inferno(3, direction = -1)
pal3 <- viridisLite::viridis(12, direction = -1)
pal4 <- viridisLite::viridis(12, direction = -1)
pal5 <- viridisLite::viridis(12, direction = -1)
pal6 <- viridisLite::viridis(12, direction = -1)



#+ include = FALSE 
a1<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NOx",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
a1

b1<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - SO2",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
b1
c1<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM10",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
c1
d1<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM2.5",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
d1

e1<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NMVOC",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
e1
f1<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NH3",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
f1
energy_map <- grid.arrange(a1, b1, c1, d1, e1, f1, ncol = 2, nrow = 3)



ggsave(plot = a1, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A1 - Energy_2030_WAM_B_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

ggsave(plot = b1, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A1 - Energy_2030_WAM_B_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = c1, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A1 - Energy_2030_WAM_B_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = d1, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A1 - Energy_2030_WAM_B_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = e1, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A1 - Energy_2030_WAM_B_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = f1, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A1 - Energy_2030_WAM_B_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)





# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_industry <- st_read(dsn = "2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/1A2 - Industry_2030_WAM_B.gpkg")

classes.NOx <- classIntervals(sf_data_industry$NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data_industry$SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data_industry$PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_industry$PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_industry$NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data_industry$NH3, n = 12, style = "fisher")

sf_data_industry <- sf_data_industry %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T,dig.lab=7),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T,dig.lab=7)
  )

pal1 <- viridisLite::inferno(12, direction = -1)
pal2 <- viridisLite::viridis(30)
pal3 <- viridisLite::viridis(30)
pal4 <- viridisLite::viridis(30)
pal5 <- viridisLite::viridis(30)
pal6 <- viridisLite::viridis(30)

#+ include = FALSE 
a2<-ggplot() +
  geom_sf(data = sf_data_industry,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NOx",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

a2
b2<-ggplot() +
  geom_sf(data = sf_data_industry,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - SO2",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
b2
c2<-ggplot() +
  geom_sf(data = sf_data_industry,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM10",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
c2
d2<-ggplot() +
  geom_sf(data = sf_data_industry,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM2.5",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
d2
e2<-ggplot() +
  geom_sf(data = sf_data_industry,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NMVOC",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
e2
f2<-ggplot() +
  geom_sf(data = sf_data_industry,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NH3",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
f2
industry_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)

ggsave(plot = a2, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A2 - Industry_2030_WAM_B_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

ggsave(plot = b2, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A2 - Industry_2030_WAM_B_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = c2, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A2 - Industry_2030_WAM_B_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = d2, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A2 - Industry_2030_WAM_B_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = e2, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A2 - Industry_2030_WAM_B_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = f2, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A2 - Industry_2030_WAM_B_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_transport <- st_read(dsn = "2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/1A3 - Transport_2030_WAM_B.gpkg")

classes.NOx <- classIntervals(sf_data_transport$NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data_transport$SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data_transport$PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_transport$PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_transport$NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data_transport$NH3, n = 12, style = "fisher")

sf_data_transport <- sf_data_transport %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T)
  )

pal1 <- viridisLite::inferno(12, direction = -1)
pal2 <- viridisLite::viridis(12)
pal3 <- viridisLite::viridis(12)
pal4 <- viridisLite::viridis(12)
pal5 <- viridisLite::viridis(12)
pal6 <- viridisLite::viridis(12)

#+ include = FALSE 
a3<-ggplot() +
  geom_sf(data = sf_data_transport,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NOx",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

b3<-ggplot() +
  geom_sf(data = sf_data_transport,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - SO2",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

c3<-ggplot() +
  geom_sf(data = sf_data_transport,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM10",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

d3<-ggplot() +
  geom_sf(data = sf_data_transport,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM2.5",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

e3<-ggplot() +
  geom_sf(data = sf_data_transport,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NMVOC",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

f3<-ggplot() +
  geom_sf(data = sf_data_transport,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NH3",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

transport_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)



ggsave(plot = a3, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A3 - Transport_2030_WAM_B_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

ggsave(plot = b3, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A3 - Transport_2030_WAM_B_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = c3, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A3 - Transport_2030_WAM_B_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = d3, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A3 - Transport_2030_WAM_B_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = e3, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A3 - Transport_2030_WAM_B_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = f3, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A3 - Transport_2030_WAM_B_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_residential <- st_read(dsn = "2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/1A4 - Residential-Tertiary_2030_WAM_B.gpkg")

classes.NOx <- classIntervals(sf_data_residential$NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data_residential$SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data_residential$PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_residential$PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_residential$NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data_residential$NH3, n = 12, style = "fisher")

sf_data_residential <- sf_data_residential %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T,dig.lab=7),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T,dig.lab=7)
  )

pal1 <- viridisLite::inferno(12, direction = -1)
pal2 <- viridisLite::viridis(12)
pal3 <- viridisLite::viridis(12)
pal4 <- viridisLite::viridis(12)
pal5 <- viridisLite::viridis(12)
pal6 <- viridisLite::viridis(12)

#+ include = FALSE 
a4<-ggplot() +
  geom_sf(data = sf_data_residential,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NOx",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
a4
b4<-ggplot() +
  geom_sf(data = sf_data_residential,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - SO2",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
b4
c4<-ggplot() +
  geom_sf(data = sf_data_residential,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM10",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
c4
d4<-ggplot() +
  geom_sf(data = sf_data_residential,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM2.5",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
d4
e4<-ggplot() +
  geom_sf(data = sf_data_residential,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NMVOC",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
e4
f4<-ggplot() +
  geom_sf(data = sf_data_residential,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NH3",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
f4
residential_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)



ggsave(plot = a4, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A4 - Residential-Tertiary_2030_WAM_B_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

ggsave(plot = b4, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A4 - Residential-Tertiary_2030_WAM_B_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = c4, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A4 - Residential-Tertiary_2030_WAM_B_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = d4, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A4 - Residential-Tertiary_2030_WAM_B_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = e4, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A4 - Residential-Tertiary_2030_WAM_B_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = f4, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1A4 - Residential-Tertiary_2030_WAM_B_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_fugitive <- st_read(dsn = "2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/1B - Fugitive emissions_2030_WAM_B.gpkg")

summary(sf_data_fugitive)

classes.NOx <- classIntervals(sf_data_fugitive$NOx, n = 2, style = "fisher")
classes.SO2 <- classIntervals(sf_data_fugitive$SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data_fugitive$PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_fugitive$PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_fugitive$NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data_fugitive$NH3, n = 12, style = "fisher")

sf_data_fugitive <- sf_data_fugitive %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T,dig.lab=7),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T,dig.lab=7)
  )

pal1 <- viridisLite::inferno(12, direction = -1)
pal2 <- viridisLite::inferno(2, direction = -1)
pal3 <- viridisLite::viridis(12)
pal4 <- viridisLite::viridis(12)
pal5 <- viridisLite::viridis(12)
pal6 <- viridisLite::viridis(12)


sf_data_fugitive$percent_class_NOx <- "[0,0]"

sf_data_fugitive$percent_class_SO2 <- "[0,0]"

sf_data_fugitive$percent_class_NH3 <- "[0,0]"


#+ include = FALSE 
a5<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal2,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NOx",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
a5
b5<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - SO2",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
b5
c5<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM10",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
c5
d5<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM2.5",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
d5
e5<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NMVOC",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
e5
f5<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NH3",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
f5
fugitive_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)



ggsave(plot = a5, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1B - Fugitive emissions_2030_WAM_B_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

ggsave(plot = b5, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1B - Fugitive emissions_2030_WAM_B_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = c5, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1B - Fugitive emissions_2030_WAM_B_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = d5, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1B - Fugitive emissions_2030_WAM_B_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = e5, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1B - Fugitive emissions_2030_WAM_B_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = f5, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_1B - Fugitive emissions_2030_WAM_B_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)





# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_other <- st_read(dsn = "2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/2 - Other processes_2030_WAM_B.gpkg")

summary(sf_data_other)

classes.NOx <- classIntervals(sf_data_other$NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data_other$SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data_other$PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_other$PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_other$NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data_other$NH3, n = 12, style = "fisher")

sf_data_other <- sf_data_other %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T,dig.lab=7),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T,dig.lab=7)
  )

pal1 <- viridisLite::inferno(12, direction = -1)
pal2 <- viridisLite::viridis(12)
pal3 <- viridisLite::viridis(12)
pal4 <- viridisLite::viridis(12)
pal5 <- viridisLite::viridis(12)
pal6 <- viridisLite::viridis(12)

sf_data_other$percent_class_SO2 <- "[0,0]"


a6<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NOx",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
a6
b6<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - SO2",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
b6
c6<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM10",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
c6
d6<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM2.5",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
d6
e6<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NMVOC",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
e6
f6<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NH3",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
f6
other_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)




ggsave(plot = a6, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_2 - Other processes_2030_WAM_B_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

ggsave(plot = b6, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_2 - Other processes_2030_WAM_B_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = c6, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_2 - Other processes_2030_WAM_B_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = d6, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_2 - Other processes_2030_WAM_B_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = e6, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_2 - Other processes_2030_WAM_B_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = f6, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_2 - Other processes_2030_WAM_B_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)















# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_agriculture <- st_read(dsn = "2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/3 - Agriculture_2030_WAM_B.gpkg")

summary(sf_data_agriculture)

classes.NOx <- classIntervals(sf_data_agriculture$NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data_agriculture$SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data_agriculture$PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_agriculture$PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_agriculture$NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data_agriculture$NH3, n = 12, style = "fisher")

sf_data_agriculture <- sf_data_agriculture %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T,dig.lab=7),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T,dig.lab=7)
  )

pal1 <- viridisLite::inferno(12, direction = -1)
pal2 <- viridisLite::viridis(12)
pal3 <- viridisLite::viridis(12)
pal4 <- viridisLite::viridis(12)
pal5 <- viridisLite::viridis(12)
pal6 <- viridisLite::viridis(12)

a7<-ggplot() +
  geom_sf(data = sf_data_agriculture,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NOx",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
a7
b7<-ggplot() +
  geom_sf(data = sf_data_agriculture,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - SO2",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
b7
c7<-ggplot() +
  geom_sf(data = sf_data_agriculture,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM10",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
c7
d7<-ggplot() +
  geom_sf(data = sf_data_agriculture,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM2.5",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
d7
e7<-ggplot() +
  geom_sf(data = sf_data_agriculture,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NMVOC",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
e7
f7<-ggplot() +
  geom_sf(data = sf_data_agriculture,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NH3",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
f7
agriculture_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)





ggsave(plot = a7, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_3 - Agriculture_2030_WAM_B_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

ggsave(plot = b7, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_3 - Agriculture_2030_WAM_B_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = c7, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_3 - Agriculture_2030_WAM_B_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = d7, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_3 - Agriculture_2030_WAM_B_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = e7, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_3 - Agriculture_2030_WAM_B_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = f7, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_3 - Agriculture_2030_WAM_B_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)






# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_waste <- st_read(dsn = "2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/5 - Waste_2030_WAM_B.gpkg")

summary(sf_data_waste)


classes.NOx <- classIntervals(sf_data_waste$NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data_waste$SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data_waste$PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_waste$PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_waste$NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data_waste$NH3, n = 12, style = "fisher")

sf_data_waste <- sf_data_waste %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T,dig.lab=7),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T,dig.lab=7)
  )

pal1 <- viridisLite::inferno(12, direction = -1)
pal2 <- viridisLite::inferno(3, direction = -1)
pal3 <- viridisLite::viridis(12)
pal4 <- viridisLite::viridis(12)
pal5 <- viridisLite::viridis(12)
pal6 <- viridisLite::viridis(12)


a8<-ggplot() +
  geom_sf(data = sf_data_waste,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal2,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NOx",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
a8
b8<-ggplot() +
  geom_sf(data = sf_data_waste,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal2,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - SO2",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
b8
c8<-ggplot() +
  geom_sf(data = sf_data_waste,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal2,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM10",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
c8
d8<-ggplot() +
  geom_sf(data = sf_data_waste,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal2,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - PM2.5",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
d8
e8<-ggplot() +
  geom_sf(data = sf_data_waste,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NMVOC",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
e8
f8<-ggplot() +
  geom_sf(data = sf_data_waste,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization (2030_WAM_B) - NH3",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2021)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        legend.position = "bottom", ###################### legend
        panel.background = element_blank())  +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
f8
waste_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)




ggsave(plot = a8, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_5 - Waste_2030_WAM_B_NOx.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

ggsave(plot = b8, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_5 - Waste_2030_WAM_B_SO2.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = c8, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_5 - Waste_2030_WAM_B_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = d8, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_5 - Waste_2030_WAM_B_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = e8, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_5 - Waste_2030_WAM_B_NMVOC.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)
ggsave(plot = f8, 
       filename = "2030_WAM_B/Maps_2030_WAM_B/Maps_per_each_GNFR_sector/Map_5 - Waste_2030_WAM_B_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)









