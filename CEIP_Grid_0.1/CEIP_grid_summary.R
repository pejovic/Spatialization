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

sf_data <- st_read(dsn = "Products/Sum_up_by_cell_by_pollutant.gpkg")

grid_0.1 <- st_read(dsn = "CEIP_Grid_0.1/Grid_0.1deg_WGS84.gpkg")

grid_0.1 %<>% dplyr::select(Name) %>% dplyr::mutate(ID = substring(Name, 22)) %>% st_zm(drop = TRUE)


sf_cent_0.05 <- st_centroid(sf_data) 
# mapview(sf_cent_0.05) + mapview(sf_data)

# mapedit za ceetri tacke u pogranicnom delu
mped <- mapedit::editFeatures(sf_cent_0.05)
mped <- mapedit::editFeatures(mped)

# st_write(mped, dsn = "CEIP_Grid_0.1/CEIP/tacke_ccentroidi_2015.gpkg", layer = "tac_cent_2015_edit")

p.grid_0.1 <- grid_0.1 %>% # Spatialization
  st_join(mped, join = st_contains) %>%
  group_by(ID.x) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  rename(ID = ID.x)

vars

sum.p1 <- sf_data %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)

sum.p <- p.grid_0.1 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)

data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p1, sum.p, data.frame(sum.p1 - sum.p))) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

p.grid_0.1

st_write(p.grid_0.1, dsn = "CEIP_Grid_0.1/Predaja/RS_Grid_0.1_RF2015.shp")
writexl::write_xlsx(p.grid_0.1 %>% st_drop_geometry(), "CEIP_Grid_0.1/Predaja/RS_Grid_0.1_RF2015.xlsx")

p.grid_0.1_NOx <- p.grid_0.1 %>% dplyr::select(ID, NOx)
p.grid_0.1_SO2 <- p.grid_0.1 %>% dplyr::select(ID, SO2)
p.grid_0.1_PM10 <- p.grid_0.1 %>% dplyr::select(ID, PM10)
p.grid_0.1_PM2.5 <- p.grid_0.1 %>% dplyr::select(ID, PM2.5)
p.grid_0.1_NMVOC <- p.grid_0.1 %>% dplyr::select(ID, NMVOC)
p.grid_0.1_NH3 <- p.grid_0.1 %>% dplyr::select(ID, NH3)

st_write(p.grid_0.1_NOx, dsn = "CEIP_Grid_0.1/Predaja/RS_Grid_0.1_RF2015_NOx.shp")
st_write(p.grid_0.1_SO2, dsn = "CEIP_Grid_0.1/Predaja/RS_Grid_0.1_RF2015_SO2.shp")
st_write(p.grid_0.1_PM10, dsn = "CEIP_Grid_0.1/Predaja/RS_Grid_0.1_RF2015_PM10.shp")
st_write(p.grid_0.1_PM2.5, dsn = "CEIP_Grid_0.1/Predaja/RS_Grid_0.1_RF2015_PM2_5.shp")
st_write(p.grid_0.1_NMVOC, dsn = "CEIP_Grid_0.1/Predaja/RS_Grid_0.1_RF2015_NMVOC.shp")
st_write(p.grid_0.1_NH3, dsn = "CEIP_Grid_0.1/Predaja/RS_Grid_0.1_RF2015_NH3.shp")



# -----------------------------------------------------------------------------------------------------------
# Maps
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


sf_data <- p.grid_0.1


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
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
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
       filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_NOx.jpg", 
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
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
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
       filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_SO2.jpg", 
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
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
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
       filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_PM10.jpg", 
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
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
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
       filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_PM2.5.jpg", 
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
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
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
       filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_NMVOC.jpg", 
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
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
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
       filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)







