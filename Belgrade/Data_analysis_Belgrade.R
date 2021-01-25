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
library(magrittr)
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
emep_grid_belgrade <- emep_grid[sf_opstine_bg, ]
emep_grid_belgrade %<>% 
  dplyr::mutate(ID = row_number())

mapview(sf_NH3_bg, zcol = "emission 2015 (tons)") + mapview(sf_data_beograd) + mapview(emep_grid) + mapview(sf_opstine_bg)




# Sa translacijom 

edg_NH3_mov <- edg_NH3 %>% dplyr::mutate(lat = (lat - 0.05), 
                                         lon = (lon + 0.05))


sf_edg_NH3_mov <- st_as_sf(edg_NH3_mov, 
                           coords = c("lon", "lat"), 
                           crs = 4326)


sf_edg_NH3_mov_bg <- sf_edg_NH3_mov[sf_opstine_bg, ]

mapview(sf_edg_NH3_mov_bg, zcol = "emission 2015 (tons)") + mapview(sf_data_beograd_cent) + mapview(emep_grid_belgrade)



# Preklop

sf_data_beograd_cent <- sf::st_centroid(sf_data_beograd)


p.grid_0.1 <- emep_grid_belgrade %>% # Spatialization
  st_join(., sf_data_beograd_cent, join = st_contains) %>%
  dplyr::group_by(ID.x) %>%
  dplyr::summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>%
  rename(ID = ID.x)

p.grid_0.1_1 <- emep_grid_belgrade %>% # Spatialization
  st_join(., sf_edg_NH3_mov_bg, join = st_contains) %>%
  rename(NH3_grid_0.1 = `emission 2015 (tons)`) %>%
  dplyr::select(NH3_grid_0.1) %>%
  dplyr::mutate(ID = row_number()) %>%
  dplyr::mutate_all(~replace(., is.na(.), 0))


p.grid_0.1$NH3_grid <- p.grid_0.1_1$NH3_grid_0.1[match(p.grid_0.1$ID, p.grid_0.1_1$ID)]  



head(p.grid_0.1)
summary(p.grid_0.1)


p.grid_0.1 %<>% 
  dplyr::mutate(diff_NH3 = NH3_grid - NH3)

mapview(p.grid_0.1, zcol = "diff_NH3")








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
classes.NOx <- classIntervals(sf_data$diff_NOx, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data$diff_SO2, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data$diff_PM10, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data$diff_PM2.5, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data$diff_NMVOC, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data$diff_NH3, n = 12, style = "fisher")

sf_data <- sf_data %>%
  mutate(#percent_class_NOx = cut(diff_NOx, classes.NOx$brks, include.lowest = T,dig.lab=5),
         #percent_class_SO2 = cut(diff_SO2, classes.SO2$brks, include.lowest = T,dig.lab=7),
         #percent_class_PM10 = cut(diff_PM10, classes.PM10$brks, include.lowest = T,dig.lab=5),
         #percent_class_PM2.5 = cut(diff_PM2.5, classes.PM2.5$brks, include.lowest = T,dig.lab=5),
         #percent_class_NMVOC = cut(diff_NMVOC, classes.NMVOC$brks, include.lowest = T,dig.lab=5),
         percent_class_NH3 = cut(diff_NH3, classes.NH3$brks, include.lowest = T,dig.lab=5)
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



