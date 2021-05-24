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





opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)
unique(sf_opstine$NAME_2)

sf_opstine_pa <- sf_opstine %>% dplyr::filter(NAME_2 == "Pančevo")
sf_opstine_uz <- sf_opstine %>% dplyr::filter(NAME_2 == "Užice")

sf_opstine_va <- sf_opstine %>% dplyr::filter(NAME_2 == "Valjevo")
sf_opstine_kg <- sf_opstine %>% dplyr::filter(NAME_2 == "Kragujevac")
sf_opstine_ni <- sf_opstine %>% dplyr::filter(NAME_2 == "Niš")



clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
sf_clc18_urb <- subset(sf_clc18, CODE_18 == "111" | CODE_18 == "112") %>% # CLC urban zones
  st_transform(crs = "+init=epsg:32634") %>%
  dplyr::select(geometry)


sf_municipalities <- sf_opstine %>% dplyr::select(NAME_2, SDG, Br_domacinstva, Br_domacinstva_SDG) %>%
  dplyr::rename(NAME = NAME_2, No_houeses_DHS = SDG, No_houses = Br_domacinstva, No_houses_OHS = Br_domacinstva_SDG)


sf_municipalities %<>% st_transform(4326)
sf_municipalities %<>% mutate(Area_mun = st_area(.))




sf_clc18_urb %<>% dplyr::mutate(Area_polygon = sf::st_area(.)) %>% units::drop_units(.)
sf_clc18_urb %<>% st_transform(4326)

sf_clc18_urb_pa <- sf_clc18_urb[sf_opstine_pa,]
sf_clc18_urb_uz <- sf_clc18_urb[sf_opstine_uz,]
sf_clc18_urb_va <- sf_clc18_urb[sf_opstine_va,]
sf_clc18_urb_kg <- sf_clc18_urb[sf_opstine_kg,]
sf_clc18_urb_ni <- sf_clc18_urb[sf_opstine_ni,]




mapview(sf_clc18_urb_ni)




map_pa <- ggplot() +
  geom_sf(data = sf_clc18_urb_pa,
          aes(fill = "orange")) +
  scale_fill_manual(values = "orange",
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       subtitle = paste("Spatial resolution 0.05°x0.05°, Territory of the City of Pančevo \n", "Area (Urban areas): ", 
                        sum(round(sf_clc18_urb_pa$Area_polygon/ 1000000), 2), " km^2", 
                        "\n Buildings DHS: 11986 (CENSUS)" , "\n Buildings OHS: 31158 (CENSUS)", sep = ""),
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        axis.title = element_blank(),
        legend.position = "None", 
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_pa, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


map_pa


ggsave(plot = map_pa, 
       filename = "Verification/Map_Pancevo_urban_areas.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)




map_uz <- ggplot() +
  geom_sf(data = sf_clc18_urb_uz,
          aes(fill = "orange")) +
  scale_fill_manual(values = "orange",
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       subtitle = paste("Spatial resolution 0.05°x0.05°, Territory of the City of Užice \n", "Area (Urban areas): ", 
                        sum(round(sf_clc18_urb_uz$Area_polygon/ 1000000), 2), " km^2", 
                        "\n Buildings DHS: 5786 (CENSUS)" , "\n Buildings OHS: 21191 (CENSUS)", sep = ""),
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        axis.title = element_blank(),
        legend.position = "None", 
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_uz, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


map_uz

ggsave(plot = map_uz, 
       filename = "Verification/Map_Uzice_urban_areas.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)


map_va <- ggplot() +
  geom_sf(data = sf_clc18_urb_va,
          aes(fill = "orange")) +
  scale_fill_manual(values = "orange",
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       subtitle = paste("Spatial resolution 0.05°x0.05°, Territory of the City of Valjevo \n", "Area (Urban areas): ", 
                        sum(round(sf_clc18_urb_va$Area_polygon/ 1000000), 2), " km^2", 
                        "\n Buildings DHS: 4232 (CENSUS)" , "\n Buildings OHS: 27169 (CENSUS)", sep = ""),
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        axis.title = element_blank(),
        legend.position = "None", 
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_va, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


map_va

ggsave(plot = map_va, 
       filename = "Verification/Map_Valjevo_urban_areas.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)


map_kg <- ggplot() +
  geom_sf(data = sf_clc18_urb_kg,
          aes(fill = "orange")) +
  scale_fill_manual(values = "orange",
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       subtitle = paste("Spatial resolution 0.05°x0.05°, Territory of the City of Kragujevac \n", "Area (Urban areas): ", 
                        sum(round(sf_clc18_urb_kg$Area_polygon/ 1000000), 2), " km^2", 
                        "\n Buildings DHS: 19693 (CENSUS)" , "\n Buildings OHS: 40298 (CENSUS)", sep = ""),
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        axis.title = element_blank(),
        legend.position = "None", 
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_kg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


map_kg

ggsave(plot = map_kg, 
       filename = "Verification/Map_Kragujevac_urban_areas.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)

map_ni <- ggplot() +
  geom_sf(data = sf_clc18_urb_ni,
          aes(fill = "orange")) +
  scale_fill_manual(values = "orange",
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       subtitle = paste("Spatial resolution 0.05°x0.05°, Territory of the City of Niš \n", "Area (Urban areas): ", 
                        sum(round(sf_clc18_urb_ni$Area_polygon/ 1000000), 2), " km^2", 
                        "\n Buildings DHS: 0 (CENSUS)" , "\n Buildings OHS: 89903 (CENSUS)", sep = ""),
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        axis.title = element_blank(),
        legend.position = "None", 
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine_ni, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


map_ni


ggsave(plot = map_ni, 
       filename = "Verification/Map_Nis_urban_areas.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)




maps <- grid.arrange(map_pa, map_uz, map_va, map_kg, map_ni, ncol = 2)



ggsave(plot = maps, 
       filename = "Verification/Map_municipalities_urban_areas.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=600)





