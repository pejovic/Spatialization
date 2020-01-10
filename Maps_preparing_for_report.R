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

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Map of grid
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_grid <- st_read(dsn = "Grid/Grid_5km_Serbia.gpkg")

grid_map <- ggplot() +
  geom_sf(data = sf_grid, colour = "black", fill = "#43a2ca") + 
  labs(x = NULL, y = NULL,
       title = "Grid for the Pollutant inventory spatialization",
       subtitle = "Spatial resolution: 5x5 km, Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  theme_bw()
  #theme(line = element_blank(),
        #axis.text = element_blank(),
        #axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        #panel.background = element_blank()) +
  #coord_sf(datum = NA)


ggsave(plot = grid_map,filename = "Maps/Map_grid.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# CLC map
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_clc <- st_read(dsn = "GIS_layers/CLC_12-18.gpkg")


clc_legend <- read.csv(file="Data/clc_legend.csv", header=TRUE, sep=",") %>% as.data.frame()

sf_clc$RGB <- as.character(clc_legend$RGB[match(as.character(sf_clc$CODE_18),as.character(clc_legend$CLC_CODE))])
#sf_clc$RGB <- clc_legend$RGB %in% c(sf_clc$CODE_18,clc_legend$CLC_CODE)
#unique(sf_clc$CODE_18)

sf_clc %<>% dplyr::mutate(RGB = str_replace_all(RGB, "-", ","))
sf_clc %<>% tidyr::separate(RGB, c("R", "G", "B"), sep = ",") 

sf_clc$rgb <- rgb(as.numeric(sf_clc$R)/255, as.numeric(sf_clc$G)/255, as.numeric(sf_clc$B)/255)

# sf_clc %<>% dplyr::rename(`CLC class code` = CODE_18)

colors <- distinct(sf_clc, CODE_18, rgb)
pal <- colors$rgb
names(pal) <- colors$CODE_18

CLC_map <- ggplot() +
  geom_sf(data = sf_clc, aes(fill = CODE_18)) + 
  labs(x = NULL, y = NULL,
       title = "Corine Land Cover map [2012-2018]",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  scale_fill_manual(values = pal)+
  theme_bw()

ggsave(plot = CLC_map,filename = "Maps/Map_CLC.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Administrative zones – municipalities
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_mun <- st_read(dsn = "GIS_layers/Municipalities.gpkg")
sf_mun %<>% dplyr::mutate(`Area [ha]` = Area/10000, 
                          Name = as.character(Name),
                          X = st_coordinates(st_centroid(sf_mun))[,1], 
                          Y = st_coordinates(st_centroid(sf_mun))[,2])

Map_mun <- ggplot() + 
  geom_sf(data = sf_mun, aes(fill = `Area [ha]`))+ 
  #geom_sf_text(data = sf_mun, aes(X, Y, label = Name))
  labs(x = NULL, y = NULL,
       title = "Map of Municipalities",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  theme_bw()


ggsave(plot = Map_mun,filename = "Maps/Map_Municipalities.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Roads
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_roads <- st_read(dsn = "Data/Roads_PGDS_intersected_with_SRB_boundary.gpkg")
sf_granica <- st_read(dsn = "Data/Granica_SRB.gpkg")
sf_urbana <- st_read(dsn = "Products/es_pgds_samo_urbana_podrucja_koja_seku.gpkg")


Map_roads <- ggplot()+
  geom_sf(data = sf_roads, aes(size = lwd1*1.5))+
  scale_size_identity()+
  geom_sf(data = sf_granica, colour = "ForestGreen", fill = NA)+
  geom_sf(data = sf_urbana, fill = "red", colour = "orange")+
  labs(x = NULL, y = NULL,
       title = "Road network with VCD values",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  theme_bw()

ggsave(plot = Map_roads, filename = "Maps/Map_Roads.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Urban roads
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_roads_urban <- st_read(dsn = "Data/putevi/OSM_putevi_urbana_podrucja.gpkg")
sf_granica <- st_read(dsn = "Data/Granica_SRB.gpkg")
sf_urb <- st_read(dsn = "Products/urban_areas.gpkg")

sf_bel <- st_read(dsn = "Data/Mun_Belgrade_urban.gpkg")
sf_bel_roads <- sf_roads_urban %>% st_intersection(sf_bel)

ggm2 <- ggplot()+
  geom_sf(data = sf_roads_urban)+
  geom_sf(data = sf_granica, colour = "ForestGreen", fill = NA)+
  geom_sf(data = sf_urb, fill = "red", colour = "orange")+
  theme_bw()+
  theme(legend.position="none", axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(color='red', fill="white"))



ggm1<- ggplot()+
  geom_sf(data = sf_bel, fill = NA)+
  geom_sf(data = sf_bel_roads)+
  labs(x = NULL, y = NULL,
       title = "OSM road network for urban areas",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  theme_bw()
  
library(cowplot)

gg_inset_map1 = ggdraw() +
  draw_plot(ggm1)+
  draw_plot(ggm2, x = 0.05, y = 0.50, width = 0.25, height = 0.25)
  
gg_inset_map1

ggsave(plot = gg_inset_map1, filename = "Maps/Map_OSM_Urban_Roads.jpg", width = 30, height = 30, units = "cm", device = "jpeg", dpi = 300)



