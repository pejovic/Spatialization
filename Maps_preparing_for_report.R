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


ggsave(plot = grid_map,filename = "Maps/Map_grid.jpg", width = 20, height = 20, units = "cm", device = "jpeg")



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





