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




# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Map of grid
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# sf_grid <- st_read(dsn = "Grid/Grid_5km_Serbia.gpkg")
sf.grid.4326 <- st_read(dsn = "Grid/Grid_Serbia_0.05deg.gpkg")

mapview(sf.grid.4326)
grid_map <- ggplot() +
  geom_sf(data = sf.grid.4326, colour = "red", fill = "orange", alpha = 0.2) + 
  labs(x = NULL, y = NULL,
       title = "Grid for the Pollutant inventory spatialization",
       subtitle = "Spatial resolution: 0.05°x0.05°, Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  theme_bw()
  #theme(line = element_blank(),
        #axis.text = element_blank(),
        #axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        #panel.background = element_blank()) +
  #coord_sf(datum = NA)


ggsave(plot = grid_map,filename = "Maps/Map_grid_0.05deg.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



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


sf_clc_4326 <- st_transform(sf_clc, 4326)

CLC_map <- ggplot() +
  geom_sf(data = sf_clc_4326, aes(fill = CODE_18)) + 
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
# Administrative zones – regions
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_reg <- st_read(dsn = "data/regions/Regions_Serbia.shp")
sf_reg %<>% dplyr::mutate(`Area [ha]` = st_area(.)/10000, 
                          Name = as.character(NAME_1),
                          X = st_coordinates(st_centroid(sf_reg))[,1], 
                          Y = st_coordinates(st_centroid(sf_reg))[,2])

Map_reg <- ggplot() + 
  geom_sf(data = sf_reg)+ #, aes(fill = `Area [ha]`) 
  geom_sf(data = sf_reg, fill = NA, colour = "red", lwd = 0.6)+
  #geom_sf_text(data = sf_mun, aes(X, Y, label = Name))
  labs(x = NULL, y = NULL,
       title = "Map of Regions",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2020)")+
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  #geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  #geom_sf_label_repel(data = sf_opstine_sel, aes(label = NAME_2), nudge_x = 0.3, nudge_y = 0.3, seed = 10)+
  geom_sf_label_repel(data = sf_reg, aes(label = NAME_1))

Map_reg 


ggsave(plot = Map_reg, filename = "Maps/Map_Regions.jpg", width = 30, height = 30, units = "cm", device = "jpeg")




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
  geom_sf(data=sf_brojaci)+
  theme_bw()

ggsave(plot = Map_roads, filename = "Maps/Map_Roads.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Roads
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

brojaci <- readOGR("Data/brojaci/Polozaj_automatskih_brojaca_bez_duplikata.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8",
                   stringsAsFactors = FALSE)



sf_brojaci <- st_as_sf(brojaci) %>% mutate_at(vars(starts_with("PGDS")), .funs = as.numeric) %>% dplyr::rename(ID = ID_BROJAÄ)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Map of VCDs
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(ggspatial)
sf_roads <- st_read(dsn = "Data/Roads_PGDS_intersected_with_SRB_boundary.gpkg")
sf_roads.4326 <- st_transform(sf_roads, 4326)
vcds <- st_read(dsn = "Data/brojaci/VCDs.gpkg")
vcds %<>% st_zm(., drop = TRUE)
vcds %<>% dplyr::filter(Kategorija != "nije u mrezi") %>%
  dplyr::mutate(Category = Kategorija)

vcds_map <- ggplot(data = vcds)+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Territory of the Repubic of Serbia",
       title = "Spatial locations of Vehicle Counting Devices")+#,
  geom_sf(aes(color = Category), size= 2.5)+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  geom_sf(data = sf_roads.4326)
vcds_map
ggsave(plot = vcds_map, filename = "Maps/Map_vcds.jpg", width = 30, height = 30, units = "cm", device = "jpeg")





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


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Railways
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_rail <- st_read(dsn = "GIS_layers/Railways.gpkg")
sf_granica <- st_read(dsn = "Data/Granica_SRB.gpkg")

map_rail <- ggplot()+
  geom_sf(data = sf_rail, aes(size = 0.7, colour = "#c51b8a"))+
  scale_size_identity()+
  geom_sf(data = sf_granica, colour = "ForestGreen", fill = NA)+
  labs(x = NULL, y = NULL,
       title = "Map of Railways",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  theme_bw()+
  theme(legend.position="none")

ggsave(plot = map_rail, 
       filename = "Maps/Map_Railways.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 300)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Navigable rivers
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_rivers <- st_read(dsn = "Data/Navigable_rivers_for_map.gpkg")
sf_granica <- st_read(dsn = "Data/Granica_SRB.gpkg")

map_river <- ggplot()+
  geom_sf(data = sf_rivers, fill = "blue")+
  geom_sf(data = sf_granica, colour = "ForestGreen", fill = NA)+
  labs(x = NULL, y = NULL,
       title = "Map of Navigable rivers",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  theme_bw()
ggsave(plot = map_river, 
       filename = "Maps/Map_Navigable_rivers.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 300)



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Rural and urban areas
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")

sf_rur %<>% dplyr::mutate(`Area [ha]` = st_area(.)/10000) %>% mutate(`Area [ha]` = unclass(`Area [ha]`))

map_rur <- ggplot()+
  geom_sf(data = sf_rur, fill = "#fcc5c0", colour = NA)+
  #geom_sf(data = sf_granica, colour = "ForestGreen", fill = NA)+
  labs(x = NULL, y = NULL,
       title = "Map of Rurual areas",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  theme_bw()
  #scale_fill_gradientn(colours=c("#fde0dd", "#7a0177"))

ggsave(plot = map_rur, 
        filename = "Maps/Map_Rurual_areas_new.jpg", 
        width = 30, 
        height = 30, 
        units = "cm", 
        device = "jpeg", 
        dpi = 300)

sf_urb <- st_read(dsn = "GIS_layers/Urban_areas.gpkg")
sf_urb %<>% dplyr::mutate(`Area [ha]` = st_area(.)/10000) %>% mutate(`Area [ha]` = unclass(`Area [ha]`))

map_urb <- ggplot()+
  geom_sf(data = sf_urb, aes(fill = `Area [ha]`), colour = "red")+
  #geom_sf(data = sf_granica, colour = "ForestGreen", fill = NA)+
  labs(x = NULL, y = NULL,
       title = "Map of Urban areas",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  theme_bw()+
  scale_fill_gradientn(colours=c("#bcbddc", "#54278f"))


ggsave(plot = map_urb, 
       filename = "Maps/Map_Urban_areas.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 300)



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Industrial and commercial sites
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_ind <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Industrial_sites_new.gpkg") 

sf_ind %<>% dplyr::mutate(`Area [ha]` = unclass(st_area(.)/10000)) %>%
  dplyr::select(`Area [ha]`)

sf_comm <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/OSM_commercial_Serbia.gpkg")
sf_NS <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Poligon_za_primer_uvecani.gpkg")

sf_ind.int <- sf_ind %>% 
  sf::st_intersection(., sf_NS) %>%
  dplyr::mutate(`Idustrial sites - Area [ha]` = unclass(st_area(.)/10000))
sf_comm.int <- sf_comm %>% 
  sf::st_intersection(., sf_NS) %>%
  dplyr::mutate(`Commercial units - Area [ha]` = unclass(st_area(.)/10000))

library(ggnewscale)

indcomm <- ggplot()+
  geom_sf(data = sf_ind.int, aes(fill = `Idustrial sites - Area [ha]`), colour = NA)+
  scale_fill_gradientn(colours=c("#c7e9b4", "#41b6c4"))+
  new_scale_fill() +
  geom_sf(data = sf_comm.int, aes(fill = `Commercial units - Area [ha]`), colour = NA)+
  scale_fill_gradientn(colours=c("#fa9fb5", "#c51b8a"))+
  labs(x = NULL, y = NULL,
       title = "Map of Industrial sites and Commercial units",
       subtitle = "Territory of the Repubic of Serbia - Example for the city of Novi Sad",
       caption = "© GiLab (2019/20)")+
  theme_bw()


ggsave(plot = indcomm, 
       filename = "Maps/Map_Industrial_Commercial.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 300)


# ggplot()+
#   geom_sf(data = sf_ind, colour = "red")+
#   geom_sf(data = sf_comm, colour = "blue")+
#   scale_fill_gradientn(colours=c("#c7e9b4", "#41b6c4"))+
#   labs(x = NULL, y = NULL,
#        title = "Map of Industrial sites",
#        subtitle = "Territory of the Repubic of Serbia - Example for the city of Novi Sad",
#        caption = "© GiLab (2019/20)")+
#   theme_bw()


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Population density grid 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(raster)
library(stars)
library(viridis)

pop_raster <- raster("Version_2_update/Spatialization/Proxy_data_new/popdens_32634.tif")
pop_raster <- raster("Version_3_update/Spatialization/New_data/pop_dens_4326.tif")

pop_star <- st_as_stars(pop_raster)

mapview(pop_raster)

popdens.map <- ggplot()+
  geom_stars(data = pop_star)+
  labs(x = NULL, y = NULL,
       title = "Population density map",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)\n
       CRS: WGS84")+
  theme_bw()+
  scale_fill_viridis(option = "B", limits = c(0,250), na.value = NA)+
  guides(fill=guide_legend(title="Population density\n[0.006°/pix]"))


ggsave(plot = popdens.map, 
       filename = "Maps/Map_Population_density_4326.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 300)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Airports and domestic airports
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
ia_sf <- st_read(dsn = "GIS_layers/International_aviation_airports.gpkg")%>% st_transform(., 4326)
da_sf <- st_read(dsn =  "GIS_layers/Domestic_aviation_airports.gpkg") %>% st_transform(., 4326)
sf_granica <- st_read(dsn = "Data/Granica_SRB.gpkg") %>% st_transform(., 4326) 

  
a_map <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "International (blue) and Domestic aviation (red)",
       title = "Spatial locations of Airports")+#,
  geom_sf(data = ia_sf, aes(color = "red"), size= 3.5)+
  geom_sf(data = da_sf, aes(color = "blue"), size= 3)+
  geom_sf(data = sf_granica, colour = "ForestGreen", fill = NA)+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "None")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
a_map
ggsave(plot = a_map, filename = "Maps/Map_airports.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Fuel stations, wastewater treatment plants and cremation locations
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# fuels - 1B - Fugitive emissions: 1B2av-Fugitive emissions from liquid fuels: Distribution of oil products

fuel.s <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Fuel_stations_OSM_32634.gpkg")
fuel.sf <- fuel.s %>% dplyr::select() %>% dplyr::mutate(class = "1-Fuel stations")
# wastewater treatment plants - 5 - Waste: 5D2-Industrial wastewater handling

sf.waste <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Wastewater_plants_OSM_32634.gpkg") %>%
  dplyr::select() %>%
  dplyr::mutate(class = "2-Wastewater treatment plants")


#cremation - 5 - Waste: 5C1bv-Cremation
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "5-Waste"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
source.5C1bv <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))
source.5C1bv$sources$points <- readxl::read_xlsx(path = source.file, range = "D24:S25", sheet = source.sheet, col_names = header)
source.5C1bv$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D34:I34", sheet = source.sheet, col_names = vars)
source.5C1bv$total$inventory <- readxl::read_xlsx(path = source.file, range = "D35:I35", sheet = source.sheet, col_names = vars)
sf.5C1bv <- corsum2sf(source.5C1bv, distribute = TRUE) %>% # Preparing data for final spatialization
  st_transform(crs = "+init=epsg:32634")

cremation <- sf.5C1bv %>% dplyr::mutate(geom = geometry, class = "3-Cremation locations") %>% dplyr::select(class) %>% dplyr::rename(geom = geometry)


sf.final <- rbind(fuel.sf, sf.waste, cremation)

unique(sf.final$class)


library(ggspatial)

sf_granica <- st_read(dsn = "Data/Granica_SRB.gpkg") %>% st_transform(., 4326) 


f_map <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Fuel stations, wastewater treatment plants and cremation locations",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.final, aes(color = class), size= 2.5)+
  geom_sf(data = sf_granica, colour = "ForestGreen", fill = NA)+
  scale_color_manual(values=c("#999999", "#56B4E9", "red"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"))+#,
        #legend.position = "None")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
f_map
ggsave(plot = f_map, filename = "Maps/Map_fuel-waste-cremation.jpg", width = 30, height = 30, units = "cm", device = "jpeg")







# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Map 1A1a - Public heat and electricity production
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_granica <- st_read(dsn = "Data/Granica_SRB.gpkg") %>% st_transform(., 4326) 

source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "1A1-Energy"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]

source.1A1a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1a$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S37", sheet = source.sheet, col_names = header)
source.1A1a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D40:I40", sheet = source.sheet, col_names = vars)
source.1A1a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D46:I46", sheet = source.sheet, col_names = vars)


sf.1A1a <- corsum2sf(source.1A1a) %>%
  st_transform(crs = "+init=epsg:32634")


sf.1A1a %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A1a - Public heat and electricity production")

map.1A1a <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A1 - Energy",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A1a, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "ForestGreen", fill = NA)+
  scale_color_manual(values=c("blueviolet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"))+#,
        #legend.position = "None")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A1a
ggsave(plot = map.1A1a, filename = "Maps/Subcategories/Map_1A1a.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Map 1A2c/2B (chemical), 1A2e/2H2 (food industry)
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "1A2-2-Industry"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]

# # 1A2c - Chemicals
source.1A2c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))
source.1A2c$sources$points <- readxl::read_xlsx(path = source.file, range = "D55:S69", sheet = source.sheet, col_names = header)
source.1A2c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D71:I71", sheet = source.sheet, col_names = vars)
source.1A2c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D86:I86", sheet = source.sheet, col_names = vars)

sf.1A2c <- corsum2sf(source.1A2c) %>%
  st_transform(crs = "+init=epsg:32634")

# 1A2e / 2H2 - Food, beverages and tobacco
source.1A2e <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2e$sources$points <- readxl::read_xlsx(path = source.file, range = "D110:S131", sheet = source.sheet, col_names = header)
source.1A2e$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D137:I137", sheet = source.sheet, col_names = vars)
source.1A2e$total$inventory <- readxl::read_xlsx(path = source.file, range = "D151:I151", sheet = source.sheet, col_names = vars)

sf.1A2e <- corsum2sf(source.1A2e, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")


# =====================================================================================

sf.1A2c %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2c - Chemicals") 
sf.1A2e %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2e / 2H2 - Food, beverages and tobacco") 

sf.ind <- rbind(sf.1A2c, sf.1A2e)

map.ind <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2-2-Industry",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.ind, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "ForestGreen", fill = NA)+
  scale_color_manual(values=c("#FE318B", "#FF8A47"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"))+#,
        #legend.position = "None")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.ind
ggsave(plot = map.ind, filename = "Maps/Subcategories/Map_1A2c-1A2e.jpg", width = 30, height = 30, units = "cm", device = "jpeg")





# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Maps by category
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# data.spat <- list.files('D:/R_projects/Spatialization/Products/3 - Agriculture/')
# 
# data.spat.list <- list()                                                   
# for(i in 1:length(data.spat)){                                             
#   data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/3 - Agriculture/",data.spat[i], sep = ""))
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
# st_write(sf_data, dsn="Products/Sum-up_By_category/3 - Agriculture.gpkg", layer='Agriculture')

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









# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_energy <- st_read(dsn = "Products/Sum-up_By_category/1A1 - Energy.gpkg")

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


opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)


#+ include = FALSE 
a1<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
                    name = "NMVOC") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
  scale_fill_manual(values = pal2,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "GNFR sector: 1A1 - Energy",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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



ggsave(plot = f1, 
       filename = "Maps/FINAL2015/Maps_per_each_GNFR_sector/Map_1A1 - Energy_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_industry <- st_read(dsn = "Products/Sum-up_By_category/1A2 - Industry.gpkg")

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
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "GNFR sector: 1A2 - Industry",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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

ggsave(plot = f2, 
       filename = "Maps/FINAL2015/Maps_per_each_GNFR_sector/Map_1A2 - Industry_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_transport <- st_read(dsn = "Products/Sum-up_By_category/1A3 - Transport.gpkg")

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
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "GNFR sector: 1A3 - Transport",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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


ggsave(plot = f3, 
       filename = "Maps/FINAL2015/Maps_per_each_GNFR_sector/Map_1A3 - Transport_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_residential <- st_read(dsn = "Products/Sum-up_By_category/1A4 - Residential-Tertiary.gpkg")

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
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "GNFR sector: 1A4 - Residential-Tertiary",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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


ggsave(plot = f4, 
       filename = "Maps/FINAL2015/Maps_per_each_GNFR_sector/Map_1A4 - Residential-Tertiary_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_fugitive <- st_read(dsn = "Products/Sum-up_By_category/1B - Fugitive emissions.gpkg")

classes.NOx <- classIntervals(sf_data_fugitive$NOx, n = 12, style = "fisher")
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

#+ include = FALSE 
a5<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "GNFR sector: 1B - Fugitive emissions",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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

ggsave(plot = f5, 
       filename = "Maps/FINAL2015/Maps_per_each_GNFR_sector/Map_1B - Fugitive emissions_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_other <- st_read(dsn = "Products/Sum-up_By_category/2 - Other processes.gpkg")

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


a6<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "GNFR sector: 2 - Other processes",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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


ggsave(plot = d6, 
       filename = "Maps/FINAL2015/Maps_per_each_GNFR_sector/Map_2 - Other processes_PM2.5.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_agriculture <- st_read(dsn = "Products/Sum-up_By_category/3 - Agriculture.gpkg")

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
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "GNFR sector: 3 - Agriculture",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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



ggsave(plot = c7, 
       filename = "Maps/FINAL2015/Maps_per_each_GNFR_sector/Map_3 - Agriculture_PM10.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_waste <- st_read(dsn = "Products/Sum-up_By_category/5 - Waste.gpkg")

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
       title = "Pollutant inventory spatialization - NOx",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - SO2",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "GNFR sector: 5 - Waste",
       caption = "Spatial resolution 0.05°x0.05°, Teritory of the Republic of Serbia\n UBFCE (2020)")+
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


ggsave(plot = f8, 
       filename = "Maps/FINAL2015/Maps_per_each_GNFR_sector/Map_5 - Waste_NH3.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)












