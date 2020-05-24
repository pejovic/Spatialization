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
# Roads
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

brojaci <- readOGR("Data/brojaci/Polozaj_automatskih_brojaca_bez_duplikata.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8",
                   stringsAsFactors = FALSE)



sf_brojaci <- st_as_sf(brojaci) %>% mutate_at(vars(starts_with("PGDS")), .funs = as.numeric) %>% dplyr::rename(ID = ID_BROJAÄ)



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


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_energy <- st_read(dsn = "Products/Sum-up_By_category/1A1 - Energy.gpkg")

classes.NOx <- classIntervals(sf_data_energy$NOx, n = 30, style = "fisher")
classes.SO2 <- classIntervals(sf_data_energy$SO2, n = 30, style = "fisher")
classes.PM10 <- classIntervals(sf_data_energy$PM10, n = 30, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_energy$PM2.5, n = 30, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_energy$NMVOC, n = 30, style = "fisher")
classes.NH3 <- classIntervals(sf_data_energy$NH3, n = 30, style = "fisher")

sf_data_energy <- sf_data_energy %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T)
  )

pal1 <- viridisLite::viridis(30)
pal2 <- viridisLite::viridis(30)
pal3 <- viridisLite::viridis(30)
pal4 <- viridisLite::viridis(30)
pal5 <- viridisLite::viridis(30)
pal6 <- viridisLite::viridis(30)

#+ include = FALSE 
a<-ggplot() +
  geom_sf(data = sf_data_energy,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

b<-ggplot() +
  geom_sf(data = sf_data_energy,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

c<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal3,
                    name = "PM10") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

d<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal4,
                    name = "PM2.5") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

e<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal5,
                    name = "NMVOC") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

f<-ggplot() +
  geom_sf(data = sf_data_energy,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal6,
                    name = "NH3") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

energy_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)


ggsave(plot = energy_map, 
       filename = "Maps/Map_energy.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_industry <- st_read(dsn = "Products/Sum-up_By_category/1A2 - Industry.gpkg")

classes.NOx <- classIntervals(sf_data_industry$NOx, n = 30, style = "fisher")
classes.SO2 <- classIntervals(sf_data_industry$SO2, n = 30, style = "fisher")
classes.PM10 <- classIntervals(sf_data_industry$PM10, n = 30, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_industry$PM2.5, n = 30, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_industry$NMVOC, n = 30, style = "fisher")
classes.NH3 <- classIntervals(sf_data_industry$NH3, n = 30, style = "fisher")

sf_data_industry <- sf_data_industry %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T)
  )

pal1 <- viridisLite::viridis(30)
pal2 <- viridisLite::viridis(30)
pal3 <- viridisLite::viridis(30)
pal4 <- viridisLite::viridis(30)
pal5 <- viridisLite::viridis(30)
pal6 <- viridisLite::viridis(30)

#+ include = FALSE 
a<-ggplot() +
  geom_sf(data = sf_data_industry,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

b<-ggplot() +
  geom_sf(data = sf_data_industry,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

c<-ggplot() +
  geom_sf(data = sf_data_industry,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal3,
                    name = "PM10") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

d<-ggplot() +
  geom_sf(data = sf_data_industry,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal4,
                    name = "PM2.5") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

e<-ggplot() +
  geom_sf(data = sf_data_industry,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal5,
                    name = "NMVOC") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

f<-ggplot() +
  geom_sf(data = sf_data_industry,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal6,
                    name = "NH3") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

industry_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)


ggsave(plot = industry_map, 
       filename = "Maps/Map_industry.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_transport <- st_read(dsn = "Products/Sum-up_By_category/1A3 - Transport.gpkg")

classes.NOx <- classIntervals(sf_data_transport$NOx, n = 30, style = "fisher")
classes.SO2 <- classIntervals(sf_data_transport$SO2, n = 30, style = "fisher")
classes.PM10 <- classIntervals(sf_data_transport$PM10, n = 30, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_transport$PM2.5, n = 30, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_transport$NMVOC, n = 30, style = "fisher")
classes.NH3 <- classIntervals(sf_data_transport$NH3, n = 30, style = "fisher")

sf_data_transport <- sf_data_transport %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T)
  )

pal1 <- viridisLite::viridis(30)
pal2 <- viridisLite::viridis(30)
pal3 <- viridisLite::viridis(30)
pal4 <- viridisLite::viridis(30)
pal5 <- viridisLite::viridis(30)
pal6 <- viridisLite::viridis(30)

#+ include = FALSE 
a<-ggplot() +
  geom_sf(data = sf_data_transport,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

b<-ggplot() +
  geom_sf(data = sf_data_transport,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

c<-ggplot() +
  geom_sf(data = sf_data_transport,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal3,
                    name = "PM10") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

d<-ggplot() +
  geom_sf(data = sf_data_transport,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal4,
                    name = "PM2.5") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

e<-ggplot() +
  geom_sf(data = sf_data_transport,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal5,
                    name = "NMVOC") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

f<-ggplot() +
  geom_sf(data = sf_data_transport,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal6,
                    name = "NH3") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

transport_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)


ggsave(plot = transport_map, 
       filename = "Maps/Map_transport.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_residential <- st_read(dsn = "Products/Sum-up_By_category/1A4 - Residential-Tertiary.gpkg")

classes.NOx <- classIntervals(sf_data_residential$NOx, n = 30, style = "fisher")
classes.SO2 <- classIntervals(sf_data_residential$SO2, n = 30, style = "fisher")
classes.PM10 <- classIntervals(sf_data_residential$PM10, n = 30, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_residential$PM2.5, n = 30, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_residential$NMVOC, n = 30, style = "fisher")
classes.NH3 <- classIntervals(sf_data_residential$NH3, n = 30, style = "fisher")

sf_data_residential <- sf_data_residential %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T)
  )

pal1 <- viridisLite::viridis(30)
pal2 <- viridisLite::viridis(30)
pal3 <- viridisLite::viridis(30)
pal4 <- viridisLite::viridis(30)
pal5 <- viridisLite::viridis(30)
pal6 <- viridisLite::viridis(30)

#+ include = FALSE 
a<-ggplot() +
  geom_sf(data = sf_data_residential,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

b<-ggplot() +
  geom_sf(data = sf_data_residential,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

c<-ggplot() +
  geom_sf(data = sf_data_residential,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal3,
                    name = "PM10") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

d<-ggplot() +
  geom_sf(data = sf_data_residential,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal4,
                    name = "PM2.5") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

e<-ggplot() +
  geom_sf(data = sf_data_residential,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal5,
                    name = "NMVOC") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

f<-ggplot() +
  geom_sf(data = sf_data_residential,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal6,
                    name = "NH3") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

residential_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)


ggsave(plot = residential_map, 
       filename = "Maps/Map_residential.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_fugitive <- st_read(dsn = "Products/Sum-up_By_category/1B - Fugitive emissions.gpkg")

classes.NOx <- classIntervals(sf_data_fugitive$NOx, n = 30, style = "fisher")
classes.SO2 <- classIntervals(sf_data_fugitive$SO2, n = 30, style = "fisher")
classes.PM10 <- classIntervals(sf_data_fugitive$PM10, n = 30, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_fugitive$PM2.5, n = 30, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_fugitive$NMVOC, n = 30, style = "fisher")
classes.NH3 <- classIntervals(sf_data_fugitive$NH3, n = 30, style = "fisher")

sf_data_fugitive <- sf_data_fugitive %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T)
  )

pal1 <- viridisLite::viridis(30)
pal2 <- viridisLite::viridis(30)
pal3 <- viridisLite::viridis(30)
pal4 <- viridisLite::viridis(30)
pal5 <- viridisLite::viridis(30)
pal6 <- viridisLite::viridis(30)

#+ include = FALSE 
a<-ggplot() +
  geom_sf(data = sf_data_fugitive,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

b<-ggplot() +
  geom_sf(data = sf_data_fugitive,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

c<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal3,
                    name = "PM10") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

d<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal4,
                    name = "PM2.5") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

e<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal5,
                    name = "NMVOC") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

f<-ggplot() +
  geom_sf(data = sf_data_fugitive,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal6,
                    name = "NH3") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

fugitive_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)


ggsave(plot = fugitive_map, 
       filename = "Maps/Map_fugitive.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_other <- st_read(dsn = "Products/Sum-up_By_category/2 - Other processes.gpkg")

classes.NOx <- classIntervals(sf_data_other$NOx, n = 30, style = "fisher")
classes.SO2 <- classIntervals(sf_data_other$SO2, n = 30, style = "fisher")
classes.PM10 <- classIntervals(sf_data_other$PM10, n = 30, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_other$PM2.5, n = 30, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_other$NMVOC, n = 30, style = "fisher")
classes.NH3 <- classIntervals(sf_data_other$NH3, n = 30, style = "fisher")

sf_data_other <- sf_data_other %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T)
  )

pal1 <- viridisLite::viridis(30)
pal2 <- viridisLite::viridis(30)
pal3 <- viridisLite::viridis(30)
pal4 <- viridisLite::viridis(30)
pal5 <- viridisLite::viridis(30)
pal6 <- viridisLite::viridis(30)


a<-ggplot() +
  geom_sf(data = sf_data_other,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

b<-ggplot() +
  geom_sf(data = sf_data_other,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

c<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal3,
                    name = "PM10") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

d<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal4,
                    name = "PM2.5") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

e<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal5,
                    name = "NMVOC") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

f<-ggplot() +
  geom_sf(data = sf_data_other,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal6,
                    name = "NH3") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

other_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)


ggsave(plot = other_map, 
       filename = "Maps/Map_other.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_agriculture <- st_read(dsn = "Products/Sum-up_By_category/3 - Agriculture.gpkg")

classes.NOx <- classIntervals(sf_data_agriculture$NOx, n = 30, style = "fisher")
classes.SO2 <- classIntervals(sf_data_agriculture$SO2, n = 30, style = "fisher")
classes.PM10 <- classIntervals(sf_data_agriculture$PM10, n = 30, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_agriculture$PM2.5, n = 30, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_agriculture$NMVOC, n = 30, style = "fisher")
classes.NH3 <- classIntervals(sf_data_agriculture$NH3, n = 30, style = "fisher")

sf_data_agriculture <- sf_data_agriculture %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T)
  )

pal1 <- viridisLite::viridis(30)
pal2 <- viridisLite::viridis(30)
pal3 <- viridisLite::viridis(30)
pal4 <- viridisLite::viridis(30)
pal5 <- viridisLite::viridis(30)
pal6 <- viridisLite::viridis(30)


a<-ggplot() +
  geom_sf(data = sf_data_agriculture,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

b<-ggplot() +
  geom_sf(data = sf_data_agriculture,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

c<-ggplot() +
  geom_sf(data = sf_data_agriculture,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal3,
                    name = "PM10") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

d<-ggplot() +
  geom_sf(data = sf_data_agriculture,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal4,
                    name = "PM2.5") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

e<-ggplot() +
  geom_sf(data = sf_data_agriculture,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal5,
                    name = "NMVOC") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

f<-ggplot() +
  geom_sf(data = sf_data_agriculture,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal6,
                    name = "NH3") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

agriculture_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)


ggsave(plot = agriculture_map, 
       filename = "Maps/Map_agriculture.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
sf_data_waste <- st_read(dsn = "Products/Sum-up_By_category/5 - Waste.gpkg")

classes.NOx <- classIntervals(sf_data_waste$NOx, n = 30, style = "fisher")
classes.SO2 <- classIntervals(sf_data_waste$SO2, n = 30, style = "fisher")
classes.PM10 <- classIntervals(sf_data_waste$PM10, n = 30, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data_waste$PM2.5, n = 30, style = "fisher")
classes.NMVOC <- classIntervals(sf_data_waste$NMVOC, n = 30, style = "fisher")
classes.NH3 <- classIntervals(sf_data_waste$NH3, n = 30, style = "fisher")

sf_data_waste <- sf_data_waste %>%
  mutate(percent_class_NOx = cut(NOx, classes.NOx$brks, include.lowest = T),
         percent_class_SO2 = cut(SO2, classes.SO2$brks, include.lowest = T),
         percent_class_PM10 = cut(PM10, classes.PM10$brks, include.lowest = T),
         percent_class_PM2.5 = cut(PM2.5, classes.PM2.5$brks, include.lowest = T),
         percent_class_NMVOC = cut(NMVOC, classes.NMVOC$brks, include.lowest = T),
         percent_class_NH3 = cut(NH3, classes.NH3$brks, include.lowest = T)
  )

pal1 <- viridisLite::viridis(30)
pal2 <- viridisLite::viridis(30)
pal3 <- viridisLite::viridis(30)
pal4 <- viridisLite::viridis(30)
pal5 <- viridisLite::viridis(30)
pal6 <- viridisLite::viridis(30)


a<-ggplot() +
  geom_sf(data = sf_data_waste,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

b<-ggplot() +
  geom_sf(data = sf_data_waste,
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
        legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  coord_sf(datum = NA)

c<-ggplot() +
  geom_sf(data = sf_data_waste,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal3,
                    name = "PM10") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM10",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

d<-ggplot() +
  geom_sf(data = sf_data_waste,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal4,
                    name = "PM2.5") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - PM2.5",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

e<-ggplot() +
  geom_sf(data = sf_data_waste,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal5,
                    name = "NMVOC") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NMVOC",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

f<-ggplot() +
  geom_sf(data = sf_data_waste,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal6,
                    name = "NH3") +
  labs(x = NULL, y = NULL,
       title = "Pollutant inventory spatialization - NH3",
       subtitle = "Spatial resolution 5x5km, Teritory of Serbia",
       caption = "© GiLab (2019)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "None", ###################### legend
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)

waste_map <- grid.arrange(a, b, c, d, e, f, ncol = 2, nrow = 3)


ggsave(plot = waste_map, 
       filename = "Maps/Map_waste.jpg", 
       width = 30, 
       height = 30, 
       units = "cm", 
       device = "jpeg", 
       dpi = 600)












