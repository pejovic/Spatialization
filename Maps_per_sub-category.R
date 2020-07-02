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

source.file = "Pollutant inventory spatialized-d30102019.xlsx"
sf_granica <- st_read(dsn = "Data/Granica_SRB.gpkg") %>% st_transform(., 4326) 


# #####################################################
# Energy 
# #####################################################

source.sheet =  "1A1-Energy"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]


# 1A1a – Public heat production and Electricity production

source.1A1a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1a$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S37", sheet = source.sheet, col_names = header)
source.1A1a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D40:I40", sheet = source.sheet, col_names = vars)
source.1A1a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D46:I46", sheet = source.sheet, col_names = vars)


sf.1A1a <- corsum2sf(source.1A1a) %>%
  st_transform(crs = "+init=epsg:32634")


sf.1A1a %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A1a - Public heat and electricity production")

sf.1A1a$`Sub-category: `[1:21] <- "Point sources for district heating"
sf.1A1a$`Sub-category: `[22:29] <- "Point sources for electricity production"
sf.1A1a %<>% sf::st_transform(4326)

map.1A1a <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A1 - Energy industries",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A1a, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("blueviolet", "tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
  legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A1a
ggsave(plot = map.1A1a, filename = "Maps/Subcategories/Energy/Map_1A1a.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1A1b-Refineries and 1B2aiv- Fugitive emissions from liquid fuels : refining, storage
source.1A1b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1b$sources$points <- readxl::read_xlsx(path = source.file, range = "D47:S50", sheet = source.sheet, col_names = header)
source.1A1b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D51:I51", sheet = source.sheet, col_names = vars)
source.1A1b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D52:I52", sheet = source.sheet, col_names = vars)

sf.1A1b <- corsum2sf(source.1A1b) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1A1b %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A1b - Refineries")
sf.1A1b%<>% sf::st_transform(4326)
map.1A1b <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A1 - Energy industries",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A1b, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("deepskyblue"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A1b
ggsave(plot = map.1A1b, filename = "Maps/Subcategories/Energy/Map_1A1b.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 1B2c- Fugitive emissions : Venting and flaring
source.1B2c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1B2c$sources$points <- readxl::read_xlsx(path = source.file, range = "D59:S62", sheet = source.sheet, col_names = header)
source.1B2c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D63:I63", sheet = source.sheet, col_names = vars)
source.1B2c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D64:I64", sheet = source.sheet, col_names = vars)

sf.1B2c <- corsum2sf(source.1B2c) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1B2c %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1B2c - Fugitive emissions: Venting and flaring")
sf.1B2c %<>% sf::st_transform(4326)
map.1B2c <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A1 - Energy industries",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1B2c, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("deepskyblue"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1B2c
ggsave(plot = map.1B2c, filename = "Maps/Subcategories/Energy/Map_1B2c.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 1A1c-Manufacture of solid fuels
source.1A1c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1c$sources$points <- readxl::read_xlsx(path = source.file, range = "D65:S73", sheet = source.sheet, col_names = header)
source.1A1c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D81:I81", sheet = source.sheet, col_names = vars)
source.1A1c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D82:I82", sheet = source.sheet, col_names = vars)

sf.1A1c <- corsum2sf(source.1A1c) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1A1c %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A1c - Manufacture of solid fuels")
sf.1A1c%<>% sf::st_transform(4326)
map.1A1c <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A1 - Energy industries",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A1c, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("deepskyblue"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A1c
ggsave(plot = map.1A1c, filename = "Maps/Subcategories/Energy/Map_1A1c.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 1B1b-Fugitive emissions from solid fuels : Solid fuel transformation

source.1B1b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1B1b$sources$points <- readxl::read_xlsx(path = source.file, range = "D83:S83", sheet = source.sheet, col_names = header)
source.1B1b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D91:I91", sheet = source.sheet, col_names = vars)
source.1B1b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D92:I92", sheet = source.sheet, col_names = vars)

sf.1B1b <- corsum2sf(source.1B1b) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1B1b %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1B1b - Fugitive emissions from solid fuels: Solid fuel transformation")
sf.1B1b%<>% sf::st_transform(4326)
map.1B1b <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A1 - Energy industries",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1B1b, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("deepskyblue"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1B1b
ggsave(plot = map.1B1b, filename = "Maps/Subcategories/Energy/Map_1B1b.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# #####################################################


# #####################################################
# 1A2 / 2 – Manufacturing industries and Industrial processes
# #####################################################

source.sheet =  "1A2-2-Industry"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]

# 1A2a/2C1 - Iron and steel production

source.1A2a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2a$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S18", sheet = source.sheet, col_names = header)
source.1A2a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D29:I29", sheet = source.sheet, col_names = vars)
source.1A2a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D34:I34", sheet = source.sheet, col_names = vars)


sf.1A2a <- corsum2sf(source.1A2a) %>%
  st_transform(crs = "+init=epsg:32634")


sf.1A2a %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2a/2C1 - Iron and steel production")
sf.1A2a%<>% sf::st_transform(4326)
map.1A2a <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2a, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2a
ggsave(plot = map.1A2a, filename = "Maps/Subcategories/Industry/Map_1A2a.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1A2b / 2C (other than 2C1) - Non-ferrous metals

source.1A2b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2b$sources$points <- readxl::read_xlsx(path = source.file, range = "D35:S40", sheet = source.sheet, col_names = header)
source.1A2b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D49:I49", sheet = source.sheet, col_names = vars)
source.1A2b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D54:I54", sheet = source.sheet, col_names = vars)

sf.1A2b <- corsum2sf(source.1A2b) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1A2b %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2b/2C - Non-ferrous metals")
sf.1A2b %<>% sf::st_transform(4326) 
map.1A2b <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2b, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2b
ggsave(plot = map.1A2b, filename = "Maps/Subcategories/Industry/Map_1A2b.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1A2c/2B - Chemicals

source.1A2c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2c$sources$points <- readxl::read_xlsx(path = source.file, range = "D55:S69", sheet = source.sheet, col_names = header)
source.1A2c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D71:I71", sheet = source.sheet, col_names = vars)
source.1A2c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D86:I86", sheet = source.sheet, col_names = vars)

sf.1A2c <- corsum2sf(source.1A2c) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1A2c %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2c/2B - Chemicals")
sf.1A2c%<>% sf::st_transform(4326)
map.1A2c <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2c, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2c
ggsave(plot = map.1A2c, filename = "Maps/Subcategories/Industry/Map_1A2c.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 1A2d/2H1 - Pulp, paper and print
source.1A2d <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2d$sources$points <- readxl::read_xlsx(path = source.file, range = "D87:S94", sheet = source.sheet, col_names = header)
source.1A2d$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D103:I103", sheet = source.sheet, col_names = vars)
source.1A2d$total$inventory <- readxl::read_xlsx(path = source.file, range = "D109:I109", sheet = source.sheet, col_names = vars)

sf.1A2d <- corsum2sf(source.1A2d, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1A2d %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2d/2H1 - Pulp, paper and print")
sf.1A2d%<>% sf::st_transform(4326)
map.1A2d <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2d, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2d
ggsave(plot = map.1A2d, filename = "Maps/Subcategories/Industry/Map_1A2d.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1A2e/2H2 - Food, beverages and tobacco

source.1A2e <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2e$sources$points <- readxl::read_xlsx(path = source.file, range = "D110:S131", sheet = source.sheet, col_names = header)
source.1A2e$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D137:I137", sheet = source.sheet, col_names = vars)
source.1A2e$total$inventory <- readxl::read_xlsx(path = source.file, range = "D151:I151", sheet = source.sheet, col_names = vars)

sf.1A2e <- corsum2sf(source.1A2e, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1A2e %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2e/2H2 - Food, beverages and tobacco")
sf.1A2e%<>% sf::st_transform(4326)
map.1A2e <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2e, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2e
ggsave(plot = map.1A2e, filename = "Maps/Subcategories/Industry/Map_1A2e.jpg", width = 30, height = 30, units = "cm", device = "jpeg")




# 1A2e.bread - Food, beverages and tobacco - bread
sf.1A2e.bread %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2e.bread - Food, beverages and tobacco - bread")
sf.1A2e.bread%<>% sf::st_transform(4326)
map.1A2e.bread <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2e.bread, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2e.bread
ggsave(plot = map.1A2e.bread, filename = "Maps/Subcategories/Industry/Map_1A2e.bread.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# 1A2e.wine - Food, beverages and tobacco - wine

sf.1A2e.wine %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2e.wine - Food, beverages and tobacco - wine")
sf.1A2e.wine%<>% sf::st_transform(4326)
map.1A2e.wine <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2e.wine, aes(fill = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2e.wine
ggsave(plot = map.1A2e.wine, filename = "Maps/Subcategories/Industry/Map_1A2e.wine.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1A2f/2A (2A1 lime, 2A2 cement and 2A3 glass) - Non-metallic minerals

source.1A2f <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2f$sources$points <- readxl::read_xlsx(path = source.file, range = "D152:S180", sheet = source.sheet, col_names = header)
source.1A2f$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D182:I182", sheet = source.sheet, col_names = vars)
source.1A2f$total$inventory <- readxl::read_xlsx(path = source.file, range = "D189:I189", sheet = source.sheet, col_names = vars)

sf.1A2f <- corsum2sf(source.1A2f, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1A2f %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2f/2A - Non-metallic minerals")
sf.1A2f %<>% sf::st_transform(4326)
map.1A2f <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2f, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2f
ggsave(plot = map.1A2f, filename = "Maps/Subcategories/Industry/Map_1A2f.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1A2g - Auto-production
source.1A2gvi <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2gvi$sources$points <- readxl::read_xlsx(path = source.file, range = "D210:S240", sheet = source.sheet, col_names = header)
source.1A2gvi$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D253:I253", sheet = source.sheet, col_names = vars)
source.1A2gvi$total$inventory <- readxl::read_xlsx(path = source.file, range = "D258:I258", sheet = source.sheet, col_names = vars)

sf.1A2gvi <- corsum2sf(source.1A2gvi, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1A2gvi %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2g - Auto-production")
sf.1A2gvi %<>% sf::st_transform(4326)
map.1A2gvi <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2gvi, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2gvi
ggsave(plot = map.1A2gvi, filename = "Maps/Subcategories/Industry/Map_1A2gvi.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# 1A2g - Other industries


source.1A2g <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2g$sources$points <- readxl::read_xlsx(path = source.file, range = "D190:S194", sheet = source.sheet, col_names = header)
source.1A2g$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D202:I202", sheet = source.sheet, col_names = vars)
source.1A2g$total$inventory <- readxl::read_xlsx(path = source.file, range = "D209:I209", sheet = source.sheet, col_names = vars)

sf.1A2g <- corsum2sf(source.1A2g, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1A2g %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2g - Other industries")


clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
clc121 <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Industrial_sites_new.gpkg") %>%
  st_set_crs(32634)

# Combine all known points into one sf object
points_all <- rbind(sf.1A2a, sf.1A2b, sf.1A2c, sf.1A2d, sf.1A2e, sf.1A2f, sf.1A2gvi) %>% # , sf.1A2g
  st_transform(crs = "+init=epsg:32634")
# clc121.otherIndustries <- st_join(clc121, points_all, join = st_disjoint) 
# mapview(points_all) + mapview(clc121.otherIndustries)

clc121.sp <- as(clc121, "Spatial")
points_all.sp <- as(points_all, "Spatial")
ind <- over(clc121.sp, points_all.sp)
clc121$ind <- ind

clc121 %<>% dplyr::mutate(ind1 = ifelse(is.na(ind), TRUE, FALSE))
clc121 %<>% dplyr::filter(ind1 == TRUE) 

# Polygon Centroid
clc121.otherIndustries <- clc121 %>% dplyr::mutate(Area_Ha = unclass(st_area(.)/10000), SHAPE_Area = unclass(st_area(.)))

clc121.oI.cen <- st_centroid(clc121.otherIndustries) %>%
  select(ID, Area_Ha, SHAPE_Area) %>%
  rename(ID_CLC = ID)

clc121.oI.cen %<>% dplyr::select(geom) %>% dplyr::rename(geometry = geom)
clc121.oI.cen %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2g - Other industries")

sf.1A2g %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2g - Other industries")
clc121.oI.cen %<>% sf::st_transform(4326)
sf.1A2g %<>% sf::st_transform(4326)

sf.1A2g <- rbind(sf.1A2g, clc121.oI.cen)
map.1A2g <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2g, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2g
ggsave(plot = map.1A2g, filename = "Maps/Subcategories/Industry/Map_1A2g.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1A2gvii - Mobile combustion in manufacturing industries and construction

source.1A2gvii <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2gvii$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D268:I268", sheet = source.sheet, col_names = vars)
source.1A2gvii$total$inventory <- readxl::read_xlsx(path = source.file, range = "D269:I269", sheet = source.sheet, col_names = vars)

#+ include = FALSE
clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)

clc133 <- subset(sf_clc18, CODE_18 == "133") %>% # Construction sites
  st_set_crs(32634)

clc121 <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Industrial_sites_new.gpkg") %>%
  sf::st_set_crs(32634) %>%
  dplyr::rename(geometry = geom)

clc133 <- rbind(clc133, clc121)
clc133 %<>% dplyr::mutate(Area_Ha = unclass(st_area(.)/10000), SHAPE_Area = unclass(st_area(.)))
clc133[,vars] <- NA
source.1A2gvii$sources$polygon <- clc133

sf.1A2gvii <- corsum2sf_polygon(source.1A2gvii, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1A2gvii %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A2gvii - Mobile combustion in manufacturing industries and construction")
sf.1A2gvii %<>% sf::st_transform(4326)
map.1A2gvii <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A2/2–Manufacturing industries and Industrial processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A2gvii, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("tomato"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A2gvii
ggsave(plot = map.1A2gvii, filename = "Maps/Subcategories/Industry/Map_1A2gvii.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# #####################################################


# #####################################################
# 2 - Other processes
# #####################################################
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "2-Other processes"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]

# 2A5a - Quarrying and mining of minerals other than coal

source.2A5a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2A5a$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S19", sheet = source.sheet, col_names = header)
source.2A5a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D27:I27", sheet = source.sheet, col_names = vars)
source.2A5a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D28:I28", sheet = source.sheet, col_names = vars)


sf.2A5a <- corsum2sf(source.2A5a, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")


sf.2A5a %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2A5a-Quarrying and mining of minerals other than coal") %>% 
  sf::st_transform(4326)

map.2A5a <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2A5a, aes(color = `Sub-category: `), size= 2)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2A5a
ggsave(plot = map.2A5a, filename = "Maps/Subcategories/Other processes/Map_2A5a.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 2A5b - Construction and demolition
# POKRENUTO IZ SKRIPTE
sf.2A5b # za kartu ne raditi intersect i source.2A5b$sources$polygon <- sf_final
sf.2A5b %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2A5b - Construction and demolition") %>% 
  sf::st_transform(4326)

map.2A5b <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2A5b, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2A5b
ggsave(plot = map.2A5b, filename = "Maps/Subcategories/Other processes/Map_2A5b.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 2A5c-Storage, handling and transport of mineral products
# POKRENUTO IZ SKRIPTE
sf.2A5c 
sf.2A5c %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2A5c-Storage, handling and transport of mineral products") %>% 
  sf::st_transform(4326)

map.2A5c <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2A5c, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2A5c
ggsave(plot = map.2A5c, filename = "Maps/Subcategories/Other processes/Map_2A5c.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 2D3a-Domestic solvent use including fungicides
# POKRENUTO IZ SKRIPTE
sf.2D3a 
sf.2D3a %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3a - Domestic solvent use including fungicides") %>% 
  sf::st_transform(4326)

map.2D3a <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3a, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3a
ggsave(plot = map.2D3a, filename = "Maps/Subcategories/Other processes/Map_2D3a.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 2D3b-Road paving with asphalt
# POKRENUTO IZ SKRIPTE

sf.2D3b 
sf.2D3b %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3b - Road paving with asphalt") %>% 
  sf::st_transform(4326)

map.2D3b <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3b, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_color_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3b
ggsave(plot = map.2D3b, filename = "Maps/Subcategories/Other processes/Map_2D3b.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 2D3c-Asphalt roofing
# POKRENUTO IZ SKRIPTE

sf.2D3c <- sf.2A5b
sf.2D3c %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3c - Asphalt roofing") %>% 
  sf::st_transform(4326)

map.2D3c <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3c, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3c
ggsave(plot = map.2D3c, filename = "Maps/Subcategories/Other processes/Map_2D3c.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 2D3d-Coating applications

## 2D3d - Coating application - paint for construction
sf.2D3d.pfc

sf.2D3d.pfc %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3d.pfc - Coating application - paint for construction") %>% 
  sf::st_transform(4326)

map.2D3d.pfc <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3d.pfc, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_colour_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3d.pfc
ggsave(plot = map.2D3d.pfc, filename = "Maps/Subcategories/Other processes/Map_2D3d.pfc.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

## 2D3d - Coating application - Car/bus/truck/van coating + leather finishing
sf.2D3d.cbtlf

sf.2D3d.cbtlf %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3d.cbtlf-Coating application-Car/bus/truck/van coating+leather finishing") %>% 
  sf::st_transform(4326)

map.2D3d.cbtlf <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3d.cbtlf, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3d.cbtlf
ggsave(plot = map.2D3d.cbtlf, filename = "Maps/Subcategories/Other processes/Map_2D3d.cbtlf.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 2D3e-Degreasing
sf.2D3e

sf.2D3e %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3e - Degreasing") %>% 
  sf::st_transform(4326)

map.2D3e <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3e, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3e
ggsave(plot = map.2D3e, filename = "Maps/Subcategories/Other processes/Map_2D3e.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 2D3f-Dry cleaning
sf.2D3f 
sf.2D3f %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3f - Dry cleaning") %>% 
  sf::st_transform(4326)

map.2D3f <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3f, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3f
ggsave(plot = map.2D3f, filename = "Maps/Subcategories/Other processes/Map_2D3f.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# 2D3g-Chemical products

## 2D3g - Chemical products - Rubber processing

sf.2D3g.rp

sf.2D3g.rp %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3g - Chemical products: 2D3g.rp, 2D3g.pigm, 2D3g.ms, 2D3g.lt") %>% 
  sf::st_transform(4326)

map.2D3g.rp <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3g.rp, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3g.rp
ggsave(plot = map.2D3g.rp, filename = "Maps/Subcategories/Other processes/Map_2D3g.rp.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 2D3h-Printing
sf.2D3h

sf.2D3h %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3h - Printing") %>% 
  sf::st_transform(4326)

map.2D3h <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3h, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3h
ggsave(plot = map.2D3h, filename = "Maps/Subcategories/Other processes/Map_2D3h.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# 2D3i-Other solvent and product use

sf.2D3i.feneox
sf.2D3i.feneox %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3i-Other solvent and product use: 2D3i.feneox, 2D3i.utcv") %>% 
  sf::st_transform(4326)

map.2D3i.feneox <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3i.feneox, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3i.feneox
ggsave(plot = map.2D3i.feneox, filename = "Maps/Subcategories/Other processes/Map_2D3i.feneox.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

sf.2D3i.pow

sf.2D3i.pow %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2D3i-Other solvent and product use: 2D3i.pow, 2D3i.t, 2D3i.uos") %>% 
  sf::st_transform(4326)

map.2D3i.pow <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2D3i.pow, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_colour_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2D3i.pow
ggsave(plot = map.2D3i.pow, filename = "Maps/Subcategories/Other processes/Map_2D3i.pow.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 2I-Wood processing

sf.2I

sf.2I %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "2I - Wood processing") %>% 
  sf::st_transform(4326)

map.2I <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 2 - Other processes",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.2I, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("violet"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.2I
ggsave(plot = map.2I, filename = "Maps/Subcategories/Other processes/Map_2I.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# #####################################################

# #####################################################
# 1B - Fugitive emissions
# #####################################################

source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "1B-Fugitive emissions"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]

# 1B1a-Fugitive emission from solid fuels: Coal mining and handling
sf.1B1a
sf.1B1a %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1B1a-Fugitive emission from solid fuels: Coal mining and handling") %>% 
  sf::st_transform(4326)

map.1B1a <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1B - Fugitive emissions",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1B1a, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("purple"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1B1a
ggsave(plot = map.1B1a, filename = "Maps/Subcategories/Fugitive emissions/Map_1B1a.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 1B2ai -Fugitive emissions from liquid fuels : Exploration, production, transport

sf.1B2ai
sf.1B2ai %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1B2ai-Fugitive emissions from liquid fuels: Exploration, production, transport") %>% 
  sf::st_transform(4326)

map.1B2ai <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1B - Fugitive emissions",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1B2ai, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_colour_manual(values=c("purple"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1B2ai
ggsave(plot = map.1B2ai, filename = "Maps/Subcategories/Fugitive emissions/Map_1B2ai.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1B2av-Fugitive emissions from liquid fuels: Distribution of oil products
sf.1B2av
sf.1B2av %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1B2av-Fugitive emissions from liquid fuels: Distribution of oil products") %>% 
  sf::st_transform(4326)

map.1B2av <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1B - Fugitive emissions",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1B2av, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_colour_manual(values=c("purple"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1B2av
ggsave(plot = map.1B2av, filename = "Maps/Subcategories/Fugitive emissions/Map_1B2av.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1B2b-Fugitive emissions from natural gas: Exploration, production, transport

sf.1B2b
sf.1B2b %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1B2b-Fugitive emissions from natural gas: Exploration, production, transport") %>% 
  sf::st_transform(4326)

map.1B2b <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1B - Fugitive emissions",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1B2b, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_colour_manual(values=c("purple"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1B2b
ggsave(plot = map.1B2b, filename = "Maps/Subcategories/Fugitive emissions/Map_1B2b.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# #####################################################


# #####################################################
# 1A4-Residential/Tertiary
# #####################################################
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "1A4-Residential-Tertiary"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]


# 1A4ai-Commercial/institutional: Stationary Combustion

sf.1A4ai

sf.1A4ai %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A4ai - Commercial/institutional: Stationary Combustion") %>% 
  sf::st_transform(4326)

map.1A4ai <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A4 - Residential/Tertiary",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A4ai, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("deepskyblue"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A4ai
ggsave(plot = map.1A4ai, filename = "Maps/Subcategories/Residential-Tertiary/Map_1A4ai.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 1A4bi-Residential: Stationary combustion

sf.1A4bi

sf.1A4bi %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A4bi - Residential: Stationary combustion") %>% 
  sf::st_transform(4326)

map.1A4bi <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A4 - Residential/Tertiary",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A4bi, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("deepskyblue"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A4bi
ggsave(plot = map.1A4bi, filename = "Maps/Subcategories/Residential-Tertiary/Map_1A4bi.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1A4ci-Agriculture/Forestry/Fishing: Stationary combustion

sf.1A4ci

sf.1A4ci %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A4ci - Agriculture/Forestry/Fishing: Stationary combustion") %>% 
  sf::st_transform(4326)

map.1A4ci <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A4 - Residential/Tertiary",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A4ci, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("deepskyblue"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A4ci
ggsave(plot = map.1A4ci, filename = "Maps/Subcategories/Residential-Tertiary/Map_1A4ci.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 1A4cii-Agriculture/Forestry/Fishing: Off-road vehicles and other machinery

sf.1A4cii

sf.1A4cii %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "1A4cii-Agriculture/Forestry/Fishing: Off-road vehicles and other machinery") %>% 
  sf::st_transform(4326)

map.1A4cii <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 1A4 - Residential/Tertiary",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.1A4cii, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("deepskyblue"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.1A4cii
ggsave(plot = map.1A4cii, filename = "Maps/Subcategories/Residential-Tertiary/Map_1A4cii.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# #####################################################

# #####################################################
# 1A3 – Transports
# #####################################################
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "1A3-Transport"
header <- readxl::read_xlsx(path = source.file, range = "D9:S9", sheet = source.sheet) %>% names()
vars <- header[1:6]

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
       title = "Map of road network with Vehicle Counting Devices")+#,
  geom_sf(data = sf_roads.4326, aes(size = lwd1*1.5))+
  scale_size_identity()+
  geom_sf(aes(color = Category), size= 2.5)+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

vcds_map

ggsave(plot = vcds_map, filename = "Maps/Subcategories/Transport/Map_vcds.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# VCDs map zoomed

vcds_zoomed <- st_read(dsn = "Data/vcds_map_zoomed/vcds_zoomed.gpkg")

roads_zoomed <- st_read(dsn = "Data/vcds_map_zoomed/roads_zoomed.gpkg")

vc <- vcds_zoomed %>% sf::st_transform(4326)
ro <- roads_zoomed %>% sf::st_transform(4326)

vc %<>% st_zm(., drop = TRUE)
vc %<>% dplyr::filter(Kategorija != "nije u mrezi") %>%
  dplyr::mutate(Category = Kategorija)

vcds_map_zoomed <- ggplot(data = vc)+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Territory of the Repubic of Serbia",
       title = "Map of road network with Vehicle Counting Devices - IDW interpolation")+#,
  geom_sf(data = ro, aes(size = lwd1*1.5))+
  scale_size_identity()+
  geom_sf(aes(color = Category), size= 2.5)+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

vcds_map_zoomed

ggsave(plot = vcds_map_zoomed, filename = "Maps/Subcategories/Transport/Map_vcds_zoomed.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# #####################################################


# #####################################################
# 5 – Waste
# #####################################################
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "5-Waste"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]

# 5A-Biological treatment of waste - Solid waste disposal on land

sf.5A
sf.5A %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "5A-Biological treatment of waste - Solid waste disposal on land") %>% 
  sf::st_transform(4326)

map.5A <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 5 – Waste",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.5A, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("#1f7a1f"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.5A
ggsave(plot = map.5A, filename = "Maps/Subcategories/Waste/Map_5A.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 5C1bv-Cremation

sf.5C1bv
sf.5C1bv %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "5C1bv - Cremation") %>% 
  sf::st_transform(4326)

map.5C1bv <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 5 – Waste",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.5C1bv, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_colour_manual(values=c("#1f7a1f"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.5C1bv
ggsave(plot = map.5C1bv, filename = "Maps/Subcategories/Waste/Map_5C1bv.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 5D1-Domestic wastewater handling

sf.5D1
sf.5D1 %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "5D1 - Domestic wastewater handling") %>% 
  sf::st_transform(4326)

map.5D1 <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 5 – Waste",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.5D1, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("#1f7a1f"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.5D1
ggsave(plot = map.5D1, filename = "Maps/Subcategories/Waste/Map_5D1.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 5D2-Industrial wastewater handling

sf.5D2

sf.5D2 %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "5D2 - Industrial wastewater handling") %>% 
  sf::st_transform(4326)

map.5D2 <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 5 – Waste",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.5D2, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_colour_manual(values=c("#1f7a1f"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.5D2
ggsave(plot = map.5D2, filename = "Maps/Subcategories/Waste/Map_5D2.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# #####################################################

# #####################################################
# 3 - Agriculture
# #####################################################
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "3-Agriculture"
header <- readxl::read_xlsx(path = source.file, range = "D7:S7", sheet = source.sheet) %>% names()
vars <- header[1:6]

# Dairy cattle, non-dairy cattle, sheep, goats, turkeys, other poultry

sf.3B1a <- sf_rural 

sf.3B1a %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "3B1a-Dairy cattle, non-dairy cattle, sheep, goats, turkeys, other poultry") %>% 
  sf::st_transform(4326)

map.3B1a <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 3 - Agriculture",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.3B1a, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("red"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.3B1a
ggsave(plot = map.3B1a, filename = "Maps/Subcategories/Agriculture/Map_3B1a.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# Swine (3B3) 
sf.3B3 
sf.3B3 %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "3B3 - Swine") %>% 
  sf::st_transform(4326)

map.3B3 <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 3 - Agriculture",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.3B3, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_colour_manual(values=c("red"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.3B3
ggsave(plot = map.3B3, filename = "Maps/Subcategories/Agriculture/Map_3B3.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 3B4gi & 3B4gii-Laying hens & Broilers

sf.3B4gi_gii 
sf.3B4gi_gii %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "3B4gi & 3B4gii - Laying hens & Broilers") %>% 
  sf::st_transform(4326)

map.3B4gi_gii <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 3 - Agriculture",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.3B4gi_gii, aes(colour = `Sub-category: `))+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_colour_manual(values=c("red"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.3B4gi_gii
ggsave(plot = map.3B4gi_gii, filename = "Maps/Subcategories/Agriculture/Map_3B4gi_gii.jpg", width = 30, height = 30, units = "cm", device = "jpeg")




# 3Da1 - Inorganic N-fertilizers (includes also urea application)
sf.3Da1

sf.3Da1 %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "3Da1 - Inorganic N-fertilizers (includes also urea application)") %>% 
  sf::st_transform(4326)

map.3Da1 <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 3 - Agriculture",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.3Da1, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("red"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.3Da1
ggsave(plot = map.3Da1, filename = "Maps/Subcategories/Agriculture/Map_3Da1.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# 3Da2a - Animal manure applied to soils
sf.3Da2a <- sf.3Da1
sf.3Da2a %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "3Da2a - Animal manure applied to soils") %>% 
  sf::st_transform(4326)

map.3Da2a <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 3 - Agriculture",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.3Da2a, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("red"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.3Da2a
ggsave(plot = map.3Da2a, filename = "Maps/Subcategories/Agriculture/Map_3Da2a.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# 3Da3 - Urine and dung deposited by grazing animals
sf.3Da3
sf.3Da3 %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "3Da3 - Urine and dung deposited by grazing animals") %>% 
  sf::st_transform(4326)

map.3Da3 <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 3 - Agriculture",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.3Da3, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("red"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.3Da3
ggsave(plot = map.3Da3, filename = "Maps/Subcategories/Agriculture/Map_3Da3.jpg", width = 30, height = 30, units = "cm", device = "jpeg")


# 3Dc-Farm-level agricultural operations including storage, handling and transport of agricultural products

sf.3Dc
sf.3Dc %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "3Dc-Farm-level agricultural operations including storage, handling and transport of agricultural products") %>% 
  sf::st_transform(4326)

map.3Dc <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 3 - Agriculture",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.3Dc, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("red"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.3Dc
ggsave(plot = map.3Dc, filename = "Maps/Subcategories/Agriculture/Map_3Dc.jpg", width = 30, height = 30, units = "cm", device = "jpeg")



# Cultivated crops
sf.3De

sf.3De %<>% dplyr::select() %>% dplyr::mutate(`Sub-category: ` = "3De - Cultivated crops") %>% 
  sf::st_transform(4326)

map.3De <- ggplot()+
  labs(x = "Longitude [deg]", y="Latitude [deg]",
       caption = "Coordinate Reference System - WGS84",
       subtitle = "Category: 3 - Agriculture",
       title = "Spatial locations - GIS layers")+#,
  geom_sf(data = sf.3De, aes(fill = `Sub-category: `), colour = NA)+
  geom_sf(data = sf_granica, colour = "black", fill = NA)+
  scale_fill_manual(values=c("red"))+
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
map.3De
ggsave(plot = map.3De, filename = "Maps/Subcategories/Agriculture/Map_3De.jpg", width = 30, height = 30, units = "cm", device = "jpeg")

# #####################################################
