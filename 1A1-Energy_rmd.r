#' ---
#' title: "Pollutant inventory spatialization"
#' author:
#'    - "Milan Kilibarda"
#'    - "Dragutin Protic"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output:   
#'    html_document:
#'      keep_md: true
#'      theme: "simplex"
#'      highlight: tango
#'      toc: true
#'      toc_depth: 5
#'      toc_float: true
#'      fig_caption: yes
#' ---
#' 
#+ include = FALSE 
# <img src="Logo-Trans.png" align="center" alt="logo" width="2000" height = "3000" style = "border: none; float: right;">
#' 
#' # 1A1 - Energys
#' This document provides the methodlogy and the main results regarding the spatialization of pollutation inventory.
#' 
#' 
#+ include = TRUE, echo = FALSE, results = 'hide', warning = FALSE, message = FALSE
#library(XLConect)
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

#' 
#' 
#+ include = FALSE
# Plot settings
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
#' 
#' 
#' 
#+ include = FALSE, echo = FALSE
# source.list = source.1A1a
corsum2sf <- function(source.list){
  source.list$sources$points[, vars] <- source.list$sources$points[, vars] %>% dplyr::mutate_all(~replace(., is.na(.), 0))
  source.list[[2]][[2]][, vars] <- source.list[[2]][[2]][, vars] %>% dplyr::mutate_all(~replace(., is.na(.), 0))
  
  points.sum <- source.list$sources$points %>% 
    dplyr::select(., vars) %>% 
    apply(., 2, sum) %>% 
    t(.) %>% 
    as.data.frame() %>%
    dplyr::mutate_if(is.numeric, round, 2)
  
  points.total <- source.list[[2]][[2]][, vars] %>% 
    dplyr::mutate_if(is.numeric, round, 2) %>%
    as.data.frame()
  
  if(!identical(points.sum, points.total)){
    d <- (points.total - points.sum)[1, ]
    zero.ind <- source.list$sources$points[, vars] == 0
    w <- replace(source.list$sources$points[, vars], zero.ind, NA) %>% 
      apply(., 2, function(x) x/sum(x)) %>%
      as.data.frame() %>%
      dplyr::mutate_all(~replace(., is.na(.), 0))
      
    cor.data <- as.matrix(w) %*% diag(d) + source.list$sources$points[, vars]
    source.list$sources$points[, vars] <- cor.data
    source.sf <- st_as_sf(x = source.list$sources$points, coords = c("Longitude", "Latitude"),  crs = "+proj=longlat +datum=WGS84")
  }else{
    source.sf <- st_as_sf(x = source.list$sources$points, coords = c("Longitude", "Latitude"),  crs = "+proj=longlat +datum=WGS84")
  }
  return(source.sf)
}
# Function for spatial data visualisation at web maps
# Parameters:
#    1. sf.sources -  sf object with sources to be spatialised
#    2. sf.spatialised - sf object with polygons after spatialisation
spatialised.mapview <- function(sf.sources, layer.name.1 = "", sf.spatialised, layer.name.2 = "", vars = vars){
  sf.spatialised$Spatialised <- NA
  sf.spatialised[ ,vars] %<>% st_drop_geometry() %>% dplyr::mutate_all(.,as.double)
  sf.spatialised$Spatialised[sf.spatialised$NOx == 0 & sf.spatialised$SO2 == 0 & sf.spatialised$PM10 == 0 & sf.spatialised$PM2.5 == 0 & sf.spatialised$NMVOC == 0 & sf.spatialised$NH3 == 0] <- 0
  sf.spatialised$Spatialised[sf.spatialised$NOx !=0 | sf.spatialised$SO2 !=0 | sf.spatialised$PM10 !=0 | sf.spatialised$PM2.5 !=0 | sf.spatialised$NMVOC !=0 | sf.spatialised$NH3 != 0] <- 1
  web_map <- mapview(sf.spatialised, layer.name = layer.name.2, zcol = "Spatialised") + mapview(sf.sources, layer.name = layer.name.1, col.regions = "red")
  return(web_map)
}

#'
#'
#+ include = FALSE
source.file = "Pollutant inventory spatialized-d-v3.xlsx"
source.sheet =  "1A1-Energy"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Grid_5km_Serbia_new.gpkg")
sf.grid.5km <- st_as_sf(grid.5km)
#'
#'
#' ## 1A1a - Public electricity production
#'
#'
#'
#+ include = FALSE
source.1A1a_ep <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1a_ep$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S29", sheet = source.sheet, col_names = header)
source.1A1a_ep$total$spatialize <- readxl::read_xlsx(path = source.file, range = "T34:Y34", sheet = source.sheet, col_names = vars)
source.1A1a_ep$total$inventory <- readxl::read_xlsx(path = source.file, range = "T34:Y34", sheet = source.sheet, col_names = vars)


sf.1A1a_ep <- corsum2sf(source.1A1a_ep) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A1a_ep %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.1A1a_ep',
            rownames = FALSE, escape = FALSE, selection = "single",
            extensions = c('Buttons'),
            class = 'white-space: nowrap',
            options = list(
              pageLength = 5,
              dom = 'Bfrtip',
              buttons = list('pageLength'),
              searchHighlight = TRUE,
              scrollX = TRUE,
              scrollY = TRUE
            ))
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A1a_ep <- sf.1A1a_ep %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A1a_ep <- source.1A1a_ep[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A1a_ep, total.1A1a_ep, data.frame(sum.1A1a_ep == total.1A1a_ep)-1)) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE, echo = FALSE, result = FALSE
# p.1A1a_ep <- sf.grid.5km %>%
#   st_join(., sf.1A1a_ep, join = st_intersects) %>%
#   dplyr::select(ID, NOx, SO2, PM10, PM2.5, NMVOC, NH3)
p.1A1a_ep <- sf.grid.5km %>%
  st_join(sf.1A1a_ep) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A1a_ep, layer.name = "Sources 1A1a_ep", col.regions = "red") + mapview(p.1A1a_ep)
spatialised.mapview(sf.sources = sf.1A1a_ep, layer.name.1 = "Sources 1A1a_ep", sf.spatialised = p.1A1a_ep, layer.name.2 = "Spatialised 1A1a_ep", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A1a_ep <- p.1A1a_ep %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A1a_ep, total.1A1a_ep, data.frame(sum.p.1A1a_ep == total.1A1a_ep)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

# p.1A1a_ep %>% dplyr::arrange(., ID)
#+ include = FALSE
p.1A1a_ep %<>% sf::st_transform(4326)
# st_write(p.1A1a_ep, dsn = "Products/1A1 - Energy/1A1a_ep.gpkg", layer = '1A1a_ep')



#'
#'
#' ## 1A1a - Public heat production
#'
#'
#'
#+ include = FALSE
source.1A1a_hp <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1a_hp$sources$points <- readxl::read_xlsx(path = source.file, range = "D30:S37", sheet = source.sheet, col_names = header)
source.1A1a_hp$total$spatialize <- readxl::read_xlsx(path = source.file, range = "T35:Y35", sheet = source.sheet, col_names = vars)
source.1A1a_hp$total$inventory <- readxl::read_xlsx(path = source.file, range = "T35:Y35", sheet = source.sheet, col_names = vars)


sf.1A1a_hp <- corsum2sf(source.1A1a_hp) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A1a_hp %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.1A1a_hp',
            rownames = FALSE, escape = FALSE, selection = "single",
            extensions = c('Buttons'),
            class = 'white-space: nowrap',
            options = list(
              pageLength = 5,
              dom = 'Bfrtip',
              buttons = list('pageLength'),
              searchHighlight = TRUE,
              scrollX = TRUE,
              scrollY = TRUE
            ))
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A1a_hp <- sf.1A1a_hp %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A1a_hp <- source.1A1a_hp[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A1a_hp, total.1A1a_hp, data.frame(sum.1A1a_hp == total.1A1a_hp)-1)) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE, echo = FALSE, result = FALSE
# p.1A1a_hp <- sf.grid.5km %>%
#   st_join(., sf.1A1a_hp, join = st_intersects) %>%
#   dplyr::select(ID, NOx, SO2, PM10, PM2.5, NMVOC, NH3)
p.1A1a_hp <- sf.grid.5km %>%
  st_join(sf.1A1a_hp) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A1a_hp, layer.name = "Sources 1A1a_hp", col.regions = "red") + mapview(p.1A1a_hp)
spatialised.mapview(sf.sources = sf.1A1a_hp, layer.name.1 = "Sources 1A1a_hp", sf.spatialised = p.1A1a_hp, layer.name.2 = "Spatialised 1A1a_hp", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A1a_hp <- p.1A1a_hp %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A1a_hp, total.1A1a_hp, data.frame(sum.p.1A1a_hp == total.1A1a_hp)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
p.1A1a_hp %<>% sf::st_transform(4326)
st_write(p.1A1a_hp, dsn = "Products/1A1 - Energy/1A1a_hp.gpkg", layer = '1A1a_hp')

#'
#'
#'
#' ## 1A1b - Refineries
#+ include = FALSE
source.1A1b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1b$sources$points <- readxl::read_xlsx(path = source.file, range = "D47:S50", sheet = source.sheet, col_names = header)
source.1A1b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D51:I51", sheet = source.sheet, col_names = vars)
source.1A1b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D52:I52", sheet = source.sheet, col_names = vars)

sf.1A1b <- corsum2sf(source.1A1b) %>%
  st_transform(crs = "+init=epsg:32634")
#'
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A1b %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.1A1b',
            rownames = FALSE, escape = FALSE, selection = "single",
            extensions = c('Buttons'),
            class = 'white-space: nowrap',
            options = list(
              pageLength = 5,
              dom = 'Bfrtip',
              buttons = list('pageLength'),
              searchHighlight = TRUE,
              scrollX = TRUE,
              scrollY = TRUE
            ))
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A1b <- sf.1A1b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A1b <- source.1A1b[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A1b, total.1A1b, data.frame(sum.1A1b == total.1A1b)-1)) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE, echo = FALSE, result = FALSE
p.1A1b <- sf.grid.5km %>%
  st_join(sf.1A1b) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A1b, layer.name = "Sources 1A1b", col.regions = "red") + mapview(p.1A1b)
spatialised.mapview(sf.sources = sf.1A1b, layer.name.1 = "Sources 1A1b", sf.spatialised = p.1A1b, layer.name.2 = "Spatialised 1A1b", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A1b <- p.1A1b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A1b, total.1A1b, data.frame(sum.p.1A1b == total.1A1b)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
p.1A1b %<>% sf::st_transform(4326)
st_write(p.1A1b, dsn="Products/1A1 - Energy/1A1b_1B2aiv.gpkg", layer='1A1b_1B2aiv')


# !!!!!!!!!!!!!!!!!!!!!!
# MERGED WITH RAFINERIES
# !!!!!!!!!!!!!!!!!!!!!!


#'
#' ## 1B2aiv - Fugitive emissions from liquid fuels: Refining, storage
#'
#+ include = FALSE

# source.1B2aiv <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))
# 
# source.1B2aiv$sources$points <- readxl::read_xlsx(path = source.file, range = "D53:S56", sheet = source.sheet, col_names = header)
# source.1B2aiv$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D57:I57", sheet = source.sheet, col_names = vars)
# source.1B2aiv$total$inventory <- readxl::read_xlsx(path = source.file, range = "D58:I58", sheet = source.sheet, col_names = vars)
# 
# sf.1B2aiv <- corsum2sf(source.1B2aiv) %>%
#   st_transform(crs = "+init=epsg:32634")
# 
# #+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1B2aiv %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 7: sf.1A2aiv',
#             rownames = FALSE, escape = FALSE, selection = "single",
#             extensions = c('Buttons'),
#             class = 'white-space: nowrap',
#             options = list(
#               pageLength = 5,
#               dom = 'Bfrtip',
#               buttons = list('pageLength'),
#               searchHighlight = TRUE,
#               scrollX = TRUE,
#               scrollY = TRUE
#             ))
# #'
# #'
# #+ echo = FALSE, result = TRUE, eval = TRUE
# sum.1B2aiv <- sf.1B2aiv %>% 
#   st_drop_geometry() %>%
#   dplyr::select(., vars) %>% 
#   apply(., 2, sum) %>% 
#   t(.) %>% 
#   as.data.frame() %>%
#   dplyr::mutate_if(is.numeric, round, 2)
# total.1B2aiv <- source.1B2aiv[[2]][[2]][, vars] %>% 
#   mutate_all(~replace(., is.na(.), 0)) %>%
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   as.data.frame()
# 
# data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1B2aiv, total.1B2aiv, data.frame(sum.1B2aiv == total.1B2aiv)-1)) %>%
#   datatable(., caption = 'Table 8: Summary differences',
#             options = list(pageLength = 5)
#   )
# #+ include = FALSE, echo = FALSE, result = FALSE
# p.1B2aiv <- sf.grid.5km %>%
#   st_join(sf.1B2aiv) %>%
#   group_by(ID) %>%
#   summarize(NOx = sum(NOx, na.rm = TRUE),
#             SO2 = sum(SO2, na.rm = TRUE),
#             PM10 = sum(PM10, na.rm = TRUE),
#             PM2.5 = sum(PM2.5, na.rm = TRUE),
#             NMVOC = sum(NMVOC, na.rm = TRUE),
#             NH3 = sum(NH3, na.rm = TRUE)) %>% 
#   mutate(ID = as.numeric(ID))
# 
# #+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# # mapview(sf.1B2aiv, layer.name = "Sources 1B2aiv", col.regions = "red") + mapview(p.1B2aiv)
# spatialised.mapview(sf.sources = sf.1B2aiv, layer.name.1 = "Sources 1B2aiv", sf.spatialised = p.1B2aiv, layer.name.2 = "Spatialised 1B2aiv", vars = vars)
# 
# #+ echo = FALSE, result = TRUE, eval = TRUE
# sum.p.1B2aiv <- p.1B2aiv %>% 
#   st_drop_geometry() %>%
#   dplyr::select(., vars) %>% 
#   apply(., 2, sum) %>% 
#   t(.) %>% 
#   as.data.frame() %>%
#   dplyr::mutate_if(is.numeric, round, 2)
# data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1B2aiv, total.1B2aiv, data.frame(sum.p.1B2aiv == total.1B2aiv)-1)) %>%
#   datatable(., caption = 'Table 9: Summary differences after spatialisation',
#             options = list(pageLength = 5)
#   )
# 
#+ include = FALSE
# st_write(p.1B2aiv, dsn="Products/1A1 - Energy/1B2aiv.gpkg", layer='1B2aiv')

#'
#'
#'
#'
#'
#'
#' ## 1B2c - Fugitive emissions: Venting and flaring
#' 
#+ include = FALSE
source.1B2c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1B2c$sources$points <- readxl::read_xlsx(path = source.file, range = "D53:S56", sheet = source.sheet, col_names = header)
source.1B2c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D57:I57", sheet = source.sheet, col_names = vars)
source.1B2c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D58:I58", sheet = source.sheet, col_names = vars)

sf.1B2c <- corsum2sf(source.1B2c) %>%
  st_transform(crs = "+init=epsg:32634")
#'
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1B2c %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 10: sf.1B2c',
            rownames = FALSE, escape = FALSE, selection = "single",
            extensions = c('Buttons'),
            class = 'white-space: nowrap',
            options = list(
              pageLength = 5,
              dom = 'Bfrtip',
              buttons = list('pageLength'),
              searchHighlight = TRUE,
              scrollX = TRUE,
              scrollY = TRUE
            ))
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1B2c <- sf.1B2c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1B2c <- source.1B2c[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1B2c, total.1B2c, data.frame(sum.1B2c == total.1B2c)-1)) %>%
  datatable(., caption = 'Table 11: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE, echo = FALSE, result = FALSE
p.1B2c <- sf.grid.5km %>%
  st_join(sf.1B2c) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1B2c, layer.name = "Sources 1B2c", col.regions = "red") + mapview(p.1B2c)
spatialised.mapview(sf.sources = sf.1B2c, layer.name.1 = "Sources 1B2c", sf.spatialised = p.1B2c, layer.name.2 = "Spatialised 1B2c", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1B2c <- p.1B2c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1B2c, total.1B2c, data.frame(sum.p.1B2c == total.1B2c)-1)) %>%
  datatable(., caption = 'Table 12: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
p.1B2c %<>% sf::st_transform(4326)
# st_write(p.1B2c, dsn="Products/1A1 - Energy/1B2c.gpkg", layer='1B2c')

#'
#'
#'
#' ## 1A1c - Manufacturing of solid fuels
#'
#+ include = FALSE
source.1A1c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1c$sources$points <- readxl::read_xlsx(path = source.file, range = "D59:S67", sheet = source.sheet, col_names = header)
source.1A1c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D75:I75", sheet = source.sheet, col_names = vars)
source.1A1c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D76:I76", sheet = source.sheet, col_names = vars)

sf.1A1c <- corsum2sf(source.1A1c) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A1c %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 13: sf.1A1c',
            rownames = FALSE, escape = FALSE, selection = "single",
            extensions = c('Buttons'),
            class = 'white-space: nowrap',
            options = list(
              pageLength = 5,
              dom = 'Bfrtip',
              buttons = list('pageLength'),
              searchHighlight = TRUE,
              scrollX = TRUE,
              scrollY = TRUE
            ))
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A1c <- sf.1A1c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A1c <- source.1A1c[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A1c, total.1A1c, data.frame(sum.1A1c == total.1A1c)-1)) %>%
  datatable(., caption = 'Table 14: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE, echo = FALSE, result = FALSE
p.1A1c <- sf.grid.5km %>%
  st_join(sf.1A1c) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))


#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A1c, layer.name = "Sources 1A1c", col.regions = "red") + mapview(p.1A1c)
spatialised.mapview(sf.sources = sf.1A1c, layer.name.1 = "Sources 1A1c", sf.spatialised = p.1A1c, layer.name.2 = "Spatialised 1A1c", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A1c <- p.1A1c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A1c, total.1A1c, data.frame(sum.p.1A1c == total.1A1c)-1)) %>%
  datatable(., caption = 'Table 15: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
p.1A1c %<>% sf::st_transform(4326)
# st_write(p.1A1c, dsn="Products/1A1 - Energy/1A1c.gpkg", layer='1A1c')

#'
#'
#'
#'
#'
#'
#' ## 1B1b - Fugitive emissions from solid fuels: Solid fuel transformation
#'
#+ include = FALSE
source.1B1b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1B1b$sources$points <- readxl::read_xlsx(path = source.file, range = "D77:S77", sheet = source.sheet, col_names = header)
source.1B1b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D79:I79", sheet = source.sheet, col_names = vars)
source.1B1b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D80:I80", sheet = source.sheet, col_names = vars)

sf.1B1b <- corsum2sf(source.1B1b) %>%
  st_transform(crs = "+init=epsg:32634")
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1B1b %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 16: sf.1B1b',
            rownames = FALSE, escape = FALSE, selection = "single",
            extensions = c('Buttons'),
            class = 'white-space: nowrap',
            options = list(
              pageLength = 5,
              dom = 'Bfrtip',
              buttons = list('pageLength'),
              searchHighlight = TRUE,
              scrollX = TRUE,
              scrollY = TRUE
            ))
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1B1b <- sf.1B1b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1B1b <- source.1B1b[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1B1b, total.1B1b, data.frame(sum.1B1b == total.1B1b)-1)) %>%
  datatable(., caption = 'Table 17: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE, echo = FALSE, result = FALSE
p.1B1b <- sf.grid.5km %>%
  st_join(sf.1B1b) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1B1b, layer.name = "Sources 1B1b", col.regions = "red") + mapview(p.1B1b)
spatialised.mapview(sf.sources = sf.1B1b, layer.name.1 = "Sources 1B1b", sf.spatialised = p.1B1b, layer.name.2 = "Spatialised 1B1b", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1B1b <- p.1B1b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1B1b, total.1B1b, data.frame(sum.p.1B1b == total.1B1b)-1)) %>%
  datatable(., caption = 'Table 18: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
p.1B1b %<>% sf::st_transform(4326)
# st_write(p.1B1b, dsn="Products/1A1 - Energy/1B1b.gpkg", layer='1B1b')

#'
#'
#'
#'
#'
#'
#'
#+ include = FALSE 
# rmarkdown::render(here::here("/1A1-Energy_rmd.r"), output_dir = here::here("/Reports"))


