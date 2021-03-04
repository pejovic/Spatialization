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
#' # 1B-Fugitive emissions
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
library(SerbianCyrLat)
library(s2)
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
#source.list = source.1A2gvi
# Parameters:
#  - distribute - TRUE or FALSE, depend on that if someone want to distribute total inventory emissions to sources, when difference exists
corsum2sf <- function(source.list, distribute = FALSE){
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
  
  if(!identical(points.sum, points.total) & distribute == TRUE){
    d <- (points.total - points.sum)[1, ]
    zero.ind <- source.list$sources$points[, vars] == 0
    w <- replace(source.list$sources$points[, vars], zero.ind, 1) %>% 
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
# source.list = source.1A2gvii
corsum2sf_polygon <- function(source.list, distribute = FALSE){
  source.list$sources$polygon[, vars] <- source.list$sources$polygon[, vars] %>% dplyr::mutate_all(~replace(., is.na(.), 0)) %>% st_drop_geometry()
  source.list[[2]][[2]][, vars] <- source.list[[2]][[2]][, vars] %>% dplyr::mutate_all(~replace(., is.na(.), 0))
  
  polygon.sum <- source.list$sources$polygon %>% 
    st_drop_geometry() %>%
    dplyr::select(., vars) %>% 
    apply(., 2, sum) %>% 
    t(.) %>% 
    as.data.frame() %>%
    dplyr::mutate_if(is.numeric, round, 2)
  
  polygon.total <- source.list[[2]][[2]][, vars] %>% 
    dplyr::mutate_if(is.numeric, round, 2) %>%
    as.data.frame()
  
  if(!identical(polygon.sum, polygon.total) & distribute == TRUE){
    d <- (polygon.total - polygon.sum)[1, ]
    zero.ind <- source.list$sources$polygon[, vars] %>% st_drop_geometry() == 0
    w <- replace(source.list$sources$polygon[, vars] %>% st_drop_geometry(), zero.ind, 1) %>% 
      apply(., 2, function(x) x/sum(x)) %>%
      as.data.frame() %>%
      dplyr::mutate_all(~replace(., is.na(.), 0))
    
    cor.data <- as.matrix(w) %*% diag(d) + source.list$sources$polygon[, vars]%>% st_drop_geometry()
    source.list$sources$polygon[, vars] <- cor.data
    source.sf <- source.list$sources$polygon
  }else{
    source.sf <- source.list$sources$polygon
  }
  return(source.sf)
}

corsum2sf_lines <- function(source.list, distribute = FALSE){
  source.list$sources$lines[, vars] <- source.list$sources$lines[, vars] %>% dplyr::mutate_all(~replace(., is.na(.), 0)) %>% st_drop_geometry()
  source.list[[2]][[2]][, vars] <- source.list[[2]][[2]][, vars] %>% dplyr::mutate_all(~replace(., is.na(.), 0))
  
  lines.sum <- source.list$sources$lines %>% 
    st_drop_geometry() %>%
    dplyr::select(., vars) %>% 
    apply(., 2, sum) %>% 
    t(.) %>% 
    as.data.frame() %>%
    dplyr::mutate_if(is.numeric, round, 2)
  
  lines.total <- source.list[[2]][[2]][, vars] %>% 
    dplyr::mutate_if(is.numeric, round, 2) %>%
    as.data.frame()
  
  if(!identical(lines.sum, lines.total) & distribute == TRUE){
    d <- (lines.total - lines.sum)[1, ]
    zero.ind <- source.list$sources$lines[, vars] %>% st_drop_geometry() == 0
    w <- replace(source.list$sources$lines[, vars] %>% st_drop_geometry(), zero.ind, 1) %>% 
      apply(., 2, function(x) x/sum(x)) %>%
      as.data.frame() %>%
      dplyr::mutate_all(~replace(., is.na(.), 0))
    
    cor.data <- as.matrix(w) %*% diag(d) + source.list$sources$lines[, vars]%>% st_drop_geometry()
    source.list$sources$lines[, vars] <- cor.data
    source.sf <- source.list$sources$lines
  }else{
    source.sf <- source.list$sources$lines
  }
  return(source.sf)
}
corsum2sf_point.sf <- function(source.list, distribute = FALSE){
  source.list$sources$points[, vars] <- source.list$sources$points[, vars] %>% dplyr::mutate_all(~replace(., is.na(.), 0)) %>% st_drop_geometry()
  source.list[[2]][[2]][, vars] <- source.list[[2]][[2]][, vars] %>% dplyr::mutate_all(~replace(., is.na(.), 0))
  
  points.sum <- source.list$sources$points %>% 
    st_drop_geometry() %>%
    dplyr::select(., vars) %>% 
    apply(., 2, sum) %>% 
    t(.) %>% 
    as.data.frame() %>%
    dplyr::mutate_if(is.numeric, round, 2)
  
  points.total <- source.list[[2]][[2]][, vars] %>% 
    dplyr::mutate_if(is.numeric, round, 2) %>%
    as.data.frame()
  
  if(!identical(points.sum, points.total) & distribute == TRUE){
    d <- (points.total - points.sum)[1, ]
    zero.ind <- source.list$sources$points[, vars] %>% st_drop_geometry() == 0
    w <- replace(source.list$sources$points[, vars] %>% st_drop_geometry(), zero.ind, 1) %>% 
      apply(., 2, function(x) x/sum(x)) %>%
      as.data.frame() %>%
      dplyr::mutate_all(~replace(., is.na(.), 0))
    
    cor.data <- as.matrix(w) %*% diag(d) + source.list$sources$points[, vars]%>% st_drop_geometry()
    source.list$sources$points[, vars] <- cor.data
    source.sf <- source.list$sources$points
  }else{
    source.sf <- source.list$sources$points
  }
  return(source.sf)
}

# Function for spatial data visualisation at web maps
# Parameters:
#    1. sf.sources -  sf object with sources to be spatialised
#    2. sf.spatialised - sf object with polygons after spatialisation
spatialised.mapview <- function(sf.sources, layer.name.1 = "", sf.spatialised, layer.name.2 = "", vars = vars, source.lines = FALSE){
  sf.spatialised$Spatialised <- NA
  sf.spatialised[ ,vars] %<>% st_drop_geometry() %>% dplyr::mutate_all(.,as.double)
  sf.spatialised$Spatialised[sf.spatialised$NOx == 0 & sf.spatialised$SO2 == 0 & sf.spatialised$PM10 == 0 & sf.spatialised$PM2.5 == 0 & sf.spatialised$NMVOC == 0 & sf.spatialised$NH3 == 0] <- 0
  sf.spatialised$Spatialised[sf.spatialised$NOx !=0 | sf.spatialised$SO2 !=0 | sf.spatialised$PM10 !=0 | sf.spatialised$PM2.5 !=0 | sf.spatialised$NMVOC !=0 | sf.spatialised$NH3 != 0] <- 1
  
  if(source.lines == FALSE){
    web_map <- mapview(sf.spatialised, layer.name = layer.name.2, zcol = "Spatialised") + mapview(sf.sources, layer.name = layer.name.1, col.regions = "red")
  }else {
    web_map <- mapview(sf.sources, layer.name = layer.name.1, color = "red") +  mapview(sf.spatialised, layer.name = layer.name.2, zcol = "Spatialised")
  }
  return(web_map)
}

#'
#'
#+ include = FALSE
source.file = "2030_WAM_B/Pollutant inventory spatialized-za_2030_WAM_B.xlsx"
source.sheet =  "1B-Fugitive emissions"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Grid_Serbia_0.05deg.gpkg")
sf.grid.5km <- st_as_sf(grid.5km)
sf.grid.5km %<>% dplyr::mutate(ID = id)



#'
#'
#' ## 1B1a-Fugitive emissions from solid fuels: Coal mining and handling
#'
#'
#'
#+ include = FALSE
source.1B1a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

# source.1B1a$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S21", sheet = source.sheet, col_names = header)
source.1B1a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D18:I18", sheet = source.sheet, col_names = vars)
source.1B1a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D19:I19", sheet = source.sheet, col_names = vars)

#sf.1B1a <- corsum2sf(source.1B1a) #%>%
  #st_transform(crs = "+init=epsg:32634")
#
##+ include = FALSE
#clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
#sf_clc18 <- st_as_sf(clc_18)
#clc131 <- subset(sf_clc18, CODE_18 == "131") %>% # Mine sites
#  st_set_crs(32634)
#clc131[,vars] <- NA
#
#buf_131 <- st_buffer(sf.1B1a$geometry, dist = 1100)
#
#clc131_buff <- st_join(clc131, st_sf(buf_131) %>% mutate(id = seq(1:dim(.))), join = st_intersects) %>%
#  filter(!is.na(id))
#
#clc131.int <- st_intersection(clc131_buff, sf.grid.5km) %>%
#  dplyr::select(.,vars)

coal_mining_polygons <- st_read(dsn = "Data/coal_mining/coal_mining_polygons.gpkg")
coal_mining_polygons[, vars] <- NA
coal_mining_polygons %<>% sf::st_transform(4326)

coal_mining_polygons.int <- st_intersection(coal_mining_polygons, sf.grid.5km) %>%
  dplyr::select(.,vars)
source.1B1a$sources$polygon <- coal_mining_polygons.int

sf.1B1a <- corsum2sf_polygon(source.1B1a, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

# st_write(sf.1B1a, dsn="D:/coal_polygons.gpkg", layer='sf.1B1a')
# st_write(sf.1B1a, dsn="D:/coal_mining_points.gpkg", layer='sf.1B1a')
# clc131_buff %<>% dplyr::select()
# st_write(clc131_buff, dsn="D:/coal_mining_polygons.gpkg", layer='clc131_buff')


#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1B1a %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.1B1a',
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
sum.1B1a <- sf.1B1a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1B1a <- source.1B1a[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1B1a, total.1B1a, data.frame(total.1B1a - sum.1B1a))) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.1B1a <- sf.1B1a %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.1B1a$Area)
diff.1B1a <- data.frame(total.1B1a - sum.1B1a)
sf.1B1a <- sf.1B1a %>%
  mutate(NOx = ((diff.1B1a$NOx/sum_Area)*Area),
         SO2 = ((diff.1B1a$SO2/sum_Area)*Area),
         PM10 = ((diff.1B1a$PM10/sum_Area)*Area),
         PM2.5 = ((diff.1B1a$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.1B1a$NMVOC/sum_Area)*Area),
         NH3 = ((diff.1B1a$NH3/sum_Area)*Area))
sf.1B1a %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1B1a <- sf.grid.5km %>%
  st_join(sf.1B1a, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.1B1a, layer.name = "Spatialised 1B1a") + mapview(sf.1B1a, layer.name = "Sources 1B1a", col.regions = "red") 
spatialised.mapview(sf.sources = sf.1B1a, layer.name.1 = "Sources 1B1a", sf.spatialised = p.1B1a, layer.name.2 = "Spatialised 1B1a", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1B1a <- p.1B1a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1B1a, total.1B1a, data.frame(sum.p.1B1a == total.1B1a)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
 st_write(p.1B1a, dsn="2030_WAM_B/Products_2030_WAM_B/1B - Fugitive emissions_2030_WAM_B/1B1a.gpkg", layer='1B1a')


#'
#'
#' ## 1B2ai-Fugitive emissions from liquid fuels: Exploration, production, transport
#'
#'
#'
#+ include = FALSE
source.1B2ai <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1B2ai$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D29:I29", sheet = source.sheet, col_names = vars)
source.1B2ai$total$inventory <- readxl::read_xlsx(path = source.file, range = "D30:I30", sheet = source.sheet, col_names = vars)

#+ include = FALSE

source.file1 = "Pollutant inventory spatialized-d-v3.xlsx"
source.sheet1 =  "1A1-Energy"
header1 <- readxl::read_xlsx(path = source.file1, range = "D8:S8", sheet = source.sheet1) %>% names()
vars1 <- header1[1:6]

source.1A1b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1b$sources$points <- readxl::read_xlsx(path = source.file1, range = "D47:S50", sheet = source.sheet1, col_names = header1)

rafineries <-  source.1A1b$sources$points

sum.r_NOx <- sum(rafineries$NOx)
sum.r_SO2 <- sum(rafineries$SO2)
sum.r_PM10 <- sum(rafineries$PM10)
sum.r_PM2.5 <- sum(rafineries$PM2.5)
sum.r_NMVOC <- sum(rafineries$NMVOC)
sum.r_NH3 <- sum(rafineries$NH3)

source.1B2ai$sources$points <- rafineries

sf.1B2ai <- corsum2sf(source.1B2ai, distribute = TRUE) #%>%
  #st_transform(crs = "+init=epsg:32634")
sf.1B2ai$NOx <- 0
sf.1B2ai$SO2 <- 0
sf.1B2ai$PM10 <- 0
sf.1B2ai$PM2.5 <- 0
sf.1B2ai$NH3 <- 0
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1B2ai %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.1B2ai',
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
sum.1B2ai <- sf.1B2ai %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1B2ai <- source.1B2ai[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1B2ai, total.1B2ai, data.frame(total.1B2ai - sum.1B2ai))) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

# sum_Length <- sum(sf.1B2ai$Length)
# diff.1B2ai <- data.frame(total.1B2ai - sum.1B2ai)
# sf.1B2ai <- sf.1B2ai %>%
#   mutate(NOx = ((diff.1B2ai$NOx/sum_Length)*Length),
#          SO2 = ((diff.1B2ai$SO2/sum_Length)*Length),
#          PM10 = ((diff.1B2ai$PM10/sum_Length)*Length),
#          PM2.5 = ((diff.1B2ai$PM2.5/sum_Length)*Length),
#          NMVOC = ((diff.1B2ai$NMVOC/sum_Length)*Length),
#          NH3 = ((diff.1B2ai$NH3/sum_Length)*Length))
# sf.1B2ai %<>% select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1B2ai <- sf.grid.5km %>%
  st_join(sf.1B2ai, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.1B2ai, layer.name = "Spatialised 1B2ai") + mapview(sf.1B2ai, layer.name = "Sources 1B2ai", color = "red") 
spatialised.mapview(sf.sources = sf.1B2ai, layer.name.1 = "Sources 1B2ai", sf.spatialised = p.1B2ai, layer.name.2 = "Spatialised 1B2ai", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1B2ai <- p.1B2ai %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1B2ai, total.1B2ai, data.frame(sum.p.1B2ai == total.1B2ai)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
 st_write(p.1B2ai, dsn="2030_WAM_B/Products_2030_WAM_B/1B - Fugitive emissions_2030_WAM_B/1B2ai.gpkg", layer='1B2ai')

#'
#'
#' ## 1B2av-Fugitive emissions from liquid fuels: Distribution of oil products
#'
#'
#'
#+ include = FALSE
source.1B2av <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1B2av$sources$points <- readxl::read_xlsx(path = source.file, range = "D31:S31", sheet = source.sheet, col_names = header)
source.1B2av$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D40:I40", sheet = source.sheet, col_names = vars)
source.1B2av$total$inventory <- readxl::read_xlsx(path = source.file, range = "D40:I41", sheet = source.sheet, col_names = vars)


sf.1B2av <- corsum2sf(source.1B2av, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")


# :::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::

# RAFINERIJE
# source.file = "Pollutant inventory spatialized-d-v3.xlsx"
# source.sheet =  "1A1-Energy"
# header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
# vars <- header[1:6]
# 
# source.1A1a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))
# 
# source.1A1a$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S37", sheet = source.sheet, col_names = header)
# source.1A1a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D40:I40", sheet = source.sheet, col_names = vars)
# source.1A1a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D46:I46", sheet = source.sheet, col_names = vars)
# 
# 
# sf.1A1a <- corsum2sf(source.1A1a) %>%
#   st_transform(crs = "+init=epsg:32634") %>%
#                  dplyr::select()
sf.1A1a <- sf.1B2ai
# :::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::

source.file = "2030_WAM_B/Pollutant inventory spatialized-za_2030_WAM_B.xlsx"
source.sheet =  "1B-Fugitive emissions"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]

# :::::::::::::::::::::::::::::::::::::::::::


#+ include = FALSE
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

opstine_ind <- readOGR("Data/opstine/tfr_mac_bom.shp", 
                       use_iconv=TRUE,  
                       encoding = "UTF-8")
sf_opstine_ind <- st_as_sf(opstine_ind)

sf_opstine$ind <- sf_opstine_ind$type[match(sf_opstine$NAME_2, sf_opstine_ind$Name_Mun)]

sf_opstine$ind[sf_opstine$NAME_2 == "Kosjerić"] <- 0
sf_opstine$ind[sf_opstine$NAME_2 == "Novi Sad"] <- 1
sf_opstine$ind[sf_opstine$NAME_2 == "Surčin"] <- 1
sf_opstine$ind[sf_opstine$NAME_2 == "Kragujevac"] <- 1
sf_opstine$ind[sf_opstine$NAME_2 == "Žitorađa"] <- 0

sf_opstine %<>% dplyr::select(NAME_2, ind) %>%
  st_transform(crs = "+init=epsg:32634")

#+ include = FALSE
stanovnistvo <- readxl::read_xls(path = "Data/Stanovnistvo_2015.xls", sheet = "OpstiPodaci")
lcl(loc = "C")
stanovnistvo <- cyr_lat(stanovnistvo)
names(stanovnistvo) <- cyr_lat(names(stanovnistvo)) 
stanovnistvo <- stanovnistvo %>%
  mutate_all(~replace_na(., 0))
stanovnistvo$Opština[stanovnistvo$Opština == "Indjija"] <- "Inđija"
stanovnistvo$Opština[stanovnistvo$Opština == "LJubovija"] <- "Ljubovija"
stanovnistvo$Opština[stanovnistvo$Opština == "Mali Idjoš"] <- "Mali Iđoš"
stanovnistvo$Opština[stanovnistvo$Opština == "Savski venac"] <- "Savski Venac"
stanovnistvo$Opština[stanovnistvo$Opština == "Stari grad"] <- "Stari Grad"
stanovnistvo$Opština[stanovnistvo$Opština == "Petrovac na Mlavi"] <- "Petrovac"
stanovnistvo$Opština[stanovnistvo$Opština == "Arandjelovac"] <- "Aranđelovac"
stanovnistvo$Opština[stanovnistvo$Opština == "LJig"] <- "Ljig"
stanovnistvo$Opština[stanovnistvo$Opština == "Žitoradja"] <- "Žitorađa"
stanovnistvo$Opština[stanovnistvo$Opština == "Medvedja"] <- "Medveđa"
sf_opstine$Br_stanovnistvo <- stanovnistvo$Stanovništvo[match(sf_opstine$NAME_2, stanovnistvo$Opština)]

sf_opstine[,vars] <- NA
sf_opstine %<>% st_transform(4326)
sf_opstine.int <- st_intersection(sf_opstine, sf.grid.5km)  



fuel.s <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Fuel_stations_OSM_32634.gpkg")

#fuel.s %<>% st_intersection(., sf_opstine.int) %>%
#  dplyr::select(Br_stanovnistvo)

sf.1B2av %<>% dplyr::select()
sf.1B2av[, vars] <- NA

sf.1A1a %<>% dplyr::select()
sf.1A1a[, vars] <- NA


sourcee <- rbind(sf.1B2av, sf.1A1a) # Banatski dvor, rafinerije
sourcee %<>% st_intersection(., sf_opstine.int) %>%
    dplyr::select(Br_stanovnistvo)

sourcee[, vars] <- NA
source.1B2av$sources$points <- sourcee
source.1B2av$total$inventory <- source.1B2av$total$inventory/2 
sf.1B2av <- corsum2sf_point.sf(source.1B2av, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

sf.1B2av1 <- sf.1B2av

fuel.s %<>% dplyr::select() %>% dplyr::rename(geometry = geom)
fuel.s %<>% st_transform(4326)
fuel.s %<>% st_intersection(., sf_opstine.int) %>%
  dplyr::select(Br_stanovnistvo)
fuel.s[, vars] <- NA

source.1B2av$sources$points <- fuel.s
sf.1B2av <- corsum2sf_point.sf(source.1B2av, distribute = FALSE)# %>%
  #st_transform(crs = "+init=epsg:32634")

sf.1B2av2 <- sf.1B2av
sf.1B2av1 %<>% st_transform(4326)
sf.1B2av <- rbind(sf.1B2av1, sf.1B2av2)
source.1B2av$total$inventory <- readxl::read_xlsx(path = source.file, range = "D41:I41", sheet = source.sheet, col_names = vars)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
fuel.s %>% 
  st_drop_geometry() %>%
  mutate_all(~replace_na(., 0)) %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.1B2av',
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
sum.1B2av <- sf.1B2av %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1B2av <- source.1B2av[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1B2av, total.1B2av, data.frame(total.1B2av - sum.1B2av))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.1B2av$Br_stanovnistvo)
diff.1B2av <- data.frame(total.1B2av - sum.1B2av)
sf.1B2av <- sf.1B2av %>%
  mutate(NOx = ((diff.1B2av$NOx/sum_s)*Br_stanovnistvo),
         SO2 = ((diff.1B2av$SO2/sum_s)*Br_stanovnistvo),
         PM10 = ((diff.1B2av$PM10/sum_s)*Br_stanovnistvo),
         PM2.5 = ((diff.1B2av$PM2.5/sum_s)*Br_stanovnistvo),
         NMVOC = ((diff.1B2av$NMVOC/sum_s)*Br_stanovnistvo),
         NH3 = ((diff.1B2av$NH3/sum_s)*Br_stanovnistvo))
sf.1B2av %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1B2av <- sf.grid.5km %>%
  st_join(sf.1B2av, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.1B2av, layer.name = "Spatialised 1B2av") + mapview(sf.1B2av, layer.name = "Sources 1B2av", col.regions = "red") 
spatialised.mapview(sf.sources = sf.1B2av, layer.name.1 = "Sources 1B2av", sf.spatialised = p.1B2av, layer.name.2 = "Spatialised 1B2av", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1B2av <- p.1B2av %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1B2av, total.1B2av, data.frame(sum.p.1B2av == total.1B2av)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
 st_write(p.1B2av, dsn="2030_WAM_B/Products_2030_WAM_B/1B - Fugitive emissions_2030_WAM_B/1B2av.gpkg", layer='1B2av')

#'
#'
#' ## 1B2b-Fugitive emissions from natural gas: Exploration, production, transport
#'
#'
#'
#+ include = FALSE
source.1B2b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1B2b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D51:I51", sheet = source.sheet, col_names = vars)
source.1B2b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D52:I52", sheet = source.sheet, col_names = vars)

#+ include = FALSE

source.1B2b$sources$points <- rafineries

sf.1B2b <- corsum2sf(source.1B2b, distribute = TRUE) #%>%
  #st_transform(crs = "+init=epsg:32634")
sf.1B2b$NOx <- 0
sf.1B2b$SO2 <- 0
sf.1B2b$PM10 <- 0
sf.1B2b$PM2.5 <- 0
sf.1B2b$NH3 <- 0



#source.1B2b$sources$lines <- sf_roads.int

#sf.1B2b <- corsum2sf_lines(source.1B2b, distribute = FALSE) %>%
#  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1B2b %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 10: sf.1B2b',
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
sum.1B2b <- sf.1B2b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1B2b <- source.1B2b[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1B2b, total.1B2b, data.frame(total.1B2b - sum.1B2b))) %>%
  datatable(., caption = 'Table 11: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

# sum_Length <- sum(sf.1B2b$Length)
# diff.1B2b <- data.frame(total.1B2b - sum.1B2b)
# sf.1B2b <- sf.1B2b %>%
#   mutate(NOx = ((diff.1B2b$NOx/sum_Length)*Length),
#          SO2 = ((diff.1B2b$SO2/sum_Length)*Length),
#          PM10 = ((diff.1B2b$PM10/sum_Length)*Length),
#          PM2.5 = ((diff.1B2b$PM2.5/sum_Length)*Length),
#          NMVOC = ((diff.1B2b$NMVOC/sum_Length)*Length),
#          NH3 = ((diff.1B2b$NH3/sum_Length)*Length))
# sf.1B2b %<>% select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1B2b <- sf.grid.5km %>%
  st_join(sf.1B2b, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.1B2b, layer.name = "Spatialised 1B2b") + mapview(sf.1B2b, layer.name = "Sources 1B2b", color = "red") 
spatialised.mapview(sf.sources = sf.1B2b, layer.name.1 = "Sources 1B2b", sf.spatialised = p.1B2b, layer.name.2 = "Spatialised 1B2b", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1B2b <- p.1B2b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1B2b, total.1B2b, data.frame(sum.p.1B2b == total.1B2b)-1)) %>%
  datatable(., caption = 'Table 12: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
 st_write(p.1B2b, dsn="2030_WAM_B/Products_2030_WAM_B/1B - Fugitive emissions_2030_WAM_B/1B2b.gpkg", layer='1B2b')





