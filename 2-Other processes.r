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
#' # 2-Other processes
#' This document provides the methodlogy and the main results regarding the spatialization of pollutation inventory.
#' 
#' 
#+ include = TRUE, echo = FALSE, results = 'hide', warning = FALSE, message = FALSE
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
library(raster)
library(stars)
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
#+ include = FALSE, echo = FALSE
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
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "2-Other processes"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Polygons_5km_UTM_34N.shp")
sf.grid.5km <- st_as_sf(grid.5km) 

#'
#'
#' ## 2A5a - Quarrying and mining of minerals other than coal
#'
#'
#'
#+ include = FALSE
source.2A5a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2A5a$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S19", sheet = source.sheet, col_names = header)
source.2A5a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D27:I27", sheet = source.sheet, col_names = vars)
source.2A5a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D28:I28", sheet = source.sheet, col_names = vars)

  
sf.2A5a <- corsum2sf(source.2A5a, distribute = FALSE) %>%
   st_transform(crs = "+init=epsg:32634")

#st_write(sf.2A5a, dsn="D:/other_than_coal.gpkg", layer='sf.2A5a')




#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2A5a %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.2A5a',
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
sum.2A5a <- sf.2A5a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2A5a <- source.2A5a[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2A5a, total.2A5a, data.frame(sum.2A5a == total.2A5a)-1)) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE, echo = FALSE, result = FALSE
p.2A5a <- sf.grid.5km %>%
  st_join(sf.2A5a) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.2A5a, layer.name = "Sources 2A5a", col.regions = "red") + mapview(p.2A5a)
spatialised.mapview(sf.sources = sf.2A5a, layer.name.1 = "Sources 2A5a", sf.spatialised = p.2A5a, layer.name.2 = "Spatialised 2A5a", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2A5a <- p.2A5a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2A5a, total.2A5a, data.frame(total.2A5a-sum.p.2A5a ))) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )




#### OSTATAK

clc131.quarries <- sf::st_read(dsn = "Data/mineral_sites_quarries/clc131_without_coal_polygons_and_25ha_bez_11_lokacija.gpkg") 

clc131.quarries %<>% dplyr::mutate(Area_Ha = unclass(st_area(.)/10000), SHAPE_Area = unclass(st_area(.)))

clc131.quarries[,vars] <- NA
clc131.quarries.int <- st_intersection(clc131.quarries, sf.grid.5km) %>%
  dplyr::select(.,vars)


clc131.quarries.int %<>% dplyr::mutate(Area_Ha = unclass(st_area(.)/10000), SHAPE_Area = unclass(st_area(.)))


sum.2A5a <- sf.2A5a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2A5a <- source.2A5a[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()


sum_s <- sum(clc131.quarries.int$SHAPE_Area)

diff.2A5a <- data.frame(total.2A5a - sum.2A5a)
clc131.quarries.int <- clc131.quarries.int %>%
  mutate(NOx = ((diff.2A5a$NOx/sum_s)*SHAPE_Area),
         SO2 = ((diff.2A5a$SO2/sum_s)*SHAPE_Area),
         PM10 = ((diff.2A5a$PM10/sum_s)*SHAPE_Area),
         PM2.5 = ((diff.2A5a$PM2.5/sum_s)*SHAPE_Area),
         NMVOC = ((diff.2A5a$NMVOC/sum_s)*SHAPE_Area),
         NH3 = ((diff.2A5a$NH3/sum_s)*SHAPE_Area))
clc131.quarries.int %<>% dplyr::select(vars)

clc131.quarries.int$ID <- dplyr::row_number(clc131.quarries.int$NOx)

source.2A5a$sources$points <- NA
source.2A5a$sources$polygon <- clc131.quarries.int

sf.2A5a <- corsum2sf_polygon(source.2A5a, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2A5a %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 27: sf.2A5a',
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
sum.2A5a <- sf.2A5a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2A5a <- source.2A5a[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2A5a, total.2A5a, data.frame(total.2A5a-sum.2A5a))) %>%
  datatable(., caption = 'Table 28: Summary differences',
            options = list(pageLength = 5)
  )

#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

# sf.2A5a <- sf.2A5a %>%
#   mutate(Area = st_area(.))
# 
# sum_Area <- sum(sf.2A5a$Area)
# diff.2A5a <- data.frame(total.2A5a - sum.2A5a)
# sf.2A5a <- sf.2A5a %>%
#   mutate(NOx = ((diff.2A5a$NOx/sum_Area)*Area),
#          SO2 = ((diff.2A5a$SO2/sum_Area)*Area),
#          PM10 = ((diff.2A5a$PM10/sum_Area)*Area),
#          PM2.5 = ((diff.2A5a$PM2.5/sum_Area)*Area),
#          NMVOC = ((diff.2A5a$NMVOC/sum_Area)*Area),
#          NH3 = ((diff.2A5a$NH3/sum_Area)*Area))
# sf.2A5a %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2A5a.1 <- sf.grid.5km %>%
  st_join(sf.2A5a, join = st_contains) %>% 
  group_by(ID.x) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID.x))

sum.p.2A5a <- p.2A5a.1 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2A5a, total.2A5a, data.frame(sum.p.2A5a == total.2A5a)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )


p.2A5a.1$ID
p.2A5a$ID


p.2A5a %<>% dplyr::mutate(NOx = NOx + p.2A5a.1$NOx,
                         SO2 = SO2 + p.2A5a.1$SO2,
                         PM10 =PM10 + p.2A5a.1$PM10, 
                         PM2.5 = PM2.5 + p.2A5a.1$PM2.5,
                         NMVOC = NMVOC + p.2A5a.1$NMVOC,
                         NH3 = NH3 + p.2A5a.1$NH3)

sum.p.2A5a <- p.2A5a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2A5a, total.2A5a, data.frame(sum.p.2A5a == total.2A5a)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )


#+ include = FALSE
# st_write(p.2A5a, dsn="Products/2 - Other processes/2A5a.gpkg", layer='2A5a')

#'
#'
#' ## 2A5b - Construction and demolition
#'
#'
#'
#+ include = FALSE
source.2A5b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2A5b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D40:I40", sheet = source.sheet, col_names = vars)
source.2A5b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D41:I41", sheet = source.sheet, col_names = vars)

#+ include = FALSE
clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
sf_clc18_urb <- subset(sf_clc18, CODE_18 == "111" | CODE_18 == "112") %>% # CLC urban zones
  st_transform(crs = "+init=epsg:32634")


# Including rural areas, industrial and commercial zones
sf.urb <- sf_clc18_urb %>% 
  dplyr::select(geometry) 
sf.rur <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg") %>%
  dplyr::select(geom) %>%
  dplyr::rename(geometry = geom)
sf.comm <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/OSM_commercial_Serbia.gpkg") %>%
  dplyr::select(geom) %>%
  dplyr::rename(geometry = geom)
sf.ind <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Industrial_sites_new.gpkg") %>%
  dplyr::select(geom) %>%
  dplyr::rename(geometry = geom)


sf.final <- rbind(sf.urb, sf.rur, sf.comm, sf.ind)

#+ include = FALSE
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

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

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")
sf_final <- st_join(sf.final, sf_opstine, largest = TRUE) 

sf_final %<>% dplyr::select(.,Br_stanovnistvo, NAME_2)
sf_final[,vars] <- NA

sf_final.int <- st_intersection(sf_final, sf.grid.5km) %>%
  filter(!is.na(Br_stanovnistvo))
source.2A5b$sources$polygon <- sf_final.int

sf.2A5b <- corsum2sf_polygon(source.2A5b, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2A5b %>% 
  st_drop_geometry() %>%
  dplyr::rename(Population_weight = Br_stanovnistvo) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2A5b',
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
sum.2A5b <- sf.2A5b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2A5b <- source.2A5b[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2A5b, total.2A5b, data.frame(total.2A5b - sum.2A5b))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.2A5b$Br_stanovnistvo)
diff.2A5b <- data.frame(total.2A5b - sum.2A5b)
sf.2A5b <- sf.2A5b %>%
  mutate(NOx = ((diff.2A5b$NOx/sum_s)*Br_stanovnistvo),
         SO2 = ((diff.2A5b$SO2/sum_s)*Br_stanovnistvo),
         PM10 = ((diff.2A5b$PM10/sum_s)*Br_stanovnistvo),
         PM2.5 = ((diff.2A5b$PM2.5/sum_s)*Br_stanovnistvo),
         NMVOC = ((diff.2A5b$NMVOC/sum_s)*Br_stanovnistvo),
         NH3 = ((diff.2A5b$NH3/sum_s)*Br_stanovnistvo))
sf.2A5b %<>% select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2A5b <- sf.grid.5km %>%
  st_join(sf.2A5b, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.2A5b, layer.name = "Spatialised 2A5b") + mapview(sf.2A5b, layer.name = "Sources 2A5b", col.regions = "red") 
spatialised.mapview(sf.sources = sf.2A5b, layer.name.1 = "Sources 2A5b", sf.spatialised = p.2A5b, layer.name.2 = "Spatialised 2A5b", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2A5b <- p.2A5b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2A5b, total.2A5b, data.frame(sum.p.2A5b == total.2A5b)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2A5b, dsn="Products/2 - Other processes/2A5b.gpkg", layer='2A5b')


#'
#'
#' ## 2A5c - Storage, handling and transport of mineral products
#'
#'
#'
#+ include = FALSE
source.2A5c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2A5c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D53:I53", sheet = source.sheet, col_names = vars)
source.2A5c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D54:I54", sheet = source.sheet, col_names = vars)

#+ include = FALSE
# o.t.coals <- sf.2A5a 
# o.t.coals[,vars] <- NA
# 
# buf_otcoals <- st_buffer(o.t.coals$geometry, dist = 15000)
# 
# 
# roads <- readOGR("Data/putevi/Saobracajne_deonice_i_odseci_sa_brojaca.shp", 
#                  use_iconv=TRUE,  
#                  encoding = "UTF-8",
#                  stringsAsFactors = FALSE)
# sf_roads <- st_as_sf(roads) %>%
#   st_transform(crs = "+init=epsg:32634") 
# 
# 
# 
# 
# roads_buff <- st_join(sf_roads, st_sf(buf_otcoals) %>% mutate(id = seq(1:dim(.))), join = st_intersects) %>%
#   filter(!is.na(id))
# roads_buff[,vars] <- NA
# roads.int <- st_intersection(roads_buff, sf.grid.5km) %>%
#   select(.,vars)
# 
# source.2A5c$sources$lines <- roads.int
# sf.2A5c <- corsum2sf_lines(source.2A5c, distribute = FALSE) %>%
#   st_transform(crs = "+init=epsg:32634")


source.2A5c$sources$polygon <- clc131.quarries.int

sf.2A5c <- corsum2sf_polygon(source.2A5c, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")


#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2A5c %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.2A5c',
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
sum.2A5c <- sf.2A5c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2A5c <- source.2A5c[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2A5c, total.2A5c, data.frame(total.2A5c - sum.2A5c))) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
#   sf.2A5c %<>% dplyr::mutate(Length = st_length(.))
#   sum_Length <- sum(sf.2A5c$Length)
#   diff.2A5c <- data.frame(total.2A5c - sum.2A5c)
#   sf.2A5c <- sf.2A5c %>%
#     mutate(NOx = ((diff.2A5c$NOx/sum_Length)*Length),
#            SO2 = ((diff.2A5c$SO2/sum_Length)*Length),
#            PM10 = ((diff.2A5c$PM10/sum_Length)*Length),
#            PM2.5 = ((diff.2A5c$PM2.5/sum_Length)*Length),
#            NMVOC = ((diff.2A5c$NMVOC/sum_Length)*Length),
#            NH3 = ((diff.2A5c$NH3/sum_Length)*Length))
#   sf.2A5c %<>% select(vars)
#   #'
#   #'
#   #+ include = FALSE, echo = FALSE, result = FALSE
#   p.2A5c <- sf.grid.5km %>%
#     st_join(sf.2A5c, join = st_contains) %>% 
#     group_by(ID) %>%
#     summarize(NOx = sum(NOx, na.rm = TRUE),
#               SO2 = sum(SO2, na.rm = TRUE),
#               PM10 = sum(PM10, na.rm = TRUE),
#               PM2.5 = sum(PM2.5, na.rm = TRUE),
#               NMVOC = sum(NMVOC, na.rm = TRUE),
#               NH3 = sum(NH3, na.rm = TRUE)) %>% 
#     mutate(ID = as.numeric(ID))

sf.2A5c <- sf.2A5c %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.2A5c$Area)
diff.2A5c <- data.frame(total.2A5c - sum.2A5c)
sf.2A5c <- sf.2A5c %>%
  mutate(NOx = ((diff.2A5c$NOx/sum_Area)*Area),
         SO2 = ((diff.2A5c$SO2/sum_Area)*Area),
         PM10 = ((diff.2A5c$PM10/sum_Area)*Area),
         PM2.5 = ((diff.2A5c$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.2A5c$NMVOC/sum_Area)*Area),
         NH3 = ((diff.2A5c$NH3/sum_Area)*Area))
sf.2A5c %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2A5c <- sf.grid.5km %>%
  st_join(sf.2A5c, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))



#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2A5c, layer.name.1 = "Sources 2A5c", sf.spatialised = p.2A5c, layer.name.2 = "Spatialised 2A5c", vars = vars, source.lines = TRUE)  
  # mapview(o.t.coals, layer.name = "Other than coals locations")

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2A5c <- p.2A5c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2A5c, total.2A5c, data.frame(sum.p.2A5c == total.2A5c)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2A5c, dsn="Products/2 - Other processes/2A5c.gpkg", layer='2A5c')

#'
#'
#' ## 2D3b - Road paving with asphalt
#'
#'
#'
#+ include = FALSE
source.2D3b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D73:I73", sheet = source.sheet, col_names = vars)

#+ include = FALSE

sf_roads[,vars] <- NA
roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars)

source.2D3b$sources$lines <- roads.int
sf.2D3b <- corsum2sf_lines(source.2D3b, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3b %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.2D3b',
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
sum.2D3b <- sf.2D3b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3b <- source.2D3b[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3b, total.2D3b, data.frame(total.2D3b - sum.2D3b))) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sf.2D3b %<>% dplyr::mutate(Length = st_length(.))
sum_Length <- sum(sf.2D3b$Length)
diff.2D3b <- data.frame(total.2D3b - sum.2D3b)
sf.2D3b <- sf.2D3b %>%
  mutate(NOx = ((diff.2D3b$NOx/sum_Length)*Length),
         SO2 = ((diff.2D3b$SO2/sum_Length)*Length),
         PM10 = ((diff.2D3b$PM10/sum_Length)*Length),
         PM2.5 = ((diff.2D3b$PM2.5/sum_Length)*Length),
         NMVOC = ((diff.2D3b$NMVOC/sum_Length)*Length),
         NH3 = ((diff.2D3b$NH3/sum_Length)*Length))
sf.2D3b %<>% select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3b <- sf.grid.5km %>%
  st_join(sf.2D3b, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3b, layer.name.1 = "Sources 2D3b", sf.spatialised = p.2D3b, layer.name.2 = "Spatialised 2D3b", vars = vars, source.lines = TRUE) 
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3b <- p.2D3b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3b, total.2D3b, data.frame(sum.p.2D3b == total.2D3b)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3b, dsn="Products/2 - Other processes/2D3b.gpkg", layer='2D3b')

#'
#'
#' ## 2D3c - Asphalt roofing
#'
#'
#'
#+ include = FALSE
source.2D3c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D40:I40", sheet = source.sheet, col_names = vars)
source.2D3c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D74:I74", sheet = source.sheet, col_names = vars)

source.2D3c$sources$polygon <- sf_final.int

sf.2D3c <- corsum2sf_polygon(source.2D3c, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3c %>% 
  st_drop_geometry() %>%
  dplyr::rename(Population_weight = Br_stanovnistvo) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3c',
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
sum.2D3c <- sf.2D3c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3c <- source.2D3c[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3c, total.2D3c, data.frame(total.2D3c - sum.2D3c))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.2D3c$Br_stanovnistvo)
diff.2D3c <- data.frame(total.2D3c - sum.2D3c)
sf.2D3c <- sf.2D3c %>%
  mutate(NOx = ((diff.2D3c$NOx/sum_s)*Br_stanovnistvo),
         SO2 = ((diff.2D3c$SO2/sum_s)*Br_stanovnistvo),
         PM10 = ((diff.2D3c$PM10/sum_s)*Br_stanovnistvo),
         PM2.5 = ((diff.2D3c$PM2.5/sum_s)*Br_stanovnistvo),
         NMVOC = ((diff.2D3c$NMVOC/sum_s)*Br_stanovnistvo),
         NH3 = ((diff.2D3c$NH3/sum_s)*Br_stanovnistvo))
sf.2D3c %<>% select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3c <- sf.grid.5km %>%
  st_join(sf.2D3c, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.2D3c, layer.name = "Spatialised 2D3c") + mapview(sf.2D3c, layer.name = "Sources 2D3c", col.regions = "red") 
spatialised.mapview(sf.sources = sf.2D3c, layer.name.1 = "Sources 2D3c", sf.spatialised = p.2D3c, layer.name.2 = "Spatialised 2D3c", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3c <- p.2D3c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3c, total.2D3c, data.frame(sum.p.2D3c == total.2D3c)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3c, dsn="Products/2 - Other processes/2D3c.gpkg", layer='2D3c')


#'
#'
#' ## 2D3d - Coating application - paint for construction
#'
#'
#'
#+ include = FALSE
source.2D3d.pfc <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3d.pfc$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3d.pfc$total$inventory <- readxl::read_xlsx(path = source.file, range = "D75:I75", sheet = source.sheet, col_names = vars)

#+ include = FALSE
#+ include = FALSE

pop_raster <- raster("Version_2_update/Spatialization/Proxy_data_new/popdens_32634.tif")
pop_star <- st_as_stars(pop_raster)

pop.dens.spdf <- as(pop_raster, "SpatialPixelsDataFrame") 

pop.dens.sf <- st_as_sf(pop.dens.spdf)
pop.dens.sf %<>% dplyr::filter(., !(popdens_32634 == 251))

# Ukloniti tacke izvan grida za Srbiju
pop.dens.sf %<>% st_intersection(., sf.grid.5km)

pop.dens.sf[,vars] <- NA
source.2D3d.pfc$sources$points <- pop.dens.sf

sf.2D3d.pfc <- corsum2sf_point.sf(source.2D3d.pfc, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3d.pfc %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.2D3d.pfc',
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
sum.2D3d.pfc <- sf.2D3d.pfc %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3d.pfc <- source.2D3d.pfc[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3d.pfc, total.2D3d.pfc, data.frame(total.2D3d.pfc - sum.2D3d.pfc))) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.2D3d.pfc$popdens_32634)
diff.2D3d.pfc <- data.frame(total.2D3d.pfc - sum.2D3d.pfc)
sf.2D3d.pfc <- sf.2D3d.pfc %>%
  mutate(NOx = ((diff.2D3d.pfc$NOx/sum_s)*popdens_32634),
         SO2 = ((diff.2D3d.pfc$SO2/sum_s)*popdens_32634),
         PM10 = ((diff.2D3d.pfc$PM10/sum_s)*popdens_32634),
         PM2.5 = ((diff.2D3d.pfc$PM2.5/sum_s)*popdens_32634),
         NMVOC = ((diff.2D3d.pfc$NMVOC/sum_s)*popdens_32634),
         NH3 = ((diff.2D3d.pfc$NH3/sum_s)*popdens_32634))
sf.2D3d.pfc %<>% dplyr::select(vars)

#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3d.pfc <- sf.grid.5km %>%
  st_join(sf.2D3d.pfc, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3d.pfc, layer.name.1 = "Sources 2D3d.pfc", sf.spatialised = p.2D3d.pfc, layer.name.2 = "Spatialised 2D3d.pfc", vars = vars, source.lines = TRUE) 
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3d.pfc <- p.2D3d.pfc %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3d.pfc, total.2D3d.pfc, data.frame(sum.p.2D3d.pfc == total.2D3d.pfc)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3d.pfc, dsn="Products/2 - Other processes/2D3d.pfc.gpkg", layer='2D3d.pfc')




#'
#'
#' ## 2D3d - Coating application - Car/bus/truck/van coating + leather finishing
#'
#'
#'
#+ include = FALSE
source.2D3d.cbtlf <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3d.cbtlf$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3d.cbtlf$total$inventory <- readxl::read_xlsx(path = source.file, range = "U79:Z79", sheet = source.sheet, col_names = vars)


clc121 <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Industrial_sites_new.gpkg") %>%
  sf::st_set_crs(32634) %>%
  dplyr::rename(geometry = geom)

clc121 %<>% dplyr::mutate(Area_Ha = unclass(st_area(.)/10000), SHAPE_Area = unclass(st_area(.)))

clc121[,vars] <- NA
clc121.int <- st_intersection(clc121, sf.grid.5km) %>%
  dplyr::select(.,vars)

source.2D3d.cbtlf$sources$polygon <- clc121.int

sf.2D3d.cbtlf <- corsum2sf_polygon(source.2D3d.cbtlf, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3d.cbtlf %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 27: sf.2D3d.cbtlf',
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
sum.2D3d.cbtlf <- sf.2D3d.cbtlf %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3d.cbtlf <- source.2D3d.cbtlf[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3d.cbtlf, total.2D3d.cbtlf, data.frame(total.2D3d.cbtlf-sum.2D3d.cbtlf))) %>%
  datatable(., caption = 'Table 28: Summary differences',
            options = list(pageLength = 5)
  )

#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.2D3d.cbtlf <- sf.2D3d.cbtlf %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.2D3d.cbtlf$Area)
diff.2D3d.cbtlf <- data.frame(total.2D3d.cbtlf - sum.2D3d.cbtlf)
sf.2D3d.cbtlf <- sf.2D3d.cbtlf %>%
  mutate(NOx = ((diff.2D3d.cbtlf$NOx/sum_Area)*Area),
         SO2 = ((diff.2D3d.cbtlf$SO2/sum_Area)*Area),
         PM10 = ((diff.2D3d.cbtlf$PM10/sum_Area)*Area),
         PM2.5 = ((diff.2D3d.cbtlf$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.2D3d.cbtlf$NMVOC/sum_Area)*Area),
         NH3 = ((diff.2D3d.cbtlf$NH3/sum_Area)*Area))
sf.2D3d.cbtlf %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3d.cbtlf <- sf.grid.5km %>%
  st_join(sf.2D3d.cbtlf, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.2D3d.cbtlf, layer.name = "Spatialised 2D3d.cbtlf") + mapview(sf.2D3d.cbtlf, layer.name = "Sources 2D3d.cbtlf", col.regions = "red") 
spatialised.mapview(sf.sources = sf.2D3d.cbtlf, layer.name.1 = "Sources 2D3d.cbtlf", sf.spatialised = p.2D3d.cbtlf, layer.name.2 = "Spatialised 2D3d.cbtlf", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3d.cbtlf <- p.2D3d.cbtlf %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3d.cbtlf, total.2D3d.cbtlf, data.frame(sum.p.2D3d.cbtlf == total.2D3d.cbtlf)-1)) %>%
  datatable(., caption = 'Table 29: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3d.cbtlf, dsn="Products/2 - Other processes/2D3d.cbtlf.gpkg", layer='2D3d.cbtlf')


#'
#'
#' ## 2D3g - Chemical products - Rubber processing
#'
#'
#'
#+ include = FALSE
source.2D3g.rp <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3g.rp$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3g.rp$total$inventory <- readxl::read_xlsx(path = source.file, range = "D81:I81", sheet = source.sheet, col_names = vars)

source.2D3g.rp$sources$polygon <- clc121.int

sf.2D3g.rp <- corsum2sf_polygon(source.2D3g.rp, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3g.rp %>% 
  st_drop_geometry() %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3g.rp',
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
sum.2D3g.rp <- sf.2D3g.rp %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3g.rp <- source.2D3g.rp[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3g.rp, total.2D3g.rp, data.frame(total.2D3g.rp - sum.2D3g.rp))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sf.2D3g.rp %<>% dplyr::mutate(Area = unclass(st_area(.)))

sum_s <- sum(sf.2D3g.rp$Area)
diff.2D3g.rp <- data.frame(total.2D3g.rp - sum.2D3g.rp)
sf.2D3g.rp <- sf.2D3g.rp %>%
  mutate(NOx = ((diff.2D3g.rp$NOx/sum_s)*Area),
         SO2 = ((diff.2D3g.rp$SO2/sum_s)*Area),
         PM10 = ((diff.2D3g.rp$PM10/sum_s)*Area),
         PM2.5 = ((diff.2D3g.rp$PM2.5/sum_s)*Area),
         NMVOC = ((diff.2D3g.rp$NMVOC/sum_s)*Area),
         NH3 = ((diff.2D3g.rp$NH3/sum_s)*Area))
sf.2D3g.rp %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3g.rp <- sf.grid.5km %>%
  st_join(sf.2D3g.rp, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3g.rp, layer.name.1 = "Sources 2D3g.rp", sf.spatialised = p.2D3g.rp, layer.name.2 = "Spatialised 2D3g.rp", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3g.rp <- p.2D3g.rp %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3g.rp, total.2D3g.rp, data.frame(sum.p.2D3g.rp == total.2D3g.rp)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3g.rp, dsn="Products/2 - Other processes/2D3g.rp.gpkg", layer='2D3g.rp')




#'
#'
#' ## 2D3g - Chemical products - Paint/ink/glue manufacturing
#'
#'
#'
#+ include = FALSE
source.2D3g.pigm <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3g.pigm$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3g.pigm$total$inventory <- readxl::read_xlsx(path = source.file, range = "D82:I82", sheet = source.sheet, col_names = vars)

source.2D3g.pigm$sources$polygon <- clc121.int

sf.2D3g.pigm <- corsum2sf_polygon(source.2D3g.pigm, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3g.pigm %>% 
  st_drop_geometry() %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3g.pigm',
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
sum.2D3g.pigm <- sf.2D3g.pigm %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3g.pigm <- source.2D3g.pigm[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3g.pigm, total.2D3g.pigm, data.frame(total.2D3g.pigm - sum.2D3g.pigm))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sf.2D3g.pigm %<>% dplyr::mutate(Area = unclass(st_area(.)))

sum_s <- sum(sf.2D3g.pigm$Area)
diff.2D3g.pigm <- data.frame(total.2D3g.pigm - sum.2D3g.pigm)
sf.2D3g.pigm <- sf.2D3g.pigm %>%
  mutate(NOx = ((diff.2D3g.pigm$NOx/sum_s)*Area),
         SO2 = ((diff.2D3g.pigm$SO2/sum_s)*Area),
         PM10 = ((diff.2D3g.pigm$PM10/sum_s)*Area),
         PM2.5 = ((diff.2D3g.pigm$PM2.5/sum_s)*Area),
         NMVOC = ((diff.2D3g.pigm$NMVOC/sum_s)*Area),
         NH3 = ((diff.2D3g.pigm$NH3/sum_s)*Area))
sf.2D3g.pigm %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3g.pigm <- sf.grid.5km %>%
  st_join(sf.2D3g.pigm, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3g.pigm, layer.name.1 = "Sources 2D3g.pigm", sf.spatialised = p.2D3g.pigm, layer.name.2 = "Spatialised 2D3g.pigm", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3g.pigm <- p.2D3g.pigm %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3g.pigm, total.2D3g.pigm, data.frame(sum.p.2D3g.pigm == total.2D3g.pigm)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3g.pigm, dsn="Products/2 - Other processes/2D3g.pigm.gpkg", layer='2D3g.pigm')





#'
#'
#' ## 2D3g - Chemical products - Manufacture of shoes
#'
#'
#'
#+ include = FALSE
source.2D3g.ms <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3g.ms$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3g.ms$total$inventory <- readxl::read_xlsx(path = source.file, range = "D83:I83", sheet = source.sheet, col_names = vars)

source.2D3g.ms$sources$polygon <- clc121.int

sf.2D3g.ms <- corsum2sf_polygon(source.2D3g.ms, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3g.ms %>% 
  st_drop_geometry() %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3g.ms',
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
sum.2D3g.ms <- sf.2D3g.ms %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3g.ms <- source.2D3g.ms[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3g.ms, total.2D3g.ms, data.frame(total.2D3g.ms - sum.2D3g.ms))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sf.2D3g.ms %<>% dplyr::mutate(Area = unclass(st_area(.)))

sum_s <- sum(sf.2D3g.ms$Area)
diff.2D3g.ms <- data.frame(total.2D3g.ms - sum.2D3g.ms)
sf.2D3g.ms <- sf.2D3g.ms %>%
  mutate(NOx = ((diff.2D3g.ms$NOx/sum_s)*Area),
         SO2 = ((diff.2D3g.ms$SO2/sum_s)*Area),
         PM10 = ((diff.2D3g.ms$PM10/sum_s)*Area),
         PM2.5 = ((diff.2D3g.ms$PM2.5/sum_s)*Area),
         NMVOC = ((diff.2D3g.ms$NMVOC/sum_s)*Area),
         NH3 = ((diff.2D3g.ms$NH3/sum_s)*Area))
sf.2D3g.ms %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3g.ms <- sf.grid.5km %>%
  st_join(sf.2D3g.ms, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3g.ms, layer.name.1 = "Sources 2D3g.ms", sf.spatialised = p.2D3g.ms, layer.name.2 = "Spatialised 2D3g.ms", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3g.ms <- p.2D3g.ms %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3g.ms, total.2D3g.ms, data.frame(sum.p.2D3g.ms == total.2D3g.ms)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3g.ms, dsn="Products/2 - Other processes/2D3g.ms.gpkg", layer='2D3g.ms')






#'
#'
#' ## 2D3g - Chemical products - Leather tanning
#'
#'
#'
#+ include = FALSE
source.2D3g.lt <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3g.lt$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3g.lt$total$inventory <- readxl::read_xlsx(path = source.file, range = "D84:I84", sheet = source.sheet, col_names = vars)

source.2D3g.lt$sources$polygon <- clc121.int

sf.2D3g.lt <- corsum2sf_polygon(source.2D3g.lt, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3g.lt %>% 
  st_drop_geometry() %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3g.lt',
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
sum.2D3g.lt <- sf.2D3g.lt %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3g.lt <- source.2D3g.lt[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3g.lt, total.2D3g.lt, data.frame(total.2D3g.lt - sum.2D3g.lt))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sf.2D3g.lt %<>% dplyr::mutate(Area = unclass(st_area(.)))

sum_s <- sum(sf.2D3g.lt$Area)
diff.2D3g.lt <- data.frame(total.2D3g.lt - sum.2D3g.lt)
sf.2D3g.lt <- sf.2D3g.lt %>%
  mutate(NOx = ((diff.2D3g.lt$NOx/sum_s)*Area),
         SO2 = ((diff.2D3g.lt$SO2/sum_s)*Area),
         PM10 = ((diff.2D3g.lt$PM10/sum_s)*Area),
         PM2.5 = ((diff.2D3g.lt$PM2.5/sum_s)*Area),
         NMVOC = ((diff.2D3g.lt$NMVOC/sum_s)*Area),
         NH3 = ((diff.2D3g.lt$NH3/sum_s)*Area))
sf.2D3g.lt %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3g.lt <- sf.grid.5km %>%
  st_join(sf.2D3g.lt, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3g.lt, layer.name.1 = "Sources 2D3g.lt", sf.spatialised = p.2D3g.lt, layer.name.2 = "Spatialised 2D3g.lt", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3g.lt <- p.2D3g.lt %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3g.lt, total.2D3g.lt, data.frame(sum.p.2D3g.lt == total.2D3g.lt)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3g.lt, dsn="Products/2 - Other processes/2D3g.lt.gpkg", layer='2D3g.lt')






#'
#'
#' ## 2D3i - Other solvent and product use - Fat, edible and non-edible oil extraction (kg seed)
#'
#'
#'
#+ include = FALSE
source.2D3i.feneox <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3i.feneox$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3i.feneox$total$inventory <- readxl::read_xlsx(path = source.file, range = "D86:I86", sheet = source.sheet, col_names = vars)

source.2D3i.feneox$sources$polygon <- clc121.int

sf.2D3i.feneox <- corsum2sf_polygon(source.2D3i.feneox, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3i.feneox %>% 
  st_drop_geometry() %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3i.feneox',
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
sum.2D3i.feneox <- sf.2D3i.feneox %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3i.feneox <- source.2D3i.feneox[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3i.feneox, total.2D3i.feneox, data.frame(total.2D3i.feneox - sum.2D3i.feneox))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sf.2D3i.feneox %<>% dplyr::mutate(Area = unclass(st_area(.)))

sum_s <- sum(sf.2D3i.feneox$Area)
diff.2D3i.feneox <- data.frame(total.2D3i.feneox - sum.2D3i.feneox)
sf.2D3i.feneox <- sf.2D3i.feneox %>%
  mutate(NOx = ((diff.2D3i.feneox$NOx/sum_s)*Area),
         SO2 = ((diff.2D3i.feneox$SO2/sum_s)*Area),
         PM10 = ((diff.2D3i.feneox$PM10/sum_s)*Area),
         PM2.5 = ((diff.2D3i.feneox$PM2.5/sum_s)*Area),
         NMVOC = ((diff.2D3i.feneox$NMVOC/sum_s)*Area),
         NH3 = ((diff.2D3i.feneox$NH3/sum_s)*Area))
sf.2D3i.feneox %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3i.feneox <- sf.grid.5km %>%
  st_join(sf.2D3i.feneox, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3i.feneox, layer.name.1 = "Sources 2D3i.feneox", sf.spatialised = p.2D3i.feneox, layer.name.2 = "Spatialised 2D3i.feneox", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3i.feneox <- p.2D3i.feneox %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3i.feneox, total.2D3i.feneox, data.frame(sum.p.2D3i.feneox == total.2D3i.feneox)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3i.feneox, dsn="Products/2 - Other processes/2D3i.feneox.gpkg", layer='2D3i.feneox')










#'
#'
#' ## 2D3i - Other solvent and product use - Preservation of wood
#'
#'
#'
#+ include = FALSE
source.2D3i.pow <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3i.pow$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3i.pow$total$inventory <- readxl::read_xlsx(path = source.file, range = "D87:I87", sheet = source.sheet, col_names = vars)

source.2D3i.pow$sources$points <- pop.dens.sf

sf.2D3i.pow <- corsum2sf_point.sf(source.2D3i.pow, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3i.pow %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.2D3i.pow',
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
sum.2D3i.pow <- sf.2D3i.pow %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3i.pow <- source.2D3i.pow[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3i.pow, total.2D3i.pow, data.frame(total.2D3i.pow - sum.2D3i.pow))) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.2D3i.pow$popdens_32634)
diff.2D3i.pow <- data.frame(total.2D3i.pow - sum.2D3i.pow)
sf.2D3i.pow <- sf.2D3i.pow %>%
  mutate(NOx = ((diff.2D3i.pow$NOx/sum_s)*popdens_32634),
         SO2 = ((diff.2D3i.pow$SO2/sum_s)*popdens_32634),
         PM10 = ((diff.2D3i.pow$PM10/sum_s)*popdens_32634),
         PM2.5 = ((diff.2D3i.pow$PM2.5/sum_s)*popdens_32634),
         NMVOC = ((diff.2D3i.pow$NMVOC/sum_s)*popdens_32634),
         NH3 = ((diff.2D3i.pow$NH3/sum_s)*popdens_32634))
sf.2D3i.pow %<>% dplyr::select(vars)

#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3i.pow <- sf.grid.5km %>%
  st_join(sf.2D3i.pow, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3i.pow, layer.name.1 = "Sources 2D3i.pow", sf.spatialised = p.2D3i.pow, layer.name.2 = "Spatialised 2D3i.pow", vars = vars, source.lines = TRUE) 
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3i.pow <- p.2D3i.pow %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3i.pow, total.2D3i.pow, data.frame(sum.p.2D3i.pow == total.2D3i.pow)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3i.pow, dsn="Products/2 - Other processes/2D3i.pow.gpkg", layer='2D3i.pow')








#'
#'
#' ## 2D3i - Other solvent and product use - Underseal treatment and conservation of vehicles
#'
#'
#'
#+ include = FALSE
source.2D3i.utcv <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3i.utcv$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3i.utcv$total$inventory <- readxl::read_xlsx(path = source.file, range = "D88:I88", sheet = source.sheet, col_names = vars)


source.2D3i.utcv$sources$polygon <- clc121.int

sf.2D3i.utcv <- corsum2sf_polygon(source.2D3i.utcv, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3i.utcv %>% 
  st_drop_geometry() %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3i.utcv',
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
sum.2D3i.utcv <- sf.2D3i.utcv %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3i.utcv <- source.2D3i.utcv[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3i.utcv, total.2D3i.utcv, data.frame(total.2D3i.utcv - sum.2D3i.utcv))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sf.2D3i.utcv %<>% dplyr::mutate(Area = unclass(st_area(.)))

sum_s <- sum(sf.2D3i.utcv$Area)
diff.2D3i.utcv <- data.frame(total.2D3i.utcv - sum.2D3i.utcv)
sf.2D3i.utcv <- sf.2D3i.utcv %>%
  mutate(NOx = ((diff.2D3i.utcv$NOx/sum_s)*Area),
         SO2 = ((diff.2D3i.utcv$SO2/sum_s)*Area),
         PM10 = ((diff.2D3i.utcv$PM10/sum_s)*Area),
         PM2.5 = ((diff.2D3i.utcv$PM2.5/sum_s)*Area),
         NMVOC = ((diff.2D3i.utcv$NMVOC/sum_s)*Area),
         NH3 = ((diff.2D3i.utcv$NH3/sum_s)*Area))
sf.2D3i.utcv %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3i.utcv <- sf.grid.5km %>%
  st_join(sf.2D3i.utcv, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3i.utcv, layer.name.1 = "Sources 2D3i.utcv", sf.spatialised = p.2D3i.utcv, layer.name.2 = "Spatialised 2D3i.utcv", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3i.utcv <- p.2D3i.utcv %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3i.utcv, total.2D3i.utcv, data.frame(sum.p.2D3i.utcv == total.2D3i.utcv)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3i.utcv, dsn="Products/2 - Other processes/2D3i.utcv.gpkg", layer='2D3i.utcv')












#'
#'
#' ## 2D3i - Other solvent and product use - Tobacco (t tobacco)
#'
#'
#'
#+ include = FALSE
source.2D3i.t <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3i.t$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3i.t$total$inventory <- readxl::read_xlsx(path = source.file, range = "D89:I89", sheet = source.sheet, col_names = vars)

source.2D3i.t$sources$points <- pop.dens.sf

sf.2D3i.t <- corsum2sf_point.sf(source.2D3i.t, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3i.t %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.2D3i.t',
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
sum.2D3i.t <- sf.2D3i.t %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3i.t <- source.2D3i.t[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3i.t, total.2D3i.t, data.frame(total.2D3i.t - sum.2D3i.t))) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.2D3i.t$popdens_32634)
diff.2D3i.t <- data.frame(total.2D3i.t - sum.2D3i.t)
sf.2D3i.t <- sf.2D3i.t %>%
  mutate(NOx = ((diff.2D3i.t$NOx/sum_s)*popdens_32634),
         SO2 = ((diff.2D3i.t$SO2/sum_s)*popdens_32634),
         PM10 = ((diff.2D3i.t$PM10/sum_s)*popdens_32634),
         PM2.5 = ((diff.2D3i.t$PM2.5/sum_s)*popdens_32634),
         NMVOC = ((diff.2D3i.t$NMVOC/sum_s)*popdens_32634),
         NH3 = ((diff.2D3i.t$NH3/sum_s)*popdens_32634))
sf.2D3i.t %<>% dplyr::select(vars)

#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3i.t <- sf.grid.5km %>%
  st_join(sf.2D3i.t, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3i.t, layer.name.1 = "Sources 2D3i.t", sf.spatialised = p.2D3i.t, layer.name.2 = "Spatialised 2D3i.t", vars = vars, source.lines = TRUE) 
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3i.t <- p.2D3i.t %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3i.t, total.2D3i.t, data.frame(sum.p.2D3i.t == total.2D3i.t)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3i.t, dsn="Products/2 - Other processes/2D3i.t.gpkg", layer='2D3i.t')






#'
#'
#' ## 2D3i - Other solvent and product use - Use of shoes
#'
#'
#'
#+ include = FALSE
source.2D3i.uos <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3i.uos$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3i.uos$total$inventory <- readxl::read_xlsx(path = source.file, range = "D90:I90", sheet = source.sheet, col_names = vars)

source.2D3i.uos$sources$points <- pop.dens.sf

sf.2D3i.uos <- corsum2sf_point.sf(source.2D3i.uos, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3i.uos %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.2D3i.uos',
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
sum.2D3i.uos <- sf.2D3i.uos %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3i.uos <- source.2D3i.uos[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3i.uos, total.2D3i.uos, data.frame(total.2D3i.uos - sum.2D3i.uos))) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.2D3i.uos$popdens_32634)
diff.2D3i.uos <- data.frame(total.2D3i.uos - sum.2D3i.uos)
sf.2D3i.uos <- sf.2D3i.uos %>%
  mutate(NOx = ((diff.2D3i.uos$NOx/sum_s)*popdens_32634),
         SO2 = ((diff.2D3i.uos$SO2/sum_s)*popdens_32634),
         PM10 = ((diff.2D3i.uos$PM10/sum_s)*popdens_32634),
         PM2.5 = ((diff.2D3i.uos$PM2.5/sum_s)*popdens_32634),
         NMVOC = ((diff.2D3i.uos$NMVOC/sum_s)*popdens_32634),
         NH3 = ((diff.2D3i.uos$NH3/sum_s)*popdens_32634))
sf.2D3i.uos %<>% dplyr::select(vars)

#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3i.uos <- sf.grid.5km %>%
  st_join(sf.2D3i.uos, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3i.uos, layer.name.1 = "Sources 2D3i.uos", sf.spatialised = p.2D3i.uos, layer.name.2 = "Spatialised 2D3i.uos", vars = vars, source.lines = TRUE) 
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3i.uos <- p.2D3i.uos %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3i.uos, total.2D3i.uos, data.frame(sum.p.2D3i.uos == total.2D3i.uos)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3i.uos, dsn="Products/2 - Other processes/2D3i.uos.gpkg", layer='2D3i.uos')




#'
#'
#' ## 2D3a - Domestic solvent use including fungicides
#'
#'
#'
#+ include = FALSE
source.2D3a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D92:I92", sheet = source.sheet, col_names = vars)



library(raster)
library(stars)
#+ include = FALSE

pop_raster <- raster("Version_2_update/Spatialization/Proxy_data_new/popdens_32634.tif")
pop_star <- st_as_stars(pop_raster)

pop.dens.spdf <- as(pop_raster, "SpatialPixelsDataFrame") 

pop.dens.sf <- st_as_sf(pop.dens.spdf)
pop.dens.sf %<>% dplyr::filter(., !(popdens_32634 == 251))

# Ukloniti tacke izvan grida za Srbiju
pop.dens.sf %<>% st_intersection(., sf.grid.5km)

pop.dens.sf[,vars] <- NA
source.2D3a$sources$points <- pop.dens.sf

sf.2D3a <- corsum2sf_point.sf(source.2D3a, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3a %>% 
  st_drop_geometry() %>%
  dplyr::rename(Population_density = popdens_32634) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3a',
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
sum.2D3a <- sf.2D3a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3a <- source.2D3a[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3a, total.2D3a, data.frame(total.2D3a - sum.2D3a))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.2D3a$popdens_32634)
diff.2D3a <- data.frame(total.2D3a - sum.2D3a)
sf.2D3a <- sf.2D3a %>%
  mutate(NOx = ((diff.2D3a$NOx/sum_s)*popdens_32634),
         SO2 = ((diff.2D3a$SO2/sum_s)*popdens_32634),
         PM10 = ((diff.2D3a$PM10/sum_s)*popdens_32634),
         PM2.5 = ((diff.2D3a$PM2.5/sum_s)*popdens_32634),
         NMVOC = ((diff.2D3a$NMVOC/sum_s)*popdens_32634),
         NH3 = ((diff.2D3a$NH3/sum_s)*popdens_32634))
sf.2D3a %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3a <- sf.grid.5km %>%
  st_join(sf.2D3a, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3a, layer.name.1 = "Sources 2D3a", sf.spatialised = p.2D3a, layer.name.2 = "Spatialised 2D3a", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3a <- p.2D3a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3a, total.2D3a, data.frame(sum.p.2D3a == total.2D3a)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3a, dsn="Products/2 - Other processes/2D3a.gpkg", layer='2D3a')

#'
#'
#' ## 2D3e - Degreasing
#'
#'
#'
#+ include = FALSE
source.2D3e <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3e$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3e$total$inventory <- readxl::read_xlsx(path = source.file, range = "D93:I93", sheet = source.sheet, col_names = vars)

source.2D3e$sources$polygon <- clc121.int

sf.2D3e <- corsum2sf_polygon(source.2D3e, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3e %>% 
  st_drop_geometry() %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3e',
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
sum.2D3e <- sf.2D3e %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3e <- source.2D3e[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3e, total.2D3e, data.frame(total.2D3e - sum.2D3e))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sf.2D3e %<>% dplyr::mutate(Area = unclass(st_area(.)))

sum_s <- sum(sf.2D3e$Area)
diff.2D3e <- data.frame(total.2D3e - sum.2D3e)
sf.2D3e <- sf.2D3e %>%
  mutate(NOx = ((diff.2D3e$NOx/sum_s)*Area),
         SO2 = ((diff.2D3e$SO2/sum_s)*Area),
         PM10 = ((diff.2D3e$PM10/sum_s)*Area),
         PM2.5 = ((diff.2D3e$PM2.5/sum_s)*Area),
         NMVOC = ((diff.2D3e$NMVOC/sum_s)*Area),
         NH3 = ((diff.2D3e$NH3/sum_s)*Area))
sf.2D3e %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3e <- sf.grid.5km %>%
  st_join(sf.2D3e, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3e, layer.name.1 = "Sources 2D3e", sf.spatialised = p.2D3e, layer.name.2 = "Spatialised 2D3e", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3e <- p.2D3e %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3e, total.2D3e, data.frame(sum.p.2D3e == total.2D3e)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3e, dsn="Products/2 - Other processes/2D3e.gpkg", layer='2D3e')

#'
#'
#' ## 2D3f - Dry cleaning
#'
#'
#'
#+ include = FALSE
source.2D3f <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3f$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3f$total$inventory <- readxl::read_xlsx(path = source.file, range = "D94:I94", sheet = source.sheet, col_names = vars)
#sf_clc18_urb[,vars] <- NA # za kartu
source.2D3f$sources$polygon <- sf_clc18_urb.int

sf.2D3f <- corsum2sf_polygon(source.2D3f, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3f %>% 
  st_drop_geometry() %>%
  dplyr::rename(Population_weight = Br_stanovnistvo) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3f',
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
sum.2D3f <- sf.2D3f %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3f <- source.2D3f[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3f, total.2D3f, data.frame(total.2D3f - sum.2D3f))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.2D3f$Br_stanovnistvo)
diff.2D3f <- data.frame(total.2D3f - sum.2D3f)
sf.2D3f <- sf.2D3f %>%
  mutate(NOx = ((diff.2D3f$NOx/sum_s)*Br_stanovnistvo),
         SO2 = ((diff.2D3f$SO2/sum_s)*Br_stanovnistvo),
         PM10 = ((diff.2D3f$PM10/sum_s)*Br_stanovnistvo),
         PM2.5 = ((diff.2D3f$PM2.5/sum_s)*Br_stanovnistvo),
         NMVOC = ((diff.2D3f$NMVOC/sum_s)*Br_stanovnistvo),
         NH3 = ((diff.2D3f$NH3/sum_s)*Br_stanovnistvo))
sf.2D3f %<>% select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3f <- sf.grid.5km %>%
  st_join(sf.2D3f, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3f, layer.name.1 = "Sources 2D3f", sf.spatialised = p.2D3f, layer.name.2 = "Spatialised 2D3f", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3f <- p.2D3f %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3f, total.2D3f, data.frame(sum.p.2D3f == total.2D3f)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3f, dsn="Products/2 - Other processes/2D3f.gpkg", layer='2D3f')

#'
#'
#' ## 2D3h - Printing
#'
#'
#'
#+ include = FALSE
source.2D3h <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2D3h$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.2D3h$total$inventory <- readxl::read_xlsx(path = source.file, range = "D95:I95", sheet = source.sheet, col_names = vars)

source.2D3h$sources$polygon <- clc121.int

sf.2D3h <- corsum2sf_polygon(source.2D3h, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2D3h %>% 
  st_drop_geometry() %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2D3h',
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
sum.2D3h <- sf.2D3h %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2D3h <- source.2D3h[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2D3h, total.2D3h, data.frame(total.2D3h - sum.2D3h))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sf.2D3h %<>% dplyr::mutate(Area = unclass(st_area(.)))

sum_s <- sum(sf.2D3h$Area)
diff.2D3h <- data.frame(total.2D3h - sum.2D3h)
sf.2D3h <- sf.2D3h %>%
  mutate(NOx = ((diff.2D3h$NOx/sum_s)*Area),
         SO2 = ((diff.2D3h$SO2/sum_s)*Area),
         PM10 = ((diff.2D3h$PM10/sum_s)*Area),
         PM2.5 = ((diff.2D3h$PM2.5/sum_s)*Area),
         NMVOC = ((diff.2D3h$NMVOC/sum_s)*Area),
         NH3 = ((diff.2D3h$NH3/sum_s)*Area))
sf.2D3h %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2D3h <- sf.grid.5km %>%
  st_join(sf.2D3h, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.2D3h, layer.name.1 = "Sources 2D3h", sf.spatialised = p.2D3h, layer.name.2 = "Spatialised 2D3h", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2D3h <- p.2D3h %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2D3h, total.2D3h, data.frame(sum.p.2D3h == total.2D3h)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2D3h, dsn="Products/2 - Other processes/2D3h.gpkg", layer='2D3h')


#'
#'
#' ## 2I - Wood processing
#'
#'
#'
#+ include = FALSE

source.2I <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.2I$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D113:I113", sheet = source.sheet, col_names = vars)
source.2I$total$inventory <- readxl::read_xlsx(path = source.file, range = "D114:I114", sheet = source.sheet, col_names = vars)


#+ include = FALSE
# Including rural areas, industrial sites

sf.rur <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg") %>%
  dplyr::select(geom) %>%
  dplyr::rename(geometry = geom)
sf.ind <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Industrial_sites_new.gpkg") %>%
  dplyr::select(geom) %>%
  dplyr::rename(geometry = geom)


sf.final <- rbind(sf.rur, sf.ind)


#+ include = FALSE
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
wood <- readxl::read_xls(path = "Data/Tehnicko_drvo_2015.xls", sheet = "Sumarstvo")
lcl(loc = "C")
wood <- cyr_lat(wood)
names(wood) <- cyr_lat(names(wood)) 
wood <- wood  %>% 
  mutate_at(.vars = 7:9, .funs = as.numeric) %>%
  mutate_all(~replace_na(., 0))

wood$Opština[wood$Opština == "Indjija"] <- "Inđija"
wood$Opština[wood$Opština == "LJubovija"] <- "Ljubovija"
wood$Opština[wood$Opština == "Mali Idjoš"] <- "Mali Iđoš"
wood$Opština[wood$Opština == "Savski venac"] <- "Savski Venac"
wood$Opština[wood$Opština == "Stari grad"] <- "Stari Grad"
wood$Opština[wood$Opština == "Petrovac na Mlavi"] <- "Petrovac"
wood$Opština[wood$Opština == "Arandjelovac"] <- "Aranđelovac"
wood$Opština[wood$Opština == "LJig"] <- "Ljig"
wood$Opština[wood$Opština == "Žitoradja"] <- "Žitorađa"
wood$Opština[wood$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$pl <- wood$`Posečena drvna zapremina lišćara`[match(sf_opstine$NAME_2, wood$Opština)]
sf_opstine$pc <- wood$`Posečena drvna zapremina četinara`[match(sf_opstine$NAME_2, wood$Opština)]
sf_opstine$tlp <- wood$`Tehničko_lišćara_procenat`[match(sf_opstine$NAME_2, wood$Opština)]
sf_opstine$tcp <- wood$`Tehničko_četinara_procenat`[match(sf_opstine$NAME_2, wood$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")
sf_final <- st_join(sf_final, sf_opstine, largest = TRUE) 

sf_final %<>% dplyr::select(.,pl, pc, tlp, tcp)
sf_final[,vars] <- NA

sf_final.int <- st_intersection(sf_final, sf.grid.5km) %>%
  filter(!is.na(pl) & !is.na(pc) & !is.na(tlp) & !is.na(tcp))

source.2I$sources$polygon <- sf_final.int

sf.2I <- corsum2sf_polygon(source.2I, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.2I %>% 
  st_drop_geometry() %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.2I',
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
sum.2I <- sf.2I %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.2I <- source.2I[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.2I, total.2I, data.frame(total.2I - sum.2I))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.2I %<>%
  mutate(pd = ((pl/100)*tlp) + ((pc/100)*tcp))

sum_s <- sum(sf.2I$pd)
diff.2I <- data.frame(total.2I - sum.2I)
sf.2I <- sf.2I %>%
  mutate(NOx = ((diff.2I$NOx/sum_s)*pd),
         SO2 = ((diff.2I$SO2/sum_s)*pd),
         PM10 = ((diff.2I$PM10/sum_s)*pd),
         PM2.5 = ((diff.2I$PM2.5/sum_s)*pd),
         NMVOC = ((diff.2I$NMVOC/sum_s)*pd),
         NH3 = ((diff.2I$NH3/sum_s)*pd))
sf.2I %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.2I <- sf.grid.5km %>%
  st_join(sf.2I, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.2I, layer.name = "Spatialised 2I") + mapview(sf.2I, layer.name = "Sources 2I", col.regions = "red") 
spatialised.mapview(sf.sources = sf.2I, layer.name.1 = "Sources 2I", sf.spatialised = p.2I, layer.name.2 = "Spatialised 2I", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.2I <- p.2I %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.2I, total.2I, data.frame(sum.p.2I == total.2I)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.2I, dsn="Products/2 - Other processes/2I.gpkg", layer='2I')




