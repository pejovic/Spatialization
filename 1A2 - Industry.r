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
#' # 1A2-2-Industry
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
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "1A2-2-Industry"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Polygons_5km_UTM_34N.shp")
sf.grid.5km <- st_as_sf(grid.5km)   
#'
#'
#' ## 1A2a / 2C1 - Iron and Steel
#'
#'
#'
#+ include = FALSE
source.1A2a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2a$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S18", sheet = source.sheet, col_names = header)
source.1A2a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D29:I29", sheet = source.sheet, col_names = vars)
source.1A2a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D34:I34", sheet = source.sheet, col_names = vars)


sf.1A2a <- corsum2sf(source.1A2a) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2a %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.1A2a',
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
sum.1A2a <- sf.1A2a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A2a <- source.1A2a[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A2a, total.1A2a, data.frame(sum.1A2a == total.1A2a)-1)) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A2a <- sf.grid.5km %>%
  st_join(sf.1A2a) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A2a, layer.name = "Sources 1A2a", col.regions = "red") + mapview(p.1A2a, layer.name = "Spatialised 1A2a")
spatialised.mapview(sf.sources = sf.1A2a, layer.name.1 = "Sources 1A2a", sf.spatialised = p.1A2a, layer.name.2 = "Spatialised 1A2a", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A2a <- p.1A2a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A2a, total.1A2a, data.frame(sum.p.1A2a == total.1A2a)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A2a, dsn="Products/1A2 - Industry/1A2a.gpkg", layer='1A2a')

#'
#'
#'
#' ## 1A2b - Non-ferrous metals
#+ include = FALSE
source.1A2b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2b$sources$points <- readxl::read_xlsx(path = source.file, range = "D35:S40", sheet = source.sheet, col_names = header)
source.1A2b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D49:I49", sheet = source.sheet, col_names = vars)
source.1A2b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D54:I54", sheet = source.sheet, col_names = vars)

sf.1A2b <- corsum2sf(source.1A2b) %>%
  st_transform(crs = "+init=epsg:32634")
#'
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2b %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.1A2b',
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
sum.1A2b <- sf.1A2b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A2b <- source.1A2b[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A2b, total.1A2b, data.frame(sum.1A2b == total.1A2b)-1)) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A2b <- sf.grid.5km %>%
  st_join(sf.1A2b) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A2b, layer.name = "Sources 1A2b", col.regions = "red") + mapview(p.1A2b, layer.name = "Spatialised 1A2b")
spatialised.mapview(sf.sources = sf.1A2b, layer.name.1 = "Sources 1A2b", sf.spatialised = p.1A2b, layer.name.2 = "Spatialised 1A2b", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A2b <- p.1A2b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A2b, total.1A2b, data.frame(sum.p.1A2b == total.1A2b)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A2b, dsn="Products/1A2 - Industry/1A2b.gpkg", layer='1A2b')

#'
#'
#' ## 1A2c - Chemicals
#'
#+ include = FALSE

source.1A2c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2c$sources$points <- readxl::read_xlsx(path = source.file, range = "D55:S69", sheet = source.sheet, col_names = header)
source.1A2c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D71:I71", sheet = source.sheet, col_names = vars)
source.1A2c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D86:I86", sheet = source.sheet, col_names = vars)

sf.1A2c <- corsum2sf(source.1A2c) %>%
  st_transform(crs = "+init=epsg:32634")

#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2c %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.1A2aiv',
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
sum.1A2c <- sf.1A2c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A2c <- source.1A2c[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A2c, total.1A2c, data.frame(sum.1A2c == total.1A2c)-1)) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A2c <- sf.grid.5km %>%
  st_join(sf.1A2c) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A2c, layer.name = "Sources 1A2c", col.regions = "red") + mapview(p.1A2c, layer.name = "Spatialised 1A2c")
spatialised.mapview(sf.sources = sf.1A2c, layer.name.1 = "Sources 1A2c", sf.spatialised = p.1A2c, layer.name.2 = "Spatialised 1A2c", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A2c <- p.1A2c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A2c, total.1A2c, data.frame(sum.p.1A2c == total.1A2c)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A2c, dsn="Products/1A2 - Industry/1A2c.gpkg", layer='1A2c')

#'
#'
#'
#'
#'
#'
#' ## 1A2d - Pulp, paper and print
#' 
#+ include = FALSE
source.1A2d <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2d$sources$points <- readxl::read_xlsx(path = source.file, range = "D87:S94", sheet = source.sheet, col_names = header)
source.1A2d$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D103:I103", sheet = source.sheet, col_names = vars)
source.1A2d$total$inventory <- readxl::read_xlsx(path = source.file, range = "D109:I109", sheet = source.sheet, col_names = vars)

sf.1A2d <- corsum2sf(source.1A2d, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")
#'
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2d %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 10: sf.1A2d',
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
sum.1A2d <- sf.1A2d %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A2d <- source.1A2d[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A2d, total.1A2d, data.frame(sum.1A2d == total.1A2d)-1)) %>%
  datatable(., caption = 'Table 11: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A2d <- sf.grid.5km %>%
  st_join(sf.1A2d) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A2d, layer.name = "Sources 1A2d", col.regions = "red") + mapview(p.1A2d, layer.name = "Spatialised 1A2d")
spatialised.mapview(sf.sources = sf.1A2d, layer.name.1 = "Sources 1A2d", sf.spatialised = p.1A2d, layer.name.2 = "Spatialised 1A2d", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A2d <- p.1A2d %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A2d, total.1A2d, data.frame(sum.p.1A2d == total.1A2d)-1)) %>%
  datatable(., caption = 'Table 12: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A2d, dsn="Products/1A2 - Industry/1A2d.gpkg", layer='1A2d')

#'
#'
#'
#' ## 1A2e - Food, beverages and tobacco
#'
#+ include = FALSE
source.1A2e <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2e$sources$points <- readxl::read_xlsx(path = source.file, range = "D110:S131", sheet = source.sheet, col_names = header)
source.1A2e$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D137:I137", sheet = source.sheet, col_names = vars)
source.1A2e$total$inventory <- readxl::read_xlsx(path = source.file, range = "D151:I151", sheet = source.sheet, col_names = vars)

sf.1A2e <- corsum2sf(source.1A2e, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2e %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 13: sf.1A2e',
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
sum.1A2e <- sf.1A2e %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A2e <- source.1A2e[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A2e, total.1A2e, data.frame(sum.1A2e == total.1A2e)-1)) %>%
  datatable(., caption = 'Table 14: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A2e <- sf.grid.5km %>%
  st_join(sf.1A2e) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A2e, layer.name = "Sources 1A2e", col.regions = "red") + mapview(p.1A2e, layer.name = "Spatialised 1A2e")
spatialised.mapview(sf.sources = sf.1A2e, layer.name.1 = "Sources 1A2e", sf.spatialised = p.1A2e, layer.name.2 = "Spatialised 1A2e", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A2e <- p.1A2e %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A2e, total.1A2e, data.frame(sum.p.1A2e == total.1A2e)-1)) %>%
  datatable(., caption = 'Table 15: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A2e, dsn="Products/1A2 - Industry/1A2e.gpkg", layer='1A2e')

#'
#'
#'
#'
#'
#'
#' ## 1A2f - Non-metallic minerals
#'
#+ include = FALSE
source.1A2f <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2f$sources$points <- readxl::read_xlsx(path = source.file, range = "D152:S180", sheet = source.sheet, col_names = header)
source.1A2f$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D182:I182", sheet = source.sheet, col_names = vars)
source.1A2f$total$inventory <- readxl::read_xlsx(path = source.file, range = "D189:I189", sheet = source.sheet, col_names = vars)

sf.1A2f <- corsum2sf(source.1A2f, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2f %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 16: sf.1A2f',
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
sum.1A2f <- sf.1A2f %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A2f <- source.1A2f[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A2f, total.1A2f, data.frame(sum.1A2f == total.1A2f)-1)) %>%
  datatable(., caption = 'Table 17: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A2f <- sf.grid.5km %>%
  st_join(sf.1A2f) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A2f, layer.name = "Sources 1A2f", col.regions = "red") + mapview(p.1A2f, layer.name = "Spatialised 1A2f")
spatialised.mapview(sf.sources = sf.1A2f, layer.name.1 = "Sources 1A2f", sf.spatialised = p.1A2f, layer.name.2 = "Spatialised 1A2f", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A2f <- p.1A2f %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A2f, total.1A2f, data.frame(sum.p.1A2f == total.1A2f)-1)) %>%
  datatable(., caption = 'Table 18: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A2f, dsn="Products/1A2 - Industry/1A2f.gpkg", layer='1A2f')

#'
#'
#'
#'
#'
#' ## 1A2g - Other industries
#'
#+ include = FALSE
source.1A2g <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2g$sources$points <- readxl::read_xlsx(path = source.file, range = "D190:S194", sheet = source.sheet, col_names = header)
source.1A2g$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D202:I202", sheet = source.sheet, col_names = vars)
source.1A2g$total$inventory <- readxl::read_xlsx(path = source.file, range = "D209:I209", sheet = source.sheet, col_names = vars)

sf.1A2g <- corsum2sf(source.1A2g, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2g %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 19: sf.1A2g',
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
sum.1A2g <- sf.1A2g %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A2g <- source.1A2g[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A2g, total.1A2g, data.frame(total.1A2g-sum.1A2g))) %>%
  datatable(., caption = 'Table 20: Summary differences',
            options = list(pageLength = 5)
  )


#+ include = FALSE
clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
clc121 <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Industrial_sites_new.gpkg") %>%
  st_set_crs(32634)
#+ include = FALSE
# Combine all known points into one sf object
points_all <- rbind(sf.1A2a, sf.1A2b, sf.1A2c, sf.1A2d, sf.1A2e, sf.1A2f, sf.1A2g) %>%
  st_transform(crs = "+init=epsg:32634")
clc121.otherIndustries <- st_join(clc121, points_all, join = st_intersects) %>%
  subset(is.na(NOx) | is.na(SO2) | is.na(PM10) | is.na(PM2.5) | is.na(NMVOC) | is.na(NH3))

#+ include = FALSE
# Polygon Centroid
clc121.otherIndustries %<>% dplyr::mutate(Area_Ha = unclass(st_area(.)/10000), SHAPE_Area = unclass(st_area(.)))

clc121.oI.cen <- st_centroid(clc121.otherIndustries) %>%
  select(ID, Area_Ha, SHAPE_Area, NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  rename(ID_CLC = ID)
clc121.oI.cen <- st_join(clc121.oI.cen, sf.grid.5km) %>%
  subset(!is.na(ID))

#+ include = FALSE
# Distribute values based on area of each polygon and add it to centroids
sum_Area <- sum(clc121.oI.cen$SHAPE_Area)
diff.1A2g <- data.frame(total.1A2g - sum.1A2g)
clc121.oI.cen <- clc121.oI.cen %>%
  mutate(NOx = ((diff.1A2g$NOx/sum_Area)*SHAPE_Area),
         SO2 = ((diff.1A2g$SO2/sum_Area)*SHAPE_Area),
         PM10 = ((diff.1A2g$PM10/sum_Area)*SHAPE_Area),
         PM2.5 = ((diff.1A2g$PM2.5/sum_Area)*SHAPE_Area),
         NMVOC = ((diff.1A2g$NMVOC/sum_Area)*SHAPE_Area),
         NH3 = ((diff.1A2g$NH3/sum_Area)*SHAPE_Area))
sf.1A2g %<>% select(vars) 
clc121.oI.cen %<>% select(vars) %>% dplyr::rename(geometry = geom)
sf.1A2g <- rbind(sf.1A2g, clc121.oI.cen)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2g %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 21: sf.1A2g',
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
sum.1A2g <- sf.1A2g %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A2g <- source.1A2g[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A2g, total.1A2g, data.frame(total.1A2g-sum.1A2g))) %>%
  datatable(., caption = 'Table 22: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A2g <- sf.grid.5km %>%
  st_join(sf.1A2g) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A2g, layer.name = "Sources 1A2g", col.regions = "red") + mapview(p.1A2g, layer.name = "Spatialised 1A2g")
spatialised.mapview(sf.sources = sf.1A2g, layer.name.1 = "Sources 1A2g", sf.spatialised = p.1A2g, layer.name.2 = "Spatialised 1A2g", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A2g <- p.1A2g %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A2g, total.1A2g, data.frame(sum.p.1A2g == total.1A2g)-1)) %>%
  datatable(., caption = 'Table 23: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
#st_write(p.1A2g, dsn="Products/1A2 - Industry/1A2g.gpkg", layer='1A2g')

#'
#'
#'
#' ## 1A2g - Auto-production
#'
#+ include = FALSE
# NIJE AUTOMOBILSKA INDUSTRIJA, DATE SU LOKACIJE, ZAMENI U INVENTORY FAJLU ---> ZAMENJENO

source.1A2gvi <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2gvi$sources$points <- readxl::read_xlsx(path = source.file, range = "D210:S240", sheet = source.sheet, col_names = header)
source.1A2gvi$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D253:I253", sheet = source.sheet, col_names = vars)
source.1A2gvi$total$inventory <- readxl::read_xlsx(path = source.file, range = "D258:I258", sheet = source.sheet, col_names = vars)

sf.1A2gvi <- corsum2sf(source.1A2gvi, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")
mapview(sf.1A2gvi)
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2gvi %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 24: sf.1A2g - Auto-production',
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
sum.1A2gvi <- sf.1A2gvi %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A2gvi <- source.1A2gvi[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A2gvi, total.1A2gvi, data.frame(total.1A2gvi-sum.1A2gvi))) %>%
  datatable(., caption = 'Table 25: Summary differences',
            options = list(pageLength = 5)
  )

#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A2gvi <- sf.grid.5km %>%
  st_join(sf.1A2gvi) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A2gvi, layer.name = "Sources 1A2g - Auto-production", col.regions = "red") + mapview(p.1A2gvi, layer.name = "Spatialised 1A2gvi")
spatialised.mapview(sf.sources = sf.1A2gvi, layer.name.1 = "Sources 1A2g - Auto-production", sf.spatialised = p.1A2gvi, layer.name.2 = "Spatialised 1A2gvi", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A2gvi <- p.1A2gvi %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A2gvi, total.1A2gvi, data.frame(sum.p.1A2gvi == total.1A2gvi)-1)) %>%
  datatable(., caption = 'Table 26: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A2gvi, dsn="Products/1A2 - Industry/1A2g-Auto-production.gpkg", layer='1A2g-Auto-production')

#'
#'
#'
#' ## 1A2gvii - Mobile combustion in manufacturing industries and construction
#'
#+ include = FALSE
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
clc133.int <- st_intersection(clc133, sf.grid.5km) %>%
  select(.,vars)

source.1A2gvii$sources$polygon <- clc133.int

sf.1A2gvii <- corsum2sf_polygon(source.1A2gvii, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2gvii %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 27: sf.1A2gvii',
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
sum.1A2gvii <- sf.1A2gvii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A2gvii <- source.1A2gvii[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A2gvii, total.1A2gvii, data.frame(total.1A2gvii-sum.1A2gvii))) %>%
  datatable(., caption = 'Table 28: Summary differences',
            options = list(pageLength = 5)
  )

#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.1A2gvii <- sf.1A2gvii %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.1A2gvii$Area)
diff.1A2gvii <- data.frame(total.1A2gvii - sum.1A2gvii)
sf.1A2gvii <- sf.1A2gvii %>%
  mutate(NOx = ((diff.1A2gvii$NOx/sum_Area)*Area),
         SO2 = ((diff.1A2gvii$SO2/sum_Area)*Area),
         PM10 = ((diff.1A2gvii$PM10/sum_Area)*Area),
         PM2.5 = ((diff.1A2gvii$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.1A2gvii$NMVOC/sum_Area)*Area),
         NH3 = ((diff.1A2gvii$NH3/sum_Area)*Area))
sf.1A2gvii %<>% select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A2gvii <- sf.grid.5km %>%
  st_join(sf.1A2gvii, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.1A2gvii, layer.name = "Spatialised 1A2gvii") + mapview(sf.1A2gvii, layer.name = "Sources 1A2gvii", col.regions = "red") 
spatialised.mapview(sf.sources = sf.1A2gvii, layer.name.1 = "Sources 1A2gvii", sf.spatialised = p.1A2gvii, layer.name.2 = "Spatialised 1A2gvii", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A2gvii <- p.1A2gvii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A2gvii, total.1A2gvii, data.frame(sum.p.1A2gvii == total.1A2gvii)-1)) %>%
  datatable(., caption = 'Table 29: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A2gvii, dsn="Products/1A2 - Industry/1A2gvii.gpkg", layer='1A2gvii')

#'
#'
#+ include = FALSE 
# rmarkdown::render(here::here("/1A2 - Industry.r"), output_dir = here::here("/Reports"))

