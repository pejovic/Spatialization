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
#' # 1A4-Residential-Tertiary
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
library(stringr)
library(s2)
library(magrittr)
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
#+ include = FALSE, echo = FALSE
# Parameters:
#  - distribute - TRUE or FALSE, depend on that if someone want to distribute total inventory emissions to sources, when difference exists
#+ include = FALSE, echo = FALSE
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
#+ include = FALSE, echo = FALSE   
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

#+ include = FALSE, echo = FALSE
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
source.file = "D:/R_projects/Spatialization/Pollutant inventory spatialized-d-v3.xlsx"
source.sheet =  "1A4-Residential-Tertiary"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("D:/R_projects/Spatialization/Grid/Grid_Serbia_0.05deg.gpkg")
sf.grid.5km <- st_as_sf(grid.5km)
sf.grid.5km %<>% dplyr::mutate(ID = id)



#'
#'
#' ## 1A4bi - Residential: Stationary combustion
#'
#'
#'
#+ include = TRUE
source.1A4bi <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A4bi$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D32:I32", sheet = source.sheet, col_names = vars)
source.1A4bi$total$inventory <- readxl::read_xlsx(path = source.file, range = "D37:I37", sheet = source.sheet, col_names = vars)


#+ include = TRUE
clc_18 <- readOGR("D:/R_projects/Spatialization/Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)

sf_clc18_urb <- subset(sf_clc18, CODE_18 == "111" | CODE_18 == "112") %>% # CLC urban zones
  st_transform(crs = "+init=epsg:32634")

#+ include = TRUE
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("D:/R_projects/Spatialization/Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = TRUE
toplane <- readxl::read_xls(path = "D:/R_projects/Spatialization/Data/toplane/Toplane_2015.xls") %>%
  mutate(GRAD = str_to_title(GRAD))

sf_opstine$SDG <- toplane$`Broj domacinstava prikljucenih na SDG`[match(sf_opstine$NAME_2, toplane$GRAD)] 

#+ include = TRUE
domacinstva <- readxl::read_xls(path = "D:/R_projects/Spatialization/Data/Domacinstva_2015.xls", sheet = "Stanovnistvo8")
lcl(loc = "C")
domacinstva <- cyr_lat(domacinstva)
names(domacinstva) <- cyr_lat(names(domacinstva)) 
domacinstva <- domacinstva %>%
  mutate_all(~replace_na(., 0))
domacinstva$Opština[domacinstva$Opština == "Indjija"] <- "Inđija"
domacinstva$Opština[domacinstva$Opština == "LJubovija"] <- "Ljubovija"
domacinstva$Opština[domacinstva$Opština == "Mali Idjoš"] <- "Mali Iđoš"
domacinstva$Opština[domacinstva$Opština == "Savski venac"] <- "Savski Venac"
domacinstva$Opština[domacinstva$Opština == "Stari grad"] <- "Stari Grad"
domacinstva$Opština[domacinstva$Opština == "Petrovac na Mlavi"] <- "Petrovac"
domacinstva$Opština[domacinstva$Opština == "Arandjelovac"] <- "Aranđelovac"
domacinstva$Opština[domacinstva$Opština == "LJig"] <- "Ljig"
domacinstva$Opština[domacinstva$Opština == "Žitoradja"] <- "Žitorađa"
domacinstva$Opština[domacinstva$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_domacinstva <- domacinstva$Ukupno_domaćinstva[match(sf_opstine$NAME_2, domacinstva$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(Br_domacinstva_SDG = Br_domacinstva - SDG)

sf_clc18_urb <- st_join(sf_clc18_urb, sf_opstine, largest = TRUE) 

sf_clc18_urb %<>% dplyr::select(.,Br_domacinstva_SDG, NAME_2)
sf_clc18_urb[,vars] <- NA
sf_clc18_urb %<>% sf::st_transform(4326)
sf_clc18_urb.int <- st_intersection(sf_clc18_urb, sf.grid.5km) %>% 
  filter(!is.na(Br_domacinstva_SDG))

source.1A4bi$sources$polygon <- sf_clc18_urb.int

sf.1A4bi <- corsum2sf_polygon(source.1A4bi, distribute = FALSE) 

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A4bi <- sf.1A4bi %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A4bi <- source.1A4bi[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A4bi, total.1A4bi, data.frame(total.1A4bi - sum.1A4bi))) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = TRUE, result = FALSE

sum_s <- sum(sf.1A4bi$Br_domacinstva_SDG)
diff.1A4bi <- data.frame(total.1A4bi - sum.1A4bi)
sf.1A4bi <- sf.1A4bi %>%
  mutate(NOx = ((diff.1A4bi$NOx/sum_s)*Br_domacinstva_SDG),
         SO2 = ((diff.1A4bi$SO2/sum_s)*Br_domacinstva_SDG),
         PM10 = ((diff.1A4bi$PM10/sum_s)*Br_domacinstva_SDG),
         PM2.5 = ((diff.1A4bi$PM2.5/sum_s)*Br_domacinstva_SDG),
         NMVOC = ((diff.1A4bi$NMVOC/sum_s)*Br_domacinstva_SDG),
         NH3 = ((diff.1A4bi$NH3/sum_s)*Br_domacinstva_SDG))
sf.1A4bi %<>% dplyr::select(vars)
#'
#'
#+ include = TRUE, result = FALSE
p.1A4bi <- sf.grid.5km %>%
  st_join(sf.1A4bi, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A4bi, layer.name.1 = "Sources 1A4bi", sf.spatialised = p.1A4bi, layer.name.2 = "Spatialised 1A4bi", vars = vars)

#+ include = TRUE, result = TRUE, eval = TRUE
sum.p.1A4bi <- p.1A4bi %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A4bi, total.1A4bi, data.frame(sum.p.1A4bi == total.1A4bi)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

