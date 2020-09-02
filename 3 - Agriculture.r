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
#' # 3-Agriculture
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
source.file = "Pollutant inventory spatialized-d-v3.xlsx"
source.sheet =  "3-Agriculture"
header <- readxl::read_xlsx(path = source.file, range = "D7:S7", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Grid_Serbia_0.05deg.gpkg")
sf.grid.5km <- st_as_sf(grid.5km)
sf.grid.5km %<>% dplyr::mutate(ID = id)


#'
#'
#' ## 3B3-Manure management - Swine
#' 
#+ include = FALSE
source.3B3 <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3B3$sources$points <- readxl::read_xlsx(path = source.file, range = "D8:S56", sheet = source.sheet, col_names = header)
source.3B3$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D63:I63", sheet = source.sheet, col_names = vars)
source.3B3$total$inventory <- readxl::read_xlsx(path = source.file, range = "D64:I64", sheet = source.sheet, col_names = vars)

sf.3B3 <- corsum2sf(source.3B3, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")
# mapview(sf.3B3) + mapview(sf.grid.5km)



#+ include = FALSE
# Sys.setlocale(locale = 'Serbian (Latin)')
# opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
#                    use_iconv=TRUE,  
#                    encoding = "UTF-8")
# sf_opstine <- st_as_sf(opstine)
# 
# #+ include = FALSE
# svinje <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
# lcl(loc = "C")
# svinje <- cyr_lat(svinje)
# names(svinje) <- cyr_lat(names(svinje)) 
# svinje <- svinje %>%
#   mutate_all(~replace_na(., 0))
# svinje$Opština[svinje$Opština == "Indjija"] <- "Inđija"
# svinje$Opština[svinje$Opština == "LJubovija"] <- "Ljubovija"
# svinje$Opština[svinje$Opština == "Mali Idjoš"] <- "Mali Iđoš"
# svinje$Opština[svinje$Opština == "Savski venac"] <- "Savski Venac"
# svinje$Opština[svinje$Opština == "Stari grad"] <- "Stari Grad"
# svinje$Opština[svinje$Opština == "Petrovac na Mlavi"] <- "Petrovac"
# svinje$Opština[svinje$Opština == "Arandjelovac"] <- "Aranđelovac"
# svinje$Opština[svinje$Opština == "LJig"] <- "Ljig"
# svinje$Opština[svinje$Opština == "Žitoradja"] <- "Žitorađa"
# svinje$Opština[svinje$Opština == "Medvedja"] <- "Medveđa"
# 
# sf_opstine$Br_svinje <- svinje$Svinje[match(sf_opstine$NAME_2, svinje$Opština)]
# 
# sf_opstine %<>% 
#   st_transform(crs = "+init=epsg:32634")
# 
# # sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)
# sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")
# 
# sf_rural <- st_join(sf_rur, sf_opstine, largest = TRUE) 
# 
# sf_rural %<>% dplyr::select(.,Br_svinje, NAME_2.y) %>% 
#   filter(!is.na(Br_svinje))
# sf_rural[,vars] <- NA
# 
# sf_rur.int <- st_intersection(sf_rural, sf.grid.5km) %>% 
#   filter(!is.na(Br_svinje))
# 
# sum.3B3 <- sf.3B3 %>% 
#   st_drop_geometry() %>%
#   dplyr::select(., vars) %>% 
#   apply(., 2, sum) %>% 
#   t(.) %>% 
#   as.data.frame() %>%
#   dplyr::mutate_if(is.numeric, round, 2)
# total.3B3 <- source.3B3[[2]][[2]][, vars] %>% 
#   mutate_all(~replace(., is.na(.), 0)) %>%
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   as.data.frame()
# 
# sum_s <- sum(sf_rur.int$Br_svinje)
# diff.3B3 <- data.frame(total.3B3 - sum.3B3)
# sf_rur.int <- sf_rur.int %>%
#   mutate(NOx = ((diff.3B3$NOx/sum_s)*Br_svinje),
#          SO2 = ((diff.3B3$SO2/sum_s)*Br_svinje),
#          PM10 = ((diff.3B3$PM10/sum_s)*Br_svinje),
#          PM2.5 = ((diff.3B3$PM2.5/sum_s)*Br_svinje),
#          NMVOC = ((diff.3B3$NMVOC/sum_s)*Br_svinje),
#          NH3 = ((diff.3B3$NH3/sum_s)*Br_svinje))
# sf_rur.int %<>% dplyr::select(vars)
# 
# sf_rur.int$ID <- dplyr::row_number(sf_rur.int$NOx)
# 
# sf_rur.int <- sf_rur.int %>%
#   st_join(sf.3B3) %>%
#   dplyr::mutate_all(~replace(., is.na(.), 0)) %>%
#   group_by(ID.x) %>%
#   dplyr::mutate(NOx = (NOx.x + NOx.y),
#             SO2 = (SO2.x + SO2.y),
#             PM10 = (PM10.x + PM10.y),
#             PM2.5 = (PM2.5.x + PM2.5.y),
#             NMVOC = (NMVOC.x + NMVOC.y),
#             NH3 = (NH3.x + NH3.y)) %>% 
#   dplyr::mutate(ID = as.numeric(ID.x)) %>% 
#   dplyr::select(vars, ID) %>%
#   ungroup()
# 
# 
# 
# source.3B3$sources$points <- NA
# source.3B3$sources$polygon <- sf_rur.int
# 
# sf.3B3 <- corsum2sf_polygon(source.3B3, distribute = TRUE) %>%
#   st_transform(crs = "+init=epsg:32634")
# 
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.3B3 %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.3B3',
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
sum.3B3 <- sf.3B3 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3B3 <- source.3B3[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3B3, total.3B3, data.frame(total.3B3 - sum.3B3))) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Iskoristi te vrednosti da ti budu tezine!!!!!!!!!!!!!!!!
 sum.NOx <- sum(sf.3B3$NOx)
 sum.SO2 <- sum(sf.3B3$SO2)
 sum.PM10 <- sum(sf.3B3$PM10)
 sum.PM2.5 <- sum(sf.3B3$PM2.5)
 sum.NMVOC <- sum(sf.3B3$NMVOC)
 sum.NH3 <- sum(sf.3B3$NH3)
 
 vars_1 <- sf.3B3[, vars] %>%
   st_drop_geometry()
 sf.3B3[,vars] <- NA
 
 sf.3B3 %<>% 
   mutate(
     NOx = (sum.3B3$NOx/sum.NOx)*vars_1$NOx, # !!!!!!!!!!!!!!!!!!!!!!!
     SO2 = (total.3B3$SO2/sum.SO2)*vars_1$SO2,
     PM10 = (total.3B3$PM10/sum.PM10)*vars_1$PM10,
     PM2.5 = (total.3B3$PM2.5/sum.PM2.5)*vars_1$PM2.5,
     NMVOC = (total.3B3$NMVOC/sum.NMVOC)*vars_1$NMVOC,
     NH3 = (sum.3B3$NH3/sum.NH3)*vars_1$NH3) %>% # !!!!!!!!!!!!!!!!!!!!!!!
   mutate_all(~replace(., is.na(.), 0)) 

sf.3B3 %<>%
  dplyr::select(-ID)

#+ include = FALSE, echo = FALSE, result = FALSE
p.3B3 <- sf.grid.5km %>%
  st_join(sf.3B3) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID)) %>%
  dplyr::ungroup()

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3B3, layer.name.1 = "Sources 3B3", sf.spatialised = p.3B3, layer.name.2 = "Spatialised 3B3", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B3 <- p.3B3 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B3, total.3B3, data.frame(sum.p.3B3 == total.3B3)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )


### OSTATAK

Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                  use_iconv=TRUE,  
                  encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
svinje <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
svinje <- cyr_lat(svinje)
names(svinje) <- cyr_lat(names(svinje)) 
svinje <- svinje %>%
 mutate_all(~replace_na(., 0))
svinje$Opština[svinje$Opština == "Indjija"] <- "Inđija"
svinje$Opština[svinje$Opština == "LJubovija"] <- "Ljubovija"
svinje$Opština[svinje$Opština == "Mali Idjoš"] <- "Mali Iđoš"
svinje$Opština[svinje$Opština == "Savski venac"] <- "Savski Venac"
svinje$Opština[svinje$Opština == "Stari grad"] <- "Stari Grad"
svinje$Opština[svinje$Opština == "Petrovac na Mlavi"] <- "Petrovac"
svinje$Opština[svinje$Opština == "Arandjelovac"] <- "Aranđelovac"
svinje$Opština[svinje$Opština == "LJig"] <- "Ljig"
svinje$Opština[svinje$Opština == "Žitoradja"] <- "Žitorađa"
svinje$Opština[svinje$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_svinje <- svinje$Svinje[match(sf_opstine$NAME_2, svinje$Opština)]

sf_opstine %<>% 
 st_transform(crs = "+init=epsg:32634")

# sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)
sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")

sf_rural <- st_join(sf_rur, sf_opstine, largest = TRUE) 

sf_rural %<>% dplyr::select(.,Br_svinje, NAME_2) %>% 
 filter(!is.na(Br_svinje))
sf_rural[,vars] <- NA
sf_rural %<>% st_transform(4326)
sf_rur.int <- st_intersection(sf_rural, sf.grid.5km) %>% 
 filter(!is.na(Br_svinje))

# POZVATI PONOVO SF SA TACKAMA
sum.3B3 <- sf.3B3 %>% 
 st_drop_geometry() %>%
 dplyr::select(., vars) %>% 
 apply(., 2, sum) %>% 
 t(.) %>% 
 as.data.frame() %>%
 dplyr::mutate_if(is.numeric, round, 2)
total.3B3 <- source.3B3[[2]][[2]][, vars] %>% 
 mutate_all(~replace(., is.na(.), 0)) %>%
 dplyr::mutate_if(is.numeric, round, 2) %>%
 as.data.frame()

sum_s <- sum(sf_rur.int$Br_svinje)
diff.3B3 <- data.frame(total.3B3 - sum.3B3)
sf_rur.int <- sf_rur.int %>%
 mutate(NOx = ((diff.3B3$NOx/sum_s)*Br_svinje),
        SO2 = ((diff.3B3$SO2/sum_s)*Br_svinje),
        PM10 = ((diff.3B3$PM10/sum_s)*Br_svinje),
        PM2.5 = ((diff.3B3$PM2.5/sum_s)*Br_svinje),
        NMVOC = ((diff.3B3$NMVOC/sum_s)*Br_svinje),
        NH3 = ((diff.3B3$NH3/sum_s)*Br_svinje))
sf_rur.int %<>% dplyr::select(vars)

sf_rur.int$ID <- dplyr::row_number(sf_rur.int$NOx)

# sf_rur.int <- sf_rur.int %>%
#  st_join(sf.3B3) %>%
#  dplyr::mutate_all(~replace(., is.na(.), 0)) %>%
#  group_by(ID) %>%
#  dplyr::mutate(NOx = (NOx.x + NOx.y),
#            SO2 = (SO2.x + SO2.y),
#            PM10 = (PM10.x + PM10.y),
#            PM2.5 = (PM2.5.x + PM2.5.y),
#            NMVOC = (NMVOC.x + NMVOC.y),
#            NH3 = (NH3.x + NH3.y)) %>% 
#  #dplyr::mutate(ID = as.numeric(ID)) %>% 
#  dplyr::select(vars, ID) %>%
#  ungroup()



source.3B3$sources$points <- NA
source.3B3$sources$polygon <- sf_rur.int

sf.3B3 <- corsum2sf_polygon(source.3B3, distribute = FALSE) #%>%
 #st_transform(crs = "+init=epsg:32634")


sf.3B3 %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.3B3',
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
sum.3B3 <- sf.3B3 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3B3 <- source.3B3[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3B3, total.3B3, data.frame(total.3B3 - sum.3B3))) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )


#+ include = FALSE, echo = FALSE, result = FALSE
p.3B3.1 <- sf.grid.5km %>%
  st_join(sf.3B3, join = st_contains) %>%
  group_by(ID.x) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID.x)) %>%
  dplyr::ungroup()

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3B3, layer.name.1 = "Sources 3B3", sf.spatialised = p.3B3.1, layer.name.2 = "Spatialised 3B3", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B3 <- p.3B3.1 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B3, total.3B3, data.frame(sum.p.3B3 == total.3B3)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

p.3B3$ID
p.3B3.1$ID

p.3B3 %<>% dplyr::mutate(NOx = NOx + p.3B3.1$NOx,
                        SO2 = SO2 + p.3B3.1$SO2,
                        PM10 =PM10 + p.3B3.1$PM10, 
                        PM2.5 = PM2.5 + p.3B3.1$PM2.5,
                        NMVOC = NMVOC + p.3B3.1$NMVOC,
                        NH3 = NH3 + p.3B3.1$NH3)

sum.p.3B3 <- p.3B3 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B3, total.3B3, data.frame(sum.p.3B3 == total.3B3)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )




#+ include = FALSE
# st_write(p.3B3, dsn="Products/3 - Agriculture/3B3.gpkg", layer='3B3')

#'
#'
#' ## 3B4gi & 3B4gii-Laying hens & Broilers
#' 
#+ include = FALSE
source.3B4gi_gii <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3B4gi_gii$sources$points <- readxl::read_xlsx(path = source.file, range = "D65:S110", sheet = source.sheet, col_names = header)
source.3B4gi_gii$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D117:I117", sheet = source.sheet, col_names = vars)
source.3B4gi_gii$total$inventory <- readxl::read_xlsx(path = source.file, range = "D118:I118", sheet = source.sheet, col_names = vars)

sf.3B4gi_gii <- corsum2sf(source.3B4gi_gii, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")






#+ include = FALSE
# Sys.setlocale(locale = 'Serbian (Latin)')
# opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
#                    use_iconv=TRUE,  
#                    encoding = "UTF-8")
# sf_opstine <- st_as_sf(opstine)
# 
# #+ include = FALSE
# zivina <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
# lcl(loc = "C")
# zivina <- cyr_lat(zivina)
# names(zivina) <- cyr_lat(names(zivina)) 
# zivina <- zivina %>%
#   mutate_all(~replace_na(., 0))
# zivina$Opština[zivina$Opština == "Indjija"] <- "Inđija"
# zivina$Opština[zivina$Opština == "LJubovija"] <- "Ljubovija"
# zivina$Opština[zivina$Opština == "Mali Idjoš"] <- "Mali Iđoš"
# zivina$Opština[zivina$Opština == "Savski venac"] <- "Savski Venac"
# zivina$Opština[zivina$Opština == "Stari grad"] <- "Stari Grad"
# zivina$Opština[zivina$Opština == "Petrovac na Mlavi"] <- "Petrovac"
# zivina$Opština[zivina$Opština == "Arandjelovac"] <- "Aranđelovac"
# zivina$Opština[zivina$Opština == "LJig"] <- "Ljig"
# zivina$Opština[zivina$Opština == "Žitoradja"] <- "Žitorađa"
# zivina$Opština[zivina$Opština == "Medvedja"] <- "Medveđa"
# 
# sf_opstine$Br_zivina <- zivina$Živina[match(sf_opstine$NAME_2, zivina$Opština)]
# 
# sf_opstine %<>% 
#   st_transform(crs = "+init=epsg:32634")
# 
# # sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)
# sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")
# 
# sf_rural <- st_join(sf_rur, sf_opstine, largest = TRUE) 
# 
# sf_rural %<>% dplyr::select(.,Br_zivina, NAME_2.y) %>% 
#   filter(!is.na(Br_zivina))
# sf_rural[,vars] <- NA
# 
# sf_rur.int <- st_intersection(sf_rural, sf.grid.5km) %>% 
#   filter(!is.na(Br_zivina))
# 
# sum.3B4gi_gii <- sf.3B4gi_gii %>% 
#   st_drop_geometry() %>%
#   dplyr::select(., vars) %>% 
#   apply(., 2, sum) %>% 
#   t(.) %>% 
#   as.data.frame() %>%
#   dplyr::mutate_if(is.numeric, round, 2)
# total.3B4gi_gii <- source.3B4gi_gii[[2]][[2]][, vars] %>% 
#   mutate_all(~replace(., is.na(.), 0)) %>%
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   as.data.frame()
# 
# sum_s <- sum(sf_rur.int$Br_zivina)
# diff.3B4gi_gii <- data.frame(total.3B4gi_gii - sum.3B4gi_gii)
# sf_rur.int <- sf_rur.int %>%
#   mutate(NOx = ((diff.3B4gi_gii$NOx/sum_s)*Br_zivina),
#          SO2 = ((diff.3B4gi_gii$SO2/sum_s)*Br_zivina),
#          PM10 = ((diff.3B4gi_gii$PM10/sum_s)*Br_zivina),
#          PM2.5 = ((diff.3B4gi_gii$PM2.5/sum_s)*Br_zivina),
#          NMVOC = ((diff.3B4gi_gii$NMVOC/sum_s)*Br_zivina),
#          NH3 = ((diff.3B4gi_gii$NH3/sum_s)*Br_zivina))
# sf_rur.int %<>% select(vars)
# 
# sf_rur.int$ID <- dplyr::row_number(sf_rur.int$NOx)
# 
# sf_rur.int <- sf_rur.int %>%
#   st_join(sf.3B4gi_gii) %>%
#   dplyr::mutate_all(~replace(., is.na(.), 0)) %>%
#   group_by(ID) %>%
#   dplyr::mutate(NOx = (NOx.x + NOx.y),
#                 SO2 = (SO2.x + SO2.y),
#                 PM10 = (PM10.x + PM10.y),
#                 PM2.5 = (PM2.5.x + PM2.5.y),
#                 NMVOC = (NMVOC.x + NMVOC.y),
#                 NH3 = (NH3.x + NH3.y)) %>% 
#   #dplyr::mutate(ID = as.numeric(ID)) %>% 
#   dplyr::select(vars, ID) %>%
#   ungroup()
# 
# 
# 
# source.3B4gi_gii$sources$points <- NA
# source.3B4gi_gii$sources$polygon <- sf_rur.int
# 
# sf.3B4gi_gii <- corsum2sf_polygon(source.3B4gi_gii, distribute = TRUE) %>%
#   st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.3B4gi_gii %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.3B4gi_gii',
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
sum.3B4gi_gii <- sf.3B4gi_gii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3B4gi_gii <- source.3B4gi_gii[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3B4gi_gii, total.3B4gi_gii, data.frame(total.3B4gi_gii - sum.3B4gi_gii))) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Iskoristi te vrednosti da ti budu tezine!!!!!!!!!!!!!!!!
# sum.NOx <- sum(sf.3B4gi_gii$NOx)
# sum.SO2 <- sum(sf.3B4gi_gii$SO2)
# sum.PM10 <- sum(sf.3B4gi_gii$PM10)
# sum.PM2.5 <- sum(sf.3B4gi_gii$PM2.5)
# sum.NMVOC <- sum(sf.3B4gi_gii$NMVOC)
# sum.NH3 <- sum(sf.3B4gi_gii$NH3)
# 
# vars_1 <- sf.3B4gi_gii[, vars] %>%
#   st_drop_geometry()
# sf.3B4gi_gii[,vars] <- NA
# 
# sf.3B4gi_gii %<>% 
#   mutate(
#     NOx = (total.3B4gi_gii$NOx/sum.NOx)*vars_1$NOx,
#     SO2 = (total.3B4gi_gii$SO2/sum.SO2)*vars_1$SO2,
#     PM10 = (total.3B4gi_gii$PM10/sum.PM10)*vars_1$PM10,
#     PM2.5 = (total.3B4gi_gii$PM2.5/sum.PM2.5)*vars_1$PM2.5,
#     NMVOC = (total.3B4gi_gii$NMVOC/sum.NMVOC)*vars_1$NMVOC,
#     NH3 = (total.3B4gi_gii$NH3/sum.NH3)*vars_1$NH3) %>%
#   mutate_all(~replace(., is.na(.), 0)) 

#sf.3B4gi_gii %<>%
#  dplyr::select(-ID)

#+ include = FALSE, echo = FALSE, result = FALSE
p.3B4gi_gii <- sf.grid.5km %>%
  st_join(sf.3B4gi_gii, join = st_contains) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID)) %>%
  dplyr::ungroup()

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3B4gi_gii, layer.name.1 = "Sources 3B4gi_gii", sf.spatialised = p.3B4gi_gii, layer.name.2 = "Spatialised 3B4gi_gii", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B4gi_gii <- p.3B4gi_gii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B4gi_gii, total.3B4gi_gii, data.frame(sum.p.3B4gi_gii == total.3B4gi_gii)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )


### OSTATAK

Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                  use_iconv=TRUE,  
                  encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
zivina <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
zivina <- cyr_lat(zivina)
names(zivina) <- cyr_lat(names(zivina)) 
zivina <- zivina %>%
 mutate_all(~replace_na(., 0))
zivina$Opština[zivina$Opština == "Indjija"] <- "Inđija"
zivina$Opština[zivina$Opština == "LJubovija"] <- "Ljubovija"
zivina$Opština[zivina$Opština == "Mali Idjoš"] <- "Mali Iđoš"
zivina$Opština[zivina$Opština == "Savski venac"] <- "Savski Venac"
zivina$Opština[zivina$Opština == "Stari grad"] <- "Stari Grad"
zivina$Opština[zivina$Opština == "Petrovac na Mlavi"] <- "Petrovac"
zivina$Opština[zivina$Opština == "Arandjelovac"] <- "Aranđelovac"
zivina$Opština[zivina$Opština == "LJig"] <- "Ljig"
zivina$Opština[zivina$Opština == "Žitoradja"] <- "Žitorađa"
zivina$Opština[zivina$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_zivina <- zivina$Živina[match(sf_opstine$NAME_2, zivina$Opština)]

sf_opstine %<>% 
 st_transform(crs = "+init=epsg:32634")

# sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)
sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")

sf_rural <- st_join(sf_rur, sf_opstine, largest = TRUE) 

sf_rural %<>% dplyr::select(.,Br_zivina, NAME_2) %>% 
 filter(!is.na(Br_zivina))
sf_rural[,vars] <- NA
sf_rural %<>% st_transform(4326)
sf_rur.int <- st_intersection(sf_rural, sf.grid.5km) %>% 
 filter(!is.na(Br_zivina))

sum.3B4gi_gii <- sf.3B4gi_gii %>% 
 st_drop_geometry() %>%
 dplyr::select(., vars) %>% 
 apply(., 2, sum) %>% 
 t(.) %>% 
 as.data.frame() %>%
 dplyr::mutate_if(is.numeric, round, 2)
total.3B4gi_gii <- source.3B4gi_gii[[2]][[2]][, vars] %>% 
 mutate_all(~replace(., is.na(.), 0)) %>%
 dplyr::mutate_if(is.numeric, round, 2) %>%
 as.data.frame()

sum_s <- sum(sf_rur.int$Br_zivina)
diff.3B4gi_gii <- data.frame(total.3B4gi_gii - sum.3B4gi_gii)
sf_rur.int <- sf_rur.int %>%
 mutate(NOx = ((diff.3B4gi_gii$NOx/sum_s)*Br_zivina),
        SO2 = ((diff.3B4gi_gii$SO2/sum_s)*Br_zivina),
        PM10 = ((diff.3B4gi_gii$PM10/sum_s)*Br_zivina),
        PM2.5 = ((diff.3B4gi_gii$PM2.5/sum_s)*Br_zivina),
        NMVOC = ((diff.3B4gi_gii$NMVOC/sum_s)*Br_zivina),
        NH3 = ((diff.3B4gi_gii$NH3/sum_s)*Br_zivina))
sf_rur.int %<>% dplyr::select(vars)

sf_rur.int$ID <- dplyr::row_number(sf_rur.int$NOx)

# sf_rur.int <- sf_rur.int %>%
#  st_join(sf.3B4gi_gii) %>%
#  dplyr::mutate_all(~replace(., is.na(.), 0)) %>%
#  group_by(ID) %>%
#  dplyr::mutate(NOx = (NOx.x + NOx.y),
#                SO2 = (SO2.x + SO2.y),
#                PM10 = (PM10.x + PM10.y),
#                PM2.5 = (PM2.5.x + PM2.5.y),
#                NMVOC = (NMVOC.x + NMVOC.y),
#                NH3 = (NH3.x + NH3.y)) %>% 
#  #dplyr::mutate(ID = as.numeric(ID)) %>% 
#  dplyr::select(vars, ID) %>%
#  ungroup()



source.3B4gi_gii$sources$points <- NA
source.3B4gi_gii$sources$polygon <- sf_rur.int

sf.3B4gi_gii <- corsum2sf_polygon(source.3B4gi_gii, distribute = FALSE) #%>%
 #st_transform(crs = "+init=epsg:32634")



p.3B4gi_gii.1 <- sf.grid.5km %>%
  st_join(sf.3B4gi_gii, join = st_contains) %>%
  group_by(ID.x) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID.x)) %>%
  dplyr::ungroup()


#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B4gi_gii <- p.3B4gi_gii.1 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B4gi_gii, total.3B4gi_gii, data.frame(sum.p.3B4gi_gii == total.3B4gi_gii)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )



p.3B4gi_gii.1$ID
p.3B4gi_gii$ID

p.3B4gi_gii %<>% dplyr::mutate(NOx = NOx + p.3B4gi_gii.1$NOx,
                         SO2 = SO2 + p.3B4gi_gii.1$SO2,
                         PM10 =PM10 + p.3B4gi_gii.1$PM10, 
                         PM2.5 = PM2.5 + p.3B4gi_gii.1$PM2.5,
                         NMVOC = NMVOC + p.3B4gi_gii.1$NMVOC,
                         NH3 = NH3 + p.3B4gi_gii.1$NH3)


sum.p.3B4gi_gii <- p.3B4gi_gii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B4gi_gii, total.3B4gi_gii, data.frame(sum.p.3B4gi_gii == total.3B4gi_gii)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )


#+ include = FALSE
# st_write(p.3B4gi_gii, dsn="Products/3 - Agriculture/3B4gi_gii.gpkg", layer='3B4gi_gii')

#'
#'
#' ## 3B1a-Dairy cattle
#'
#'
#'
#+ include = FALSE
source.3B1a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3B1a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3B1a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D120:I120", sheet = source.sheet, col_names = vars)

#+ include = FALSE
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
goveda <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
goveda <- cyr_lat(goveda)
names(goveda) <- cyr_lat(names(goveda)) 
goveda <- goveda %>%
  mutate_all(~replace_na(., 0))
goveda$Opština[goveda$Opština == "Indjija"] <- "Inđija"
goveda$Opština[goveda$Opština == "LJubovija"] <- "Ljubovija"
goveda$Opština[goveda$Opština == "Mali Idjoš"] <- "Mali Iđoš"
goveda$Opština[goveda$Opština == "Savski venac"] <- "Savski Venac"
goveda$Opština[goveda$Opština == "Stari grad"] <- "Stari Grad"
goveda$Opština[goveda$Opština == "Petrovac na Mlavi"] <- "Petrovac"
goveda$Opština[goveda$Opština == "Arandjelovac"] <- "Aranđelovac"
goveda$Opština[goveda$Opština == "LJig"] <- "Ljig"
goveda$Opština[goveda$Opština == "Žitoradja"] <- "Žitorađa"
goveda$Opština[goveda$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_goveda <- goveda$Goveda[match(sf_opstine$NAME_2, goveda$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")

# sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)
sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")

sf_rural <- st_join(sf_rur, sf_opstine, largest = TRUE) 

sf_rural %<>% dplyr::select(.,Br_goveda, NAME_2) %>% 
  filter(!is.na(Br_goveda))
sf_rural[,vars] <- NA
sf_rural %<>% st_transform(4326)
sf_rur.int <- st_intersection(sf_rural, sf.grid.5km) %>% 
  filter(!is.na(Br_goveda))

source.3B1a$sources$polygon <- sf_rur.int

sf.3B1a <- corsum2sf_polygon(source.3B1a, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.3B1a %>% 
  st_drop_geometry() %>% 
  rename(Dairy_catle = Br_goveda, Municipality = NAME_2.y) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.3B1a',
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
sum.3B1a <- sf.3B1a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3B1a <- source.3B1a[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3B1a, total.3B1a, data.frame(total.3B1a - sum.3B1a))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sum_s <- sum(sf.3B1a$Br_goveda)
diff.3B1a <- data.frame(total.3B1a - sum.3B1a)
sf.3B1a <- sf.3B1a %>%
  mutate(NOx = ((diff.3B1a$NOx/sum_s)*Br_goveda),
         SO2 = ((diff.3B1a$SO2/sum_s)*Br_goveda),
         PM10 = ((diff.3B1a$PM10/sum_s)*Br_goveda),
         PM2.5 = ((diff.3B1a$PM2.5/sum_s)*Br_goveda),
         NMVOC = ((diff.3B1a$NMVOC/sum_s)*Br_goveda),
         NH3 = ((diff.3B1a$NH3/sum_s)*Br_goveda))
sf.3B1a %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3B1a <- sf.grid.5km %>%
  st_join(sf.3B1a, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3B1a, layer.name.1 = "Sources 3B1a", sf.spatialised = p.3B1a, layer.name.2 = "Spatialised 3B1a", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B1a <- p.3B1a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B1a, total.3B1a, data.frame(sum.p.3B1a == total.3B1a)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3B1a, dsn="Products/3 - Agriculture/3B1a.gpkg", layer='3B1a')


#'
#'
#' ## 3B1b-Non-dairy cattle 
#'
#'
#'
#+ include = FALSE
source.3B1b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3B1b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3B1b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D121:I121", sheet = source.sheet, col_names = vars)

#+ include = FALSE
source.3B1b$sources$polygon <- sf_rur.int

sf.3B1b <- corsum2sf_polygon(source.3B1b, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.3B1b %>% 
  st_drop_geometry() %>% 
  rename(Dairy_catle = Br_goveda, Municipality = NAME_2.y) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 10: sf.3B1b',
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
sum.3B1b <- sf.3B1b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3B1b <- source.3B1b[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3B1b, total.3B1b, data.frame(total.3B1b - sum.3B1b))) %>%
  datatable(., caption = 'Table 11: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sum_s <- sum(sf.3B1b$Br_goveda)
diff.3B1b <- data.frame(total.3B1b - sum.3B1b)
sf.3B1b <- sf.3B1b %>%
  mutate(NOx = ((diff.3B1b$NOx/sum_s)*Br_goveda),
         SO2 = ((diff.3B1b$SO2/sum_s)*Br_goveda),
         PM10 = ((diff.3B1b$PM10/sum_s)*Br_goveda),
         PM2.5 = ((diff.3B1b$PM2.5/sum_s)*Br_goveda),
         NMVOC = ((diff.3B1b$NMVOC/sum_s)*Br_goveda),
         NH3 = ((diff.3B1b$NH3/sum_s)*Br_goveda))
sf.3B1b %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3B1b <- sf.grid.5km %>%
  st_join(sf.3B1b, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3B1b, layer.name.1 = "Sources 3B1b", sf.spatialised = p.3B1b, layer.name.2 = "Spatialised 3B1b", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B1b <- p.3B1b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B1b, total.3B1b, data.frame(sum.p.3B1b == total.3B1b)-1)) %>%
  datatable(., caption = 'Table 12: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3B1b, dsn="Products/3 - Agriculture/3B1b.gpkg", layer='3B1b')


#'
#'
#' ## 3B2-Sheep 
#'
#'
#'
#+ include = FALSE
source.3B2 <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3B2$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3B2$total$inventory <- readxl::read_xlsx(path = source.file, range = "D122:I122", sheet = source.sheet, col_names = vars)

#+ include = FALSE
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
ovce <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
ovce <- cyr_lat(ovce)
names(ovce) <- cyr_lat(names(ovce)) 
ovce <- ovce %>%
  mutate_all(~replace_na(., 0))
ovce$Opština[ovce$Opština == "Indjija"] <- "Inđija"
ovce$Opština[ovce$Opština == "LJubovija"] <- "Ljubovija"
ovce$Opština[ovce$Opština == "Mali Idjoš"] <- "Mali Iđoš"
ovce$Opština[ovce$Opština == "Savski venac"] <- "Savski Venac"
ovce$Opština[ovce$Opština == "Stari grad"] <- "Stari Grad"
ovce$Opština[ovce$Opština == "Petrovac na Mlavi"] <- "Petrovac"
ovce$Opština[ovce$Opština == "Arandjelovac"] <- "Aranđelovac"
ovce$Opština[ovce$Opština == "LJig"] <- "Ljig"
ovce$Opština[ovce$Opština == "Žitoradja"] <- "Žitorađa"
ovce$Opština[ovce$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_ovce <- ovce$Ovce[match(sf_opstine$NAME_2, ovce$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")

# sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)

#+ include = FALSE
clc_18 <- readOGR("Data/clc/CLC18_RS.shp") # Reading data
sf_clc18 <- st_as_sf(clc_18)

sf_clc18_pasnjaci <- subset(sf_clc18, CODE_18 == "231") %>% # CLC pastures
  st_transform(crs = "+init=epsg:32634")
# sf_clc18_pasnjaci[,vars] <- NA
# sf_clc18_pasnjaci.int <- st_intersection(sf_clc18_pasnjaci, sf.grid.5km)

#sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")

sf_clc18_pasnjaci <- st_join(sf_clc18_pasnjaci, sf_opstine, largest = TRUE) 

sf_clc18_pasnjaci %<>% dplyr::select(.,Br_ovce, NAME_2) %>% 
  filter(!is.na(Br_ovce))
sf_clc18_pasnjaci[,vars] <- NA
sf_clc18_pasnjaci %<>% st_transform(4326)
sf_clc18_pasnjaci.int <- st_intersection(sf_clc18_pasnjaci, sf.grid.5km) %>% 
  filter(!is.na(Br_ovce))

source.3B2$sources$polygon <- sf_clc18_pasnjaci.int

sf.3B2 <- corsum2sf_polygon(source.3B2, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.3B2 %>% 
  st_drop_geometry() %>% 
  rename(Sheep = Br_ovce, Municipality = NAME_2.y) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 13: sf.3B2',
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
sum.3B2 <- sf.3B2 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3B2 <- source.3B2[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3B2, total.3B2, data.frame(total.3B2 - sum.3B2))) %>%
  datatable(., caption = 'Table 14: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sum_s <- sum(sf.3B2$Br_ovce)
diff.3B2 <- data.frame(total.3B2 - sum.3B2)
sf.3B2 <- sf.3B2 %>%
  mutate(NOx = ((diff.3B2$NOx/sum_s)*Br_ovce),
         SO2 = ((diff.3B2$SO2/sum_s)*Br_ovce),
         PM10 = ((diff.3B2$PM10/sum_s)*Br_ovce),
         PM2.5 = ((diff.3B2$PM2.5/sum_s)*Br_ovce),
         NMVOC = ((diff.3B2$NMVOC/sum_s)*Br_ovce),
         NH3 = ((diff.3B2$NH3/sum_s)*Br_ovce))
sf.3B2 %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3B2 <- sf.grid.5km %>%
  st_join(sf.3B2, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3B2, layer.name.1 = "Sources 3B2", sf.spatialised = p.3B2, layer.name.2 = "Spatialised 3B2", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B2 <- p.3B2 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B2, total.3B2, data.frame(sum.p.3B2 == total.3B2)-1)) %>%
  datatable(., caption = 'Table 15: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3B2, dsn="Products/3 - Agriculture/3B2.gpkg", layer='3B2')

#'
#'
#' ## 3B4d-Goats 
#'
#'
#'
#+ include = FALSE
source.3B4d <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3B4d$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3B4d$total$inventory <- readxl::read_xlsx(path = source.file, range = "D124:I124", sheet = source.sheet, col_names = vars)


#+ include = FALSE
koze <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
koze <- cyr_lat(koze)
names(koze) <- cyr_lat(names(koze)) 
koze <- koze %>%
  mutate_all(~replace_na(., 0))
koze$Opština[koze$Opština == "Indjija"] <- "Inđija"
koze$Opština[koze$Opština == "LJubovija"] <- "Ljubovija"
koze$Opština[koze$Opština == "Mali Idjoš"] <- "Mali Iđoš"
koze$Opština[koze$Opština == "Savski venac"] <- "Savski Venac"
koze$Opština[koze$Opština == "Stari grad"] <- "Stari Grad"
koze$Opština[koze$Opština == "Petrovac na Mlavi"] <- "Petrovac"
koze$Opština[koze$Opština == "Arandjelovac"] <- "Aranđelovac"
koze$Opština[koze$Opština == "LJig"] <- "Ljig"
koze$Opština[koze$Opština == "Žitoradja"] <- "Žitorađa"
koze$Opština[koze$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_koze <- koze$`Broj gazdinstava`[match(sf_opstine$NAME_2, koze$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")

# sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)
#  sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")
#  
#  sf_rural <- st_join(sf_rur, sf_opstine, largest = TRUE) 
#  
#  sf_rural %<>% dplyr::select(.,Br_koze, NAME_2.y) %>% 
#    filter(!is.na(Br_koze))
#  sf_rural[,vars] <- NA
#  
#  sf_rur.int <- st_intersection(sf_rural, sf.grid.5km) %>% 
#    filter(!is.na(Br_koze))
#  
#  source.3B4d$sources$polygon <- sf_rur.int

sf_clc18_pasnjaci <- subset(sf_clc18, CODE_18 == "231") %>% # CLC pastures
  st_transform(crs = "+init=epsg:32634")
sf_clc18_pasnjaci <- st_join(sf_clc18_pasnjaci, sf_opstine, largest = TRUE) 

sf_clc18_pasnjaci %<>% dplyr::select(.,Br_koze, NAME_2) %>% 
  filter(!is.na(Br_koze))
sf_clc18_pasnjaci[,vars] <- NA
sf_clc18_pasnjaci %<>% st_transform(4326)
sf_clc18_pasnjaci.int <- st_intersection(sf_clc18_pasnjaci, sf.grid.5km) %>% 
  filter(!is.na(Br_koze))

source.3B4d$sources$polygon <- sf_clc18_pasnjaci.int



sf.3B4d <- corsum2sf_polygon(source.3B4d, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.3B4d %>% 
  st_drop_geometry() %>% 
  rename(Goats = Br_koze, Municipality = NAME_2.y) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 16: sf.3B4d',
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
sum.3B4d <- sf.3B4d %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3B4d <- source.3B4d[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3B4d, total.3B4d, data.frame(total.3B4d - sum.3B4d))) %>%
  datatable(., caption = 'Table 17: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sum_s <- sum(sf.3B4d$Br_koze)
diff.3B4d <- data.frame(total.3B4d - sum.3B4d)
sf.3B4d <- sf.3B4d %>%
  mutate(NOx = ((diff.3B4d$NOx/sum_s)*Br_koze),
         SO2 = ((diff.3B4d$SO2/sum_s)*Br_koze),
         PM10 = ((diff.3B4d$PM10/sum_s)*Br_koze),
         PM2.5 = ((diff.3B4d$PM2.5/sum_s)*Br_koze),
         NMVOC = ((diff.3B4d$NMVOC/sum_s)*Br_koze),
         NH3 = ((diff.3B4d$NH3/sum_s)*Br_koze))
sf.3B4d %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3B4d <- sf.grid.5km %>%
  st_join(sf.3B4d, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3B4d, layer.name.1 = "Sources 3B4d", sf.spatialised = p.3B4d, layer.name.2 = "Spatialised 3B4d", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B4d <- p.3B4d %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B4d, total.3B4d, data.frame(sum.p.3B4d == total.3B4d)-1)) %>%
  datatable(., caption = 'Table 18: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3B4d, dsn="Products/3 - Agriculture/3B4d.gpkg", layer='3B4d')

#'
#'
#' ## 3B4e-Horses 
#'
#'
#'
#+ include = FALSE
source.3B4e <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3B4e$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3B4e$total$inventory <- readxl::read_xlsx(path = source.file, range = "D125:I125", sheet = source.sheet, col_names = vars)


#+ include = FALSE
konji <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
konji <- cyr_lat(konji)
names(konji) <- cyr_lat(names(konji)) 
konji <- konji %>%
  mutate_all(~replace_na(., 0))
konji$Opština[konji$Opština == "Indjija"] <- "Inđija"
konji$Opština[konji$Opština == "LJubovija"] <- "Ljubovija"
konji$Opština[konji$Opština == "Mali Idjoš"] <- "Mali Iđoš"
konji$Opština[konji$Opština == "Savski venac"] <- "Savski Venac"
konji$Opština[konji$Opština == "Stari grad"] <- "Stari Grad"
konji$Opština[konji$Opština == "Petrovac na Mlavi"] <- "Petrovac"
konji$Opština[konji$Opština == "Arandjelovac"] <- "Aranđelovac"
konji$Opština[konji$Opština == "LJig"] <- "Ljig"
konji$Opština[konji$Opština == "Žitoradja"] <- "Žitorađa"
konji$Opština[konji$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_konji <- konji$`Broj gazdinstava`[match(sf_opstine$NAME_2, konji$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")

# sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)
# sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")
# 
# sf_rural <- st_join(sf_rur, sf_opstine, largest = TRUE) 
# 
# sf_rural %<>% dplyr::select(.,Br_konji, NAME_2.y) %>% 
#   filter(!is.na(Br_konji))
# sf_rural[,vars] <- NA
# 
# sf_rur.int <- st_intersection(sf_rural, sf.grid.5km) %>% 
#   filter(!is.na(Br_konji))
# 
# source.3B4e$sources$polygon <- sf_rur.int


sf_clc18_pasnjaci <- subset(sf_clc18, CODE_18 == "231") %>% # CLC pastures
  st_transform(crs = "+init=epsg:32634")
sf_clc18_pasnjaci <- st_join(sf_clc18_pasnjaci, sf_opstine, largest = TRUE) 

sf_clc18_pasnjaci %<>% dplyr::select(.,Br_konji, NAME_2) %>% 
  filter(!is.na(Br_konji))
sf_clc18_pasnjaci[,vars] <- NA
sf_clc18_pasnjaci %<>% st_transform(4326)
sf_clc18_pasnjaci.int <- st_intersection(sf_clc18_pasnjaci, sf.grid.5km) %>% 
  filter(!is.na(Br_konji))

source.3B4e$sources$polygon <- sf_clc18_pasnjaci.int

sf.3B4e <- corsum2sf_polygon(source.3B4e, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.3B4e %>% 
  st_drop_geometry() %>% 
  rename(Horses = Br_konji, Municipality = NAME_2.y) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 19: sf.3B4e',
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
sum.3B4e <- sf.3B4e %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3B4e <- source.3B4e[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3B4e, total.3B4e, data.frame(total.3B4e - sum.3B4e))) %>%
  datatable(., caption = 'Table 20: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sum_s <- sum(sf.3B4e$Br_konji)
diff.3B4e <- data.frame(total.3B4e - sum.3B4e)
sf.3B4e <- sf.3B4e %>%
  mutate(NOx = ((diff.3B4e$NOx/sum_s)*Br_konji),
         SO2 = ((diff.3B4e$SO2/sum_s)*Br_konji),
         PM10 = ((diff.3B4e$PM10/sum_s)*Br_konji),
         PM2.5 = ((diff.3B4e$PM2.5/sum_s)*Br_konji),
         NMVOC = ((diff.3B4e$NMVOC/sum_s)*Br_konji),
         NH3 = ((diff.3B4e$NH3/sum_s)*Br_konji))
sf.3B4e %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3B4e <- sf.grid.5km %>%
  st_join(sf.3B4e, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3B4e, layer.name.1 = "Sources 3B4e", sf.spatialised = p.3B4e, layer.name.2 = "Spatialised 3B4e", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B4e <- p.3B4e %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B4e, total.3B4e, data.frame(sum.p.3B4e == total.3B4e)-1)) %>%
  datatable(., caption = 'Table 21: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3B4e, dsn="Products/3 - Agriculture/3B4e.gpkg", layer='3B4e')

#'
#'
#' ## 3B4giii-Turkeys 
#'
#'
#'
#+ include = FALSE
source.3B4giii <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3B4giii$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3B4giii$total$inventory <- readxl::read_xlsx(path = source.file, range = "D127:I127", sheet = source.sheet, col_names = vars)


#+ include = FALSE
curke <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
curke <- cyr_lat(curke)
names(curke) <- cyr_lat(names(curke)) 
curke <- curke %>%
  mutate_all(~replace_na(., 0))
curke$Opština[curke$Opština == "Indjija"] <- "Inđija"
curke$Opština[curke$Opština == "LJubovija"] <- "Ljubovija"
curke$Opština[curke$Opština == "Mali Idjoš"] <- "Mali Iđoš"
curke$Opština[curke$Opština == "Savski venac"] <- "Savski Venac"
curke$Opština[curke$Opština == "Stari grad"] <- "Stari Grad"
curke$Opština[curke$Opština == "Petrovac na Mlavi"] <- "Petrovac"
curke$Opština[curke$Opština == "Arandjelovac"] <- "Aranđelovac"
curke$Opština[curke$Opština == "LJig"] <- "Ljig"
curke$Opština[curke$Opština == "Žitoradja"] <- "Žitorađa"
curke$Opština[curke$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_curke <- curke$Živina[match(sf_opstine$NAME_2, curke$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")

# sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)
sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")

sf_rural <- st_join(sf_rur, sf_opstine, largest = TRUE) 

sf_rural %<>% dplyr::select(.,Br_curke, NAME_2) %>% 
  filter(!is.na(Br_curke))
sf_rural[,vars] <- NA
sf_rural %<>% st_transform(4326)

sf_rur.int <- st_intersection(sf_rural, sf.grid.5km) %>% 
  filter(!is.na(Br_curke))

source.3B4giii$sources$polygon <- sf_rur.int

sf.3B4giii <- corsum2sf_polygon(source.3B4giii, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.3B4giii %>% 
  st_drop_geometry() %>% 
  rename(Turkeys = Br_curke, Municipality = NAME_2.y) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 22: sf.3B4giii',
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
sum.3B4giii <- sf.3B4giii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3B4giii <- source.3B4giii[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3B4giii, total.3B4giii, data.frame(total.3B4giii - sum.3B4giii))) %>%
  datatable(., caption = 'Table 23: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sum_s <- sum(sf.3B4giii$Br_curke)
diff.3B4giii <- data.frame(total.3B4giii - sum.3B4giii)
sf.3B4giii <- sf.3B4giii %>%
  mutate(NOx = ((diff.3B4giii$NOx/sum_s)*Br_curke),
         SO2 = ((diff.3B4giii$SO2/sum_s)*Br_curke),
         PM10 = ((diff.3B4giii$PM10/sum_s)*Br_curke),
         PM2.5 = ((diff.3B4giii$PM2.5/sum_s)*Br_curke),
         NMVOC = ((diff.3B4giii$NMVOC/sum_s)*Br_curke),
         NH3 = ((diff.3B4giii$NH3/sum_s)*Br_curke))
sf.3B4giii %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3B4giii <- sf.grid.5km %>%
  st_join(sf.3B4giii, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3B4giii, layer.name.1 = "Sources 3B4giii", sf.spatialised = p.3B4giii, layer.name.2 = "Spatialised 3B4giii", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B4giii <- p.3B4giii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B4giii, total.3B4giii, data.frame(sum.p.3B4giii == total.3B4giii)-1)) %>%
  datatable(., caption = 'Table 24: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3B4giii, dsn="Products/3 - Agriculture/3B4giii.gpkg", layer='3B4giii')

#'
#'
#' ## 3B4giv-Other poultry
#'
#'
#'
#+ include = FALSE
source.3B4giv <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3B4giv$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3B4giv$total$inventory <- readxl::read_xlsx(path = source.file, range = "D128:I128", sheet = source.sheet, col_names = vars)


#+ include = FALSE
zivina <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
zivina <- cyr_lat(zivina)
names(zivina) <- cyr_lat(names(zivina)) 
zivina <- zivina %>%
  mutate_all(~replace_na(., 0))
zivina$Opština[zivina$Opština == "Indjija"] <- "Inđija"
zivina$Opština[zivina$Opština == "LJubovija"] <- "Ljubovija"
zivina$Opština[zivina$Opština == "Mali Idjoš"] <- "Mali Iđoš"
zivina$Opština[zivina$Opština == "Savski venac"] <- "Savski Venac"
zivina$Opština[zivina$Opština == "Stari grad"] <- "Stari Grad"
zivina$Opština[zivina$Opština == "Petrovac na Mlavi"] <- "Petrovac"
zivina$Opština[zivina$Opština == "Arandjelovac"] <- "Aranđelovac"
zivina$Opština[zivina$Opština == "LJig"] <- "Ljig"
zivina$Opština[zivina$Opština == "Žitoradja"] <- "Žitorađa"
zivina$Opština[zivina$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_zivina <- zivina$Živina[match(sf_opstine$NAME_2, zivina$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")

# sf_clc18_rur <- st_sym_difference(sf_opstine, sf_clc18_urb)
sf_rur <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")

sf_rural <- st_join(sf_rur, sf_opstine, largest = TRUE) 

sf_rural %<>% dplyr::select(.,Br_zivina, NAME_2) %>% 
  filter(!is.na(Br_zivina))
sf_rural[,vars] <- NA
sf_rural %<>% st_transform(4326)
sf_rur.int <- st_intersection(sf_rural, sf.grid.5km) %>% 
  filter(!is.na(Br_zivina))

source.3B4giv$sources$polygon <- sf_rur.int

sf.3B4giv <- corsum2sf_polygon(source.3B4giv, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.3B4giv %>% 
  st_drop_geometry() %>% 
  rename(Other_polutry = Br_zivina, Municipality = NAME_2.y) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 25: sf.3B4giv',
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
sum.3B4giv <- sf.3B4giv %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3B4giv <- source.3B4giv[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3B4giv, total.3B4giv, data.frame(total.3B4giv - sum.3B4giv))) %>%
  datatable(., caption = 'Table 26: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sum_s <- sum(sf.3B4giv$Br_zivina)
diff.3B4giv <- data.frame(total.3B4giv - sum.3B4giv)
sf.3B4giv <- sf.3B4giv %>%
  mutate(NOx = ((diff.3B4giv$NOx/sum_s)*Br_zivina),
         SO2 = ((diff.3B4giv$SO2/sum_s)*Br_zivina),
         PM10 = ((diff.3B4giv$PM10/sum_s)*Br_zivina),
         PM2.5 = ((diff.3B4giv$PM2.5/sum_s)*Br_zivina),
         NMVOC = ((diff.3B4giv$NMVOC/sum_s)*Br_zivina),
         NH3 = ((diff.3B4giv$NH3/sum_s)*Br_zivina))
sf.3B4giv %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3B4giv <- sf.grid.5km %>%
  st_join(sf.3B4giv, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3B4giv, layer.name.1 = "Sources 3B4giv", sf.spatialised = p.3B4giv, layer.name.2 = "Spatialised 3B4giv", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3B4giv <- p.3B4giv %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3B4giv, total.3B4giv, data.frame(sum.p.3B4giv == total.3B4giv)-1)) %>%
  datatable(., caption = 'Table 27: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3B4giv, dsn="Products/3 - Agriculture/3B4giv.gpkg", layer='3B4giv')



#'
#'
#' ## 3Da1 - Inorganic N-fertilizers (includes also urea application)
#'
#'
#'
#+ include = FALSE
source.3Da1 <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3Da1$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3Da1$total$inventory <- readxl::read_xlsx(path = source.file, range = "D130:I130", sheet = source.sheet, col_names = vars)

#+ include = FALSE
clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
sf_clc18_polj <- subset(sf_clc18, CODE_18 == "211" | CODE_18 == "221" | CODE_18 == "222" | CODE_18 == "242" | CODE_18 == "243") %>% # CLC agricultural areas
  st_transform(crs = "+init=epsg:32634")
sf_clc18_polj[,vars] <- NA
sf_clc18_polj %<>% st_transform(4326)
sf_clc18_polj.int <- st_intersection(sf_clc18_polj, sf.grid.5km)

source.3Da1$sources$polygon <- sf_clc18_polj.int

sf.3Da1 <- corsum2sf_polygon(source.3Da1, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, message = FALSE
sf.3Da1 %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 28: sf.3Da1',
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
sum.3Da1 <- sf.3Da1 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3Da1 <- source.3Da1[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3Da1, total.3Da1, data.frame(total.3Da1 - sum.3Da1))) %>%
  datatable(., caption = 'Table 29: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.3Da1 <- sf.3Da1 %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.3Da1$Area)
diff.3Da1 <- data.frame(total.3Da1 - sum.3Da1)
sf.3Da1 <- sf.3Da1 %>%
  mutate(NOx = ((diff.3Da1$NOx/sum_Area)*Area),
         SO2 = ((diff.3Da1$SO2/sum_Area)*Area),
         PM10 = ((diff.3Da1$PM10/sum_Area)*Area),
         PM2.5 = ((diff.3Da1$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.3Da1$NMVOC/sum_Area)*Area),
         NH3 = ((diff.3Da1$NH3/sum_Area)*Area))
sf.3Da1 %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3Da1 <- sf.grid.5km %>%
  st_join(sf.3Da1, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3Da1, layer.name.1 = "Sources 3Da1", sf.spatialised = p.3Da1, layer.name.2 = "Spatialised 3Da1", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3Da1 <- p.3Da1 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3Da1, total.3Da1, data.frame(sum.p.3Da1 == total.3Da1)-1)) %>%
  datatable(., caption = 'Table 30: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3Da1, dsn="Products/3 - Agriculture/3Da1.gpkg", layer='3Da1')


#'
#'
#' ## 3Da2a - Animal manure applied to soils 
#'
#'
#'
#+ include = FALSE
source.3Da2a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3Da2a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3Da2a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D131:I131", sheet = source.sheet, col_names = vars)

#+ include = FALSE
source.3Da2a$sources$polygon <- sf_clc18_polj.int

sf.3Da2a <- corsum2sf_polygon(source.3Da2a, distribute = FALSE)# %>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, message = FALSE
sf.3Da2a %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 31: sf.3Da2a',
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
sum.3Da2a <- sf.3Da2a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3Da2a <- source.3Da2a[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3Da2a, total.3Da2a, data.frame(total.3Da2a - sum.3Da2a))) %>%
  datatable(., caption = 'Table 32: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.3Da2a <- sf.3Da2a %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.3Da2a$Area)
diff.3Da2a <- data.frame(total.3Da2a - sum.3Da2a)
sf.3Da2a <- sf.3Da2a %>%
  mutate(NOx = ((diff.3Da2a$NOx/sum_Area)*Area),
         SO2 = ((diff.3Da2a$SO2/sum_Area)*Area),
         PM10 = ((diff.3Da2a$PM10/sum_Area)*Area),
         PM2.5 = ((diff.3Da2a$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.3Da2a$NMVOC/sum_Area)*Area),
         NH3 = ((diff.3Da2a$NH3/sum_Area)*Area))
sf.3Da2a %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3Da2a <- sf.grid.5km %>%
  st_join(sf.3Da2a, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3Da2a, layer.name.1 = "Sources 3Da2a", sf.spatialised = p.3Da2a, layer.name.2 = "Spatialised 3Da2a", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3Da2a <- p.3Da2a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3Da2a, total.3Da2a, data.frame(sum.p.3Da2a == total.3Da2a)-1)) %>%
  datatable(., caption = 'Table 33: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3Da2a, dsn="Products/3 - Agriculture/3Da2a.gpkg", layer='3Da2a')


#'
#'
#' ## 3Da3 - Urine and dung deposited by grazing animals
#'
#'
#'
#+ include = FALSE
source.3Da3 <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3Da3$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3Da3$total$inventory <- readxl::read_xlsx(path = source.file, range = "D134:I134", sheet = source.sheet, col_names = vars)

#+ include = FALSE
sf_clc18_pasnjaci <- subset(sf_clc18, CODE_18 == "231") %>% # CLC pastures
  st_transform(crs = "+init=epsg:32634")
sf_clc18_pasnjaci[,vars] <- NA
sf_clc18_pasnjaci %<>% st_transform(4326)
sf_clc18_pasnjaci.int <- st_intersection(sf_clc18_pasnjaci, sf.grid.5km)

source.3Da3$sources$polygon <- sf_clc18_pasnjaci.int

sf.3Da3 <- corsum2sf_polygon(source.3Da3, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, message = FALSE
sf.3Da3 %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 34: sf.3Da3',
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
sum.3Da3 <- sf.3Da3 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3Da3 <- source.3Da3[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3Da3, total.3Da3, data.frame(total.3Da3 - sum.3Da3))) %>%
  datatable(., caption = 'Table 35: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.3Da3 <- sf.3Da3 %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.3Da3$Area)
diff.3Da3 <- data.frame(total.3Da3 - sum.3Da3)
sf.3Da3 <- sf.3Da3 %>%
  mutate(NOx = ((diff.3Da3$NOx/sum_Area)*Area),
         SO2 = ((diff.3Da3$SO2/sum_Area)*Area),
         PM10 = ((diff.3Da3$PM10/sum_Area)*Area),
         PM2.5 = ((diff.3Da3$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.3Da3$NMVOC/sum_Area)*Area),
         NH3 = ((diff.3Da3$NH3/sum_Area)*Area))
sf.3Da3 %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3Da3 <- sf.grid.5km %>%
  st_join(sf.3Da3, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3Da3, layer.name.1 = "Sources 3Da3", sf.spatialised = p.3Da3, layer.name.2 = "Spatialised 3Da3", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3Da3 <- p.3Da3 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3Da3, total.3Da3, data.frame(sum.p.3Da3 == total.3Da3)-1)) %>%
  datatable(., caption = 'Table 36: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3Da3, dsn="Products/3 - Agriculture/3Da3.gpkg", layer='3Da3')


#'
#'
#' ## 3Dc - Farm-level agricultural operations including storage, handling and transport of agricultural products
#'
#'
#'
#+ include = FALSE
source.3Dc <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3Dc$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3Dc$total$inventory <- readxl::read_xlsx(path = source.file, range = "D137:I137", sheet = source.sheet, col_names = vars)

#+ include = FALSE
sf_clc18_242 <- subset(sf_clc18, CODE_18 == "242") %>% # CLC complex cultivated areas (including farms)
  st_transform(crs = "+init=epsg:32634")
sf_clc18_242[,vars] <- NA
sf_clc18_242 %<>% st_transform(4326)
sf_clc18_242.int <- st_intersection(sf_clc18_242, sf.grid.5km)

source.3Dc$sources$polygon <- sf_clc18_242.int

sf.3Dc <- corsum2sf_polygon(source.3Dc, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, message = FALSE
sf.3Dc %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 37: sf.3Dc',
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
sum.3Dc <- sf.3Dc %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3Dc <- source.3Dc[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3Dc, total.3Dc, data.frame(total.3Dc - sum.3Dc))) %>%
  datatable(., caption = 'Table 38: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.3Dc <- sf.3Dc %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.3Dc$Area)
diff.3Dc <- data.frame(total.3Dc - sum.3Dc)
sf.3Dc <- sf.3Dc %>%
  mutate(NOx = ((diff.3Dc$NOx/sum_Area)*Area),
         SO2 = ((diff.3Dc$SO2/sum_Area)*Area),
         PM10 = ((diff.3Dc$PM10/sum_Area)*Area),
         PM2.5 = ((diff.3Dc$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.3Dc$NMVOC/sum_Area)*Area),
         NH3 = ((diff.3Dc$NH3/sum_Area)*Area))
sf.3Dc %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3Dc <- sf.grid.5km %>%
  st_join(sf.3Dc, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3Dc, layer.name.1 = "Sources 3Dc", sf.spatialised = p.3Dc, layer.name.2 = "Spatialised 3Dc", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3Dc <- p.3Dc %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3Dc, total.3Dc, data.frame(sum.p.3Dc == total.3Dc)-1)) %>%
  datatable(., caption = 'Table 39: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3Dc, dsn="Products/3 - Agriculture/3Dc.gpkg", layer='3Dc')


#'
#'
#' ## 3De - Cultivated crops  
#'
#'
#'
#+ include = FALSE
source.3De <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.3De$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D119:I119", sheet = source.sheet, col_names = vars)
source.3De$total$inventory <- readxl::read_xlsx(path = source.file, range = "D139:I139", sheet = source.sheet, col_names = vars)

#+ include = FALSE
source.3De$sources$polygon <- sf_clc18_polj.int

sf.3De <- corsum2sf_polygon(source.3De, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, message = FALSE
sf.3De %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 40: sf.3De',
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
sum.3De <- sf.3De %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.3De <- source.3De[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.3De, total.3De, data.frame(total.3De - sum.3De))) %>%
  datatable(., caption = 'Table 41: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.3De <- sf.3De %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.3De$Area)
diff.3De <- data.frame(total.3De - sum.3De)
sf.3De <- sf.3De %>%
  mutate(NOx = ((diff.3De$NOx/sum_Area)*Area),
         SO2 = ((diff.3De$SO2/sum_Area)*Area),
         PM10 = ((diff.3De$PM10/sum_Area)*Area),
         PM2.5 = ((diff.3De$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.3De$NMVOC/sum_Area)*Area),
         NH3 = ((diff.3De$NH3/sum_Area)*Area))
sf.3De %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.3De <- sf.grid.5km %>%
  st_join(sf.3De, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.3De, layer.name.1 = "Sources 3De", sf.spatialised = p.3De, layer.name.2 = "Spatialised 3De", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.3De <- p.3De %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.3De, total.3De, data.frame(sum.p.3De == total.3De)-1)) %>%
  datatable(., caption = 'Table 42: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.3De, dsn="Products/3 - Agriculture/3De.gpkg", layer='3De')




