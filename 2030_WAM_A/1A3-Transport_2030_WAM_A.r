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
#' # 1A3 - Transport
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
#' ## Methodology: 
#' 
#' ### Auxiliary Data to be used:
#' - The road network, including 1A, 2A, 1B and 2B road category [^1], 
#' - Data of the Mean Daily Trafic for one Year (MDTY) obtained from ~400 automatic Vehicle Counting Devices (VCD), spreaded over the whole territory of Serbia, on three different road category (1A, 2A and 1B) [^1],
#' - Number of vehicles registered in each municipality of the Serbia for the year 2015 [^2].
#' - The map of Serbia with municipalities [^3]
#' - Road network within the urban areas [^4]
#' - Corine Land Cover for the territory of Serbia [^5]
#' 
#' [^1]: Source: Public Enterprise “Roads of Serbia”
#' [^2]: Source: Statistical Office of the Republic of Serbia.
#' [^3]: Source: GADM, the Database of Global Administrative Areas.
#' [^4]: Source: Open Street Maps
#' [^5]: Source: Corine Land Cover.
#' 
#' ### Methodology for road transport (1A3b)[^6] includes the following steps:
#' 
#' [^6]: Road transport that includes the urban/rural/highway inventory separately
#' 
#' 
#' #### Rural inventory
#' 1. Spatial classification of the whole Territory of Serbia into urban and rural areas based on Corine Land Cover data.
#' 2. Spatial interpolation of the MDTY values (for each vehicle category) from VCDs at the mid-points of the roads where VCD not exist. This will be done for each category of road, separately, by using the knn method (k-nearest neighbors). In doing so, the distances to the whole road section where AVC exist will be taken into account. Model will be trained based through the cross-validation procedure, by using the RMSE (Root Mean Squared Error) as a measure of performace. In this way, the vehicle activity for each road will be estimated.
#' 3. Based on these estimates, total rural inventory for each vehicle category will be spatially dissagregated into the 5x5km cells propotionally, by taking the estimated MDTY and the lengths of corresponding road sections that lie within the cell into account.
#' 
#'
#' #### Urban inventory 
#' 1. First, total inventory pollution will be spatially dissagregated based on the number of the vehicles registered in each municipality.
#' 2. Next, estimated urban emissions in each municipality will be further spatialized into 5x5km cells based on the the lenght of the roads that lie in each cell of urban areas.
#' 
#' #### Highway inventory  
#' 
#' - Total inventory for highway transport will be spatially dissagregated based on the MDTY values and the lenght of the corresponding highway section that lie in each cell. 
#'    
#' #### Total inventory
#' 
#' - Total air pollution from the road transport in each cell will be estimated as the sum of rural, urban and highway inventory for each cell proportionally.     
#'    
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

# source.list = source.1A3c
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
source.file = "2030_WAM_A/Pollutant inventory spatialized-za_2030_WAM_A.xlsx"
source.sheet =  "1A3-Transport"
header <- readxl::read_xlsx(path = source.file, range = "D9:S9", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Grid_Serbia_0.05deg.gpkg")
sf.grid.5km <- st_as_sf(grid.5km)
sf.grid.5km %<>% dplyr::mutate(ID = id)


#'
#'
#' ## 1A3ai-International aviation LTO (civil)
#' 
#+ include = FALSE
source.1A3ai <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3ai$sources$points <- readxl::read_xlsx(path = source.file, range = "D10:S11", sheet = source.sheet, col_names = header)
source.1A3ai$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D19:I19", sheet = source.sheet, col_names = vars)
source.1A3ai$total$inventory <- readxl::read_xlsx(path = source.file, range = "D20:I20", sheet = source.sheet, col_names = vars)


clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
sf_clc18_air <- subset(sf_clc18, CODE_18 == "124") %>% # CLC urban zones
  st_transform(crs = "+init=epsg:32634") %>%
  dplyr::select(geometry)

sf.1A3ai <- corsum2sf(source.1A3ai, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")


sf.air <- sf::st_join(sf_clc18_air, sf.1A3ai)
sf.air %<>% filter(`...10` == 'Aerodrom "Konstantin Veliki" Nis' | `...10` == 'Aerodorm "Nikola Tesla" Beograd') %>% dplyr::select(geometry)
sf.air[, vars] <- NA
sf.air %<>% sf::st_transform(4326)
sf.air.int <- st_intersection(sf.air, sf.grid.5km)
source.1A3ai$sources$points <- NA
source.1A3ai$sources$polygon <- sf.air.int

sf.1A3ai <- corsum2sf_polygon(source.1A3ai, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")


#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3ai %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 1: sf.1A3ai',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3ai <- sf.1A3ai %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3ai <- source.1A3ai[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3ai, total.1A3ai, data.frame(total.1A3ai - sum.1A3ai))) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE


sf.1A3ai$Passangers <- NA
sf.1A3ai$Passangers[1] <- 4776110
sf.1A3ai$Passangers[2] <- 4776110
sf.1A3ai$Passangers[3] <- 36258 
sf.1A3ai$Passangers[4] <- 36258 
sf.1A3ai %<>% dplyr::mutate(Weight = unclass(st_area(.)) * Passangers)

sum_Weight <- sum(sf.1A3ai$Weight)
diff.1A3ai <- data.frame(total.1A3ai - sum.1A3ai)
sf.1A3ai <- sf.1A3ai %>% # Calculating weights and distibute data
  mutate(NOx = ((diff.1A3ai$NOx/sum_Weight)*Weight),
         SO2 = ((diff.1A3ai$SO2/sum_Weight)*Weight),
         PM10 = ((diff.1A3ai$PM10/sum_Weight)*Weight),
         PM2.5 = ((diff.1A3ai$PM2.5/sum_Weight)*Weight),
         NMVOC = ((diff.1A3ai$NMVOC/sum_Weight)*Weight),
         NH3 = ((diff.1A3ai$NH3/sum_Weight)*Weight))
sf.1A3ai %<>% dplyr::select(vars)

#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3ai <- sf.grid.5km %>%
  st_join(sf.1A3ai, join = st_contains) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3ai, layer.name.1 = "Sources 1A3ai", sf.spatialised = p.1A3ai, layer.name.2 = "Spatialised 1A3ai", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3ai <- p.1A3ai %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3ai, total.1A3ai, data.frame(sum.p.1A3ai == total.1A3ai)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3ai, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3ai.gpkg", layer='1A3ai')



#'
#'
#' ## 1A3aii-Domestic aviation LTO (civil)
#' 
#' 
#+ include = FALSE
source.1A3aii <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3aii$sources$points <- readxl::read_xlsx(path = source.file, range = "D21:S41", sheet = source.sheet, col_names = header)
source.1A3aii$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D48:I48", sheet = source.sheet, col_names = vars)
source.1A3aii$total$inventory <- readxl::read_xlsx(path = source.file, range = "D49:I49", sheet = source.sheet, col_names = vars)

sf.1A3aii <- corsum2sf(source.1A3aii, distribute = TRUE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
#sf.1A3aii %>% 
#  st_drop_geometry() %>% 
#  dplyr::mutate_if(is.numeric, round, 2) %>%
#  datatable(., caption = 'Table 4: sf.1A3aii',
#            rownames = FALSE, escape = FALSE, selection = "single",
#            extensions = c('Buttons'),
#            class = 'white-space: nowrap',
#            options = list(
#              pageLength = 5,
#              dom = 'Bfrtip',
#              buttons = list('pageLength'),
#              searchHighlight = TRUE,
#              scrollX = TRUE,
#              scrollY = TRUE
#            ))
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3aii <- sf.1A3aii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3aii <- source.1A3aii[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3aii, total.1A3aii, data.frame(sum.1A3aii == total.1A3aii)-1)) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3aii <- sf.grid.5km %>%
  st_join(sf.1A3aii) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3aii, layer.name.1 = "Sources 1A3aii", sf.spatialised = p.1A3aii, layer.name.2 = "Spatialised 1A3aii", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3aii <- p.1A3aii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3aii, total.1A3aii, data.frame(sum.p.1A3aii == total.1A3aii)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3aii, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3aii.gpkg", layer='1A3aii')

#'
#'
#' ## 1A3bi-Road Transport: Passengers cars
#' 
#' 
#' ### Urban transport
#+ include = FALSE
source.1A3bi_U <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bi_U$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D59:I59", sheet = source.sheet, col_names = vars)
source.1A3bi_U$total$inventory <- readxl::read_xlsx(path = source.file, range = "D60:I60", sheet = source.sheet, col_names = vars)

#+ include = FALSE
urban_roads <- readOGR("Data/putevi/OSM_putevi_urbana_podrucja.gpkg", 
                    use_iconv=TRUE,  
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)
sf_urban_roads <- st_as_sf(urban_roads) %>%
  st_transform(crs = "+init=epsg:32634") 


sf_urban_roads[,vars] <- NA
sf_urban_roads %<>% sf::st_transform(4326)
sf_urban_roads.int <- st_intersection(sf_urban_roads, sf.grid.5km) %>%
  dplyr::select(.,vars) %>%
  dplyr::mutate(Length = st_length(.))


source.1A3bi_U$sources$lines <- sf_urban_roads.int

sf.1A3bi_U <- corsum2sf_lines(source.1A3bi_U, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bi_U %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 7: sf.1A3bi_U',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bi_U <- sf.1A3bi_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bi_U <- source.1A3bi_U[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bi_U, total.1A3bi_U, data.frame(total.1A3bi_U - sum.1A3bi_U))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_LengthUrban <- sum(sf.1A3bi_U$Length)
diff.1A3bi_U <- data.frame(total.1A3bi_U - sum.1A3bi_U)
sf.1A3bi_U <- sf.1A3bi_U %>%
  mutate(NOx = ((diff.1A3bi_U$NOx/sum_LengthUrban)*Length),
         SO2 = ((diff.1A3bi_U$SO2/sum_LengthUrban)*Length),
         PM10 = ((diff.1A3bi_U$PM10/sum_LengthUrban)*Length),
         PM2.5 = ((diff.1A3bi_U$PM2.5/sum_LengthUrban)*Length),
         NMVOC = ((diff.1A3bi_U$NMVOC/sum_LengthUrban)*Length),
         NH3 = ((diff.1A3bi_U$NH3/sum_LengthUrban)*Length))
sf.1A3bi_U %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bi_U <- sf.grid.5km %>%
  st_join(sf.1A3bi_U, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.1A3bi_U, layer.name = "Spatialised 1A3bi_U") + mapview(sf.1A3bi_U, layer.name = "Sources 1A3bi_U", color = "red") 
spatialised.mapview(sf.sources = sf.1A3bi_U, layer.name.1 = "Sources 1A3bi_U", sf.spatialised = p.1A3bi_U, layer.name.2 = "Spatialised 1A3bi_U", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bi_U <- p.1A3bi_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bi_U, total.1A3bi_U, data.frame(sum.p.1A3bi_U == total.1A3bi_U)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3bi_U, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bi_U.gpkg", layer='1A3bi_U')

#' 
#' 
#' 
#' 
#'
#' ### Rural transport
#'
#+ include = FALSE
source.1A3bi_R <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bi_R$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D59:I59", sheet = source.sheet, col_names = vars)
source.1A3bi_R$total$inventory <- readxl::read_xlsx(path = source.file, range = "D61:I61", sheet = source.sheet, col_names = vars)


load("./Data/Putevi/rn_IIA.RDS")
load("./Data/Putevi/rn_IB.RDS")


sf_roads <- rbind(rn.IIA ,rn.IB)

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]
sf_roads %<>% st_transform(4326)
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)
  

source.1A3bi_R$sources$lines <- sf_roads.int

sf.1A3bi_R <- corsum2sf_lines(source.1A3bi_R, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bi_R %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 10: sf.1A3bi_R',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bi_R <- sf.1A3bi_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bi_R <- source.1A3bi_R[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bi_R, total.1A3bi_R, data.frame(total.1A3bi_R - sum.1A3bi_R))) %>%
  datatable(., caption = 'Table 11: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3bi_R$PGDSL)
diff.1A3bi_R <- data.frame(total.1A3bi_R - sum.1A3bi_R)
sf.1A3bi_R <- sf.1A3bi_R %>%
  mutate(NOx = ((diff.1A3bi_R$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3bi_R$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3bi_R$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3bi_R$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3bi_R$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3bi_R$NH3/sum_PGDSL)*PGDSL))
sf.1A3bi_R %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bi_R <- sf.grid.5km %>%
  st_join(sf.1A3bi_R, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.1A3c, layer.name = "Spatialised 1A3c") + mapview(sf.1A3c, layer.name = "Sources 1A3c", color = "red") 
spatialised.mapview(sf.sources = sf.1A3bi_R, layer.name.1 = "Sources 1A3bi_R", sf.spatialised = p.1A3bi_R, layer.name.2 = "Spatialised 1A3bi_R", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bi_R <- p.1A3bi_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bi_R, total.1A3bi_R, data.frame(sum.p.1A3bi_R == total.1A3bi_R)-1)) %>%
  datatable(., caption = 'Table 12: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3bi_R, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bi_R.gpkg", layer='1A3bi_R')

#'
#'
#'
#' 
#' ### Highways transport
#' 
#+ include = FALSE, echo = FALSE, result = FALSE

load("./Data/Putevi/rn_IA.RDS")
# Popunjavanje NA vrednosti (to su deonice koje algoritam nije resio). Dodeljena im je srednja vrednost protoka na 
rn.IA$PGDS_2015.est[is.na(rn.IA$PGDS_2015.est)] <- mean(rn.IA$PGDS_2015[rn.IA$Broj_puta == "A1" & !is.na(rn.IA$PGDS_2015)])


source.1A3bi_H <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bi_H$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D59:I59", sheet = source.sheet, col_names = vars)
source.1A3bi_H$total$inventory <- readxl::read_xlsx(path = source.file, range = "D62:I62", sheet = source.sheet, col_names = vars)

sf_roads <- rn.IA

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]
sf_roads %<>% sf::st_transform(4326)
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)


source.1A3bi_H$sources$lines <- sf_roads.int

sf.1A3bi_H <- corsum2sf_lines(source.1A3bi_H, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bi_H %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 13: sf.1A3bi_H',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bi_H <- sf.1A3bi_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bi_H <- source.1A3bi_H[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bi_H, total.1A3bi_H, data.frame(total.1A3bi_H - sum.1A3bi_H))) %>%
  datatable(., caption = 'Table 14: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3bi_H$PGDSL)
diff.1A3bi_H <- data.frame(total.1A3bi_H - sum.1A3bi_H)
sf.1A3bi_H <- sf.1A3bi_H %>%
  mutate(NOx = ((diff.1A3bi_H$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3bi_H$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3bi_H$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3bi_H$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3bi_H$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3bi_H$NH3/sum_PGDSL)*PGDSL))
sf.1A3bi_H %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bi_H <- sf.grid.5km %>%
  st_join(sf.1A3bi_H, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.1A3c, layer.name = "Spatialised 1A3c") + mapview(sf.1A3c, layer.name = "Sources 1A3c", color = "red") 
spatialised.mapview(sf.sources = sf.1A3bi_H, layer.name.1 = "Sources 1A3bi_H", sf.spatialised = p.1A3bi_H, layer.name.2 = "Spatialised 1A3bi_H", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bi_H <- p.1A3bi_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bi_H, total.1A3bi_H, data.frame(sum.p.1A3bi_H == total.1A3bi_H)-1)) %>%
  datatable(., caption = 'Table 15: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3bi_H, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bi_H.gpkg", layer='1A3bi_H')



#'
#'
#' ## 1A3bii-Road transport: Light-duty vehicles
#' 
#' 
#' ### Urban transport
#+ include = FALSE
source.1A3bii_U <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bii_U$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.1A3bii_U$total$inventory <- readxl::read_xlsx(path = source.file, range = "D73:I73", sheet = source.sheet, col_names = vars)

#+ include = FALSE
#urban_roads <- readOGR("Data/putevi/OSM_putevi_urbana_podrucja.gpkg", 
#                       use_iconv=TRUE,  
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#sf_urban_roads <- st_as_sf(urban_roads) %>%
#  st_transform(crs = "+init=epsg:32634") 
#
#
#sf_urban_roads[,vars] <- NA
#
#sf_urban_roads.int <- st_intersection(sf_urban_roads, sf.grid.5km) %>%
#  select(.,vars) %>%
#  mutate(Length = st_length(.))

source.1A3bii_U$sources$lines <- sf_urban_roads.int

sf.1A3bii_U <- corsum2sf_lines(source.1A3bii_U, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bii_U %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 16: sf.1A3bii_U',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bii_U <- sf.1A3bii_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bii_U <- source.1A3bii_U[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bii_U, total.1A3bii_U, data.frame(total.1A3bii_U - sum.1A3bii_U))) %>%
  datatable(., caption = 'Table 17: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_LengthUrban <- sum(sf.1A3bii_U$Length)
diff.1A3bii_U <- data.frame(total.1A3bii_U - sum.1A3bii_U)
sf.1A3bii_U <- sf.1A3bii_U %>%
  mutate(NOx = ((diff.1A3bii_U$NOx/sum_LengthUrban)*Length),
         SO2 = ((diff.1A3bii_U$SO2/sum_LengthUrban)*Length),
         PM10 = ((diff.1A3bii_U$PM10/sum_LengthUrban)*Length),
         PM2.5 = ((diff.1A3bii_U$PM2.5/sum_LengthUrban)*Length),
         NMVOC = ((diff.1A3bii_U$NMVOC/sum_LengthUrban)*Length),
         NH3 = ((diff.1A3bii_U$NH3/sum_LengthUrban)*Length))
sf.1A3bii_U %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bii_U <- sf.grid.5km %>%
  st_join(sf.1A3bii_U, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3bii_U, layer.name.1 = "Sources 1A3bii_U", sf.spatialised = p.1A3bii_U, layer.name.2 = "Spatialised 1A3bii_U", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bii_U <- p.1A3bii_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bii_U, total.1A3bii_U, data.frame(sum.p.1A3bii_U == total.1A3bii_U)-1)) %>%
  datatable(., caption = 'Table 18: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3bii_U, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bii_U.gpkg", layer='1A3bii_U')


#' 
#' 
#' 
#' 
#'
#' ### Rural transport
#'
#+ include = FALSE
source.1A3bii_R <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bii_R$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.1A3bii_R$total$inventory <- readxl::read_xlsx(path = source.file, range = "D74:I74", sheet = source.sheet, col_names = vars)


sf_roads <- rbind(rn.IIA ,rn.IB)

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]
sf_roads %<>% sf::st_transform(4326)
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)

source.1A3bii_R$sources$lines <- sf_roads.int

sf.1A3bii_R <- corsum2sf_lines(source.1A3bii_R, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bii_R %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 19: sf.1A3bii_R',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bii_R <- sf.1A3bii_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bii_R <- source.1A3bii_R[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bii_R, total.1A3bii_R, data.frame(total.1A3bii_R - sum.1A3bii_R))) %>%
  datatable(., caption = 'Table 20: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3bii_R$PGDSL)
diff.1A3bii_R <- data.frame(total.1A3bii_R - sum.1A3bii_R)
sf.1A3bii_R <- sf.1A3bii_R %>%
  mutate(NOx = ((diff.1A3bii_R$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3bii_R$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3bii_R$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3bii_R$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3bii_R$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3bii_R$NH3/sum_PGDSL)*PGDSL))
sf.1A3bii_R %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bii_R <- sf.grid.5km %>%
  st_join(sf.1A3bii_R, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3bii_R, layer.name.1 = "Sources 1A3bii_R", sf.spatialised = p.1A3bii_R, layer.name.2 = "Spatialised 1A3bii_R", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bii_R <- p.1A3bii_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bii_R, total.1A3bii_R, data.frame(sum.p.1A3bii_R == total.1A3bii_R)-1)) %>%
  datatable(., caption = 'Table 21: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3bii_R, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bii_R.gpkg", layer='1A3bii_R')

#'
#'
#'
#' 
#' ### Highways transport
#' 
#+ include = FALSE, echo = FALSE, result = FALSE

load("./Data/Putevi/rn_IA.RDS")

rn.IA$PGDS_2015.est[is.na(rn.IA$PGDS_2015.est)] <- mean(rn.IA$PGDS_2015[rn.IA$Broj_puta == "A1" & !is.na(rn.IA$PGDS_2015)])


source.1A3bii_H <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bii_H$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.1A3bii_H$total$inventory <- readxl::read_xlsx(path = source.file, range = "D75:I75", sheet = source.sheet, col_names = vars)

sf_roads <- rn.IA

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]
sf_roads %<>% sf::st_transform(4326)
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)


source.1A3bii_H$sources$lines <- sf_roads.int

sf.1A3bii_H <- corsum2sf_lines(source.1A3bii_H, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bii_H %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 22: sf.1A3bii_H',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bii_H <- sf.1A3bii_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bii_H <- source.1A3bii_H[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bii_H, total.1A3bii_H, data.frame(total.1A3bii_H - sum.1A3bii_H))) %>%
  datatable(., caption = 'Table 23: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3bii_H$PGDSL)
diff.1A3bii_H <- data.frame(total.1A3bii_H - sum.1A3bii_H)
sf.1A3bii_H <- sf.1A3bii_H %>%
  mutate(NOx = ((diff.1A3bii_H$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3bii_H$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3bii_H$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3bii_H$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3bii_H$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3bii_H$NH3/sum_PGDSL)*PGDSL))
sf.1A3bii_H %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bii_H <- sf.grid.5km %>%
  st_join(sf.1A3bii_H, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3bii_H, layer.name.1 = "Sources 1A3bii_H", sf.spatialised = p.1A3bii_H, layer.name.2 = "Spatialised 1A3bii_H", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bii_H <- p.1A3bii_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bii_H, total.1A3bii_H, data.frame(sum.p.1A3bii_H == total.1A3bii_H)-1)) %>%
  datatable(., caption = 'Table 24: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3bii_H, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bii_H.gpkg", layer='1A3bii_H')

#'
#'
#' ## 1A3biii-Road transport: Heavy-duty vehicles
#' 
#' 
#' ### Urban transport
#+ include = FALSE
source.1A3biii_U <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3biii_U$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D85:I85", sheet = source.sheet, col_names = vars)
source.1A3biii_U$total$inventory <- readxl::read_xlsx(path = source.file, range = "D86:I86", sheet = source.sheet, col_names = vars)

#+ include = FALSE
#urban_roads <- readOGR("Data/putevi/OSM_putevi_urbana_podrucja.gpkg", 
#                       use_iconv=TRUE,  
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#sf_urban_roads <- st_as_sf(urban_roads) %>%
#  st_transform(crs = "+init=epsg:32634") 
#
#
#sf_urban_roads[,vars] <- NA
#
#sf_urban_roads.int <- st_intersection(sf_urban_roads, sf.grid.5km) %>%
#  select(.,vars) %>%
#  mutate(Length = st_length(.))

source.1A3biii_U$sources$lines <- sf_urban_roads.int

sf.1A3biii_U <- corsum2sf_lines(source.1A3biii_U, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3biii_U %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 25: sf.1A3biii_U',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3biii_U <- sf.1A3biii_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3biii_U <- source.1A3biii_U[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3biii_U, total.1A3biii_U, data.frame(total.1A3biii_U - sum.1A3biii_U))) %>%
  datatable(., caption = 'Table 26: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_LengthUrban <- sum(sf.1A3biii_U$Length)
diff.1A3biii_U <- data.frame(total.1A3biii_U - sum.1A3biii_U)
sf.1A3biii_U <- sf.1A3biii_U %>%
  mutate(NOx = ((diff.1A3biii_U$NOx/sum_LengthUrban)*Length),
         SO2 = ((diff.1A3biii_U$SO2/sum_LengthUrban)*Length),
         PM10 = ((diff.1A3biii_U$PM10/sum_LengthUrban)*Length),
         PM2.5 = ((diff.1A3biii_U$PM2.5/sum_LengthUrban)*Length),
         NMVOC = ((diff.1A3biii_U$NMVOC/sum_LengthUrban)*Length),
         NH3 = ((diff.1A3biii_U$NH3/sum_LengthUrban)*Length))
sf.1A3biii_U %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3biii_U <- sf.grid.5km %>%
  st_join(sf.1A3biii_U, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3biii_U, layer.name.1 = "Sources 1A3biii_U", sf.spatialised = p.1A3biii_U, layer.name.2 = "Spatialised 1A3biii_U", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3biii_U <- p.1A3biii_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3biii_U, total.1A3biii_U, data.frame(sum.p.1A3biii_U == total.1A3biii_U)-1)) %>%
  datatable(., caption = 'Table 27: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3biii_U, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3biii_U.gpkg", layer='1A3biii_U')


#' 
#' 
#' 
#' 
#'
#' ### Rural transport
#'
#+ include = FALSE
source.1A3biii_R <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3biii_R$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D85:I85", sheet = source.sheet, col_names = vars)
source.1A3biii_R$total$inventory <- readxl::read_xlsx(path = source.file, range = "D87:I87", sheet = source.sheet, col_names = vars)


sf_roads <- rbind(rn.IIA ,rn.IB)

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]

sf_roads %<>% st_transform(4326)
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)

source.1A3biii_R$sources$lines <- sf_roads.int

sf.1A3biii_R <- corsum2sf_lines(source.1A3biii_R, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3biii_R %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 28: sf.1A3biii_R',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3biii_R <- sf.1A3biii_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3biii_R <- source.1A3biii_R[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3biii_R, total.1A3biii_R, data.frame(total.1A3biii_R - sum.1A3biii_R))) %>%
  datatable(., caption = 'Table 29: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3biii_R$PGDSL)
diff.1A3biii_R <- data.frame(total.1A3biii_R - sum.1A3biii_R)
sf.1A3biii_R <- sf.1A3biii_R %>%
  mutate(NOx = ((diff.1A3biii_R$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3biii_R$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3biii_R$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3biii_R$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3biii_R$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3biii_R$NH3/sum_PGDSL)*PGDSL))
sf.1A3biii_R %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3biii_R <- sf.grid.5km %>%
  st_join(sf.1A3biii_R, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3biii_R, layer.name.1 = "Sources 1A3biii_R", sf.spatialised = p.1A3biii_R, layer.name.2 = "Spatialised 1A3biii_R", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3biii_R <- p.1A3biii_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3biii_R, total.1A3biii_R, data.frame(sum.p.1A3biii_R == total.1A3biii_R)-1)) %>%
  datatable(., caption = 'Table 30: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3biii_R, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3biii_R.gpkg", layer='1A3biii_R')

#'
#'
#'
#' 
#' ### Highways transport
#' 
#+ include = FALSE, echo = FALSE, result = FALSE

load("./Data/Putevi/rn_IA.RDS")

rn.IA$PGDS_2015.est[is.na(rn.IA$PGDS_2015.est)] <- mean(rn.IA$PGDS_2015[rn.IA$Broj_puta == "A1" & !is.na(rn.IA$PGDS_2015)])


source.1A3biii_H <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3biii_H$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D85:I85", sheet = source.sheet, col_names = vars)
source.1A3biii_H$total$inventory <- readxl::read_xlsx(path = source.file, range = "D88:I88", sheet = source.sheet, col_names = vars)

sf_roads <- rn.IA

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]

sf_roads %<>% st_transform(4326)
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)


source.1A3biii_H$sources$lines <- sf_roads.int

sf.1A3biii_H <- corsum2sf_lines(source.1A3biii_H, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3biii_H %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 31: sf.1A3biii_H',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3biii_H <- sf.1A3biii_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3biii_H <- source.1A3biii_H[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3biii_H, total.1A3biii_H, data.frame(total.1A3biii_H - sum.1A3biii_H))) %>%
  datatable(., caption = 'Table 32: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3biii_H$PGDSL)
diff.1A3biii_H <- data.frame(total.1A3biii_H - sum.1A3biii_H)
sf.1A3biii_H <- sf.1A3biii_H %>%
  mutate(NOx = ((diff.1A3biii_H$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3biii_H$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3biii_H$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3biii_H$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3biii_H$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3biii_H$NH3/sum_PGDSL)*PGDSL))
sf.1A3biii_H %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3biii_H <- sf.grid.5km %>%
  st_join(sf.1A3biii_H, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3biii_H, layer.name.1 = "Sources 1A3biii_H", sf.spatialised = p.1A3biii_H, layer.name.2 = "Spatialised 1A3biii_H", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3biii_H <- p.1A3biii_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3biii_H, total.1A3biii_H, data.frame(sum.p.1A3biii_H == total.1A3biii_H)-1)) %>%
  datatable(., caption = 'Table 33: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3biii_H, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3biii_H.gpkg", layer='1A3biii_H')




#'
#'
#' ## 1A3biii_bc-Road transport: Buses & Coaches
#' 
#' 
#' ### Urban transport
#' 
#+ include = FALSE
source.1A3biii_bc_U <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3biii_bc_U$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D98:I98", sheet = source.sheet, col_names = vars)
source.1A3biii_bc_U$total$inventory <- readxl::read_xlsx(path = source.file, range = "D99:I99", sheet = source.sheet, col_names = vars)

#+ include = FALSE
#urban_roads <- readOGR("Data/putevi/OSM_putevi_urbana_podrucja.gpkg", 
#                       use_iconv=TRUE,  
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#sf_urban_roads <- st_as_sf(urban_roads) %>%
#  st_transform(crs = "+init=epsg:32634") 
#
#
#sf_urban_roads[,vars] <- NA
#
#sf_urban_roads.int <- st_intersection(sf_urban_roads, sf.grid.5km) %>%
#  select(.,vars) %>%
#  mutate(Length = st_length(.))

source.1A3biii_bc_U$sources$lines <- sf_urban_roads.int

sf.1A3biii_bc_U <- corsum2sf_lines(source.1A3biii_bc_U, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3biii_bc_U %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 34: sf.1A3biii_bc_U',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3biii_bc_U <- sf.1A3biii_bc_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3biii_bc_U <- source.1A3biii_bc_U[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3biii_bc_U, total.1A3biii_bc_U, data.frame(total.1A3biii_bc_U - sum.1A3biii_bc_U))) %>%
  datatable(., caption = 'Table 35: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_LengthUrban <- sum(sf.1A3biii_bc_U$Length)
diff.1A3biii_bc_U <- data.frame(total.1A3biii_bc_U - sum.1A3biii_bc_U)
sf.1A3biii_bc_U <- sf.1A3biii_bc_U %>%
  mutate(NOx = ((diff.1A3biii_bc_U$NOx/sum_LengthUrban)*Length),
         SO2 = ((diff.1A3biii_bc_U$SO2/sum_LengthUrban)*Length),
         PM10 = ((diff.1A3biii_bc_U$PM10/sum_LengthUrban)*Length),
         PM2.5 = ((diff.1A3biii_bc_U$PM2.5/sum_LengthUrban)*Length),
         NMVOC = ((diff.1A3biii_bc_U$NMVOC/sum_LengthUrban)*Length),
         NH3 = ((diff.1A3biii_bc_U$NH3/sum_LengthUrban)*Length))
sf.1A3biii_bc_U %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3biii_bc_U <- sf.grid.5km %>%
  st_join(sf.1A3biii_bc_U, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3biii_bc_U, layer.name.1 = "Sources 1A3biii_bc_U", sf.spatialised = p.1A3biii_bc_U, layer.name.2 = "Spatialised 1A3biii_bc_U", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3biii_bc_U <- p.1A3biii_bc_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3biii_bc_U, total.1A3biii_bc_U, data.frame(sum.p.1A3biii_bc_U == total.1A3biii_bc_U)-1)) %>%
  datatable(., caption = 'Table 36: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3biii_bc_U, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3biii_bc_U.gpkg", layer='1A3biii_bc_U')


#' 
#' 
#' 
#' 
#'
#' ### Rural transport
#'
#+ include = FALSE
source.1A3biii_bc_R <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3biii_bc_R$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D98:I98", sheet = source.sheet, col_names = vars)
source.1A3biii_bc_R$total$inventory <- readxl::read_xlsx(path = source.file, range = "D100:I100", sheet = source.sheet, col_names = vars)


sf_roads <- rbind(rn.IIA ,rn.IB)

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]
sf_roads %<>% st_transform(4326)

sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)

source.1A3biii_bc_R$sources$lines <- sf_roads.int

sf.1A3biii_bc_R <- corsum2sf_lines(source.1A3biii_bc_R, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3biii_bc_R %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 37: sf.1A3biii_bc_R',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3biii_bc_R <- sf.1A3biii_bc_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3biii_bc_R <- source.1A3biii_bc_R[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3biii_bc_R, total.1A3biii_bc_R, data.frame(total.1A3biii_bc_R - sum.1A3biii_bc_R))) %>%
  datatable(., caption = 'Table 38: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3biii_bc_R$PGDSL)
diff.1A3biii_bc_R <- data.frame(total.1A3biii_bc_R - sum.1A3biii_bc_R)
sf.1A3biii_bc_R <- sf.1A3biii_bc_R %>%
  mutate(NOx = ((diff.1A3biii_bc_R$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3biii_bc_R$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3biii_bc_R$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3biii_bc_R$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3biii_bc_R$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3biii_bc_R$NH3/sum_PGDSL)*PGDSL))
sf.1A3biii_bc_R %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3biii_bc_R <- sf.grid.5km %>%
  st_join(sf.1A3biii_bc_R, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3biii_bc_R, layer.name.1 = "Sources 1A3biii_bc_R", sf.spatialised = p.1A3biii_bc_R, layer.name.2 = "Spatialised 1A3biii_bc_R", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3biii_bc_R <- p.1A3biii_bc_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3biii_bc_R, total.1A3biii_bc_R, data.frame(sum.p.1A3biii_bc_R == total.1A3biii_bc_R)-1)) %>%
  datatable(., caption = 'Table 39: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3biii_bc_R, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3biii_bc_R.gpkg", layer='1A3biii_bc_R')

#'
#'
#'
#' 
#' ### Highways transport
#' 
#+ include = FALSE, echo = FALSE, result = FALSE

load("./Data/Putevi/rn_IA.RDS")

rn.IA$PGDS_2015.est[is.na(rn.IA$PGDS_2015.est)] <- mean(rn.IA$PGDS_2015[rn.IA$Broj_puta == "A1" & !is.na(rn.IA$PGDS_2015)])


source.1A3biii_bc_H <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3biii_bc_H$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D98:I98", sheet = source.sheet, col_names = vars)
source.1A3biii_bc_H$total$inventory <- readxl::read_xlsx(path = source.file, range = "D101:I101", sheet = source.sheet, col_names = vars)

sf_roads <- rn.IA

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]
sf_roads %<>% st_transform(4326) 
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)


source.1A3biii_bc_H$sources$lines <- sf_roads.int

sf.1A3biii_bc_H <- corsum2sf_lines(source.1A3biii_bc_H, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3biii_bc_H %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 40: sf.1A3biii_bc_H',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3biii_bc_H <- sf.1A3biii_bc_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3biii_bc_H <- source.1A3biii_bc_H[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3biii_bc_H, total.1A3biii_bc_H, data.frame(total.1A3biii_bc_H - sum.1A3biii_bc_H))) %>%
  datatable(., caption = 'Table 41: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3biii_bc_H$PGDSL)
diff.1A3biii_bc_H <- data.frame(total.1A3biii_bc_H - sum.1A3biii_bc_H)
sf.1A3biii_bc_H <- sf.1A3biii_bc_H %>%
  mutate(NOx = ((diff.1A3biii_bc_H$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3biii_bc_H$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3biii_bc_H$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3biii_bc_H$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3biii_bc_H$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3biii_bc_H$NH3/sum_PGDSL)*PGDSL))
sf.1A3biii_bc_H %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3biii_bc_H <- sf.grid.5km %>%
  st_join(sf.1A3biii_bc_H, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3biii_bc_H, layer.name.1 = "Sources 1A3biii_bc_H", sf.spatialised = p.1A3biii_bc_H, layer.name.2 = "Spatialised 1A3biii_bc_H", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3biii_bc_H <- p.1A3biii_bc_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3biii_bc_H, total.1A3biii_bc_H, data.frame(sum.p.1A3biii_bc_H == total.1A3biii_bc_H)-1)) %>%
  datatable(., caption = 'Table 42: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3biii_bc_H, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3biii_bc_H.gpkg", layer='1A3biii_bc_H')

#'
#'
#' ## 1A3biv-Road transport: Mopeds & motorcycles
#' 
#' 
#' ### Urban transport
#+ include = FALSE
source.1A3biv_U <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3biv_U$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D111:I111", sheet = source.sheet, col_names = vars)
source.1A3biv_U$total$inventory <- readxl::read_xlsx(path = source.file, range = "D112:I112", sheet = source.sheet, col_names = vars)

#+ include = FALSE
#urban_roads <- readOGR("Data/putevi/OSM_putevi_urbana_podrucja.gpkg", 
#                       use_iconv=TRUE,  
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#sf_urban_roads <- st_as_sf(urban_roads) %>%
#  st_transform(crs = "+init=epsg:32634") 
#
#
#sf_urban_roads[,vars] <- NA
#
#sf_urban_roads.int <- st_intersection(sf_urban_roads, sf.grid.5km) %>%
#  select(.,vars) %>%
#  mutate(Length = st_length(.))

source.1A3biv_U$sources$lines <- sf_urban_roads.int

sf.1A3biv_U <- corsum2sf_lines(source.1A3biv_U, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3biv_U %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 43: sf.1A3biv_U',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3biv_U <- sf.1A3biv_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3biv_U <- source.1A3biv_U[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3biv_U, total.1A3biv_U, data.frame(total.1A3biv_U - sum.1A3biv_U))) %>%
  datatable(., caption = 'Table 44: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_LengthUrban <- sum(sf.1A3biv_U$Length)
diff.1A3biv_U <- data.frame(total.1A3biv_U - sum.1A3biv_U)
sf.1A3biv_U <- sf.1A3biv_U %>%
  mutate(NOx = ((diff.1A3biv_U$NOx/sum_LengthUrban)*Length),
         SO2 = ((diff.1A3biv_U$SO2/sum_LengthUrban)*Length),
         PM10 = ((diff.1A3biv_U$PM10/sum_LengthUrban)*Length),
         PM2.5 = ((diff.1A3biv_U$PM2.5/sum_LengthUrban)*Length),
         NMVOC = ((diff.1A3biv_U$NMVOC/sum_LengthUrban)*Length),
         NH3 = ((diff.1A3biv_U$NH3/sum_LengthUrban)*Length))
sf.1A3biv_U %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3biv_U <- sf.grid.5km %>%
  st_join(sf.1A3biv_U, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3biv_U, layer.name.1 = "Sources 1A3biv_U", sf.spatialised = p.1A3biv_U, layer.name.2 = "Spatialised 1A3biv_U", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3biv_U <- p.1A3biv_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3biv_U, total.1A3biv_U, data.frame(sum.p.1A3biv_U == total.1A3biv_U)-1)) %>%
  datatable(., caption = 'Table 45: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3biv_U, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3biv_U.gpkg", layer='1A3biv_U')

#' 
#' 
#' 
#' 
#'
#' ### Rural transport
#'
#+ include = FALSE
source.1A3biv_R <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3biv_R$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D111:I111", sheet = source.sheet, col_names = vars)
source.1A3biv_R$total$inventory <- readxl::read_xlsx(path = source.file, range = "D113:I113", sheet = source.sheet, col_names = vars)


sf_roads <- rbind(rn.IIA ,rn.IB)

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]
sf_roads %<>% st_transform(4326)
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)

source.1A3biv_R$sources$lines <- sf_roads.int

sf.1A3biv_R <- corsum2sf_lines(source.1A3biv_R, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3biv_R %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 46: sf.1A3biv_R',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3biv_R <- sf.1A3biv_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3biv_R <- source.1A3biv_R[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3biv_R, total.1A3biv_R, data.frame(total.1A3biv_R - sum.1A3biv_R))) %>%
  datatable(., caption = 'Table 47: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3biv_R$PGDSL)
diff.1A3biv_R <- data.frame(total.1A3biv_R - sum.1A3biv_R)
sf.1A3biv_R <- sf.1A3biv_R %>%
  mutate(NOx = ((diff.1A3biv_R$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3biv_R$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3biv_R$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3biv_R$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3biv_R$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3biv_R$NH3/sum_PGDSL)*PGDSL))
sf.1A3biv_R %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3biv_R <- sf.grid.5km %>%
  st_join(sf.1A3biv_R, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3biv_R, layer.name.1 = "Sources 1A3biv_R", sf.spatialised = p.1A3biv_R, layer.name.2 = "Spatialised 1A3biv_R", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3biv_R <- p.1A3biv_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3biv_R, total.1A3biv_R, data.frame(sum.p.1A3biv_R == total.1A3biv_R)-1)) %>%
  datatable(., caption = 'Table 48: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3biv_R, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3biv_R.gpkg", layer='1A3biv_R')

#'
#'
#'
#' 
#' ### Highways transport
#' 
#+ include = FALSE, echo = FALSE, result = FALSE

load("./Data/Putevi/rn_IA.RDS")

rn.IA$PGDS_2015.est[is.na(rn.IA$PGDS_2015.est)] <- mean(rn.IA$PGDS_2015[rn.IA$Broj_puta == "A1" & !is.na(rn.IA$PGDS_2015)])


source.1A3biv_H <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3biv_H$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D111:I111", sheet = source.sheet, col_names = vars)
source.1A3biv_H$total$inventory <- readxl::read_xlsx(path = source.file, range = "D114:I114", sheet = source.sheet, col_names = vars)

sf_roads <- rn.IA

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]
sf_roads %<>% st_transform(4326)
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)


source.1A3biv_H$sources$lines <- sf_roads.int

sf.1A3biv_H <- corsum2sf_lines(source.1A3biv_H, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3biv_H %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 49: sf.1A3biv_H',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3biv_H <- sf.1A3biv_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3biv_H <- source.1A3biv_H[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3biv_H, total.1A3biv_H, data.frame(total.1A3biv_H - sum.1A3biv_H))) %>%
  datatable(., caption = 'Table 50: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3biv_H$PGDSL)
diff.1A3biv_H <- data.frame(total.1A3biv_H - sum.1A3biv_H)
sf.1A3biv_H <- sf.1A3biv_H %>%
  mutate(NOx = ((diff.1A3biv_H$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3biv_H$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3biv_H$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3biv_H$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3biv_H$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3biv_H$NH3/sum_PGDSL)*PGDSL))
sf.1A3biv_H %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3biv_H <- sf.grid.5km %>%
  st_join(sf.1A3biv_H, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3biv_H, layer.name.1 = "Sources 1A3biv_H", sf.spatialised = p.1A3biv_H, layer.name.2 = "Spatialised 1A3biv_H", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3biv_H <- p.1A3biv_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3biv_H, total.1A3biv_H, data.frame(sum.p.1A3biv_H == total.1A3biv_H)-1)) %>%
  datatable(., caption = 'Table 51: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3biv_H, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3biv_H.gpkg", layer='1A3biv_H')


#'
#'
#' ## 1A3bv-Road transport: Gasoline evaporation
#' 
#' 
#' ### Urban transport
#' 
#+ include = FALSE
source.1A3bv_U <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bv_U$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D124:I124", sheet = source.sheet, col_names = vars)
source.1A3bv_U$total$inventory <- readxl::read_xlsx(path = source.file, range = "D125:I125", sheet = source.sheet, col_names = vars)

#+ include = FALSE
#urban_roads <- readOGR("Data/putevi/OSM_putevi_urbana_podrucja.gpkg", 
#                       use_iconv=TRUE,  
#                       encoding = "UTF-8",
#                       stringsAsFactors = FALSE)
#sf_urban_roads <- st_as_sf(urban_roads) %>%
#  st_transform(crs = "+init=epsg:32634") 
#
#
#sf_urban_roads[,vars] <- NA
#
#sf_urban_roads.int <- st_intersection(sf_urban_roads, sf.grid.5km) %>%
#  select(.,vars) %>%
#  mutate(Length = st_length(.))

source.1A3bv_U$sources$lines <- sf_urban_roads.int

sf.1A3bv_U <- corsum2sf_lines(source.1A3bv_U, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bv_U %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 52: sf.1A3bv_U',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bv_U <- sf.1A3bv_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bv_U <- source.1A3bv_U[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bv_U, total.1A3bv_U, data.frame(total.1A3bv_U - sum.1A3bv_U))) %>%
  datatable(., caption = 'Table 53: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_LengthUrban <- sum(sf.1A3bv_U$Length)
diff.1A3bv_U <- data.frame(total.1A3bv_U - sum.1A3bv_U)
sf.1A3bv_U <- sf.1A3bv_U %>%
  mutate(NOx = ((diff.1A3bv_U$NOx/sum_LengthUrban)*Length),
         SO2 = ((diff.1A3bv_U$SO2/sum_LengthUrban)*Length),
         PM10 = ((diff.1A3bv_U$PM10/sum_LengthUrban)*Length),
         PM2.5 = ((diff.1A3bv_U$PM2.5/sum_LengthUrban)*Length),
         NMVOC = ((diff.1A3bv_U$NMVOC/sum_LengthUrban)*Length),
         NH3 = ((diff.1A3bv_U$NH3/sum_LengthUrban)*Length))
sf.1A3bv_U %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bv_U <- sf.grid.5km %>%
  st_join(sf.1A3bv_U, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3bv_U, layer.name.1 = "Sources 1A3bv_U", sf.spatialised = p.1A3bv_U, layer.name.2 = "Spatialised 1A3bv_U", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bv_U <- p.1A3bv_U %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bv_U, total.1A3bv_U, data.frame(sum.p.1A3bv_U == total.1A3bv_U)-1)) %>%
  datatable(., caption = 'Table 54: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3bv_U, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bv_U.gpkg", layer='1A3bv_U')

#' 
#' 
#' 
#' 
#'
#' ### Rural transport
#'
#+ include = FALSE
source.1A3bv_R <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bv_R$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D124:I124", sheet = source.sheet, col_names = vars)
source.1A3bv_R$total$inventory <- readxl::read_xlsx(path = source.file, range = "D126:I126", sheet = source.sheet, col_names = vars)


sf_roads <- rbind(rn.IIA ,rn.IB)

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]
sf_roads %<>% st_transform(4326)
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)

source.1A3bv_R$sources$lines <- sf_roads.int

sf.1A3bv_R <- corsum2sf_lines(source.1A3bv_R, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bv_R %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 55: sf.1A3bv_R',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bv_R <- sf.1A3bv_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bv_R <- source.1A3bv_R[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bv_R, total.1A3bv_R, data.frame(total.1A3bv_R - sum.1A3bv_R))) %>%
  datatable(., caption = 'Table 56: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3bv_R$PGDSL)
diff.1A3bv_R <- data.frame(total.1A3bv_R - sum.1A3bv_R)
sf.1A3bv_R <- sf.1A3bv_R %>%
  mutate(NOx = ((diff.1A3bv_R$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3bv_R$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3bv_R$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3bv_R$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3bv_R$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3bv_R$NH3/sum_PGDSL)*PGDSL))
sf.1A3bv_R %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bv_R <- sf.grid.5km %>%
  st_join(sf.1A3bv_R, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3bv_R, layer.name.1 = "Sources 1A3bv_R", sf.spatialised = p.1A3bv_R, layer.name.2 = "Spatialised 1A3bv_R", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bv_R <- p.1A3bv_R %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bv_R, total.1A3bv_R, data.frame(sum.p.1A3bv_R == total.1A3bv_R)-1)) %>%
  datatable(., caption = 'Table 57: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ include = FALSE
# st_write(p.1A3bv_R, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bv_R.gpkg", layer='1A3bv_R')

#'
#'
#'
#' 
#' ### Highways transport
#' 
#+ include = FALSE, echo = FALSE, result = FALSE

load("./Data/Putevi/rn_IA.RDS")

rn.IA$PGDS_2015.est[is.na(rn.IA$PGDS_2015.est)] <- mean(rn.IA$PGDS_2015[rn.IA$Broj_puta == "A1" & !is.na(rn.IA$PGDS_2015)])


source.1A3bv_H <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bv_H$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D124:I124", sheet = source.sheet, col_names = vars)
source.1A3bv_H$total$inventory <- readxl::read_xlsx(path = source.file, range = "D127:I127", sheet = source.sheet, col_names = vars)

sf_roads <- rn.IA

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]
sf_roads %<>% st_transform(4326)
sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)


source.1A3bv_H$sources$lines <- sf_roads.int

sf.1A3bv_H <- corsum2sf_lines(source.1A3bv_H, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bv_H %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 58: sf.1A3bv_H',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bv_H <- sf.1A3bv_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bv_H <- source.1A3bv_H[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bv_H, total.1A3bv_H, data.frame(total.1A3bv_H - sum.1A3bv_H))) %>%
  datatable(., caption = 'Table 59: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_PGDSL <- sum(sf.1A3bv_H$PGDSL)
diff.1A3bv_H <- data.frame(total.1A3bv_H - sum.1A3bv_H)
sf.1A3bv_H <- sf.1A3bv_H %>%
  mutate(NOx = ((diff.1A3bv_H$NOx/sum_PGDSL)*PGDSL),
         SO2 = ((diff.1A3bv_H$SO2/sum_PGDSL)*PGDSL),
         PM10 = ((diff.1A3bv_H$PM10/sum_PGDSL)*PGDSL),
         PM2.5 = ((diff.1A3bv_H$PM2.5/sum_PGDSL)*PGDSL),
         NMVOC = ((diff.1A3bv_H$NMVOC/sum_PGDSL)*PGDSL),
         NH3 = ((diff.1A3bv_H$NH3/sum_PGDSL)*PGDSL))
sf.1A3bv_H %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bv_H <- sf.grid.5km %>%
  st_join(sf.1A3bv_H, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3bv_H, layer.name.1 = "Sources 1A3bv_H", sf.spatialised = p.1A3bv_H, layer.name.2 = "Spatialised 1A3bv_H", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bv_H <- p.1A3bv_H %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bv_H, total.1A3bv_H, data.frame(sum.p.1A3bv_H == total.1A3bv_H)-1)) %>%
  datatable(., caption = 'Table 60: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3bv_H, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bv_H.gpkg", layer='1A3bv_H')


#'
#'
#' ## 1A3bvi-Road transport: Automobile tyre and brake wear
#' 
#' 
#' ### Urban transport
#+ include = FALSE
source.1A3bvi <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bvi$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D137:I137", sheet = source.sheet, col_names = vars)
source.1A3bvi$total$inventory <- readxl::read_xlsx(path = source.file, range = "D138:I138", sheet = source.sheet, col_names = vars)

roads <- readOGR("Data/putevi/Saobracajne_deonice_i_odseci_sa_brojaca.shp", 
                 use_iconv=TRUE,  
                 encoding = "UTF-8",
                 stringsAsFactors = FALSE)
sf_roads <- st_as_sf(roads) %>%
  st_transform(crs = "+init=epsg:32634") 


sf_roads[,vars] <- NA
sf_roads %<>% st_transform(4326)
roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars)%>%
  dplyr::mutate(Length = st_length(.)) %>%
  dplyr::select(.,vars, Length)

source.1A3bvi$sources$lines <- roads.int
sf.1A3bvi <- corsum2sf_lines(source.1A3bvi, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bvi %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 61: sf.1A3bvi',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bvi <- sf.1A3bvi %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bvi <- source.1A3bvi[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bvi, total.1A3bvi, data.frame(total.1A3bvi - sum.1A3bvi))) %>%
  datatable(., caption = 'Table 62: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_Length <- sum(sf.1A3bvi$Length)
diff.1A3bvi <- data.frame(total.1A3bvi - sum.1A3bvi)
sf.1A3bvi <- sf.1A3bvi %>%
  mutate(NOx = ((diff.1A3bvi$NOx/sum_Length)*Length),
         SO2 = ((diff.1A3bvi$SO2/sum_Length)*Length),
         PM10 = ((diff.1A3bvi$PM10/sum_Length)*Length),
         PM2.5 = ((diff.1A3bvi$PM2.5/sum_Length)*Length),
         NMVOC = ((diff.1A3bvi$NMVOC/sum_Length)*Length),
         NH3 = ((diff.1A3bvi$NH3/sum_PGDSL)*Length))
sf.1A3bvi %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bvi <- sf.grid.5km %>%
  st_join(sf.1A3bvi, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3bvi, layer.name.1 = "Sources 1A3bvi", sf.spatialised = p.1A3bvi, layer.name.2 = "Spatialised 1A3bvi", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bvi <- p.1A3bvi %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bvi, total.1A3bvi, data.frame(sum.p.1A3bvi == total.1A3bvi)-1)) %>%
  datatable(., caption = 'Table 63: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3bvi, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bvi.gpkg", layer='1A3bvi')


#'
#'
#' ## 1A3bvii-Road transport: Automobile road abrasion
#' 
#' 
#' ### Urban transport
#+ include = FALSE
source.1A3bvii <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bvii$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D148:I148", sheet = source.sheet, col_names = vars)
source.1A3bvii$total$inventory <- readxl::read_xlsx(path = source.file, range = "D149:I149", sheet = source.sheet, col_names = vars)

source.1A3bvii$sources$lines <- roads.int
sf.1A3bvii <- corsum2sf_lines(source.1A3bvii, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3bvii %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 64: sf.1A3bvii',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3bvii <- sf.1A3bvii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3bvii <- source.1A3bvii[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3bvii, total.1A3bvii, data.frame(total.1A3bvii - sum.1A3bvii))) %>%
  datatable(., caption = 'Table 65: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_Length <- sum(sf.1A3bvii$Length)
diff.1A3bvii <- data.frame(total.1A3bvii - sum.1A3bvii)
sf.1A3bvii <- sf.1A3bvii %>%
  mutate(NOx = ((diff.1A3bvii$NOx/sum_Length)*Length),
         SO2 = ((diff.1A3bvii$SO2/sum_Length)*Length),
         PM10 = ((diff.1A3bvii$PM10/sum_Length)*Length),
         PM2.5 = ((diff.1A3bvii$PM2.5/sum_Length)*Length),
         NMVOC = ((diff.1A3bvii$NMVOC/sum_Length)*Length),
         NH3 = ((diff.1A3bvii$NH3/sum_PGDSL)*Length))
sf.1A3bvii %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3bvii <- sf.grid.5km %>%
  st_join(sf.1A3bvii, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3bvii, layer.name.1 = "Sources 1A3bvii", sf.spatialised = p.1A3bvii, layer.name.2 = "Spatialised 1A3bvii", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3bvii <- p.1A3bvii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3bvii, total.1A3bvii, data.frame(sum.p.1A3bvii == total.1A3bvii)-1)) %>%
  datatable(., caption = 'Table 66: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3bvii, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3bvii.gpkg", layer='1A3bvii')


#'
#'
#' ## 1A3c-Railways
#'
#'
#'
#+ include = FALSE
source.1A3c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D159:I159", sheet = source.sheet, col_names = vars)
source.1A3c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D160:I160", sheet = source.sheet, col_names = vars)

#+ include = FALSE
railways <- readOGR("Data/pruge/Pruge_osm_32634.shp", 
                 use_iconv=TRUE,  
                 encoding = "UTF-8",
                 stringsAsFactors = FALSE)
sf_railways <- st_as_sf(railways) #%>%
  #st_transform(crs = "+init=epsg:32634") 


sf_railways[,vars] <- NA
sf_railways %<>% st_transform(4326)
sf_railways.int <- st_intersection(sf_railways, sf.grid.5km) %>%
  dplyr::select(.,vars) %>%
  dplyr::mutate(Length = st_length(.))


source.1A3c$sources$lines <- sf_railways.int

sf.1A3c <- corsum2sf_lines(source.1A3c, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3c %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 67: sf.1A3c',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3c <- sf.1A3c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3c <- source.1A3c[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3c, total.1A3c, data.frame(total.1A3c - sum.1A3c))) %>%
  datatable(., caption = 'Table 68: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sum_Length <- sum(sf.1A3c$Length)
diff.1A3c <- data.frame(total.1A3c - sum.1A3c)
sf.1A3c <- sf.1A3c %>%
  mutate(NOx = ((diff.1A3c$NOx/sum_Length)*Length),
         SO2 = ((diff.1A3c$SO2/sum_Length)*Length),
         PM10 = ((diff.1A3c$PM10/sum_Length)*Length),
         PM2.5 = ((diff.1A3c$PM2.5/sum_Length)*Length),
         NMVOC = ((diff.1A3c$NMVOC/sum_Length)*Length),
         NH3 = ((diff.1A3c$NH3/sum_Length)*Length))
sf.1A3c %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3c <- sf.grid.5km %>%
  st_join(sf.1A3c, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.1A3c, layer.name = "Spatialised 1A3c") + mapview(sf.1A3c, layer.name = "Sources 1A3c", color = "red") 
spatialised.mapview(sf.sources = sf.1A3c, layer.name.1 = "Sources 1A3c", sf.spatialised = p.1A3c, layer.name.2 = "Spatialised 1A3c", vars = vars, source.lines = TRUE)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3c <- p.1A3c %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3c, total.1A3c, data.frame(sum.p.1A3c == total.1A3c)-1)) %>%
  datatable(., caption = 'Table 69: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3c, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3c.gpkg", layer='1A3c')

#'
#'
#' ## 1A3dii-National navigation (shipping)
#'
#'
#'
#+ include = FALSE
source.1A3dii <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3dii$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D170:I170", sheet = source.sheet, col_names = vars)
source.1A3dii$total$inventory <- readxl::read_xlsx(path = source.file, range = "D171:I171", sheet = source.sheet, col_names = vars)

#+ include = FALSE
n.navigation <- readOGR("Data/plovni_putevi/Plovni_putevi_32634.shp", 
                    use_iconv=TRUE,  
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)
sf_navigation <- st_as_sf(n.navigation) #%>%
  #st_transform(crs = "+init=epsg:32634") 
sf_navigation[,vars] <- NA
sf_navigation %<>% st_transform(4326)
sf_navigation.int <- st_intersection(sf_navigation, sf.grid.5km) %>%
  dplyr::select(.,vars)
source.1A3dii$sources$polygon <- sf_navigation.int
sf.1A3dii <- corsum2sf_polygon(source.1A3dii, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
# sf.1A3dii %>% 
#   st_drop_geometry() %>% 
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   datatable(., caption = 'Table 70: sf.1A3dii',
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
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A3dii <- sf.1A3dii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3dii <- source.1A3dii[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3dii, total.1A3dii, data.frame(total.1A3dii - sum.1A3dii))) %>%
  datatable(., caption = 'Table 71: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.1A3dii <- sf.1A3dii %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.1A3dii$Area)
diff.1A3dii <- data.frame(total.1A3dii - sum.1A3dii)
sf.1A3dii <- sf.1A3dii %>%
  mutate(NOx = ((diff.1A3dii$NOx/sum_Area)*Area),
         SO2 = ((diff.1A3dii$SO2/sum_Area)*Area),
         PM10 = ((diff.1A3dii$PM10/sum_Area)*Area),
         PM2.5 = ((diff.1A3dii$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.1A3dii$NMVOC/sum_Area)*Area),
         NH3 = ((diff.1A3dii$NH3/sum_Area)*Area))
sf.1A3dii %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3dii <- sf.grid.5km %>%
  st_join(sf.1A3dii, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.1A3dii, layer.name = "Spatialised 1A3dii") + mapview(sf.1A3dii, layer.name = "Sources 1A3dii", col.regions = "red") 
spatialised.mapview(sf.sources = sf.1A3dii, layer.name.1 = "Sources 1A3dii", sf.spatialised = p.1A3dii, layer.name.2 = "Spatialised 1A3dii", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3dii <- p.1A3dii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3dii, total.1A3dii, data.frame(sum.p.1A3dii == total.1A3dii)-1)) %>%
  datatable(., caption = 'Table 72: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3dii, dsn="2030_WAM_A/Products_2030_WAM_A/1A3 - Transport_2030_WAM_A/1A3dii.gpkg", layer='1A3dii')









