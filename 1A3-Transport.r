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
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "1A3-Transport"
header <- readxl::read_xlsx(path = source.file, range = "D9:S9", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Polygons_5km_UTM_34N.shp")
sf.grid.5km <- st_as_sf(grid.5km) 


#'
#'
#' ## 1A3ai-International aviation LTO (civil)
#' 
#' 
source.1A3ai_i <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3ai_i$sources$points <- readxl::read_xlsx(path = source.file, range = "D10:S11", sheet = source.sheet, col_names = header)
source.1A3ai_i$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D19:I19", sheet = source.sheet, col_names = vars)
source.1A3ai_i$total$inventory <- readxl::read_xlsx(path = source.file, range = "D20:I20", sheet = source.sheet, col_names = vars)

sf.1A3ai_i <- corsum2sf(source.1A3ai_i, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A3ai_i %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.1A3ai_i',
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
sum.1A3ai_i <- sf.1A3ai_i %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3ai_i <- source.1A3ai_i[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3ai_i, total.1A3ai_i, data.frame(sum.1A3ai_i == total.1A3ai_i)-1)) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3ai_i <- sf.grid.5km %>%
  st_join(sf.1A3ai_i) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3ai_i, layer.name.1 = "Sources 1A3ai_i", sf.spatialised = p.1A3ai_i, layer.name.2 = "Spatialised 1A3ai_i", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3ai_i <- p.1A3ai_i %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3ai_i, total.1A3ai_i, data.frame(sum.p.1A3ai_i == total.1A3ai_i)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3ai_i, dsn="Products/1A3 - Transport/1A3ai_i.gpkg", layer='1A3ai')



#'
#'
#' ## 1A3aii-Domestic aviation LTO (civil)
#' 
#' 
source.1A3aii <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3ai_i$sources$points <- readxl::read_xlsx(path = source.file, range = "D10:S11", sheet = source.sheet, col_names = header)
source.1A3ai_i$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D19:I19", sheet = source.sheet, col_names = vars)
source.1A3ai_i$total$inventory <- readxl::read_xlsx(path = source.file, range = "D20:I20", sheet = source.sheet, col_names = vars)

sf.1A3ai_i <- corsum2sf(source.1A3ai_i, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A3ai_i %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.1A3ai_i',
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
sum.1A3ai_i <- sf.1A3ai_i %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A3ai_i <- source.1A3ai_i[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A3ai_i, total.1A3ai_i, data.frame(sum.1A3ai_i == total.1A3ai_i)-1)) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = FALSE, echo = FALSE, result = FALSE
p.1A3ai_i <- sf.grid.5km %>%
  st_join(sf.1A3ai_i) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3ai_i, layer.name.1 = "Sources 1A3ai_i", sf.spatialised = p.1A3ai_i, layer.name.2 = "Spatialised 1A3ai_i", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A3ai_i <- p.1A3ai_i %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A3ai_i, total.1A3ai_i, data.frame(sum.p.1A3ai_i == total.1A3ai_i)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3ai_i, dsn="Products/1A3 - Transport/1A3ai_i.gpkg", layer='1A3ai_i')















#'
#'
#' ## 1A3bi-Road Transport: Passengers cars
#' 
#' 
#' ### Urban transport
#' 
source.1A3bi_U <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bi_U$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D41:I41", sheet = source.sheet, col_names = vars)
source.1A3bi_U$total$inventory <- readxl::read_xlsx(path = source.file, range = "D42:I42", sheet = source.sheet, col_names = vars)

#+ include = FALSE
urban_roads <- readOGR("Data/putevi/OSM_putevi_urbana_podrucja.gpkg", 
                    use_iconv=TRUE,  
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)
sf_urban_roads <- st_as_sf(urban_roads) %>%
  st_transform(crs = "+init=epsg:32634") 


sf_urban_roads[,vars] <- NA

sf_urban_roads.int <- st_intersection(sf_urban_roads, sf.grid.5km) %>%
  select(.,vars) %>%
  mutate(Length = st_length(.))


source.1A3bi_U$sources$lines <- sf_urban_roads.int

sf.1A3bi_U <- corsum2sf_lines(source.1A3bi_U, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A3bi_U %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.1A3bi_U',
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
  datatable(., caption = 'Table 5: Summary differences',
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
sf.1A3bi_U %<>% select(vars)
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
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#' 
#' 
#' 
#' 
#'
#' ### Rural transport
#'
#+ include = FALSE
source.1A3bi_R <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3bi_R$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D41:I41", sheet = source.sheet, col_names = vars)
source.1A3bi_R$total$inventory <- readxl::read_xlsx(path = source.file, range = "D43:I43", sheet = source.sheet, col_names = vars)


load("./Data/Putevi/rn_IIA.RDS")
load("./Data/Putevi/rn_IB.RDS")


sf_roads <- rbind(rn.IIA ,rn.IB)

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]

sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)
  

source.1A3bi_R$sources$lines <- sf_roads.int

sf.1A3bi_R <- corsum2sf_lines(source.1A3bi_R, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A3bi_R %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.1A3bi_R',
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
  datatable(., caption = 'Table 5: Summary differences',
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
sf.1A3bi_R %<>% select(vars)
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
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

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

source.1A3bi_H$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D41:I41", sheet = source.sheet, col_names = vars)
source.1A3bi_H$total$inventory <- readxl::read_xlsx(path = source.file, range = "D44:I44", sheet = source.sheet, col_names = vars)

sf_roads <- rn.IA

sf_roads[,vars] <- NA

sf_roads <- dplyr::mutate(sf_roads, PGDS = PGDS_2015.est)
sf_roads$PGDS[sf_roads$is.PGDS] <- sf_roads$PGDS_2015[sf_roads$is.PGDS]

sf_roads.int <- st_intersection(sf_roads, sf.grid.5km) %>%
  dplyr::select(.,vars, PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(.,vars, Length, PGDSL)


source.1A3bi_H$sources$lines <- sf_roads.int

sf.1A3bi_H <- corsum2sf_lines(source.1A3bi_H, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A3bi_H %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.1A3bi_H',
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
  datatable(., caption = 'Table 5: Summary differences',
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
sf.1A3bi_H %<>% select(vars)
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
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )


































#'
#'
#' ## 1A3c-Railways
#'
#'
#'
#+ include = FALSE
source.1A3c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D141:I141", sheet = source.sheet, col_names = vars)
source.1A3c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D142:I142", sheet = source.sheet, col_names = vars)

#+ include = FALSE
railways <- readOGR("Data/pruge/Pruge_osm_32634.shp", 
                 use_iconv=TRUE,  
                 encoding = "UTF-8",
                 stringsAsFactors = FALSE)
sf_railways <- st_as_sf(railways) %>%
  st_transform(crs = "+init=epsg:32634") 


sf_railways[,vars] <- NA

sf_railways.int <- st_intersection(sf_railways, sf.grid.5km) %>%
  select(.,vars) %>%
  mutate(Length = st_length(.))


source.1A3c$sources$lines <- sf_railways.int

sf.1A3c <- corsum2sf_lines(source.1A3c, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A3c %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.1A3c',
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
  datatable(., caption = 'Table 5: Summary differences',
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
sf.1A3c %<>% select(vars)
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
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3c, dsn="Products/1A3 - Transport/1A3c.gpkg", layer='1A3c')

#'
#'
#' ## 1A3dii-National navigation (shipping)
#'
#'
#'
#+ include = FALSE
source.1A3dii <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3dii$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D152:I152", sheet = source.sheet, col_names = vars)
source.1A3dii$total$inventory <- readxl::read_xlsx(path = source.file, range = "D153:I153", sheet = source.sheet, col_names = vars)

#+ include = FALSE
n.navigation <- readOGR("Data/plovni_putevi/Plovni_putevi_32634.shp", 
                    use_iconv=TRUE,  
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)
sf_navigation <- st_as_sf(n.navigation) %>%
  st_transform(crs = "+init=epsg:32634") 
sf_navigation[,vars] <- NA
sf_navigation.int <- st_intersection(sf_navigation, sf.grid.5km) %>%
  select(.,vars)
source.1A3dii$sources$polygon <- sf_navigation.int
sf.1A3dii <- corsum2sf_polygon(source.1A3dii, distribute = FALSE)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A3dii %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.1A3dii',
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
  datatable(., caption = 'Table 2: Summary differences',
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
sf.1A3dii %<>% select(vars)
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
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A3dii, dsn="Products/1A3 - Transport/1A3dii.gpkg", layer='1A3dii')









