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
#' # Report with geocomputation examples for point, line and polygon data
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

#+ include = TRUE
# Function for preparing point data for spatialization 
# Parameters:
#  - source.list - list with source data and total inventory
#  - distribute - TRUE or FALSE, depend on that if you want to distribute total inventory emissions to sources, when difference exists
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
#+ include = TRUE 
# Function for preparing polygon data for spatialization 
# Parameters:
#  - source.list - list with source data and total inventory
#  - distribute - TRUE or FALSE, depend on that if you want to distribute total inventory emissions to sources, when difference exists

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
#+ include = TRUE
# Function for preparing line data for spatialization 
# Parameters:
#  - source.list - list with source data and total inventory
#  - distribute - TRUE or FALSE, depend on that if you want to distribute total inventory emissions to sources, when difference exists

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

#+ include = TRUE
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
#' # Example - point data
#'
#'
#'
#+ include = TRUE
# Reading source file and grid
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "1A1-Energy"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Polygons_5km_UTM_34N.shp")
sf.grid.5km <- st_as_sf(grid.5km)
#'
#'
#' ## 1A1a - Public heat and electricity production
#'
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
# Reading source data
source.1A1a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1a$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S37", sheet = source.sheet, col_names = header)
source.1A1a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D40:I40", sheet = source.sheet, col_names = vars)
source.1A1a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D46:I46", sheet = source.sheet, col_names = vars)

# Preparing data for final spatialization
sf.1A1a <- corsum2sf(source.1A1a) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
sf.1A1a %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.1A1a',
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
#+ include = TRUE, message = FALSE, warning = FALSE
sum.1A1a <- sf.1A1a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A1a <- source.1A1a[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A1a, total.1A1a, data.frame(sum.1A1a == total.1A1a)-1)) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = TRUE, message = FALSE, warning = FALSE
p.1A1a <- sf.grid.5km %>% # Spatialization by grid cells
  st_join(sf.1A1a) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE), # Sum-up by grid cells
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(sf.1A1a, layer.name = "Sources 1A1a", col.regions = "red") + mapview(p.1A1a)
spatialised.mapview(sf.sources = sf.1A1a, layer.name.1 = "Sources 1A1a", sf.spatialised = p.1A1a, layer.name.2 = "Spatialised 1A1a", vars = vars)

#+ include = TRUE, message = FALSE, warning = FALSE
sum.p.1A1a <- p.1A1a %>% # Computational control
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A1a, total.1A1a, data.frame(sum.p.1A1a == total.1A1a)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )
#+ echo = FALSE, result = TRUE, eval = TRUE
# Export data to geopackage file
# st_write(p.1A1a, dsn="Products/1A1 - Energy/1A1a.gpkg", layer='1A1a')

#'
#' # Example - line data
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
# Reading source file and grid
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "1A3-Transport"
header <- readxl::read_xlsx(path = source.file, range = "D9:S9", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Polygons_5km_UTM_34N.shp")
sf.grid.5km <- st_as_sf(grid.5km) 

#'
#'
#' ## 1A3c-Railways
#'
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
# Reading source data
source.1A3c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A3c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D159:I159", sheet = source.sheet, col_names = vars)
source.1A3c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D160:I160", sheet = source.sheet, col_names = vars)

#+ include = TRUE, message = FALSE, warning = FALSE
# Reading data
railways <- readOGR("Data/pruge/Pruge_osm_32634.shp", 
                    use_iconv=TRUE,  
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)
# Transformation to sf:: class and transformation to UTM reference coordinate system
sf_railways <- st_as_sf(railways) %>%
  st_transform(crs = "+init=epsg:32634") 

# Make new columns
sf_railways[,vars] <- NA
# Intersection with grid cells
sf_railways.int <- st_intersection(sf_railways, sf.grid.5km) %>%
  select(.,vars, osm_id) %>%
  mutate(Length = st_length(.))

# Preparing data for final spatialization
source.1A3c$sources$lines <- sf_railways.int
sf.1A3c <- corsum2sf_lines(source.1A3c, distribute = FALSE)

#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
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
#+ include = TRUE, message = FALSE, warning = FALSE
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
#+ include = TRUE, message = FALSE, warning = FALSE

sum_Length <- sum(sf.1A3c$Length)
diff.1A3c <- data.frame(total.1A3c - sum.1A3c)
sf.1A3c <- sf.1A3c %>% # Calculate weights and distribute values
  mutate(NOx = ((diff.1A3c$NOx/sum_Length)*Length),
         SO2 = ((diff.1A3c$SO2/sum_Length)*Length),
         PM10 = ((diff.1A3c$PM10/sum_Length)*Length),
         PM2.5 = ((diff.1A3c$PM2.5/sum_Length)*Length),
         NMVOC = ((diff.1A3c$NMVOC/sum_Length)*Length),
         NH3 = ((diff.1A3c$NH3/sum_Length)*Length))
sf.1A3c %<>% select(vars)
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
p.1A3c <- sf.grid.5km %>% # Spatialization by grid cells
  st_join(sf.1A3c, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE), # Sum-up by grid cells
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A3c, layer.name.1 = "Sources 1A3c", sf.spatialised = p.1A3c, layer.name.2 = "Spatialised 1A3c", vars = vars, source.lines = TRUE)

#+ include = TRUE, message = FALSE, warning = FALSE
sum.p.1A3c <- p.1A3c %>% # Computational control
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

#+ include = TRUE, message = FALSE, warning = FALSE
# Export data to geopackage file
# st_write(p.1A3c, dsn="Products/1A3 - Transport/1A3c.gpkg", layer='1A3c')


#'
#' # Example - polygon data
#'

#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
# Reading source file and grid
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "1A4-Residential-Tertiary"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Polygons_5km_UTM_34N.shp")
sf.grid.5km <- st_as_sf(grid.5km) 

#'
#'
#' ## 1A4ai - Commercial/Institutional: Stationary Combustion
#'
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
# Reading source data
source.1A4ai <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A4ai$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D18:I18", sheet = source.sheet, col_names = vars)
source.1A4ai$total$inventory <- readxl::read_xlsx(path = source.file, range = "D22:I22", sheet = source.sheet, col_names = vars)

#+ include = TRUE, message = FALSE, warning = FALSE
# Reading data - CLC - Corine Land Cover
clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
# Subset data to get requested CLC class
sf_clc18_urb <- subset(sf_clc18, CODE_18 == "111" | CODE_18 == "112") %>% # CLC urban zones
  st_transform(crs = "+init=epsg:32634")

#+ include = TRUE, message = FALSE, warning = FALSE
# Readinf data - munucipalites polygons
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = TRUE, message = FALSE, warning = FALSE
# Read data from  Official annual report from the public company “Toplane Srbije” - Total_heating values
toplane <- readxl::read_xls(path = "Data/toplane/Toplane_2015.xls") %>%
  mutate(GRAD = str_to_title(GRAD))
sf_opstine$Toplane <- toplane$`Ukupna grejna povrsina (m2)`[match(sf_opstine$NAME_2, toplane$GRAD)] 
sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Join data - from munucipalities ('opstine') to CLC urban class
sf_clc18_urb <- st_join(sf_clc18_urb, sf_opstine, largest = TRUE) 
sf_clc18_urb %<>% dplyr::select(.,Toplane, NAME_2)
sf_clc18_urb[,vars] <- NA

# Intersection with grid cells
sf_clc18_urb.int <- st_intersection(sf_clc18_urb, sf.grid.5km) %>% 
  filter(!is.na(Toplane))

# Preparing data for final spatialization
source.1A4ai$sources$polygon <- sf_clc18_urb.int
sf.1A4ai <- corsum2sf_polygon(source.1A4ai, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
sf.1A4ai %>% 
  st_drop_geometry() %>%
  dplyr::rename(Total_heating_area = Toplane, Manucipality = NAME_2) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.1A4ai',
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
#+ include = TRUE, message = FALSE, warning = FALSE
sum.1A4ai <- sf.1A4ai %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A4ai <- source.1A4ai[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A4ai, total.1A4ai, data.frame(total.1A4ai - sum.1A4ai))) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )

#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE

sum_s <- sum(sf.1A4ai$Toplane)
diff.1A4ai <- data.frame(total.1A4ai - sum.1A4ai)
sf.1A4ai <- sf.1A4ai %>% # Calculate weights and distribute values
  mutate(NOx = ((diff.1A4ai$NOx/sum_s)*Toplane),
         SO2 = ((diff.1A4ai$SO2/sum_s)*Toplane),
         PM10 = ((diff.1A4ai$PM10/sum_s)*Toplane),
         PM2.5 = ((diff.1A4ai$PM2.5/sum_s)*Toplane),
         NMVOC = ((diff.1A4ai$NMVOC/sum_s)*Toplane),
         NH3 = ((diff.1A4ai$NH3/sum_s)*Toplane))
sf.1A4ai %<>% select(vars)
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
p.1A4ai <- sf.grid.5km %>% # Spatialization by grid cells
  st_join(sf.1A4ai, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE), # Sum-up by grid cells
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A4ai, layer.name.1 = "Sources 1A4ai", sf.spatialised = p.1A4ai, layer.name.2 = "Spatialised 1A4ai", vars = vars)

#+ include = TRUE, message = FALSE, warning = FALSE
sum.p.1A4ai <- p.1A4ai %>% # Computational control
  st_drop_geometry() %>% 
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A4ai, total.1A4ai, data.frame(sum.p.1A4ai == total.1A4ai)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = TRUE, message = FALSE, warning = FALSE
# Export data to geopackage file
# st_write(p.1A4ai, dsn="Products/1A4 - Residential-Tertiary/1A4ai.gpkg", layer='1A4ai')

