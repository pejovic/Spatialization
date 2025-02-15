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
#' # 5-Waste
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
#+ include = TRUE
# Function: 
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
#+ include = TRUE  
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
source.sheet =  "5-Waste"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Grid_Serbia_0.05deg.gpkg")
sf.grid.5km <- st_as_sf(grid.5km)
sf.grid.5km %<>% dplyr::mutate(ID = id)

#'
#'
#' ## 5A-Solid waste disposal on land
#'
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
source.5A <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.5A$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D22:I22", sheet = source.sheet, col_names = vars)
source.5A$total$inventory <- readxl::read_xlsx(path = source.file, range = "D23:I23", sheet = source.sheet, col_names = vars)

#+ include = TRUE, message = FALSE, warning = FALSE
clc_18 <- readOGR("Data/clc/CLC18_RS.shp") # Reading data
sf_clc18 <- st_as_sf(clc_18)
clc132 <- subset(sf_clc18, CODE_18 == "132") %>% # Dump sites
  st_transform(32634)
clc132[,vars] <- NA
clc132 %<>% st_transform(4326)
clc132.int <- st_intersection(clc132, sf.grid.5km) %>% # Intersection with grid cells
  dplyr::select(.,vars)

source.5A$sources$polygon <- clc132.int
sf.5A <- corsum2sf_polygon(source.5A, distribute = FALSE) #%>% # Preparing data for final spatialization
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.5A %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 1: sf.5A',
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
sum.5A <- sf.5A %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.5A <- source.5A[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.5A, total.5A, data.frame(total.5A - sum.5A))) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE

sf.5A <- sf.5A %>% # Calculating polygons area
  mutate(Area = st_area(.))

sum_Area <- sum(sf.5A$Area)
diff.5A <- data.frame(total.5A - sum.5A)
sf.5A <- sf.5A %>% # Calculating weights and distibute data
  mutate(NOx = ((diff.5A$NOx/sum_Area)*Area),
         SO2 = ((diff.5A$SO2/sum_Area)*Area),
         PM10 = ((diff.5A$PM10/sum_Area)*Area),
         PM2.5 = ((diff.5A$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.5A$NMVOC/sum_Area)*Area),
         NH3 = ((diff.5A$NH3/sum_Area)*Area))
sf.5A %<>% dplyr::select(vars)
#'
#'
#+ include = TRUE, message= FALSE, warning = FALSE
p.5A <- sf.grid.5km %>% # Spatialization
  st_join(sf.5A, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
# mapview(p.5A, layer.name = "Spatialised 5A") + mapview(sf.5A, layer.name = "Sources 5A", col.regions = "red") 
spatialised.mapview(sf.sources = sf.5A, layer.name.1 = "Sources 5A", sf.spatialised = p.5A, layer.name.2 = "Spatialised 5A", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.5A <- p.5A %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.5A, total.5A, data.frame(sum.p.5A == total.5A)-1)) %>%
  datatable(., caption = 'Table 3: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.5A, dsn="Products/5 - Waste/5A.gpkg", layer='5A')


#'
#'
#' ## 5C1bv-Cremation
#' 
#+ include = TRUE, message = FALSE, warning = FALSE
source.5C1bv <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.5C1bv$sources$points <- readxl::read_xlsx(path = source.file, range = "D24:S25", sheet = source.sheet, col_names = header)
source.5C1bv$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D34:I34", sheet = source.sheet, col_names = vars)
source.5C1bv$total$inventory <- readxl::read_xlsx(path = source.file, range = "D35:I35", sheet = source.sheet, col_names = vars)

sf.5C1bv <- corsum2sf(source.5C1bv, distribute = TRUE) #%>% # Preparing data for final spatialization
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.5C1bv %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.5C1bv',
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
sum.5C1bv <- sf.5C1bv %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.5C1bv <- source.5C1bv[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.5C1bv, total.5C1bv, data.frame(sum.5C1bv == total.5C1bv)-1)) %>%
  datatable(., caption = 'Table 5: Summary differences',
            options = list(pageLength = 5)
  )

#+ include = TRUE, message = FALSE, warning = FALSE
p.5C1bv <- sf.grid.5km %>% # Spatialization
  st_join(sf.5C1bv) %>%
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))

#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.5C1bv, layer.name.1 = "Sources 5C1bv", sf.spatialised = p.5C1bv, layer.name.2 = "Spatialised 5C1bv", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.5C1bv <- p.5C1bv %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.5C1bv, total.5C1bv, data.frame(sum.p.5C1bv == total.5C1bv)-1)) %>%
  datatable(., caption = 'Table 6: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.5C1bv, dsn="Products/5 - Waste/5C1bv.gpkg", layer='5C1bv')




#'
#'
#' ## 5D1-Domestic wastewater handling
#' 
#+ include = TRUE, message = FALSE, warning = FALSE
source.5D1 <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.5D1$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D46:I46", sheet = source.sheet, col_names = vars)
source.5D1$total$inventory <- readxl::read_xlsx(path = source.file, range = "D47:I47", sheet = source.sheet, col_names = vars)

#+ include = TRUE, message = FALSE, warning = FALSE
Sys.setlocale(locale = 'Serbian (Latin)') # Reading data
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = TRUE, message = FALSE, warning = FALSE
otpadne_vode <- readxl::read_xls(path = "Data/Vodosnabdevanje_2015.xls", sheet = "Vodosnabdevanje")
lcl(loc = "C")
otpadne_vode <- cyr_lat(otpadne_vode)
names(otpadne_vode) <- cyr_lat(names(otpadne_vode)) 


otpadne_vode %<>% as.data.frame() %>%
  #dplyr::mutate_all(~replace(., is.na(.), 0))
  mutate(across(where(is.numeric), tidyr::replace_na, 0))

otpadne_vode$Opština[otpadne_vode$Opština == "Indjija"] <- "Inđija"
otpadne_vode$Opština[otpadne_vode$Opština == "LJubovija"] <- "Ljubovija"
otpadne_vode$Opština[otpadne_vode$Opština == "Mali Idjoš"] <- "Mali Iđoš"
otpadne_vode$Opština[otpadne_vode$Opština == "Savski venac"] <- "Savski Venac"
otpadne_vode$Opština[otpadne_vode$Opština == "Stari grad"] <- "Stari Grad"
otpadne_vode$Opština[otpadne_vode$Opština == "Petrovac na Mlavi"] <- "Petrovac"
otpadne_vode$Opština[otpadne_vode$Opština == "Arandjelovac"] <- "Aranđelovac"
otpadne_vode$Opština[otpadne_vode$Opština == "LJig"] <- "Ljig"
otpadne_vode$Opština[otpadne_vode$Opština == "Žitoradja"] <- "Žitorađa"
otpadne_vode$Opština[otpadne_vode$Opština == "Medvedja"] <- "Medveđa"
sf_opstine$Otpadne_vode <- otpadne_vode$`Ukupne ispuštene otpadne vode`[match(sf_opstine$NAME_2, otpadne_vode$Opština)] # Matching data 

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")

# urbana podrucja iz CORINE
sf_clc18_urb <- sf::st_read("GIS_layers/Urban_areas.gpkg") %>% 
  st_transform(crs = "+init=epsg:32634")

# presek sa poligonima opstina kako bi se dobili manji poligoni
sf_clc18_urb_intCLC <- st_intersection(sf_clc18_urb, sf_opstine)
sf_clc18_urb_intCLC %<>% 
  dplyr::mutate(Area_pol = sf::st_area(.)) %>% 
  units::drop_units(.) %>% 
  dplyr::select(Area_pol) %>%
  dplyr::mutate(IDpol = row_number())

# join atributa tako da manji urban poligoni dobiju odgovorajuce atribute opstine u kojoj se nalaze u celosti
#mapview(sf_clc18_urb_intCLC) + mapview(sf_opstine)
sf_clc18_urb_intCLC_cent <- st_centroid(sf_clc18_urb_intCLC)
sf_clc18_urb_intCLC_1 <-  st_join(sf_clc18_urb_intCLC_cent, 
                                  sf_opstine, 
                                  join = st_within) 

sf_clc18_urb_intCLC$Otpadne_vode <- sf_clc18_urb_intCLC_1$Otpadne_vode[match(sf_clc18_urb_intCLC$IDpol, sf_clc18_urb_intCLC_1$IDpol)]
sf_clc18_urb_intCLC$Opstina <- sf_clc18_urb_intCLC_1$NAME_2[match(sf_clc18_urb_intCLC$IDpol, sf_clc18_urb_intCLC_1$IDpol)]

sf_clc18_urb_intCLC %<>% dplyr::filter(!is.na(Otpadne_vode))

aa <- sf_clc18_urb_intCLC  %>% 
  dplyr::group_by(Opstina) %>%
  dplyr::summarize(Area_by_opstina = sum(Area_pol)) %>%
  dplyr::mutate(Area_by_opstina = Area_by_opstina) %>%  
  dplyr::ungroup()

sf_clc18_urb_intCLC$Area_by_opstina <- aa$Area_by_opstina[match(sf_clc18_urb_intCLC$Opstina, aa$Opstina)]  

#sf_clc18_urb_intCLC %>% dplyr::filter(Opstina == "Bor") %>% dplyr::mutate(suma =  sum(Area_pol))
sf_clc18_urb_intCLC %<>% 
  dplyr::mutate(Otpadne_vode_by_polygon = (Otpadne_vode/Area_by_opstina)*Area_pol)

# Kontrola
# sf_clc18_urb_intCLC %>% dplyr::filter(Opstina == "Bor") %>% dplyr::mutate(suma =  sum(Area_pol), ohssum = sum(Otpadne_vode_by_polygon))
# sf_clc18_urb_intCLC  %>% dplyr::filter(is.na(Otpadne_vode_by_polygon))

sf_clc18_urb_intCLC %<>% 
  dplyr::select(Otpadne_vode_by_polygon, Area_pol, IDpol)
# mapview(sf_clc18_urb_intCLC, zcol = "OHS_by_polygon")

# presek sa poligonima grida

sf_clc18_urb_intCLC_wgs <- sf_clc18_urb_intCLC %>% sf::st_transform(4326)

sf_clc18_urb_intGrid <- st_intersection(sf_clc18_urb_intCLC_wgs, sf.grid.5km)

sf_clc18_urb_intGrid %<>% 
  dplyr::rename(ID_grid = ID) %>%
  dplyr::select(Otpadne_vode_by_polygon, IDpol, ID_grid) 

# Kontrola   
#sf_clc18_urb_intGrid  %>% 
#  dplyr::filter(is.na(ID_grid)) 

sf_clc18_urb_intGrid %<>% 
  dplyr::mutate(Area_by_poly = sf::st_area(.)) %>%
  units::drop_units(.)

bb <- sf_clc18_urb_intGrid %>% 
  dplyr::group_by(IDpol) %>%
  dplyr::summarize(Area_by_grid = sum(Area_by_poly))

sf_clc18_urb_intGrid$Area_by_grid <- bb$Area_by_grid[match(sf_clc18_urb_intGrid$IDpol, bb$IDpol)]
sf_clc18_urb_intGrid %<>% 
  dplyr::mutate(Otpadne_vode_by_grid = (Otpadne_vode_by_polygon/Area_by_grid)*Area_by_poly)

# mapview(sf_clc18_urb_intGrid, zcol = "OHS_by_grid")


sf_clc18_urb_intGrid %<>% 
  dplyr::select(Otpadne_vode_by_grid)

sf_clc18_urb_intGrid[,vars] <- NA


# sf_opstine %<>% dplyr::select(.,Otpadne_vode, NAME_2)
# sf_opstine[,vars] <- NA
# 
# urbana <- sf::st_read("GIS_layers/Urban_areas.gpkg")
# urbana_opstine <- st_join(urbana, sf_opstine, join = st_intersects)
# urbana_opstine %<>% st_transform(4326)
# sf_opstine.int <- st_intersection(urbana_opstine, sf.grid.5km) %>%
#   filter(!is.na(Otpadne_vode))

source.5D1$sources$polygon <- sf_clc18_urb_intGrid
sf.5D1 <- corsum2sf_polygon(source.5D1, distribute = FALSE) #%>% # Preparing data for final spatialization
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.5D1 %>% 
  st_drop_geometry() %>%
  dplyr::rename(Wastewater = Otpadne_vode, Munucipality = NAME_2) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.5D1',
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
sum.5D1 <- sf.5D1 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.5D1 <- source.5D1[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.5D1, total.5D1, data.frame(total.5D1 - sum.5D1))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE

sum_s <- sum(sf.5D1$Otpadne_vode_by_grid)
diff.5D1 <- data.frame(total.5D1 - sum.5D1)
sf.5D1 <- sf.5D1 %>% # Calculate weights and distribute data
  mutate(NOx = ((diff.5D1$NOx/sum_s)*Otpadne_vode_by_grid),
         SO2 = ((diff.5D1$SO2/sum_s)*Otpadne_vode_by_grid),
         PM10 = ((diff.5D1$PM10/sum_s)*Otpadne_vode_by_grid),
         PM2.5 = ((diff.5D1$PM2.5/sum_s)*Otpadne_vode_by_grid),
         NMVOC = ((diff.5D1$NMVOC/sum_s)*Otpadne_vode_by_grid),
         NH3 = ((diff.5D1$NH3/sum_s)*Otpadne_vode_by_grid))
sf.5D1 %<>% dplyr::select(vars)
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
p.5D1 <- sf.grid.5km %>% # Spatialization
  st_join(sf.5D1, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.5D1, layer.name.1 = "Sources 5D1", sf.spatialised = p.5D1, layer.name.2 = "Spatialised 5D1", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.5D1 <- p.5D1 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.5D1, total.5D1, data.frame(sum.p.5D1 == total.5D1)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.5D1, dsn="Products/5 - Waste/5D1.gpkg", layer='5D1')

#'
#'
#' ## 5D2-Industrial wastewater handling
#'
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
source.5D2 <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.5D2$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D58:I58", sheet = source.sheet, col_names = vars)
source.5D2$total$inventory <- readxl::read_xlsx(path = source.file, range = "D59:I59", sheet = source.sheet, col_names = vars)

#+ include = TRUE, message = FALSE, warning = FALSE
#sf.ind <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Industrial_sites_new.gpkg") %>%
#  dplyr::rename(geometry = geom) %>%
#  dplyr::select(geometry)
sf.waste <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/Wastewater_plants_OSM_32634.gpkg") %>%
  dplyr::rename(geometry = geom) %>%
  dplyr::select(geometry)

#sf.final <- rbind(sf.ind, sf.waste)

# Polygon Centroid
sf.waste <- sf.waste %>% dplyr::mutate(Area_Ha = unclass(st_area(.)/10000), SHAPE_Area = unclass(st_area(.)))
sf.waste[,vars] <- NA
sf.waste$ID <- 1:nrow(sf.waste)

sf.waste.oI.cen <- st_centroid(sf.waste) %>%
  dplyr::select(ID, Area_Ha, SHAPE_Area, NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  dplyr::rename(ID_CLC = ID)
sf.waste.oI.cen %<>% st_transform(4326)
sf.waste.oI.cen <- st_join(sf.waste.oI.cen, sf.grid.5km) %>%
  subset(!is.na(ID))

sf.waste.oI.cen %<>% dplyr::select(vars)
source.5D2$sources$points <- sf.waste.oI.cen
sf.5D2 <- corsum2sf_point.sf(source.5D2, distribute = TRUE) #%>%
  #st_transform(crs = "+init=epsg:32634")



#+ include = FALSE
# Distribute values based on area of each polygon and add it to centroids
# total.5D2 <- source.5D2[[2]][[2]][, vars] %>% 
#   mutate_all(~replace(., is.na(.), 0)) %>%
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   as.data.frame()
# 
# sum.5D2 <- sf.waste.oI.cen %>% 
#   st_drop_geometry() %>%
#   dplyr::select(., vars) %>% 
#   apply(., 2, sum) %>% 
#   t(.) %>% 
#   as.data.frame() %>%
#   dplyr::mutate_if(is.numeric, round, 2) %>%
#   mutate_all(~replace(., is.na(.), 0))
# 
# sum_Area <- sum(sf.waste.oI.cen$SHAPE_Area)
# diff.5D2 <- data.frame(total.5D2 - sum.5D2)
# sf.waste.oI.cen <- sf.waste.oI.cen %>%
#   mutate(NOx = ((diff.5D2$NOx/sum_Area)*SHAPE_Area),
#          SO2 = ((diff.5D2$SO2/sum_Area)*SHAPE_Area),
#          PM10 = ((diff.5D2$PM10/sum_Area)*SHAPE_Area),
#          PM2.5 = ((diff.5D2$PM2.5/sum_Area)*SHAPE_Area),
#          NMVOC = ((diff.5D2$NMVOC/sum_Area)*SHAPE_Area),
#          NH3 = ((diff.5D2$NH3/sum_Area)*SHAPE_Area))
# 
# sf.waste.oI.cen %<>% select(vars) 
# sf.5D2 <- sf.waste.oI.cen



#sf.final[, vars] <- NA
#sf.final.int <- st_intersection(sf.final, sf.grid.5km) %>% # Intersection with grid cells
#  dplyr::select(.,vars)

#source.5D2$sources$polygon <- sf.final.int
#sf.5D2 <- corsum2sf_polygon(source.5D2, distribute = FALSE) %>% # Preparing data for final spatialization
#  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.5D2 %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 10: sf.5D2',
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
sum.5D2 <- sf.5D2 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.5D2 <- source.5D2[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.5D2, total.5D2, data.frame(total.5D2 - sum.5D2))) %>%
  datatable(., caption = 'Table 11: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE

# sf.5D2 <- sf.5D2 %>% # Calculate polygons area
#   mutate(Area = st_area(.))
# 
# sum_Area <- sum(sf.5D2$Area) 
# diff.5D2 <- data.frame(total.5D2 - sum.5D2)
# sf.5D2 <- sf.5D2 %>% # Calculate weights and distibute data
#   mutate(NOx = ((diff.5D2$NOx/sum_Area)*Area),
#          SO2 = ((diff.5D2$SO2/sum_Area)*Area),
#          PM10 = ((diff.5D2$PM10/sum_Area)*Area),
#          PM2.5 = ((diff.5D2$PM2.5/sum_Area)*Area),
#          NMVOC = ((diff.5D2$NMVOC/sum_Area)*Area),
#          NH3 = ((diff.5D2$NH3/sum_Area)*Area))
# sf.5D2 %<>% dplyr::select(vars)
#'
#'
#+ include = TRUE, message = FALSE, warning = FALSE
p.5D2 <- sf.grid.5km %>% # Spatialization
  st_join(sf.5D2) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.5D2, layer.name.1 = "Sources 5D2", sf.spatialised = p.5D2, layer.name.2 = "Spatialised 5D2", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.5D2 <- p.5D2 %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.5D2, total.5D2, data.frame(sum.p.5D2 == total.5D2)-1)) %>%
  datatable(., caption = 'Table 12: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.5D2, dsn="Products/5 - Waste/5D2.gpkg", layer='5D2')

