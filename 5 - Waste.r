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
source.file = "Pollutant inventory spatialized-d30102019.xlsx"
source.sheet =  "5-Waste"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Polygons_5km_UTM_34N.shp")
sf.grid.5km <- st_as_sf(grid.5km) 


#'
#'
#' ## 5A-Solid waste disposal on land
#'
#'
#'
#+ include = FALSE
source.5A <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.5A$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D22:I22", sheet = source.sheet, col_names = vars)
source.5A$total$inventory <- readxl::read_xlsx(path = source.file, range = "D23:I23", sheet = source.sheet, col_names = vars)

#+ include = FALSE
clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
clc132 <- subset(sf_clc18, CODE_18 == "132") %>% # Dump sites
  st_transform(32634)
clc132[,vars] <- NA

clc132.int <- st_intersection(clc132, sf.grid.5km) %>%
  select(.,vars)

source.5A$sources$polygon <- clc132.int
sf.5A <- corsum2sf_polygon(source.5A, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

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
#+ include = FALSE, echo = FALSE, result = FALSE

sf.5A <- sf.5A %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.5A$Area)
diff.5A <- data.frame(total.5A - sum.5A)
sf.5A <- sf.5A %>%
  mutate(NOx = ((diff.5A$NOx/sum_Area)*Area),
         SO2 = ((diff.5A$SO2/sum_Area)*Area),
         PM10 = ((diff.5A$PM10/sum_Area)*Area),
         PM2.5 = ((diff.5A$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.5A$NMVOC/sum_Area)*Area),
         NH3 = ((diff.5A$NH3/sum_Area)*Area))
sf.5A %<>% select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.5A <- sf.grid.5km %>%
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
#+ include = FALSE
source.5C1bv <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.5C1bv$sources$points <- readxl::read_xlsx(path = source.file, range = "D24:S25", sheet = source.sheet, col_names = header)
source.5C1bv$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D34:I34", sheet = source.sheet, col_names = vars)
source.5C1bv$total$inventory <- readxl::read_xlsx(path = source.file, range = "D35:I35", sheet = source.sheet, col_names = vars)

sf.5C1bv <- corsum2sf(source.5C1bv, distribute = TRUE) %>%
  st_transform(crs = "+init=epsg:32634")

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

#+ include = FALSE, echo = FALSE, result = FALSE
p.5C1bv <- sf.grid.5km %>%
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
#+ include = FALSE
source.5D1 <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.5D1$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D46:I46", sheet = source.sheet, col_names = vars)
source.5D1$total$inventory <- readxl::read_xlsx(path = source.file, range = "D47:I47", sheet = source.sheet, col_names = vars)

#+ include = FALSE
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
otpadne_vode <- readxl::read_xls(path = "Data/Vodosnabdevanje_2015.xls", sheet = "Vodosnabdevanje")
lcl(loc = "C")
otpadne_vode <- cyr_lat(otpadne_vode)
names(otpadne_vode) <- cyr_lat(names(otpadne_vode)) 
otpadne_vode <- otpadne_vode %>%
  mutate_all(~replace_na(., 0))
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
sf_opstine$Otpadne_vode <- otpadne_vode$`Ukupne ispuštene otpadne vode`[match(sf_opstine$NAME_2, otpadne_vode$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")
sf_opstine %<>% dplyr::select(.,Otpadne_vode, NAME_2)
sf_opstine[,vars] <- NA

sf_opstine.int <- st_intersection(sf_opstine, sf.grid.5km) %>%
  filter(!is.na(Otpadne_vode))

source.5D1$sources$polygon <- sf_opstine.int
sf.5D1 <- corsum2sf_polygon(source.5D1, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.5D1 %>% 
  st_drop_geometry() %>%
  dplyr::rename(Wastewater = Otpadne_vode) %>%
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
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.5D1$Otpadne_vode)
diff.5D1 <- data.frame(total.5D1 - sum.5D1)
sf.5D1 <- sf.5D1 %>%
  mutate(NOx = ((diff.5D1$NOx/sum_s)*Otpadne_vode),
         SO2 = ((diff.5D1$SO2/sum_s)*Otpadne_vode),
         PM10 = ((diff.5D1$PM10/sum_s)*Otpadne_vode),
         PM2.5 = ((diff.5D1$PM2.5/sum_s)*Otpadne_vode),
         NMVOC = ((diff.5D1$NMVOC/sum_s)*Otpadne_vode),
         NH3 = ((diff.5D1$NH3/sum_s)*Otpadne_vode))
sf.5D1 %<>% select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.5D1 <- sf.grid.5km %>%
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
#+ include = FALSE
source.5D2 <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.5D2$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D58:I58", sheet = source.sheet, col_names = vars)
source.5D2$total$inventory <- readxl::read_xlsx(path = source.file, range = "D59:I59", sheet = source.sheet, col_names = vars)

#+ include = FALSE
clc121 <- subset(sf_clc18, CODE_18 == "121") %>% # Industrial sites
  st_transform(32634)
clc121[,vars] <- NA

clc121.int <- st_intersection(clc121, sf.grid.5km) %>%
  select(.,vars)

source.5D2$sources$polygon <- clc121.int
sf.5D2 <- corsum2sf_polygon(source.5D2, distribute = FALSE) %>%
  st_transform(crs = "+init=epsg:32634")

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
#+ include = FALSE, echo = FALSE, result = FALSE

sf.5D2 <- sf.5D2 %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.5D2$Area)
diff.5D2 <- data.frame(total.5D2 - sum.5D2)
sf.5D2 <- sf.5D2 %>%
  mutate(NOx = ((diff.5D2$NOx/sum_Area)*Area),
         SO2 = ((diff.5D2$SO2/sum_Area)*Area),
         PM10 = ((diff.5D2$PM10/sum_Area)*Area),
         PM2.5 = ((diff.5D2$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.5D2$NMVOC/sum_Area)*Area),
         NH3 = ((diff.5D2$NH3/sum_Area)*Area))
sf.5D2 %<>% select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.5D2 <- sf.grid.5km %>%
  st_join(sf.5D2, join = st_contains) %>% 
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

