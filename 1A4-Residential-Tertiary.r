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
source.sheet =  "1A4-Residential-Tertiary"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6]
grid.5km <- readOGR("Grid/Grid_Serbia_0.05deg.gpkg")
sf.grid.5km <- st_as_sf(grid.5km)
sf.grid.5km %<>% dplyr::mutate(ID = id)


#'
#'
#' ## 1A4ai - Commercial/Institutional: Stationary Combustion
#'
#'
#'
#+ include = FALSE
source.1A4ai <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A4ai$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D18:I18", sheet = source.sheet, col_names = vars)
source.1A4ai$total$inventory <- readxl::read_xlsx(path = source.file, range = "D22:I22", sheet = source.sheet, col_names = vars)


#+ include = FALSE
clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
sf_clc18_urb <- subset(sf_clc18, CODE_18 == "111" | CODE_18 == "112") %>% # CLC urban zones
  st_transform(crs = "+init=epsg:32634") %>%
  dplyr::select(geometry)

sf.comm <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/OSM_commercial_Serbia.gpkg") %>%
  dplyr::select(geom) %>%
  rename(geometry = geom)
  
sf.final <- rbind(sf_clc18_urb, sf.comm)

#+ include = FALSE
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
#  toplane <- readxl::read_xls(path = "Data/toplane/Toplane_2015.xls") %>%
#    mutate(GRAD = str_to_title(GRAD))
#  
#  sf_opstine$Toplane <- toplane$`Ukupna grejna povrsina ostalih ustanova, institucija i poslovnih jedininica (m2)`[match(sf_opstine$NAME_2, toplane$GRAD)] 

grejanje <- readxl::read_xls(path = "Data/toplane/tabela-13.xls") %>% as.data.frame()


#+ include = FALSE
lcl(loc = "C")
grejanje <- cyr_lat(grejanje)
names(grejanje) <- cyr_lat(names(grejanje)) 
grejanje <- grejanje %>%
  mutate_all(~replace_na(., 0))
grejanje$Opština[grejanje$Opština == "Indjija"] <- "Inđija"
grejanje$Opština[grejanje$Opština == "LJubovija"] <- "Ljubovija"
grejanje$Opština[grejanje$Opština == "Mali Idjoš"] <- "Mali Iđoš"
grejanje$Opština[grejanje$Opština == "Savski venac"] <- "Savski Venac"
grejanje$Opština[grejanje$Opština == "Stari grad"] <- "Stari Grad"
grejanje$Opština[grejanje$Opština == "Petrovac na Mlavi"] <- "Petrovac"
grejanje$Opština[grejanje$Opština == "Arandjelovac"] <- "Aranđelovac"
grejanje$Opština[grejanje$Opština == "LJig"] <- "Ljig"
grejanje$Opština[grejanje$Opština == "Žitoradja"] <- "Žitorađa"
grejanje$Opština[grejanje$Opština == "Medvedja"] <- "Medveđa"

g_kolone <- grejanje %>% names()
vars_g <- g_kolone[2:11]

grejanje %<>% dplyr::mutate(ukupno = ugalj_etažno+drvo_etažno+mazut_etažno+plin_etažno+drugo_etažno+ugalj_bez_etažnog+drvo_bez_etažnog+mazut_bez_etažnog+plin_bez_etažnog+drugo_bez_etažnog)

sf_opstine$grejanje_ukupno <- grejanje$ukupno[match(sf_opstine$NAME_2, grejanje$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634") %>% 
  mutate_all(~replace(., is.na(.), 0))

sf_final <- sf.final
sf_final <- st_join(sf_final, sf_opstine, largest = TRUE) 

sf_final %<>% dplyr::select(.,grejanje_ukupno, NAME_2)
sf_final[,vars] <- NA
sf_final %<>% sf::st_transform(4326)
sf_final.int <- st_intersection(sf_final, sf.grid.5km) %>% 
  filter(!is.na(grejanje_ukupno))

source.1A4ai$sources$polygon <- sf_final.int

sf.1A4ai <- corsum2sf_polygon(source.1A4ai, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
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
#+ echo = FALSE, result = TRUE, eval = TRUE
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
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.1A4ai$grejanje_ukupno)
diff.1A4ai <- data.frame(total.1A4ai - sum.1A4ai)
sf.1A4ai <- sf.1A4ai %>%
  mutate(NOx = ((diff.1A4ai$NOx/sum_s)*grejanje_ukupno),
         SO2 = ((diff.1A4ai$SO2/sum_s)*grejanje_ukupno),
         PM10 = ((diff.1A4ai$PM10/sum_s)*grejanje_ukupno),
         PM2.5 = ((diff.1A4ai$PM2.5/sum_s)*grejanje_ukupno),
         NMVOC = ((diff.1A4ai$NMVOC/sum_s)*grejanje_ukupno),
         NH3 = ((diff.1A4ai$NH3/sum_s)*grejanje_ukupno))
sf.1A4ai %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A4ai <- sf.grid.5km %>%
  st_join(sf.1A4ai, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A4ai, layer.name.1 = "Sources 1A4ai", sf.spatialised = p.1A4ai, layer.name.2 = "Spatialised 1A4ai", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A4ai <- p.1A4ai %>% 
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

#+ include = FALSE
# st_write(p.1A4ai, dsn="Products/1A4 - Residential-Tertiary/1A4ai.gpkg", layer='1A4ai')

#'
#'
#' ## 1A4bi - Residential: Stationary combustion
#'
#'
#'
#+ include = FALSE
source.1A4bi <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A4bi$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D32:I32", sheet = source.sheet, col_names = vars)
source.1A4bi$total$inventory <- readxl::read_xlsx(path = source.file, range = "D37:I37", sheet = source.sheet, col_names = vars)


#+ include = FALSE
sf_clc18_urb <- subset(sf_clc18, CODE_18 == "111" | CODE_18 == "112") %>% # CLC urban zones
  st_transform(crs = "+init=epsg:32634")

#+ include = FALSE
Sys.setlocale(locale = 'Serbian (Latin)')
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
toplane <- readxl::read_xls(path = "Data/toplane/Toplane_2015.xls") %>%
  mutate(GRAD = str_to_title(GRAD))

sf_opstine$SDG <- toplane$`Broj domacinstava prikljucenih na SDG`[match(sf_opstine$NAME_2, toplane$GRAD)] 

#+ include = FALSE
domacinstva <- readxl::read_xls(path = "Data/Domacinstva_2015.xls", sheet = "Stanovnistvo8")
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

sf_clc18_urb %<>% dplyr::select(.,Br_domacinstva, NAME_2)
sf_clc18_urb[,vars] <- NA
sf_clc18_urb %<>% sf::st_transform(4326)
sf_clc18_urb.int <- st_intersection(sf_clc18_urb, sf.grid.5km) %>% 
  filter(!is.na(Br_domacinstva))

source.1A4bi$sources$polygon <- sf_clc18_urb.int

sf.1A4bi <- corsum2sf_polygon(source.1A4bi, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")


#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, message = FALSE
sf.1A4bi %>% 
  st_drop_geometry() %>%
  dplyr::rename(Residential = Br_domacinstva, Manucipality = NAME_2) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 4: sf.1A4bi',
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
#+ include = FALSE, echo = FALSE, result = FALSE

sum_s <- sum(sf.1A4bi$Br_domacinstva)
diff.1A4bi <- data.frame(total.1A4bi - sum.1A4bi)
sf.1A4bi <- sf.1A4bi %>%
  mutate(NOx = ((diff.1A4bi$NOx/sum_s)*Br_domacinstva),
         SO2 = ((diff.1A4bi$SO2/sum_s)*Br_domacinstva),
         PM10 = ((diff.1A4bi$PM10/sum_s)*Br_domacinstva),
         PM2.5 = ((diff.1A4bi$PM2.5/sum_s)*Br_domacinstva),
         NMVOC = ((diff.1A4bi$NMVOC/sum_s)*Br_domacinstva),
         NH3 = ((diff.1A4bi$NH3/sum_s)*Br_domacinstva))
sf.1A4bi %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
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

#+ echo = FALSE, result = TRUE, eval = TRUE
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

#+ include = FALSE
# st_write(p.1A4bi, dsn="Products/1A4 - Residential-Tertiary/1A4bi.gpkg", layer='1A4bi')

#'
#'
#' ## 1A4ci - Agriculture/Forestry/Fishing: Stationary combustion
#'
#'
#'
#+ include = FALSE

# RURAL ARTIFICIAL ZONES - napravljen je novi lejer

source.1A4ci <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A4ci$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D47:I47", sheet = source.sheet, col_names = vars)
source.1A4ci$total$inventory <- readxl::read_xlsx(path = source.file, range = "D51:I51", sheet = source.sheet, col_names = vars)

#+ include = FALSE
# sf_clc18_polj <- st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")
# sf_clc18_polj[,vars] <- NA
# sf_clc18_polj.int <- st_intersection(sf_clc18_polj, sf.grid.5km)

# source.1A4ci$sources$polygon <- sf_clc18_polj.int
rural_art_zones <- st_read(dsn = "Data/seoska_podrucja_naseljena/rural_artificial_zones.gpkg")
rural_art_zones[,vars] <- NA
rural_art_zones %<>% sf::st_transform(4326)
rural_art_zones.int <- st_intersection(rural_art_zones, sf.grid.5km)

source.1A4ci$sources$polygon <- rural_art_zones.int

sf.1A4ci <- corsum2sf_polygon(source.1A4ci, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A4ci %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.1A4ci',
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
sum.1A4ci <- sf.1A4ci %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A4ci <- source.1A4ci[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A4ci, total.1A4ci, data.frame(total.1A4ci - sum.1A4ci))) %>%
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE

sf.1A4ci <- sf.1A4ci %>%
  mutate(Area = st_area(.))

sum_Area <- sum(sf.1A4ci$Area)
diff.1A4ci <- data.frame(total.1A4ci - sum.1A4ci)
sf.1A4ci <- sf.1A4ci %>%
  mutate(NOx = ((diff.1A4ci$NOx/sum_Area)*Area),
         SO2 = ((diff.1A4ci$SO2/sum_Area)*Area),
         PM10 = ((diff.1A4ci$PM10/sum_Area)*Area),
         PM2.5 = ((diff.1A4ci$PM2.5/sum_Area)*Area),
         NMVOC = ((diff.1A4ci$NMVOC/sum_Area)*Area),
         NH3 = ((diff.1A4ci$NH3/sum_Area)*Area))
sf.1A4ci %<>% dplyr::select(vars)
#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A4ci <- sf.grid.5km %>%
  st_join(sf.1A4ci, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A4ci, layer.name.1 = "Sources 1A4ci", sf.spatialised = p.1A4ci, layer.name.2 = "Spatialised 1A4ci", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A4ci <- p.1A4ci %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A4ci, total.1A4ci, data.frame(sum.p.1A4ci == total.1A4ci)-1)) %>%
  datatable(., caption = 'Table 9: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A4ci, dsn="Products/1A4 - Residential-Tertiary/1A4ci.gpkg", layer='1A4ci')

#'
#'
#' ## 1A4cii - Agriculture/Forestry/Fishing: Off-road vehicles and other machinery
#'
#'
#'
#+ include = FALSE
source.1A4cii <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A4cii$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D61:I61", sheet = source.sheet, col_names = vars)
source.1A4cii$total$inventory <- readxl::read_xlsx(path = source.file, range = "D64:I64", sheet = source.sheet, col_names = vars)

#+ include = FALSE
sf_rur <- sf::st_read(dsn = "Version_2_update/Spatialization/Proxy_data_new/rural_areas_new.gpkg")

#+ include = FALSE
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
traktori <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
traktori <- cyr_lat(traktori)
names(traktori) <- cyr_lat(names(traktori)) 
traktori <- traktori %>%
  mutate_all(~replace_na(., 0))
traktori$Opština[traktori$Opština == "Indjija"] <- "Inđija"
traktori$Opština[traktori$Opština == "LJubovija"] <- "Ljubovija"
traktori$Opština[traktori$Opština == "Mali Idjoš"] <- "Mali Iđoš"
traktori$Opština[traktori$Opština == "Savski venac"] <- "Savski Venac"
traktori$Opština[traktori$Opština == "Stari grad"] <- "Stari Grad"
traktori$Opština[traktori$Opština == "Petrovac na Mlavi"] <- "Petrovac"
traktori$Opština[traktori$Opština == "Arandjelovac"] <- "Aranđelovac"
traktori$Opština[traktori$Opština == "LJig"] <- "Ljig"
traktori$Opština[traktori$Opština == "Žitoradja"] <- "Žitorađa"
traktori$Opština[traktori$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_traktori <- traktori$Traktori[match(sf_opstine$NAME_2, traktori$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")

sf_rur %<>% st_intersection(., sf_opstine)


sf_rur %<>% dplyr::select(.,Br_traktori, NAME_2) %>% 
  filter(!is.na(Br_traktori))
sf_rur[,vars] <- NA
sf_rur %<>% sf::st_transform(4326)
sf_rur.int <- st_intersection(sf_rur, sf.grid.5km) %>% 
  filter(!is.na(Br_traktori))

source.1A4cii$sources$polygon <- sf_rur.int

sf.1A4cii <- corsum2sf_polygon(source.1A4cii, distribute = FALSE) #%>%
  #st_transform(crs = "+init=epsg:32634")

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A4cii %>% 
  st_drop_geometry() %>%
  dplyr::rename(Off_road_vehicles = Br_traktori, Manucipality = NAME_2) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 10: sf.1A4cii',
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
sum.1A4cii <- sf.1A4cii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A4cii <- source.1A4cii[[2]][[2]][, vars] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A4cii, total.1A4cii, data.frame(total.1A4cii - sum.1A4cii))) %>%
  datatable(., caption = 'Table 11: Summary differences',
            options = list(pageLength = 5)
  )


#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
sum_s <- sum(sf.1A4cii$Br_traktori)
diff.1A4cii <- data.frame(total.1A4cii - sum.1A4cii)
sf.1A4cii <- sf.1A4cii %>%
  mutate(NOx = ((diff.1A4cii$NOx/sum_s)*Br_traktori),
         SO2 = ((diff.1A4cii$SO2/sum_s)*Br_traktori),
         PM10 = ((diff.1A4cii$PM10/sum_s)*Br_traktori),
         PM2.5 = ((diff.1A4cii$PM2.5/sum_s)*Br_traktori),
         NMVOC = ((diff.1A4cii$NMVOC/sum_s)*Br_traktori),
         NH3 = ((diff.1A4cii$NH3/sum_s)*Br_traktori))
sf.1A4cii %<>% dplyr::select(vars)

#'
#'
#+ include = FALSE, echo = FALSE, result = FALSE
p.1A4cii <- sf.grid.5km %>%
  st_join(sf.1A4cii, join = st_contains) %>% 
  group_by(ID) %>%
  summarize(NOx = sum(NOx, na.rm = TRUE),
            SO2 = sum(SO2, na.rm = TRUE),
            PM10 = sum(PM10, na.rm = TRUE),
            PM2.5 = sum(PM2.5, na.rm = TRUE),
            NMVOC = sum(NMVOC, na.rm = TRUE),
            NH3 = sum(NH3, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID))
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
spatialised.mapview(sf.sources = sf.1A4cii, layer.name.1 = "Sources 1A4cii", sf.spatialised = p.1A4cii, layer.name.2 = "Spatialised 1A4cii", vars = vars)

#+ echo = FALSE, result = TRUE, eval = TRUE
sum.p.1A4cii <- p.1A4cii %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
data.frame(sum = c("spatialized", "total", "diff"), rbind(sum.p.1A4cii, total.1A4cii, data.frame(sum.p.1A4cii == total.1A4cii)-1)) %>%
  datatable(., caption = 'Table 12: Summary differences after spatialisation',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# st_write(p.1A4cii, dsn="Products/1A4 - Residential-Tertiary/1A4cii.gpkg", layer='1A4cii')


