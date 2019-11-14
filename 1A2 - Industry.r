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
# source.list = source.1A2a
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
    w <- replace(source.list$sources$points[, vars], zero.ind, NA) %>% 
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
mapview(sf.1A2a, layer.name = "Sources 1A2a", col.regions = "red") + mapview(p.1A2a, layer.name = "Spatialised 1A2a")

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
mapview(sf.1A2b, layer.name = "Sources 1A2b", col.regions = "red") + mapview(p.1A2b, layer.name = "Spatialised 1A2b")

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
source.1A2c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.1A2c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D87:I87", sheet = source.sheet, col_names = vars)

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
mapview(sf.1A2c, layer.name = "Sources 1A2c", col.regions = "red") + mapview(p.1A2c, layer.name = "Spatialised 1A2c")

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

sf.1A2d <- corsum2sf(source.1A2d) %>%
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
mapview(sf.1A2d, layer.name = "Sources 1A2d", col.regions = "red") + mapview(p.1A2d, layer.name = "Spatialised 1A2d")

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

sf.1A2e <- corsum2sf(source.1A2e) %>%
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
mapview(sf.1A2e, layer.name = "Sources 1A2e", col.regions = "red") + mapview(p.1A2e, layer.name = "Spatialised 1A2e")

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

sf.1A2f <- corsum2sf(source.1A2f) %>%
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
mapview(sf.1A2f, layer.name = "Sources 1A2f", col.regions = "red") + mapview(p.1A2f, layer.name = "Spatialised 1A2f")

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
clc121 <- subset(sf_clc18, CODE_18 == "121") %>%
  st_set_crs(32634)
#+ include = FALSE
# Combine all known points into one sf object
points_all <- rbind(sf.1A2a, sf.1A2b, sf.1A2c, sf.1A2d, sf.1A2e, sf.1A2f, sf.1A2g) %>%
  st_transform(crs = "+init=epsg:32634")
clc121.otherIndustries <- st_join(clc121, points_all, join = st_intersects) %>%
  subset(is.na(NOx) | is.na(SO2) | is.na(PM10) | is.na(PM2.5) | is.na(NMVOC) | is.na(NH3))

#+ include = FALSE
# Polygon Centroid
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
clc121.oI.cen %<>% select(vars)
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
mapview(sf.1A2g, layer.name = "Sources 1A2g", col.regions = "red") + mapview(p.1A2g, layer.name = "Spatialised 1A2g")

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
#+ include = FALSE 
# rmarkdown::render(here::here("/1A2 - Industry.r"), output_dir = here::here("/Reports"))

