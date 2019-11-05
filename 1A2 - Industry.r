#' ---
#' title: "Pollutant inventory spatialization"
#' author: GILAB team
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
corsum2sf <- function(source.list){
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
  
  if(!identical(points.sum, points.total)){
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


sf.1A2a <- corsum2sf(source.1A2a)

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
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
mapview(sf.1A2a, zcol = 'PM10', layer.name = "Sources 1A2a", map.types = c("OpenStreetMap", "Esri.WorldImagery"))

#'
#'
#'
#' ## 1A2b - Non-ferrous metals
#+ include = FALSE
source.1A2b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2b$sources$points <- readxl::read_xlsx(path = source.file, range = "D35:S40", sheet = source.sheet, col_names = header)
source.1A2b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D49:I49", sheet = source.sheet, col_names = vars)
source.1A2b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D54:I54", sheet = source.sheet, col_names = vars)

sf.1A2b <- corsum2sf(source.1A2b)
#'
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2b %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 3: sf.1A2b',
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
  datatable(., caption = 'Table 4: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
mapview(sf.1A2b, zcol = 'PM10', layer.name = "Sources 1A2b", map.types = c("OpenStreetMap", "Esri.WorldImagery"))

#'
#'
#' ## 1A2c - Chemicals
#'
#+ include = FALSE

source.1A2c <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A2c$sources$points <- readxl::read_xlsx(path = source.file, range = "D55:S69", sheet = source.sheet, col_names = header)
source.1A2c$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D72:I72", sheet = source.sheet, col_names = vars)
source.1A2c$total$inventory <- readxl::read_xlsx(path = source.file, range = "D87:I87", sheet = source.sheet, col_names = vars)

sf.1A2c <- corsum2sf(source.1A2c)

#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2c %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 5: sf.1A2aiv',
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
  datatable(., caption = 'Table 6: Summary differences',
            options = list(pageLength = 5)
  )
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
mapview(sf.1A2c, zcol = 'PM10', layer.name = "Sources 1A2c", map.types = c("OpenStreetMap", "Esri.WorldImagery"))

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

sf.1A2d <- corsum2sf(source.1A2d)
#'
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2d %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 5: sf.1A2d',
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
  datatable(., caption = 'Table 6: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
mapview(sf.1A2d, zcol = 'SO2', layer.name = "Sources 1A2d", map.types = c("OpenStreetMap", "Esri.WorldImagery"))

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

sf.1A2e <- corsum2sf(source.1A2e)

#'
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2e %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 7: sf.1A2e',
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
  datatable(., caption = 'Table 8: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
mapview(sf.1A2e, zcol = 'PM10', layer.name = "Sources 1A2e", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
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

sf.1A2f <- corsum2sf(source.1A2f)
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A2f %>% 
  st_drop_geometry() %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 9: sf.1A2f',
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
  datatable(., caption = 'Table 10: Summary differences',
            options = list(pageLength = 5)
  )
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
mapview(sf.1A2f, layer.name = "Sources 1A2f", map.types = c("OpenStreetMap", "Esri.WorldImagery"))

#'
#'
#'
#'
#'
#'
#'
#+ include = FALSE 
# rmarkdown::render(here::here("/1A2 - Industry.r"), output_dir = here::here("/Reports"))


