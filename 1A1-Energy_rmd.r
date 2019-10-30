#' ---
#' title: "Pollutant inventory spatialization"
#' author: 
#' - "Milutin Pejovic"
#' - "Petar Bursac"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output:   
#'    html_document:
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
#' # 1A1 - Energys
#' This document provides the methodlogy and the main results regarding the spatialization of pollutation inventory.
#' 
#' 
#+ include = TRUE, echo = FALSE, results = 'hide', warning = FALSE, message = FALSE
library(XLConnect)
library(tidyverse)
library(sf)
library(readxl)
library(ggpubr)
library(ggfortify)
library(here)
library(knitr)
library(kableExtra)
library(DT)

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
# source.list = source.1A1a
corsum2sf <- function(source.list){
  points.sum <- source.list$sources$points %>% 
    dplyr::select(., vars) %>% 
    dplyr::select_if(., is.numeric) %>% 
    apply(., 2, sum) %>% 
    t(.) %>% 
    as.data.frame() %>%
    dplyr::mutate_if(is.numeric, round, 2)
  
  points.total <- source.list[[2]][[2]][, names(points.sum)] %>% 
    mutate_all(~replace(., is.na(.), 0)) %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    as.data.frame()
  
  if(!identical(points.sum, points.total)){
    source.diff.vars <- names(points.sum)[!(points.total == points.sum)]
    d <- (points.total - points.sum)[1, ]
    w <- apply(source.1A1c$sources$points[,source.diff.vars], 2, function(x) x/sum(x))
    cor.data <- w %*% diag(d) + source.list$sources$points[, source.diff.vars]
    source.list$sources$points[, source.diff.vars] <- cor.data
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
source.sheet =  "1A1-Energy"
header <- readxl::read_xlsx(path = source.file, range = "D8:S8", sheet = source.sheet) %>% names()
vars <- header[1:6] 
#'
#'
#' ## 1A1a - Public heat and electricity production
#'
#'
#'
#+ include = FALSE
source.1A1a <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1a$sources$points <- readxl::read_xlsx(path = source.file, range = "D9:S37", sheet = source.sheet, col_names = header)
source.1A1a$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D40:I40", sheet = source.sheet, col_names = vars)
source.1A1a$total$inventory <- readxl::read_xlsx(path = source.file, range = "D46:I46", sheet = source.sheet, col_names = vars)


sf.1A1a <- corsum2sf(source.1A1a)

#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A1a %>% 
  st_drop_geometry() %>% 
  #dplyr::select_if(., is.numeric) %>%
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
#+ echo = FALSE, result = TRUE, eval = TRUE
sum.1A1a <- sf.1A1a %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  dplyr::select_if(., is.numeric) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A1a <- source.1A1a[[2]][[2]][, names(points.sum)] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A1a, total.1A1a, data.frame(sum.1A1a[, names(total.1A1b)] == total.1A1a)-1)) %>%
  datatable(., caption = 'Table 2: Summary differences',
            options = list(pageLength = 5)
  )
  
#'
#'
#'
#' ## 1A1b - Refineries
#+ include = FALSE
source.1A1b <- list(sources = list(points = NA, lines = NA, polygon = NA), total = list(spatialize = NA, inventory = NA))

source.1A1b$sources$points <- readxl::read_xlsx(path = source.file, range = "D47:S50", sheet = source.sheet, col_names = header)
source.1A1b$total$spatialize <- readxl::read_xlsx(path = source.file, range = "D51:I51", sheet = source.sheet, col_names = vars)
source.1A1b$total$inventory <- readxl::read_xlsx(path = source.file, range = "D52:I52", sheet = source.sheet, col_names = vars)

sf.1A1b <- corsum2sf(source.1A1b)
#'
#'
#'
#+ echo = FALSE, result = TRUE, eval = TRUE
sf.1A1b %>% 
  st_drop_geometry() %>% 
  #dplyr::select_if(., is.numeric) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  datatable(., caption = 'Table 3: sf.1A1b',
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
sum.1A1b <- sf.1A1b %>% 
  st_drop_geometry() %>%
  dplyr::select(., vars) %>% 
  dplyr::select_if(., is.numeric) %>% 
  apply(., 2, sum) %>% 
  t(.) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, 2)
total.1A1b <- source.1A1b[[2]][[2]][, names(points.sum)] %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

data.frame(sum = c("spatialize", "total", "diff"), rbind(sum.1A1b[, names(total.1A1b)], total.1A1b, data.frame(sum.1A1b[, names(total.1A1b)] == total.1A1b)-1)) %>%
  datatable(., caption = 'Table 4: Summary differences',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#+ include = FALSE 
  # rmarkdown::render(here::here("/1A1-Energy_rmd.r"), output_dir = here::here("/Reports"))
  

