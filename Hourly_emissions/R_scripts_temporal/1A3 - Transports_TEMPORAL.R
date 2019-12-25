#' ---
#' title: "Pollutant inventory spatialization for Serbia - temporal distribution"
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
#' # 1A3 - Transports
#' This document provides the methodlogy and the main results regarding the temporal distribution of pollutation inventory.
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
library(tsibble)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggforce)
library(writexl)
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
#+ include = FALSE
activity.df <- readRDS(file = "D:/R_projects/Spatialization/Hourly_emissions/Data/activity_df.rds")
load(file = "D:/R_projects/Spatialization/Hourly_emissions/Data/counters_df.rds")
counters.df <- counters_df %>% 
  mutate(ALL_mean = (IA_mean + IIA_mean + IB_mean)/3)
activity.df$VA <- counters.df$ALL_mean

summary_tab <- data.frame(Label = c("WD", "WDWW", "WT0816", "WT1624", "WT0024", "WT0622", "DL", 
                                    "WE", "WW", "RH0709", "RH1517", "PH", "SA", "HS", "SAAG", "TEMP", "SLP", "VA", "NFH", "RP"),
                          Description = c("Working days", 
                                          "Working days, working weekends", 
                                          "Working time 08-16h",
                                          "working time 16-24h",
                                          "Working time 00-24h",
                                          "Working time 06-22h",
                                          "Day light", 
                                          "Weekends",
                                          "Working weekends",
                                          "Rush hours 07-09h",
                                          "Rush hours 15-17h",
                                          "Public holidays",
                                          "Seasons", 
                                          "Heating Season",
                                          "Agriculture Season",
                                          "Temperature",
                                          "Sea Level Pressure",
                                          "Vehicles Trend Activity",
                                          "Number of Flights per Hour",
                                          "Repair - overhaul period"))
#+ echo = FALSE, result = TRUE, eval = TRUE
summary_tab %>%
  datatable(., caption = 'Table: Label description',
            options = list(pageLength = 10), 
  )%>% formatStyle(
    'Label',
    backgroundColor = "lightblue"
  )

#'
#'
#'
#'
#'
#'
#' ## 1A3ai-International aviation LTO (civil)
#+ include = FALSE






#'
#'
#'
#'
#'
#'
#' ## 1A3aii-Domestic aviation LTO (civil)
#+ include = FALSE



#'
#'
#'
#'
#'
#'
#' ## 1A3bi-Road Transport: Passengers cars
#' 
#' ### Urban transport
#+ include = FALSE

#'
#' ### Rural transport
#+ include = FALSE

#' 
#' ### Highways transport
#+ include = FALSE



#'
#'
#'
#'
#'
#'
#' ## 1A3bii-Road transport: Light-duty vehicles
#' 
#' ### Urban transport
#+ include = FALSE

#'
#' ### Rural transport
#+ include = FALSE

#' 
#' ### Highways transport
#+ include = FALSE



#'
#'
#'
#'
#'
#'
#' ## 1A3biii-Road transport: Heavy-duty vehicles
#' 
#' ### Urban transport
#+ include = FALSE

#'
#' ### Rural transport
#+ include = FALSE

#' 
#' ### Highways transport
#+ include = FALSE




#'
#'
#'
#'
#'
#'
#' ## 1A3biii_bc-Road transport: Buses & Coaches
#' 
#' ### Urban transport
#+ include = FALSE

#'
#' ### Rural transport
#+ include = FALSE

#' 
#' ### Highways transport
#+ include = FALSE




#'
#'
#'
#'
#'
#'
#' ## 1A3biv-Road transport: Mopeds & motorcycles 
#' 
#' ### Urban transport
#+ include = FALSE

#'
#' ### Rural transport
#+ include = FALSE

#' 
#' ### Highways transport
#+ include = FALSE




#'
#'
#'
#'
#'
#'
#' ## 1A3bv-Road transport: Gasoline evaporation
#' 
#' ### Urban transport
#+ include = FALSE

#'
#' ### Rural transport
#+ include = FALSE

#' 
#' ### Highways transport
#+ include = FALSE





#'
#'
#'
#'
#'
#'
#' ## 1A3bvi-Road transport: Automobile tyre and brake wear
#' 
#' ### Urban transport
#+ include = FALSE




#'
#'
#'
#'
#'
#'
#' ## 1A3bvii-Road transport: Automobile road abrasion
#' 
#' ### Urban transport
#+ include = FALSE




#'
#'
#'
#'
#'
#'
#' ## 1A3c-Railways
#+ include = FALSE




#'
#'
#'
#'
#'
#'
#' ## 1A3dii-National navigation (shipping)
#+ include = FALSE
