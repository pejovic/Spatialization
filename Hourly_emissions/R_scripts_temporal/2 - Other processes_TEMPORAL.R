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
#' # 2 - Other processes
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
            options = list(pageLength = 25), 
  )%>% formatStyle(
    'Label',
    backgroundColor = "lightblue"
  )
sigmoid = function(x) {
  1 / (1 + exp(-x))
}
#'
#'
#'
#'
#'
#'
#' ## 2A5a - Quarrying and mining of minerals other than coal
#+ include = FALSE

sf.2A5a <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2A5a.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2A5a <- sf.2A5a %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2A5a%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + !PH + (k1*SA + k2) + !RP
#
he.2A5a <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_2A5a = (WDWW * (WT0024+0.5)) * PH2 * RP2 * (TEMP+30)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2A5a))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2A5a = he_sig) %>%
  select(times, he_2A5a)
# ((SA * (-1.5))+2.5)
time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2A5a, aes(x = times, y = he_2A5a)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2A5a = (WDWW * (WT0024+0.5)) * PH2 * RP2 * (TEMP+30)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2A5a$he_2A5a), max(he.2A5a$he_2A5a), sum(he.2A5a$he_2A5a))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2A5a$sumF <- sum(he.2A5a$he_2A5a)
he.2A5a %<>% 
  dplyr::mutate(NOx_2A5a = (t.2A5a$NOx/t.2A5a$sumF)*he_2A5a, 
                NOx_2A5a_p = (NOx_2A5a/sum(NOx_2A5a))*100,
                SO2_2A5a = (t.2A5a$SO2/t.2A5a$sumF)*he_2A5a, 
                SO2_2A5a_p = (SO2_2A5a/sum(SO2_2A5a))*100,
                PM10_2A5a = (t.2A5a$PM10/t.2A5a$sumF)*he_2A5a, 
                PM10_2A5a_p = (PM10_2A5a/sum(PM10_2A5a))*100,
                PM2.5_2A5a = (t.2A5a$PM2.5/t.2A5a$sumF)*he_2A5a, 
                PM2.5_2A5a_p = (PM2.5_2A5a/sum(PM2.5_2A5a))*100,
                NMVOC_2A5a = (t.2A5a$NMVOC/t.2A5a$sumF)*he_2A5a, 
                NMVOC_2A5a_p = (NMVOC_2A5a/sum(NMVOC_2A5a))*100,
                NH3_2A5a = (t.2A5a$NH3/t.2A5a$sumF)*he_2A5a, 
                NH3_2A5a_p = (NH3_2A5a/sum(NH3_2A5a))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2A5a_p, SO2_2A5a_p, PM10_2A5a_p, PM2.5_2A5a_p, NMVOC_2A5a_p, NH3_2A5a_p) %>%
  rename(`2A5a_NOx` = NOx_2A5a_p,
         `2A5a_SO2` = SO2_2A5a_p,
         `2A5a_PM10` = PM10_2A5a_p,
         `2A5a_PM2.5` = PM2.5_2A5a_p,
         `2A5a_NMVOC` = NMVOC_2A5a_p,
         `2A5a_NH3` = NH3_2A5a_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2A5a$`2A5a_NOx`), sum(he.2A5a$`2A5a_SO2`), sum(he.2A5a$`2A5a_PM10`), sum(he.2A5a$`2A5a_PM2.5`), sum(he.2A5a$`2A5a_NMVOC`), sum(he.2A5a$`2A5a_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#'
#'
#'
#'
#'
#'
#' ## 2A5b - Construction and demolition
#+ include = FALSE

sf.2A5b <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2A5b.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2A5b <- sf.2A5b %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2A5b%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + WT1624 + !PH + (k1*SA + k2)
#

he.2A5b <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_2A5b = (WDWW * (WT0816+0.5) * (WT1624+0.5)) * PH2 * (TEMP+30))  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2A5b))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2A5b = he_sig) %>%
  select(times, he_2A5b)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2A5b, aes(x = times, y = he_2A5b)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2A5b = (WDWW * (WT0816+0.5) * (WT1624+0.5)) * PH2 * (TEMP+30))")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2A5b$he_2A5b), max(he.2A5b$he_2A5b), sum(he.2A5b$he_2A5b))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2A5b$sumF <- sum(he.2A5b$he_2A5b)
he.2A5b %<>% 
  dplyr::mutate(NOx_2A5b = (t.2A5b$NOx/t.2A5b$sumF)*he_2A5b, 
                NOx_2A5b_p = (NOx_2A5b/sum(NOx_2A5b))*100,
                SO2_2A5b = (t.2A5b$SO2/t.2A5b$sumF)*he_2A5b, 
                SO2_2A5b_p = (SO2_2A5b/sum(SO2_2A5b))*100,
                PM10_2A5b = (t.2A5b$PM10/t.2A5b$sumF)*he_2A5b, 
                PM10_2A5b_p = (PM10_2A5b/sum(PM10_2A5b))*100,
                PM2.5_2A5b = (t.2A5b$PM2.5/t.2A5b$sumF)*he_2A5b, 
                PM2.5_2A5b_p = (PM2.5_2A5b/sum(PM2.5_2A5b))*100,
                NMVOC_2A5b = (t.2A5b$NMVOC/t.2A5b$sumF)*he_2A5b, 
                NMVOC_2A5b_p = (NMVOC_2A5b/sum(NMVOC_2A5b))*100,
                NH3_2A5b = (t.2A5b$NH3/t.2A5b$sumF)*he_2A5b, 
                NH3_2A5b_p = (NH3_2A5b/sum(NH3_2A5b))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2A5b_p, SO2_2A5b_p, PM10_2A5b_p, PM2.5_2A5b_p, NMVOC_2A5b_p, NH3_2A5b_p) %>%
  rename(`2A5b_NOx` = NOx_2A5b_p,
         `2A5b_SO2` = SO2_2A5b_p,
         `2A5b_PM10` = PM10_2A5b_p,
         `2A5b_PM2.5` = PM2.5_2A5b_p,
         `2A5b_NMVOC` = NMVOC_2A5b_p,
         `2A5b_NH3` = NH3_2A5b_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2A5b$`2A5b_NOx`), sum(he.2A5b$`2A5b_SO2`), sum(he.2A5b$`2A5b_PM10`), sum(he.2A5b$`2A5b_PM2.5`), sum(he.2A5b$`2A5b_NMVOC`), sum(he.2A5b$`2A5b_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 2A5c - Storage, handling and transport of mineral products
#+ include = FALSE

sf.2A5b <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2A5b.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2A5b <- sf.2A5b %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2A5b%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + RH0709 + RH1517 + !PH + VA
#

he.2A5b <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2A5b = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * VA)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2A5b))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2A5b = he_sig) %>%
  select(times, he_2A5b)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2A5b, aes(x = times, y = he_2A5b)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2A5b = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2A5b$he_2A5b), max(he.2A5b$he_2A5b), sum(he.2A5b$he_2A5b))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2A5b$sumF <- sum(he.2A5b$he_2A5b)
he.2A5b %<>% 
  dplyr::mutate(NOx_2A5b = (t.2A5b$NOx/t.2A5b$sumF)*he_2A5b, 
                NOx_2A5b_p = (NOx_2A5b/sum(NOx_2A5b))*100,
                SO2_2A5b = (t.2A5b$SO2/t.2A5b$sumF)*he_2A5b, 
                SO2_2A5b_p = (SO2_2A5b/sum(SO2_2A5b))*100,
                PM10_2A5b = (t.2A5b$PM10/t.2A5b$sumF)*he_2A5b, 
                PM10_2A5b_p = (PM10_2A5b/sum(PM10_2A5b))*100,
                PM2.5_2A5b = (t.2A5b$PM2.5/t.2A5b$sumF)*he_2A5b, 
                PM2.5_2A5b_p = (PM2.5_2A5b/sum(PM2.5_2A5b))*100,
                NMVOC_2A5b = (t.2A5b$NMVOC/t.2A5b$sumF)*he_2A5b, 
                NMVOC_2A5b_p = (NMVOC_2A5b/sum(NMVOC_2A5b))*100,
                NH3_2A5b = (t.2A5b$NH3/t.2A5b$sumF)*he_2A5b, 
                NH3_2A5b_p = (NH3_2A5b/sum(NH3_2A5b))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2A5b_p, SO2_2A5b_p, PM10_2A5b_p, PM2.5_2A5b_p, NMVOC_2A5b_p, NH3_2A5b_p) %>%
  rename(`2A5b_NOx` = NOx_2A5b_p,
         `2A5b_SO2` = SO2_2A5b_p,
         `2A5b_PM10` = PM10_2A5b_p,
         `2A5b_PM2.5` = PM2.5_2A5b_p,
         `2A5b_NMVOC` = NMVOC_2A5b_p,
         `2A5b_NH3` = NH3_2A5b_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2A5b$`2A5b_NOx`), sum(he.2A5b$`2A5b_SO2`), sum(he.2A5b$`2A5b_PM10`), sum(he.2A5b$`2A5b_PM2.5`), sum(he.2A5b$`2A5b_NMVOC`), sum(he.2A5b$`2A5b_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 2D3b - Road paving with asphalt
#+ include = FALSE

sf.2D3b <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3b.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3b <- sf.2D3b %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3b%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + VA 
#

he.2D3b <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3b = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * VA + (TEMP+30))  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3b))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3b = he_sig) %>%
  select(times, he_2D3b)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3b, aes(x = times, y = he_2D3b)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3b = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * VA + (TEMP+30)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3b$he_2D3b), max(he.2D3b$he_2D3b), sum(he.2D3b$he_2D3b))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3b$sumF <- sum(he.2D3b$he_2D3b)
he.2D3b %<>% 
  dplyr::mutate(NOx_2D3b = (t.2D3b$NOx/t.2D3b$sumF)*he_2D3b, 
                NOx_2D3b_p = (NOx_2D3b/sum(NOx_2D3b))*100,
                SO2_2D3b = (t.2D3b$SO2/t.2D3b$sumF)*he_2D3b, 
                SO2_2D3b_p = (SO2_2D3b/sum(SO2_2D3b))*100,
                PM10_2D3b = (t.2D3b$PM10/t.2D3b$sumF)*he_2D3b, 
                PM10_2D3b_p = (PM10_2D3b/sum(PM10_2D3b))*100,
                PM2.5_2D3b = (t.2D3b$PM2.5/t.2D3b$sumF)*he_2D3b, 
                PM2.5_2D3b_p = (PM2.5_2D3b/sum(PM2.5_2D3b))*100,
                NMVOC_2D3b = (t.2D3b$NMVOC/t.2D3b$sumF)*he_2D3b, 
                NMVOC_2D3b_p = (NMVOC_2D3b/sum(NMVOC_2D3b))*100,
                NH3_2D3b = (t.2D3b$NH3/t.2D3b$sumF)*he_2D3b, 
                NH3_2D3b_p = (NH3_2D3b/sum(NH3_2D3b))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3b_p, SO2_2D3b_p, PM10_2D3b_p, PM2.5_2D3b_p, NMVOC_2D3b_p, NH3_2D3b_p) %>%
  rename(`2D3b_NOx` = NOx_2D3b_p,
         `2D3b_SO2` = SO2_2D3b_p,
         `2D3b_PM10` = PM10_2D3b_p,
         `2D3b_PM2.5` = PM2.5_2D3b_p,
         `2D3b_NMVOC` = NMVOC_2D3b_p,
         `2D3b_NH3` = NH3_2D3b_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3b$`2D3b_NOx`), sum(he.2D3b$`2D3b_SO2`), sum(he.2D3b$`2D3b_PM10`), sum(he.2D3b$`2D3b_PM2.5`), sum(he.2D3b$`2D3b_NMVOC`), sum(he.2D3b$`2D3b_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 2D3c - Asphalt roofing
#+ include = FALSE


sf.2D3c <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3c.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3c <- sf.2D3c %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3c%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + VA 
#

he.2D3c <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3c = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * VA + (TEMP+30))  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3c))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3c = he_sig) %>%
  select(times, he_2D3c)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3c, aes(x = times, y = he_2D3c)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3c = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * VA + (TEMP+30)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3c$he_2D3c), max(he.2D3c$he_2D3c), sum(he.2D3c$he_2D3c))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3c$sumF <- sum(he.2D3c$he_2D3c)
he.2D3c %<>% 
  dplyr::mutate(NOx_2D3c = (t.2D3c$NOx/t.2D3c$sumF)*he_2D3c, 
                NOx_2D3c_p = (NOx_2D3c/sum(NOx_2D3c))*100,
                SO2_2D3c = (t.2D3c$SO2/t.2D3c$sumF)*he_2D3c, 
                SO2_2D3c_p = (SO2_2D3c/sum(SO2_2D3c))*100,
                PM10_2D3c = (t.2D3c$PM10/t.2D3c$sumF)*he_2D3c, 
                PM10_2D3c_p = (PM10_2D3c/sum(PM10_2D3c))*100,
                PM2.5_2D3c = (t.2D3c$PM2.5/t.2D3c$sumF)*he_2D3c, 
                PM2.5_2D3c_p = (PM2.5_2D3c/sum(PM2.5_2D3c))*100,
                NMVOC_2D3c = (t.2D3c$NMVOC/t.2D3c$sumF)*he_2D3c, 
                NMVOC_2D3c_p = (NMVOC_2D3c/sum(NMVOC_2D3c))*100,
                NH3_2D3c = (t.2D3c$NH3/t.2D3c$sumF)*he_2D3c, 
                NH3_2D3c_p = (NH3_2D3c/sum(NH3_2D3c))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3c_p, SO2_2D3c_p, PM10_2D3c_p, PM2.5_2D3c_p, NMVOC_2D3c_p, NH3_2D3c_p) %>%
  rename(`2D3c_NOx` = NOx_2D3c_p,
         `2D3c_SO2` = SO2_2D3c_p,
         `2D3c_PM10` = PM10_2D3c_p,
         `2D3c_PM2.5` = PM2.5_2D3c_p,
         `2D3c_NMVOC` = NMVOC_2D3c_p,
         `2D3c_NH3` = NH3_2D3c_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3c$`2D3c_NOx`), sum(he.2D3c$`2D3c_SO2`), sum(he.2D3c$`2D3c_PM10`), sum(he.2D3c$`2D3c_PM2.5`), sum(he.2D3c$`2D3c_NMVOC`), sum(he.2D3c$`2D3c_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#'
#'
#'
#'
#'
#'
#' ## 2D3d - Coating application
#+ include = FALSE

sf.2D3d <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3d.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3d <- sf.2D3d %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3d%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + VA 
#

he.2D3d <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3d = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * VA + (TEMP+30))  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3d))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3d = he_sig) %>%
  select(times, he_2D3d)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3d, aes(x = times, y = he_2D3d)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3d = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * VA + (TEMP+30)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3d$he_2D3d), max(he.2D3d$he_2D3d), sum(he.2D3d$he_2D3d))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3d$sumF <- sum(he.2D3d$he_2D3d)
he.2D3d %<>% 
  dplyr::mutate(NOx_2D3d = (t.2D3d$NOx/t.2D3d$sumF)*he_2D3d, 
                NOx_2D3d_p = (NOx_2D3d/sum(NOx_2D3d))*100,
                SO2_2D3d = (t.2D3d$SO2/t.2D3d$sumF)*he_2D3d, 
                SO2_2D3d_p = (SO2_2D3d/sum(SO2_2D3d))*100,
                PM10_2D3d = (t.2D3d$PM10/t.2D3d$sumF)*he_2D3d, 
                PM10_2D3d_p = (PM10_2D3d/sum(PM10_2D3d))*100,
                PM2.5_2D3d = (t.2D3d$PM2.5/t.2D3d$sumF)*he_2D3d, 
                PM2.5_2D3d_p = (PM2.5_2D3d/sum(PM2.5_2D3d))*100,
                NMVOC_2D3d = (t.2D3d$NMVOC/t.2D3d$sumF)*he_2D3d, 
                NMVOC_2D3d_p = (NMVOC_2D3d/sum(NMVOC_2D3d))*100,
                NH3_2D3d = (t.2D3d$NH3/t.2D3d$sumF)*he_2D3d, 
                NH3_2D3d_p = (NH3_2D3d/sum(NH3_2D3d))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3d_p, SO2_2D3d_p, PM10_2D3d_p, PM2.5_2D3d_p, NMVOC_2D3d_p, NH3_2D3d_p) %>%
  rename(`2D3d_NOx` = NOx_2D3d_p,
         `2D3d_SO2` = SO2_2D3d_p,
         `2D3d_PM10` = PM10_2D3d_p,
         `2D3d_PM2.5` = PM2.5_2D3d_p,
         `2D3d_NMVOC` = NMVOC_2D3d_p,
         `2D3d_NH3` = NH3_2D3d_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3d$`2D3d_NOx`), sum(he.2D3d$`2D3d_SO2`), sum(he.2D3d$`2D3d_PM10`), sum(he.2D3d$`2D3d_PM2.5`), sum(he.2D3d$`2D3d_NMVOC`), sum(he.2D3d$`2D3d_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 2D3g - Chemical products
#+ include = FALSE

sf.2D3g <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3g.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3g <- sf.2D3g %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3g%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3g <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3g = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3g))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3g = he_sig) %>%
  select(times, he_2D3g)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3g, aes(x = times, y = he_2D3g)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3g = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3g$he_2D3g), max(he.2D3g$he_2D3g), sum(he.2D3g$he_2D3g))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3g$sumF <- sum(he.2D3g$he_2D3g)
he.2D3g %<>% 
  dplyr::mutate(NOx_2D3g = (t.2D3g$NOx/t.2D3g$sumF)*he_2D3g, 
                NOx_2D3g_p = (NOx_2D3g/sum(NOx_2D3g))*100,
                SO2_2D3g = (t.2D3g$SO2/t.2D3g$sumF)*he_2D3g, 
                SO2_2D3g_p = (SO2_2D3g/sum(SO2_2D3g))*100,
                PM10_2D3g = (t.2D3g$PM10/t.2D3g$sumF)*he_2D3g, 
                PM10_2D3g_p = (PM10_2D3g/sum(PM10_2D3g))*100,
                PM2.5_2D3g = (t.2D3g$PM2.5/t.2D3g$sumF)*he_2D3g, 
                PM2.5_2D3g_p = (PM2.5_2D3g/sum(PM2.5_2D3g))*100,
                NMVOC_2D3g = (t.2D3g$NMVOC/t.2D3g$sumF)*he_2D3g, 
                NMVOC_2D3g_p = (NMVOC_2D3g/sum(NMVOC_2D3g))*100,
                NH3_2D3g = (t.2D3g$NH3/t.2D3g$sumF)*he_2D3g, 
                NH3_2D3g_p = (NH3_2D3g/sum(NH3_2D3g))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3g_p, SO2_2D3g_p, PM10_2D3g_p, PM2.5_2D3g_p, NMVOC_2D3g_p, NH3_2D3g_p) %>%
  rename(`2D3g_NOx` = NOx_2D3g_p,
         `2D3g_SO2` = SO2_2D3g_p,
         `2D3g_PM10` = PM10_2D3g_p,
         `2D3g_PM2.5` = PM2.5_2D3g_p,
         `2D3g_NMVOC` = NMVOC_2D3g_p,
         `2D3g_NH3` = NH3_2D3g_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3g$`2D3g_NOx`), sum(he.2D3g$`2D3g_SO2`), sum(he.2D3g$`2D3g_PM10`), sum(he.2D3g$`2D3g_PM2.5`), sum(he.2D3g$`2D3g_NMVOC`), sum(he.2D3g$`2D3g_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 2D3i - Other solvent and product use
#+ include = FALSE

sf.2D3i <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3i.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3i <- sf.2D3i %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3i%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3i <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3i = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3i))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3i = he_sig) %>%
  select(times, he_2D3i)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3i, aes(x = times, y = he_2D3i)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3i = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3i$he_2D3i), max(he.2D3i$he_2D3i), sum(he.2D3i$he_2D3i))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3i$sumF <- sum(he.2D3i$he_2D3i)
he.2D3i %<>% 
  dplyr::mutate(NOx_2D3i = (t.2D3i$NOx/t.2D3i$sumF)*he_2D3i, 
                NOx_2D3i_p = (NOx_2D3i/sum(NOx_2D3i))*100,
                SO2_2D3i = (t.2D3i$SO2/t.2D3i$sumF)*he_2D3i, 
                SO2_2D3i_p = (SO2_2D3i/sum(SO2_2D3i))*100,
                PM10_2D3i = (t.2D3i$PM10/t.2D3i$sumF)*he_2D3i, 
                PM10_2D3i_p = (PM10_2D3i/sum(PM10_2D3i))*100,
                PM2.5_2D3i = (t.2D3i$PM2.5/t.2D3i$sumF)*he_2D3i, 
                PM2.5_2D3i_p = (PM2.5_2D3i/sum(PM2.5_2D3i))*100,
                NMVOC_2D3i = (t.2D3i$NMVOC/t.2D3i$sumF)*he_2D3i, 
                NMVOC_2D3i_p = (NMVOC_2D3i/sum(NMVOC_2D3i))*100,
                NH3_2D3i = (t.2D3i$NH3/t.2D3i$sumF)*he_2D3i, 
                NH3_2D3i_p = (NH3_2D3i/sum(NH3_2D3i))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3i_p, SO2_2D3i_p, PM10_2D3i_p, PM2.5_2D3i_p, NMVOC_2D3i_p, NH3_2D3i_p) %>%
  rename(`2D3i_NOx` = NOx_2D3i_p,
         `2D3i_SO2` = SO2_2D3i_p,
         `2D3i_PM10` = PM10_2D3i_p,
         `2D3i_PM2.5` = PM2.5_2D3i_p,
         `2D3i_NMVOC` = NMVOC_2D3i_p,
         `2D3i_NH3` = NH3_2D3i_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3i$`2D3i_NOx`), sum(he.2D3i$`2D3i_SO2`), sum(he.2D3i$`2D3i_PM10`), sum(he.2D3i$`2D3i_PM2.5`), sum(he.2D3i$`2D3i_NMVOC`), sum(he.2D3i$`2D3i_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 2D3a - Domestic solvent use including fungicides
#+ include = FALSE
sf.2D3a <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3a.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3a <- sf.2D3a %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3a%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3a <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3a = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3a))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3a = he_sig) %>%
  select(times, he_2D3a)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3a, aes(x = times, y = he_2D3a)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3a = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3a$he_2D3a), max(he.2D3a$he_2D3a), sum(he.2D3a$he_2D3a))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3a$sumF <- sum(he.2D3a$he_2D3a)
he.2D3a %<>% 
  dplyr::mutate(NOx_2D3a = (t.2D3a$NOx/t.2D3a$sumF)*he_2D3a, 
                NOx_2D3a_p = (NOx_2D3a/sum(NOx_2D3a))*100,
                SO2_2D3a = (t.2D3a$SO2/t.2D3a$sumF)*he_2D3a, 
                SO2_2D3a_p = (SO2_2D3a/sum(SO2_2D3a))*100,
                PM10_2D3a = (t.2D3a$PM10/t.2D3a$sumF)*he_2D3a, 
                PM10_2D3a_p = (PM10_2D3a/sum(PM10_2D3a))*100,
                PM2.5_2D3a = (t.2D3a$PM2.5/t.2D3a$sumF)*he_2D3a, 
                PM2.5_2D3a_p = (PM2.5_2D3a/sum(PM2.5_2D3a))*100,
                NMVOC_2D3a = (t.2D3a$NMVOC/t.2D3a$sumF)*he_2D3a, 
                NMVOC_2D3a_p = (NMVOC_2D3a/sum(NMVOC_2D3a))*100,
                NH3_2D3a = (t.2D3a$NH3/t.2D3a$sumF)*he_2D3a, 
                NH3_2D3a_p = (NH3_2D3a/sum(NH3_2D3a))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3a_p, SO2_2D3a_p, PM10_2D3a_p, PM2.5_2D3a_p, NMVOC_2D3a_p, NH3_2D3a_p) %>%
  rename(`2D3a_NOx` = NOx_2D3a_p,
         `2D3a_SO2` = SO2_2D3a_p,
         `2D3a_PM10` = PM10_2D3a_p,
         `2D3a_PM2.5` = PM2.5_2D3a_p,
         `2D3a_NMVOC` = NMVOC_2D3a_p,
         `2D3a_NH3` = NH3_2D3a_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3a$`2D3a_NOx`), sum(he.2D3a$`2D3a_SO2`), sum(he.2D3a$`2D3a_PM10`), sum(he.2D3a$`2D3a_PM2.5`), sum(he.2D3a$`2D3a_NMVOC`), sum(he.2D3a$`2D3a_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 2D3e - Degreasing
#+ include = FALSE
sf.2D3e <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3e.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3e <- sf.2D3e %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3e%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3e <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3e = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3e))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3e = he_sig) %>%
  select(times, he_2D3e)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3e, aes(x = times, y = he_2D3e)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3e = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3e$he_2D3e), max(he.2D3e$he_2D3e), sum(he.2D3e$he_2D3e))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3e$sumF <- sum(he.2D3e$he_2D3e)
he.2D3e %<>% 
  dplyr::mutate(NOx_2D3e = (t.2D3e$NOx/t.2D3e$sumF)*he_2D3e, 
                NOx_2D3e_p = (NOx_2D3e/sum(NOx_2D3e))*100,
                SO2_2D3e = (t.2D3e$SO2/t.2D3e$sumF)*he_2D3e, 
                SO2_2D3e_p = (SO2_2D3e/sum(SO2_2D3e))*100,
                PM10_2D3e = (t.2D3e$PM10/t.2D3e$sumF)*he_2D3e, 
                PM10_2D3e_p = (PM10_2D3e/sum(PM10_2D3e))*100,
                PM2.5_2D3e = (t.2D3e$PM2.5/t.2D3e$sumF)*he_2D3e, 
                PM2.5_2D3e_p = (PM2.5_2D3e/sum(PM2.5_2D3e))*100,
                NMVOC_2D3e = (t.2D3e$NMVOC/t.2D3e$sumF)*he_2D3e, 
                NMVOC_2D3e_p = (NMVOC_2D3e/sum(NMVOC_2D3e))*100,
                NH3_2D3e = (t.2D3e$NH3/t.2D3e$sumF)*he_2D3e, 
                NH3_2D3e_p = (NH3_2D3e/sum(NH3_2D3e))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3e_p, SO2_2D3e_p, PM10_2D3e_p, PM2.5_2D3e_p, NMVOC_2D3e_p, NH3_2D3e_p) %>%
  rename(`2D3e_NOx` = NOx_2D3e_p,
         `2D3e_SO2` = SO2_2D3e_p,
         `2D3e_PM10` = PM10_2D3e_p,
         `2D3e_PM2.5` = PM2.5_2D3e_p,
         `2D3e_NMVOC` = NMVOC_2D3e_p,
         `2D3e_NH3` = NH3_2D3e_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3e$`2D3e_NOx`), sum(he.2D3e$`2D3e_SO2`), sum(he.2D3e$`2D3e_PM10`), sum(he.2D3e$`2D3e_PM2.5`), sum(he.2D3e$`2D3e_NMVOC`), sum(he.2D3e$`2D3e_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 2D3f - Dry cleaning
#+ include = FALSE

sf.2D3f <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3f.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3f <- sf.2D3f %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3f%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3f <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3f = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3f))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3f = he_sig) %>%
  select(times, he_2D3f)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3f, aes(x = times, y = he_2D3f)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3f = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3f$he_2D3f), max(he.2D3f$he_2D3f), sum(he.2D3f$he_2D3f))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3f$sumF <- sum(he.2D3f$he_2D3f)
he.2D3f %<>% 
  dplyr::mutate(NOx_2D3f = (t.2D3f$NOx/t.2D3f$sumF)*he_2D3f, 
                NOx_2D3f_p = (NOx_2D3f/sum(NOx_2D3f))*100,
                SO2_2D3f = (t.2D3f$SO2/t.2D3f$sumF)*he_2D3f, 
                SO2_2D3f_p = (SO2_2D3f/sum(SO2_2D3f))*100,
                PM10_2D3f = (t.2D3f$PM10/t.2D3f$sumF)*he_2D3f, 
                PM10_2D3f_p = (PM10_2D3f/sum(PM10_2D3f))*100,
                PM2.5_2D3f = (t.2D3f$PM2.5/t.2D3f$sumF)*he_2D3f, 
                PM2.5_2D3f_p = (PM2.5_2D3f/sum(PM2.5_2D3f))*100,
                NMVOC_2D3f = (t.2D3f$NMVOC/t.2D3f$sumF)*he_2D3f, 
                NMVOC_2D3f_p = (NMVOC_2D3f/sum(NMVOC_2D3f))*100,
                NH3_2D3f = (t.2D3f$NH3/t.2D3f$sumF)*he_2D3f, 
                NH3_2D3f_p = (NH3_2D3f/sum(NH3_2D3f))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3f_p, SO2_2D3f_p, PM10_2D3f_p, PM2.5_2D3f_p, NMVOC_2D3f_p, NH3_2D3f_p) %>%
  rename(`2D3f_NOx` = NOx_2D3f_p,
         `2D3f_SO2` = SO2_2D3f_p,
         `2D3f_PM10` = PM10_2D3f_p,
         `2D3f_PM2.5` = PM2.5_2D3f_p,
         `2D3f_NMVOC` = NMVOC_2D3f_p,
         `2D3f_NH3` = NH3_2D3f_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3f$`2D3f_NOx`), sum(he.2D3f$`2D3f_SO2`), sum(he.2D3f$`2D3f_PM10`), sum(he.2D3f$`2D3f_PM2.5`), sum(he.2D3f$`2D3f_NMVOC`), sum(he.2D3f$`2D3f_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 2D3h - Printing
#+ include = FALSE

sf.2D3h <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3h.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3h <- sf.2D3h %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3h%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3h <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3h = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3h))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3h = he_sig) %>%
  select(times, he_2D3h)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3h, aes(x = times, y = he_2D3h)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3h = (WDWW * (DL+0.5)) * (TEMP*(-1)+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3h$he_2D3h), max(he.2D3h$he_2D3h), sum(he.2D3h$he_2D3h))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3h$sumF <- sum(he.2D3h$he_2D3h)
he.2D3h %<>% 
  dplyr::mutate(NOx_2D3h = (t.2D3h$NOx/t.2D3h$sumF)*he_2D3h, 
                NOx_2D3h_p = (NOx_2D3h/sum(NOx_2D3h))*100,
                SO2_2D3h = (t.2D3h$SO2/t.2D3h$sumF)*he_2D3h, 
                SO2_2D3h_p = (SO2_2D3h/sum(SO2_2D3h))*100,
                PM10_2D3h = (t.2D3h$PM10/t.2D3h$sumF)*he_2D3h, 
                PM10_2D3h_p = (PM10_2D3h/sum(PM10_2D3h))*100,
                PM2.5_2D3h = (t.2D3h$PM2.5/t.2D3h$sumF)*he_2D3h, 
                PM2.5_2D3h_p = (PM2.5_2D3h/sum(PM2.5_2D3h))*100,
                NMVOC_2D3h = (t.2D3h$NMVOC/t.2D3h$sumF)*he_2D3h, 
                NMVOC_2D3h_p = (NMVOC_2D3h/sum(NMVOC_2D3h))*100,
                NH3_2D3h = (t.2D3h$NH3/t.2D3h$sumF)*he_2D3h, 
                NH3_2D3h_p = (NH3_2D3h/sum(NH3_2D3h))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3h_p, SO2_2D3h_p, PM10_2D3h_p, PM2.5_2D3h_p, NMVOC_2D3h_p, NH3_2D3h_p) %>%
  rename(`2D3h_NOx` = NOx_2D3h_p,
         `2D3h_SO2` = SO2_2D3h_p,
         `2D3h_PM10` = PM10_2D3h_p,
         `2D3h_PM2.5` = PM2.5_2D3h_p,
         `2D3h_NMVOC` = NMVOC_2D3h_p,
         `2D3h_NH3` = NH3_2D3h_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3h$`2D3h_NOx`), sum(he.2D3h$`2D3h_SO2`), sum(he.2D3h$`2D3h_PM10`), sum(he.2D3h$`2D3h_PM2.5`), sum(he.2D3h$`2D3h_NMVOC`), sum(he.2D3h$`2D3h_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 2I - Wood processing
#+ include = FALSE

sf.2I <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2I.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2I <- sf.2I %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2I%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + DL + WE + !PH + SA
#

he.2I <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+1)) %>%
  dplyr::mutate(he_2I = (WDWW * (WT0816+0.5) * (DL+0.5)) * PH2 * (TEMP + 30) * WE2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2I))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2I = he_sig) %>%
  select(times, he_2I)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2I, aes(x = times, y = he_2I)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2I = (WDWW * (WT0816+0.5) * (DL+0.5)) * PH2 * (TEMP + 30) * WE2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2I$he_2I), max(he.2I$he_2I), sum(he.2I$he_2I))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2I$sumF <- sum(he.2I$he_2I)
he.2I %<>% 
  dplyr::mutate(NOx_2I = (t.2I$NOx/t.2I$sumF)*he_2I, 
                NOx_2I_p = (NOx_2I/sum(NOx_2I))*100,
                SO2_2I = (t.2I$SO2/t.2I$sumF)*he_2I, 
                SO2_2I_p = (SO2_2I/sum(SO2_2I))*100,
                PM10_2I = (t.2I$PM10/t.2I$sumF)*he_2I, 
                PM10_2I_p = (PM10_2I/sum(PM10_2I))*100,
                PM2.5_2I = (t.2I$PM2.5/t.2I$sumF)*he_2I, 
                PM2.5_2I_p = (PM2.5_2I/sum(PM2.5_2I))*100,
                NMVOC_2I = (t.2I$NMVOC/t.2I$sumF)*he_2I, 
                NMVOC_2I_p = (NMVOC_2I/sum(NMVOC_2I))*100,
                NH3_2I = (t.2I$NH3/t.2I$sumF)*he_2I, 
                NH3_2I_p = (NH3_2I/sum(NH3_2I))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2I_p, SO2_2I_p, PM10_2I_p, PM2.5_2I_p, NMVOC_2I_p, NH3_2I_p) %>%
  rename(`2I_NOx` = NOx_2I_p,
         `2I_SO2` = SO2_2I_p,
         `2I_PM10` = PM10_2I_p,
         `2I_PM2.5` = PM2.5_2I_p,
         `2I_NMVOC` = NMVOC_2I_p,
         `2I_NH3` = NH3_2I_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2I$`2I_NOx`), sum(he.2I$`2I_SO2`), sum(he.2I$`2I_PM10`), sum(he.2I$`2I_PM2.5`), sum(he.2I$`2I_NMVOC`), sum(he.2I$`2I_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )





# temporalProfile_Other_processes <- activity.df$times %>% cbind(he.2A5a[,1:6], 
#                                                            he.2A5b[,1:6], 
#                                                            he.2D3a[,1:6], 
#                                                            he.2D3b[,1:6], 
#                                                            he.2D3c[,1:6], 
#                                                            he.2D3d[,1:6], 
#                                                            he.2D3e[,1:6], 
#                                                            he.2D3f[,1:6], 
#                                                            he.2D3g[,1:6], 
#                                                            he.2D3h[,1:6], 
#                                                            he.2D3i[,1:6], 
#                                                            he.2I[,1:6]) %>% 
#   as.data.frame()
# 
# writexl::write_xlsx(temporalProfile_Other_processes, path = 'Hourly_emissions/Products/TemporalProfile_Other_processes.xlsx')

