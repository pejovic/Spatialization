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
  mutate(ALL_mean = (IA_mean + IIA_mean + IB_mean)/3,
         RURAL_mean = (IIA_mean + IB_mean)/2)
activity.df$VA <- counters.df$ALL_mean
activity.df$VA_H <- counters.df$IA_mean # Highway
activity.df$VA_R <- counters.df$RURAL_mean # Rural

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
            options = list(pageLength = 10) 
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

sf.1A3ai <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3ai.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3ai <- sf.1A3ai %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3ai%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = NFH
#

he.1A3ai <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3ai = NFH) %>%
  select(times, he_1A3ai)

time_seq <- seq.POSIXt(from = ymd_h("2015-06-01 00"),
                       to   = ymd_h("2015-06-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3ai, aes(x = times, y = he_1A3ai)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = NFH")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3ai$he_1A3ai), max(he.1A3ai$he_1A3ai), sum(he.1A3ai$he_1A3ai))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3ai$sumF <- sum(he.1A3ai$he_1A3ai)
he.1A3ai %<>% 
  dplyr::mutate(NOx_1A3ai = (t.1A3ai$NOx/t.1A3ai$sumF)*he_1A3ai, 
                NOx_1A3ai_p = (NOx_1A3ai/sum(NOx_1A3ai))*100,
                SO2_1A3ai = (t.1A3ai$SO2/t.1A3ai$sumF)*he_1A3ai, 
                SO2_1A3ai_p = (SO2_1A3ai/sum(SO2_1A3ai))*100,
                PM10_1A3ai = (t.1A3ai$PM10/t.1A3ai$sumF)*he_1A3ai, 
                PM10_1A3ai_p = (PM10_1A3ai/sum(PM10_1A3ai))*100,
                PM2.5_1A3ai = (t.1A3ai$PM2.5/t.1A3ai$sumF)*he_1A3ai, 
                PM2.5_1A3ai_p = (PM2.5_1A3ai/sum(PM2.5_1A3ai))*100,
                NMVOC_1A3ai = (t.1A3ai$NMVOC/t.1A3ai$sumF)*he_1A3ai, 
                NMVOC_1A3ai_p = (NMVOC_1A3ai/sum(NMVOC_1A3ai))*100,
                NH3_1A3ai = (t.1A3ai$NH3/t.1A3ai$sumF)*he_1A3ai, 
                NH3_1A3ai_p = (NH3_1A3ai/sum(NH3_1A3ai))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3ai_p, SO2_1A3ai_p, PM10_1A3ai_p, PM2.5_1A3ai_p, NMVOC_1A3ai_p, NH3_1A3ai_p) %>%
  rename(`1A3ai_NOx` = NOx_1A3ai_p,
         `1A3ai_SO2` = SO2_1A3ai_p,
         `1A3ai_PM10` = PM10_1A3ai_p,
         `1A3ai_PM2.5` = PM2.5_1A3ai_p,
         `1A3ai_NMVOC` = NMVOC_1A3ai_p,
         `1A3ai_NH3` = NH3_1A3ai_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3ai$`1A3ai_NOx`), sum(he.1A3ai$`1A3ai_SO2`), sum(he.1A3ai$`1A3ai_PM10`), sum(he.1A3ai$`1A3ai_PM2.5`), sum(he.1A3ai$`1A3ai_NMVOC`), sum(he.1A3ai$`1A3ai_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 1A3aii-Domestic aviation LTO (civil)
#+ include = FALSE

sf.1A3aii <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3aii.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3aii <- sf.1A3aii %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3aii%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = DL + SA
#

he.1A3aii <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3aii = (DL+0.5) * (TEMP+30)) %>%
  select(times, he_1A3aii)
# ((SA * (-1.5))+2.5)
time_seq <- seq.POSIXt(from = ymd_h("2015-06-01 00"),
                       to   = ymd_h("2015-06-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3aii, aes(x = times, y = he_1A3aii)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = DL + SA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3aii$he_1A3aii), max(he.1A3aii$he_1A3aii), sum(he.1A3aii$he_1A3aii))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3aii$sumF <- sum(he.1A3aii$he_1A3aii)
he.1A3aii %<>% 
  dplyr::mutate(NOx_1A3aii = (t.1A3aii$NOx/t.1A3aii$sumF)*he_1A3aii, 
                NOx_1A3aii_p = (NOx_1A3aii/sum(NOx_1A3aii))*100,
                SO2_1A3aii = (t.1A3aii$SO2/t.1A3aii$sumF)*he_1A3aii, 
                SO2_1A3aii_p = (SO2_1A3aii/sum(SO2_1A3aii))*100,
                PM10_1A3aii = (t.1A3aii$PM10/t.1A3aii$sumF)*he_1A3aii, 
                PM10_1A3aii_p = (PM10_1A3aii/sum(PM10_1A3aii))*100,
                PM2.5_1A3aii = (t.1A3aii$PM2.5/t.1A3aii$sumF)*he_1A3aii, 
                PM2.5_1A3aii_p = (PM2.5_1A3aii/sum(PM2.5_1A3aii))*100,
                NMVOC_1A3aii = (t.1A3aii$NMVOC/t.1A3aii$sumF)*he_1A3aii, 
                NMVOC_1A3aii_p = (NMVOC_1A3aii/sum(NMVOC_1A3aii))*100,
                NH3_1A3aii = (t.1A3aii$NH3/t.1A3aii$sumF)*he_1A3aii, 
                NH3_1A3aii_p = (NH3_1A3aii/sum(NH3_1A3aii))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3aii_p, SO2_1A3aii_p, PM10_1A3aii_p, PM2.5_1A3aii_p, NMVOC_1A3aii_p, NH3_1A3aii_p) %>%
  rename(`1A3aii_NOx` = NOx_1A3aii_p,
         `1A3aii_SO2` = SO2_1A3aii_p,
         `1A3aii_PM10` = PM10_1A3aii_p,
         `1A3aii_PM2.5` = PM2.5_1A3aii_p,
         `1A3aii_NMVOC` = NMVOC_1A3aii_p,
         `1A3aii_NH3` = NH3_1A3aii_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3aii$`1A3aii_NOx`), sum(he.1A3aii$`1A3aii_SO2`), sum(he.1A3aii$`1A3aii_PM10`), sum(he.1A3aii$`1A3aii_PM2.5`), sum(he.1A3aii$`1A3aii_NMVOC`), sum(he.1A3aii$`1A3aii_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

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


sf.1A3bi_U <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bi_U.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bi_U <- sf.1A3bi_U %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bi_U%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA
#

he.1A3bi_U <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bi_U = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * (VA+0.5) * (RH0709+0.5) * (RH1517+0.5) + (TEMP+30) + SLP) %>%
  select(times, he_1A3bi_U)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bi_U, aes(x = times, y = he_1A3bi_U)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bi_U$he_1A3bi_U), max(he.1A3bi_U$he_1A3bi_U), sum(he.1A3bi_U$he_1A3bi_U))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bi_U$sumF <- sum(he.1A3bi_U$he_1A3bi_U)
he.1A3bi_U %<>% 
  dplyr::mutate(NOx_1A3bi_U = (t.1A3bi_U$NOx/t.1A3bi_U$sumF)*he_1A3bi_U, 
                NOx_1A3bi_U_p = (NOx_1A3bi_U/sum(NOx_1A3bi_U))*100,
                SO2_1A3bi_U = (t.1A3bi_U$SO2/t.1A3bi_U$sumF)*he_1A3bi_U, 
                SO2_1A3bi_U_p = (SO2_1A3bi_U/sum(SO2_1A3bi_U))*100,
                PM10_1A3bi_U = (t.1A3bi_U$PM10/t.1A3bi_U$sumF)*he_1A3bi_U, 
                PM10_1A3bi_U_p = (PM10_1A3bi_U/sum(PM10_1A3bi_U))*100,
                PM2.5_1A3bi_U = (t.1A3bi_U$PM2.5/t.1A3bi_U$sumF)*he_1A3bi_U, 
                PM2.5_1A3bi_U_p = (PM2.5_1A3bi_U/sum(PM2.5_1A3bi_U))*100,
                NMVOC_1A3bi_U = (t.1A3bi_U$NMVOC/t.1A3bi_U$sumF)*he_1A3bi_U, 
                NMVOC_1A3bi_U_p = (NMVOC_1A3bi_U/sum(NMVOC_1A3bi_U))*100,
                NH3_1A3bi_U = (t.1A3bi_U$NH3/t.1A3bi_U$sumF)*he_1A3bi_U, 
                NH3_1A3bi_U_p = (NH3_1A3bi_U/sum(NH3_1A3bi_U))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bi_U_p, SO2_1A3bi_U_p, PM10_1A3bi_U_p, PM2.5_1A3bi_U_p, NMVOC_1A3bi_U_p, NH3_1A3bi_U_p) %>%
  rename(`1A3bi_U_NOx` = NOx_1A3bi_U_p,
         `1A3bi_U_SO2` = SO2_1A3bi_U_p,
         `1A3bi_U_PM10` = PM10_1A3bi_U_p,
         `1A3bi_U_PM2.5` = PM2.5_1A3bi_U_p,
         `1A3bi_U_NMVOC` = NMVOC_1A3bi_U_p,
         `1A3bi_U_NH3` = NH3_1A3bi_U_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bi_U$`1A3bi_U_NOx`), sum(he.1A3bi_U$`1A3bi_U_SO2`), sum(he.1A3bi_U$`1A3bi_U_PM10`), sum(he.1A3bi_U$`1A3bi_U_PM2.5`), sum(he.1A3bi_U$`1A3bi_U_NMVOC`), sum(he.1A3bi_U$`1A3bi_U_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#' ### Rural transport
#+ include = FALSE


sf.1A3bi_R <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bi_R.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bi_R <- sf.1A3bi_R %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bi_R%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IIA + VA_IB
#

he.1A3bi_R <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bi_R = VA_R) %>%
  select(times, he_1A3bi_R)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bi_R, aes(x = times, y = he_1A3bi_R)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IIA + VA_IB")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bi_R$he_1A3bi_R), max(he.1A3bi_R$he_1A3bi_R), sum(he.1A3bi_R$he_1A3bi_R))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bi_R$sumF <- sum(he.1A3bi_R$he_1A3bi_R)
he.1A3bi_R %<>% 
  dplyr::mutate(NOx_1A3bi_R = (t.1A3bi_R$NOx/t.1A3bi_R$sumF)*he_1A3bi_R, 
                NOx_1A3bi_R_p = (NOx_1A3bi_R/sum(NOx_1A3bi_R))*100,
                SO2_1A3bi_R = (t.1A3bi_R$SO2/t.1A3bi_R$sumF)*he_1A3bi_R, 
                SO2_1A3bi_R_p = (SO2_1A3bi_R/sum(SO2_1A3bi_R))*100,
                PM10_1A3bi_R = (t.1A3bi_R$PM10/t.1A3bi_R$sumF)*he_1A3bi_R, 
                PM10_1A3bi_R_p = (PM10_1A3bi_R/sum(PM10_1A3bi_R))*100,
                PM2.5_1A3bi_R = (t.1A3bi_R$PM2.5/t.1A3bi_R$sumF)*he_1A3bi_R, 
                PM2.5_1A3bi_R_p = (PM2.5_1A3bi_R/sum(PM2.5_1A3bi_R))*100,
                NMVOC_1A3bi_R = (t.1A3bi_R$NMVOC/t.1A3bi_R$sumF)*he_1A3bi_R, 
                NMVOC_1A3bi_R_p = (NMVOC_1A3bi_R/sum(NMVOC_1A3bi_R))*100,
                NH3_1A3bi_R = (t.1A3bi_R$NH3/t.1A3bi_R$sumF)*he_1A3bi_R, 
                NH3_1A3bi_R_p = (NH3_1A3bi_R/sum(NH3_1A3bi_R))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bi_R_p, SO2_1A3bi_R_p, PM10_1A3bi_R_p, PM2.5_1A3bi_R_p, NMVOC_1A3bi_R_p, NH3_1A3bi_R_p) %>%
  rename(`1A3bi_R_NOx` = NOx_1A3bi_R_p,
         `1A3bi_R_SO2` = SO2_1A3bi_R_p,
         `1A3bi_R_PM10` = PM10_1A3bi_R_p,
         `1A3bi_R_PM2.5` = PM2.5_1A3bi_R_p,
         `1A3bi_R_NMVOC` = NMVOC_1A3bi_R_p,
         `1A3bi_R_NH3` = NH3_1A3bi_R_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bi_R$`1A3bi_R_NOx`), sum(he.1A3bi_R$`1A3bi_R_SO2`), sum(he.1A3bi_R$`1A3bi_R_PM10`), sum(he.1A3bi_R$`1A3bi_R_PM2.5`), sum(he.1A3bi_R$`1A3bi_R_NMVOC`), sum(he.1A3bi_R$`1A3bi_R_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#' 
#' ### Highways transport
#+ include = FALSE

sf.1A3bi_H <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bi_H.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bi_H <- sf.1A3bi_H %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bi_H%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IA
#

he.1A3bi_H <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bi_H = VA_H) %>%
  select(times, he_1A3bi_H)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bi_H, aes(x = times, y = he_1A3bi_H)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bi_H$he_1A3bi_H), max(he.1A3bi_H$he_1A3bi_H), sum(he.1A3bi_H$he_1A3bi_H))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bi_H$sumF <- sum(he.1A3bi_H$he_1A3bi_H)
he.1A3bi_H %<>% 
  dplyr::mutate(NOx_1A3bi_H = (t.1A3bi_H$NOx/t.1A3bi_H$sumF)*he_1A3bi_H, 
                NOx_1A3bi_H_p = (NOx_1A3bi_H/sum(NOx_1A3bi_H))*100,
                SO2_1A3bi_H = (t.1A3bi_H$SO2/t.1A3bi_H$sumF)*he_1A3bi_H, 
                SO2_1A3bi_H_p = (SO2_1A3bi_H/sum(SO2_1A3bi_H))*100,
                PM10_1A3bi_H = (t.1A3bi_H$PM10/t.1A3bi_H$sumF)*he_1A3bi_H, 
                PM10_1A3bi_H_p = (PM10_1A3bi_H/sum(PM10_1A3bi_H))*100,
                PM2.5_1A3bi_H = (t.1A3bi_H$PM2.5/t.1A3bi_H$sumF)*he_1A3bi_H, 
                PM2.5_1A3bi_H_p = (PM2.5_1A3bi_H/sum(PM2.5_1A3bi_H))*100,
                NMVOC_1A3bi_H = (t.1A3bi_H$NMVOC/t.1A3bi_H$sumF)*he_1A3bi_H, 
                NMVOC_1A3bi_H_p = (NMVOC_1A3bi_H/sum(NMVOC_1A3bi_H))*100,
                NH3_1A3bi_H = (t.1A3bi_H$NH3/t.1A3bi_H$sumF)*he_1A3bi_H, 
                NH3_1A3bi_H_p = (NH3_1A3bi_H/sum(NH3_1A3bi_H))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bi_H_p, SO2_1A3bi_H_p, PM10_1A3bi_H_p, PM2.5_1A3bi_H_p, NMVOC_1A3bi_H_p, NH3_1A3bi_H_p) %>%
  rename(`1A3bi_H_NOx` = NOx_1A3bi_H_p,
         `1A3bi_H_SO2` = SO2_1A3bi_H_p,
         `1A3bi_H_PM10` = PM10_1A3bi_H_p,
         `1A3bi_H_PM2.5` = PM2.5_1A3bi_H_p,
         `1A3bi_H_NMVOC` = NMVOC_1A3bi_H_p,
         `1A3bi_H_NH3` = NH3_1A3bi_H_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bi_H$`1A3bi_H_NOx`), sum(he.1A3bi_H$`1A3bi_H_SO2`), sum(he.1A3bi_H$`1A3bi_H_PM10`), sum(he.1A3bi_H$`1A3bi_H_PM2.5`), sum(he.1A3bi_H$`1A3bi_H_NMVOC`), sum(he.1A3bi_H$`1A3bi_H_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


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


sf.1A3bii_U <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bii_U.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bii_U <- sf.1A3bii_U %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bii_U%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA
#

he.1A3bii_U <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bii_U = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * (VA+0.5) * (RH0709+0.5) * (RH1517+0.5) + (TEMP+30) + SLP) %>%
  select(times, he_1A3bii_U)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bii_U, aes(x = times, y = he_1A3bii_U)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bii_U$he_1A3bii_U), max(he.1A3bii_U$he_1A3bii_U), sum(he.1A3bii_U$he_1A3bii_U))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bii_U$sumF <- sum(he.1A3bii_U$he_1A3bii_U)
he.1A3bii_U %<>% 
  dplyr::mutate(NOx_1A3bii_U = (t.1A3bii_U$NOx/t.1A3bii_U$sumF)*he_1A3bii_U, 
                NOx_1A3bii_U_p = (NOx_1A3bii_U/sum(NOx_1A3bii_U))*100,
                SO2_1A3bii_U = (t.1A3bii_U$SO2/t.1A3bii_U$sumF)*he_1A3bii_U, 
                SO2_1A3bii_U_p = (SO2_1A3bii_U/sum(SO2_1A3bii_U))*100,
                PM10_1A3bii_U = (t.1A3bii_U$PM10/t.1A3bii_U$sumF)*he_1A3bii_U, 
                PM10_1A3bii_U_p = (PM10_1A3bii_U/sum(PM10_1A3bii_U))*100,
                PM2.5_1A3bii_U = (t.1A3bii_U$PM2.5/t.1A3bii_U$sumF)*he_1A3bii_U, 
                PM2.5_1A3bii_U_p = (PM2.5_1A3bii_U/sum(PM2.5_1A3bii_U))*100,
                NMVOC_1A3bii_U = (t.1A3bii_U$NMVOC/t.1A3bii_U$sumF)*he_1A3bii_U, 
                NMVOC_1A3bii_U_p = (NMVOC_1A3bii_U/sum(NMVOC_1A3bii_U))*100,
                NH3_1A3bii_U = (t.1A3bii_U$NH3/t.1A3bii_U$sumF)*he_1A3bii_U, 
                NH3_1A3bii_U_p = (NH3_1A3bii_U/sum(NH3_1A3bii_U))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bii_U_p, SO2_1A3bii_U_p, PM10_1A3bii_U_p, PM2.5_1A3bii_U_p, NMVOC_1A3bii_U_p, NH3_1A3bii_U_p) %>%
  rename(`1A3bii_U_NOx` = NOx_1A3bii_U_p,
         `1A3bii_U_SO2` = SO2_1A3bii_U_p,
         `1A3bii_U_PM10` = PM10_1A3bii_U_p,
         `1A3bii_U_PM2.5` = PM2.5_1A3bii_U_p,
         `1A3bii_U_NMVOC` = NMVOC_1A3bii_U_p,
         `1A3bii_U_NH3` = NH3_1A3bii_U_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bii_U$`1A3bii_U_NOx`), sum(he.1A3bii_U$`1A3bii_U_SO2`), sum(he.1A3bii_U$`1A3bii_U_PM10`), sum(he.1A3bii_U$`1A3bii_U_PM2.5`), sum(he.1A3bii_U$`1A3bii_U_NMVOC`), sum(he.1A3bii_U$`1A3bii_U_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#' ### Rural transport
#+ include = FALSE


sf.1A3bii_R <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bii_R.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bii_R <- sf.1A3bii_R %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bii_R%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IIA + VA_IB
#

he.1A3bii_R <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bii_R = VA_R) %>%
  select(times, he_1A3bii_R)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bii_R, aes(x = times, y = he_1A3bii_R)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IIA + VA_IB")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bii_R$he_1A3bii_R), max(he.1A3bii_R$he_1A3bii_R), sum(he.1A3bii_R$he_1A3bii_R))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bii_R$sumF <- sum(he.1A3bii_R$he_1A3bii_R)
he.1A3bii_R %<>% 
  dplyr::mutate(NOx_1A3bii_R = (t.1A3bii_R$NOx/t.1A3bii_R$sumF)*he_1A3bii_R, 
                NOx_1A3bii_R_p = (NOx_1A3bii_R/sum(NOx_1A3bii_R))*100,
                SO2_1A3bii_R = (t.1A3bii_R$SO2/t.1A3bii_R$sumF)*he_1A3bii_R, 
                SO2_1A3bii_R_p = (SO2_1A3bii_R/sum(SO2_1A3bii_R))*100,
                PM10_1A3bii_R = (t.1A3bii_R$PM10/t.1A3bii_R$sumF)*he_1A3bii_R, 
                PM10_1A3bii_R_p = (PM10_1A3bii_R/sum(PM10_1A3bii_R))*100,
                PM2.5_1A3bii_R = (t.1A3bii_R$PM2.5/t.1A3bii_R$sumF)*he_1A3bii_R, 
                PM2.5_1A3bii_R_p = (PM2.5_1A3bii_R/sum(PM2.5_1A3bii_R))*100,
                NMVOC_1A3bii_R = (t.1A3bii_R$NMVOC/t.1A3bii_R$sumF)*he_1A3bii_R, 
                NMVOC_1A3bii_R_p = (NMVOC_1A3bii_R/sum(NMVOC_1A3bii_R))*100,
                NH3_1A3bii_R = (t.1A3bii_R$NH3/t.1A3bii_R$sumF)*he_1A3bii_R, 
                NH3_1A3bii_R_p = (NH3_1A3bii_R/sum(NH3_1A3bii_R))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bii_R_p, SO2_1A3bii_R_p, PM10_1A3bii_R_p, PM2.5_1A3bii_R_p, NMVOC_1A3bii_R_p, NH3_1A3bii_R_p) %>%
  rename(`1A3bii_R_NOx` = NOx_1A3bii_R_p,
         `1A3bii_R_SO2` = SO2_1A3bii_R_p,
         `1A3bii_R_PM10` = PM10_1A3bii_R_p,
         `1A3bii_R_PM2.5` = PM2.5_1A3bii_R_p,
         `1A3bii_R_NMVOC` = NMVOC_1A3bii_R_p,
         `1A3bii_R_NH3` = NH3_1A3bii_R_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bii_R$`1A3bii_R_NOx`), sum(he.1A3bii_R$`1A3bii_R_SO2`), sum(he.1A3bii_R$`1A3bii_R_PM10`), sum(he.1A3bii_R$`1A3bii_R_PM2.5`), sum(he.1A3bii_R$`1A3bii_R_NMVOC`), sum(he.1A3bii_R$`1A3bii_R_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#' 
#' ### Highways transport
#+ include = FALSE

sf.1A3bii_H <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bii_H.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bii_H <- sf.1A3bii_H %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bii_H%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IA
#

he.1A3bii_H <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bii_H = VA_H) %>%
  select(times, he_1A3bii_H)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bii_H, aes(x = times, y = he_1A3bii_H)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bii_H$he_1A3bii_H), max(he.1A3bii_H$he_1A3bii_H), sum(he.1A3bii_H$he_1A3bii_H))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bii_H$sumF <- sum(he.1A3bii_H$he_1A3bii_H)
he.1A3bii_H %<>% 
  dplyr::mutate(NOx_1A3bii_H = (t.1A3bii_H$NOx/t.1A3bii_H$sumF)*he_1A3bii_H, 
                NOx_1A3bii_H_p = (NOx_1A3bii_H/sum(NOx_1A3bii_H))*100,
                SO2_1A3bii_H = (t.1A3bii_H$SO2/t.1A3bii_H$sumF)*he_1A3bii_H, 
                SO2_1A3bii_H_p = (SO2_1A3bii_H/sum(SO2_1A3bii_H))*100,
                PM10_1A3bii_H = (t.1A3bii_H$PM10/t.1A3bii_H$sumF)*he_1A3bii_H, 
                PM10_1A3bii_H_p = (PM10_1A3bii_H/sum(PM10_1A3bii_H))*100,
                PM2.5_1A3bii_H = (t.1A3bii_H$PM2.5/t.1A3bii_H$sumF)*he_1A3bii_H, 
                PM2.5_1A3bii_H_p = (PM2.5_1A3bii_H/sum(PM2.5_1A3bii_H))*100,
                NMVOC_1A3bii_H = (t.1A3bii_H$NMVOC/t.1A3bii_H$sumF)*he_1A3bii_H, 
                NMVOC_1A3bii_H_p = (NMVOC_1A3bii_H/sum(NMVOC_1A3bii_H))*100,
                NH3_1A3bii_H = (t.1A3bii_H$NH3/t.1A3bii_H$sumF)*he_1A3bii_H, 
                NH3_1A3bii_H_p = (NH3_1A3bii_H/sum(NH3_1A3bii_H))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bii_H_p, SO2_1A3bii_H_p, PM10_1A3bii_H_p, PM2.5_1A3bii_H_p, NMVOC_1A3bii_H_p, NH3_1A3bii_H_p) %>%
  rename(`1A3bii_H_NOx` = NOx_1A3bii_H_p,
         `1A3bii_H_SO2` = SO2_1A3bii_H_p,
         `1A3bii_H_PM10` = PM10_1A3bii_H_p,
         `1A3bii_H_PM2.5` = PM2.5_1A3bii_H_p,
         `1A3bii_H_NMVOC` = NMVOC_1A3bii_H_p,
         `1A3bii_H_NH3` = NH3_1A3bii_H_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bii_H$`1A3bii_H_NOx`), sum(he.1A3bii_H$`1A3bii_H_SO2`), sum(he.1A3bii_H$`1A3bii_H_PM10`), sum(he.1A3bii_H$`1A3bii_H_PM2.5`), sum(he.1A3bii_H$`1A3bii_H_NMVOC`), sum(he.1A3bii_H$`1A3bii_H_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

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


sf.1A3biii_U <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3biii_U.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3biii_U <- sf.1A3biii_U %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3biii_U%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA
#

he.1A3biii_U <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3biii_U = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * (VA+0.5) * (RH0709+0.5) * (RH1517+0.5) + (TEMP+30) + SLP) %>%
  select(times, he_1A3biii_U)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3biii_U, aes(x = times, y = he_1A3biii_U)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3biii_U$he_1A3biii_U), max(he.1A3biii_U$he_1A3biii_U), sum(he.1A3biii_U$he_1A3biii_U))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3biii_U$sumF <- sum(he.1A3biii_U$he_1A3biii_U)
he.1A3biii_U %<>% 
  dplyr::mutate(NOx_1A3biii_U = (t.1A3biii_U$NOx/t.1A3biii_U$sumF)*he_1A3biii_U, 
                NOx_1A3biii_U_p = (NOx_1A3biii_U/sum(NOx_1A3biii_U))*100,
                SO2_1A3biii_U = (t.1A3biii_U$SO2/t.1A3biii_U$sumF)*he_1A3biii_U, 
                SO2_1A3biii_U_p = (SO2_1A3biii_U/sum(SO2_1A3biii_U))*100,
                PM10_1A3biii_U = (t.1A3biii_U$PM10/t.1A3biii_U$sumF)*he_1A3biii_U, 
                PM10_1A3biii_U_p = (PM10_1A3biii_U/sum(PM10_1A3biii_U))*100,
                PM2.5_1A3biii_U = (t.1A3biii_U$PM2.5/t.1A3biii_U$sumF)*he_1A3biii_U, 
                PM2.5_1A3biii_U_p = (PM2.5_1A3biii_U/sum(PM2.5_1A3biii_U))*100,
                NMVOC_1A3biii_U = (t.1A3biii_U$NMVOC/t.1A3biii_U$sumF)*he_1A3biii_U, 
                NMVOC_1A3biii_U_p = (NMVOC_1A3biii_U/sum(NMVOC_1A3biii_U))*100,
                NH3_1A3biii_U = (t.1A3biii_U$NH3/t.1A3biii_U$sumF)*he_1A3biii_U, 
                NH3_1A3biii_U_p = (NH3_1A3biii_U/sum(NH3_1A3biii_U))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3biii_U_p, SO2_1A3biii_U_p, PM10_1A3biii_U_p, PM2.5_1A3biii_U_p, NMVOC_1A3biii_U_p, NH3_1A3biii_U_p) %>%
  rename(`1A3biii_U_NOx` = NOx_1A3biii_U_p,
         `1A3biii_U_SO2` = SO2_1A3biii_U_p,
         `1A3biii_U_PM10` = PM10_1A3biii_U_p,
         `1A3biii_U_PM2.5` = PM2.5_1A3biii_U_p,
         `1A3biii_U_NMVOC` = NMVOC_1A3biii_U_p,
         `1A3biii_U_NH3` = NH3_1A3biii_U_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3biii_U$`1A3biii_U_NOx`), sum(he.1A3biii_U$`1A3biii_U_SO2`), sum(he.1A3biii_U$`1A3biii_U_PM10`), sum(he.1A3biii_U$`1A3biii_U_PM2.5`), sum(he.1A3biii_U$`1A3biii_U_NMVOC`), sum(he.1A3biii_U$`1A3biii_U_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#' ### Rural transport
#+ include = FALSE


sf.1A3biii_R <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3biii_R.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3biii_R <- sf.1A3biii_R %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3biii_R%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IIA + VA_IB
#

he.1A3biii_R <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3biii_R = VA_R) %>%
  select(times, he_1A3biii_R)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3biii_R, aes(x = times, y = he_1A3biii_R)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IIA + VA_IB")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3biii_R$he_1A3biii_R), max(he.1A3biii_R$he_1A3biii_R), sum(he.1A3biii_R$he_1A3biii_R))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3biii_R$sumF <- sum(he.1A3biii_R$he_1A3biii_R)
he.1A3biii_R %<>% 
  dplyr::mutate(NOx_1A3biii_R = (t.1A3biii_R$NOx/t.1A3biii_R$sumF)*he_1A3biii_R, 
                NOx_1A3biii_R_p = (NOx_1A3biii_R/sum(NOx_1A3biii_R))*100,
                SO2_1A3biii_R = (t.1A3biii_R$SO2/t.1A3biii_R$sumF)*he_1A3biii_R, 
                SO2_1A3biii_R_p = (SO2_1A3biii_R/sum(SO2_1A3biii_R))*100,
                PM10_1A3biii_R = (t.1A3biii_R$PM10/t.1A3biii_R$sumF)*he_1A3biii_R, 
                PM10_1A3biii_R_p = (PM10_1A3biii_R/sum(PM10_1A3biii_R))*100,
                PM2.5_1A3biii_R = (t.1A3biii_R$PM2.5/t.1A3biii_R$sumF)*he_1A3biii_R, 
                PM2.5_1A3biii_R_p = (PM2.5_1A3biii_R/sum(PM2.5_1A3biii_R))*100,
                NMVOC_1A3biii_R = (t.1A3biii_R$NMVOC/t.1A3biii_R$sumF)*he_1A3biii_R, 
                NMVOC_1A3biii_R_p = (NMVOC_1A3biii_R/sum(NMVOC_1A3biii_R))*100,
                NH3_1A3biii_R = (t.1A3biii_R$NH3/t.1A3biii_R$sumF)*he_1A3biii_R, 
                NH3_1A3biii_R_p = (NH3_1A3biii_R/sum(NH3_1A3biii_R))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3biii_R_p, SO2_1A3biii_R_p, PM10_1A3biii_R_p, PM2.5_1A3biii_R_p, NMVOC_1A3biii_R_p, NH3_1A3biii_R_p) %>%
  rename(`1A3biii_R_NOx` = NOx_1A3biii_R_p,
         `1A3biii_R_SO2` = SO2_1A3biii_R_p,
         `1A3biii_R_PM10` = PM10_1A3biii_R_p,
         `1A3biii_R_PM2.5` = PM2.5_1A3biii_R_p,
         `1A3biii_R_NMVOC` = NMVOC_1A3biii_R_p,
         `1A3biii_R_NH3` = NH3_1A3biii_R_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3biii_R$`1A3biii_R_NOx`), sum(he.1A3biii_R$`1A3biii_R_SO2`), sum(he.1A3biii_R$`1A3biii_R_PM10`), sum(he.1A3biii_R$`1A3biii_R_PM2.5`), sum(he.1A3biii_R$`1A3biii_R_NMVOC`), sum(he.1A3biii_R$`1A3biii_R_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#' 
#' ### Highways transport
#+ include = FALSE

sf.1A3biii_H <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3biii_H.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3biii_H <- sf.1A3biii_H %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3biii_H%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IA
#

he.1A3biii_H <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3biii_H = VA_H) %>%
  select(times, he_1A3biii_H)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3biii_H, aes(x = times, y = he_1A3biii_H)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3biii_H$he_1A3biii_H), max(he.1A3biii_H$he_1A3biii_H), sum(he.1A3biii_H$he_1A3biii_H))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3biii_H$sumF <- sum(he.1A3biii_H$he_1A3biii_H)
he.1A3biii_H %<>% 
  dplyr::mutate(NOx_1A3biii_H = (t.1A3biii_H$NOx/t.1A3biii_H$sumF)*he_1A3biii_H, 
                NOx_1A3biii_H_p = (NOx_1A3biii_H/sum(NOx_1A3biii_H))*100,
                SO2_1A3biii_H = (t.1A3biii_H$SO2/t.1A3biii_H$sumF)*he_1A3biii_H, 
                SO2_1A3biii_H_p = (SO2_1A3biii_H/sum(SO2_1A3biii_H))*100,
                PM10_1A3biii_H = (t.1A3biii_H$PM10/t.1A3biii_H$sumF)*he_1A3biii_H, 
                PM10_1A3biii_H_p = (PM10_1A3biii_H/sum(PM10_1A3biii_H))*100,
                PM2.5_1A3biii_H = (t.1A3biii_H$PM2.5/t.1A3biii_H$sumF)*he_1A3biii_H, 
                PM2.5_1A3biii_H_p = (PM2.5_1A3biii_H/sum(PM2.5_1A3biii_H))*100,
                NMVOC_1A3biii_H = (t.1A3biii_H$NMVOC/t.1A3biii_H$sumF)*he_1A3biii_H, 
                NMVOC_1A3biii_H_p = (NMVOC_1A3biii_H/sum(NMVOC_1A3biii_H))*100,
                NH3_1A3biii_H = (t.1A3biii_H$NH3/t.1A3biii_H$sumF)*he_1A3biii_H, 
                NH3_1A3biii_H_p = (NH3_1A3biii_H/sum(NH3_1A3biii_H))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3biii_H_p, SO2_1A3biii_H_p, PM10_1A3biii_H_p, PM2.5_1A3biii_H_p, NMVOC_1A3biii_H_p, NH3_1A3biii_H_p) %>%
  rename(`1A3biii_H_NOx` = NOx_1A3biii_H_p,
         `1A3biii_H_SO2` = SO2_1A3biii_H_p,
         `1A3biii_H_PM10` = PM10_1A3biii_H_p,
         `1A3biii_H_PM2.5` = PM2.5_1A3biii_H_p,
         `1A3biii_H_NMVOC` = NMVOC_1A3biii_H_p,
         `1A3biii_H_NH3` = NH3_1A3biii_H_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3biii_H$`1A3biii_H_NOx`), sum(he.1A3biii_H$`1A3biii_H_SO2`), sum(he.1A3biii_H$`1A3biii_H_PM10`), sum(he.1A3biii_H$`1A3biii_H_PM2.5`), sum(he.1A3biii_H$`1A3biii_H_NMVOC`), sum(he.1A3biii_H$`1A3biii_H_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

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


sf.1A3biii_bc_U <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3biii_bc_U.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3biii_bc_U <- sf.1A3biii_bc_U %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3biii_bc_U%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA
#

he.1A3biii_bc_U <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3biii_bc_U = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * (VA+0.5) * (RH0709+0.5) * (RH1517+0.5) + (TEMP+30) + SLP) %>%
  select(times, he_1A3biii_bc_U)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3biii_bc_U, aes(x = times, y = he_1A3biii_bc_U)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3biii_bc_U$he_1A3biii_bc_U), max(he.1A3biii_bc_U$he_1A3biii_bc_U), sum(he.1A3biii_bc_U$he_1A3biii_bc_U))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3biii_bc_U$sumF <- sum(he.1A3biii_bc_U$he_1A3biii_bc_U)
he.1A3biii_bc_U %<>% 
  dplyr::mutate(NOx_1A3biii_bc_U = (t.1A3biii_bc_U$NOx/t.1A3biii_bc_U$sumF)*he_1A3biii_bc_U, 
                NOx_1A3biii_bc_U_p = (NOx_1A3biii_bc_U/sum(NOx_1A3biii_bc_U))*100,
                SO2_1A3biii_bc_U = (t.1A3biii_bc_U$SO2/t.1A3biii_bc_U$sumF)*he_1A3biii_bc_U, 
                SO2_1A3biii_bc_U_p = (SO2_1A3biii_bc_U/sum(SO2_1A3biii_bc_U))*100,
                PM10_1A3biii_bc_U = (t.1A3biii_bc_U$PM10/t.1A3biii_bc_U$sumF)*he_1A3biii_bc_U, 
                PM10_1A3biii_bc_U_p = (PM10_1A3biii_bc_U/sum(PM10_1A3biii_bc_U))*100,
                PM2.5_1A3biii_bc_U = (t.1A3biii_bc_U$PM2.5/t.1A3biii_bc_U$sumF)*he_1A3biii_bc_U, 
                PM2.5_1A3biii_bc_U_p = (PM2.5_1A3biii_bc_U/sum(PM2.5_1A3biii_bc_U))*100,
                NMVOC_1A3biii_bc_U = (t.1A3biii_bc_U$NMVOC/t.1A3biii_bc_U$sumF)*he_1A3biii_bc_U, 
                NMVOC_1A3biii_bc_U_p = (NMVOC_1A3biii_bc_U/sum(NMVOC_1A3biii_bc_U))*100,
                NH3_1A3biii_bc_U = (t.1A3biii_bc_U$NH3/t.1A3biii_bc_U$sumF)*he_1A3biii_bc_U, 
                NH3_1A3biii_bc_U_p = (NH3_1A3biii_bc_U/sum(NH3_1A3biii_bc_U))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3biii_bc_U_p, SO2_1A3biii_bc_U_p, PM10_1A3biii_bc_U_p, PM2.5_1A3biii_bc_U_p, NMVOC_1A3biii_bc_U_p, NH3_1A3biii_bc_U_p) %>%
  rename(`1A3biii_bc_U_NOx` = NOx_1A3biii_bc_U_p,
         `1A3biii_bc_U_SO2` = SO2_1A3biii_bc_U_p,
         `1A3biii_bc_U_PM10` = PM10_1A3biii_bc_U_p,
         `1A3biii_bc_U_PM2.5` = PM2.5_1A3biii_bc_U_p,
         `1A3biii_bc_U_NMVOC` = NMVOC_1A3biii_bc_U_p,
         `1A3biii_bc_U_NH3` = NH3_1A3biii_bc_U_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3biii_bc_U$`1A3biii_bc_U_NOx`), sum(he.1A3biii_bc_U$`1A3biii_bc_U_SO2`), sum(he.1A3biii_bc_U$`1A3biii_bc_U_PM10`), sum(he.1A3biii_bc_U$`1A3biii_bc_U_PM2.5`), sum(he.1A3biii_bc_U$`1A3biii_bc_U_NMVOC`), sum(he.1A3biii_bc_U$`1A3biii_bc_U_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#' ### Rural transport
#+ include = FALSE


sf.1A3biii_bc_R <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3biii_bc_R.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3biii_bc_R <- sf.1A3biii_bc_R %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3biii_bc_R%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IIA + VA_IB
#

he.1A3biii_bc_R <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3biii_bc_R = VA_R) %>%
  select(times, he_1A3biii_bc_R)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3biii_bc_R, aes(x = times, y = he_1A3biii_bc_R)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IIA + VA_IB")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3biii_bc_R$he_1A3biii_bc_R), max(he.1A3biii_bc_R$he_1A3biii_bc_R), sum(he.1A3biii_bc_R$he_1A3biii_bc_R))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3biii_bc_R$sumF <- sum(he.1A3biii_bc_R$he_1A3biii_bc_R)
he.1A3biii_bc_R %<>% 
  dplyr::mutate(NOx_1A3biii_bc_R = (t.1A3biii_bc_R$NOx/t.1A3biii_bc_R$sumF)*he_1A3biii_bc_R, 
                NOx_1A3biii_bc_R_p = (NOx_1A3biii_bc_R/sum(NOx_1A3biii_bc_R))*100,
                SO2_1A3biii_bc_R = (t.1A3biii_bc_R$SO2/t.1A3biii_bc_R$sumF)*he_1A3biii_bc_R, 
                SO2_1A3biii_bc_R_p = (SO2_1A3biii_bc_R/sum(SO2_1A3biii_bc_R))*100,
                PM10_1A3biii_bc_R = (t.1A3biii_bc_R$PM10/t.1A3biii_bc_R$sumF)*he_1A3biii_bc_R, 
                PM10_1A3biii_bc_R_p = (PM10_1A3biii_bc_R/sum(PM10_1A3biii_bc_R))*100,
                PM2.5_1A3biii_bc_R = (t.1A3biii_bc_R$PM2.5/t.1A3biii_bc_R$sumF)*he_1A3biii_bc_R, 
                PM2.5_1A3biii_bc_R_p = (PM2.5_1A3biii_bc_R/sum(PM2.5_1A3biii_bc_R))*100,
                NMVOC_1A3biii_bc_R = (t.1A3biii_bc_R$NMVOC/t.1A3biii_bc_R$sumF)*he_1A3biii_bc_R, 
                NMVOC_1A3biii_bc_R_p = (NMVOC_1A3biii_bc_R/sum(NMVOC_1A3biii_bc_R))*100,
                NH3_1A3biii_bc_R = (t.1A3biii_bc_R$NH3/t.1A3biii_bc_R$sumF)*he_1A3biii_bc_R, 
                NH3_1A3biii_bc_R_p = (NH3_1A3biii_bc_R/sum(NH3_1A3biii_bc_R))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3biii_bc_R_p, SO2_1A3biii_bc_R_p, PM10_1A3biii_bc_R_p, PM2.5_1A3biii_bc_R_p, NMVOC_1A3biii_bc_R_p, NH3_1A3biii_bc_R_p) %>%
  rename(`1A3biii_bc_R_NOx` = NOx_1A3biii_bc_R_p,
         `1A3biii_bc_R_SO2` = SO2_1A3biii_bc_R_p,
         `1A3biii_bc_R_PM10` = PM10_1A3biii_bc_R_p,
         `1A3biii_bc_R_PM2.5` = PM2.5_1A3biii_bc_R_p,
         `1A3biii_bc_R_NMVOC` = NMVOC_1A3biii_bc_R_p,
         `1A3biii_bc_R_NH3` = NH3_1A3biii_bc_R_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3biii_bc_R$`1A3biii_bc_R_NOx`), sum(he.1A3biii_bc_R$`1A3biii_bc_R_SO2`), sum(he.1A3biii_bc_R$`1A3biii_bc_R_PM10`), sum(he.1A3biii_bc_R$`1A3biii_bc_R_PM2.5`), sum(he.1A3biii_bc_R$`1A3biii_bc_R_NMVOC`), sum(he.1A3biii_bc_R$`1A3biii_bc_R_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#' 
#' ### Highways transport
#+ include = FALSE

sf.1A3biii_bc_H <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3biii_bc_H.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3biii_bc_H <- sf.1A3biii_bc_H %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3biii_bc_H%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IA
#

he.1A3biii_bc_H <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3biii_bc_H = VA_H) %>%
  select(times, he_1A3biii_bc_H)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3biii_bc_H, aes(x = times, y = he_1A3biii_bc_H)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3biii_bc_H$he_1A3biii_bc_H), max(he.1A3biii_bc_H$he_1A3biii_bc_H), sum(he.1A3biii_bc_H$he_1A3biii_bc_H))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3biii_bc_H$sumF <- sum(he.1A3biii_bc_H$he_1A3biii_bc_H)
he.1A3biii_bc_H %<>% 
  dplyr::mutate(NOx_1A3biii_bc_H = (t.1A3biii_bc_H$NOx/t.1A3biii_bc_H$sumF)*he_1A3biii_bc_H, 
                NOx_1A3biii_bc_H_p = (NOx_1A3biii_bc_H/sum(NOx_1A3biii_bc_H))*100,
                SO2_1A3biii_bc_H = (t.1A3biii_bc_H$SO2/t.1A3biii_bc_H$sumF)*he_1A3biii_bc_H, 
                SO2_1A3biii_bc_H_p = (SO2_1A3biii_bc_H/sum(SO2_1A3biii_bc_H))*100,
                PM10_1A3biii_bc_H = (t.1A3biii_bc_H$PM10/t.1A3biii_bc_H$sumF)*he_1A3biii_bc_H, 
                PM10_1A3biii_bc_H_p = (PM10_1A3biii_bc_H/sum(PM10_1A3biii_bc_H))*100,
                PM2.5_1A3biii_bc_H = (t.1A3biii_bc_H$PM2.5/t.1A3biii_bc_H$sumF)*he_1A3biii_bc_H, 
                PM2.5_1A3biii_bc_H_p = (PM2.5_1A3biii_bc_H/sum(PM2.5_1A3biii_bc_H))*100,
                NMVOC_1A3biii_bc_H = (t.1A3biii_bc_H$NMVOC/t.1A3biii_bc_H$sumF)*he_1A3biii_bc_H, 
                NMVOC_1A3biii_bc_H_p = (NMVOC_1A3biii_bc_H/sum(NMVOC_1A3biii_bc_H))*100,
                NH3_1A3biii_bc_H = (t.1A3biii_bc_H$NH3/t.1A3biii_bc_H$sumF)*he_1A3biii_bc_H, 
                NH3_1A3biii_bc_H_p = (NH3_1A3biii_bc_H/sum(NH3_1A3biii_bc_H))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3biii_bc_H_p, SO2_1A3biii_bc_H_p, PM10_1A3biii_bc_H_p, PM2.5_1A3biii_bc_H_p, NMVOC_1A3biii_bc_H_p, NH3_1A3biii_bc_H_p) %>%
  rename(`1A3biii_bc_H_NOx` = NOx_1A3biii_bc_H_p,
         `1A3biii_bc_H_SO2` = SO2_1A3biii_bc_H_p,
         `1A3biii_bc_H_PM10` = PM10_1A3biii_bc_H_p,
         `1A3biii_bc_H_PM2.5` = PM2.5_1A3biii_bc_H_p,
         `1A3biii_bc_H_NMVOC` = NMVOC_1A3biii_bc_H_p,
         `1A3biii_bc_H_NH3` = NH3_1A3biii_bc_H_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3biii_bc_H$`1A3biii_bc_H_NOx`), sum(he.1A3biii_bc_H$`1A3biii_bc_H_SO2`), sum(he.1A3biii_bc_H$`1A3biii_bc_H_PM10`), sum(he.1A3biii_bc_H$`1A3biii_bc_H_PM2.5`), sum(he.1A3biii_bc_H$`1A3biii_bc_H_NMVOC`), sum(he.1A3biii_bc_H$`1A3biii_bc_H_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )




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


sf.1A3biv_U <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3biv_U.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3biv_U <- sf.1A3biv_U %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3biv_U%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA
#

he.1A3biv_U <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3biv_U = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * (VA+0.5) * (RH0709+0.5) * (RH1517+0.5) + (TEMP+30) + SLP) %>%
  select(times, he_1A3biv_U)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3biv_U, aes(x = times, y = he_1A3biv_U)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3biv_U$he_1A3biv_U), max(he.1A3biv_U$he_1A3biv_U), sum(he.1A3biv_U$he_1A3biv_U))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3biv_U$sumF <- sum(he.1A3biv_U$he_1A3biv_U)
he.1A3biv_U %<>% 
  dplyr::mutate(NOx_1A3biv_U = (t.1A3biv_U$NOx/t.1A3biv_U$sumF)*he_1A3biv_U, 
                NOx_1A3biv_U_p = (NOx_1A3biv_U/sum(NOx_1A3biv_U))*100,
                SO2_1A3biv_U = (t.1A3biv_U$SO2/t.1A3biv_U$sumF)*he_1A3biv_U, 
                SO2_1A3biv_U_p = (SO2_1A3biv_U/sum(SO2_1A3biv_U))*100,
                PM10_1A3biv_U = (t.1A3biv_U$PM10/t.1A3biv_U$sumF)*he_1A3biv_U, 
                PM10_1A3biv_U_p = (PM10_1A3biv_U/sum(PM10_1A3biv_U))*100,
                PM2.5_1A3biv_U = (t.1A3biv_U$PM2.5/t.1A3biv_U$sumF)*he_1A3biv_U, 
                PM2.5_1A3biv_U_p = (PM2.5_1A3biv_U/sum(PM2.5_1A3biv_U))*100,
                NMVOC_1A3biv_U = (t.1A3biv_U$NMVOC/t.1A3biv_U$sumF)*he_1A3biv_U, 
                NMVOC_1A3biv_U_p = (NMVOC_1A3biv_U/sum(NMVOC_1A3biv_U))*100,
                NH3_1A3biv_U = (t.1A3biv_U$NH3/t.1A3biv_U$sumF)*he_1A3biv_U, 
                NH3_1A3biv_U_p = (NH3_1A3biv_U/sum(NH3_1A3biv_U))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3biv_U_p, SO2_1A3biv_U_p, PM10_1A3biv_U_p, PM2.5_1A3biv_U_p, NMVOC_1A3biv_U_p, NH3_1A3biv_U_p) %>%
  rename(`1A3biv_U_NOx` = NOx_1A3biv_U_p,
         `1A3biv_U_SO2` = SO2_1A3biv_U_p,
         `1A3biv_U_PM10` = PM10_1A3biv_U_p,
         `1A3biv_U_PM2.5` = PM2.5_1A3biv_U_p,
         `1A3biv_U_NMVOC` = NMVOC_1A3biv_U_p,
         `1A3biv_U_NH3` = NH3_1A3biv_U_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3biv_U$`1A3biv_U_NOx`), sum(he.1A3biv_U$`1A3biv_U_SO2`), sum(he.1A3biv_U$`1A3biv_U_PM10`), sum(he.1A3biv_U$`1A3biv_U_PM2.5`), sum(he.1A3biv_U$`1A3biv_U_NMVOC`), sum(he.1A3biv_U$`1A3biv_U_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#' ### Rural transport
#+ include = FALSE


sf.1A3biv_R <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3biv_R.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3biv_R <- sf.1A3biv_R %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3biv_R%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IIA + VA_IB
#

he.1A3biv_R <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3biv_R = VA_R) %>%
  select(times, he_1A3biv_R)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3biv_R, aes(x = times, y = he_1A3biv_R)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IIA + VA_IB")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3biv_R$he_1A3biv_R), max(he.1A3biv_R$he_1A3biv_R), sum(he.1A3biv_R$he_1A3biv_R))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3biv_R$sumF <- sum(he.1A3biv_R$he_1A3biv_R)
he.1A3biv_R %<>% 
  dplyr::mutate(NOx_1A3biv_R = (t.1A3biv_R$NOx/t.1A3biv_R$sumF)*he_1A3biv_R, 
                NOx_1A3biv_R_p = (NOx_1A3biv_R/sum(NOx_1A3biv_R))*100,
                SO2_1A3biv_R = (t.1A3biv_R$SO2/t.1A3biv_R$sumF)*he_1A3biv_R, 
                SO2_1A3biv_R_p = (SO2_1A3biv_R/sum(SO2_1A3biv_R))*100,
                PM10_1A3biv_R = (t.1A3biv_R$PM10/t.1A3biv_R$sumF)*he_1A3biv_R, 
                PM10_1A3biv_R_p = (PM10_1A3biv_R/sum(PM10_1A3biv_R))*100,
                PM2.5_1A3biv_R = (t.1A3biv_R$PM2.5/t.1A3biv_R$sumF)*he_1A3biv_R, 
                PM2.5_1A3biv_R_p = (PM2.5_1A3biv_R/sum(PM2.5_1A3biv_R))*100,
                NMVOC_1A3biv_R = (t.1A3biv_R$NMVOC/t.1A3biv_R$sumF)*he_1A3biv_R, 
                NMVOC_1A3biv_R_p = (NMVOC_1A3biv_R/sum(NMVOC_1A3biv_R))*100,
                NH3_1A3biv_R = (t.1A3biv_R$NH3/t.1A3biv_R$sumF)*he_1A3biv_R, 
                NH3_1A3biv_R_p = (NH3_1A3biv_R/sum(NH3_1A3biv_R))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3biv_R_p, SO2_1A3biv_R_p, PM10_1A3biv_R_p, PM2.5_1A3biv_R_p, NMVOC_1A3biv_R_p, NH3_1A3biv_R_p) %>%
  rename(`1A3biv_R_NOx` = NOx_1A3biv_R_p,
         `1A3biv_R_SO2` = SO2_1A3biv_R_p,
         `1A3biv_R_PM10` = PM10_1A3biv_R_p,
         `1A3biv_R_PM2.5` = PM2.5_1A3biv_R_p,
         `1A3biv_R_NMVOC` = NMVOC_1A3biv_R_p,
         `1A3biv_R_NH3` = NH3_1A3biv_R_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3biv_R$`1A3biv_R_NOx`), sum(he.1A3biv_R$`1A3biv_R_SO2`), sum(he.1A3biv_R$`1A3biv_R_PM10`), sum(he.1A3biv_R$`1A3biv_R_PM2.5`), sum(he.1A3biv_R$`1A3biv_R_NMVOC`), sum(he.1A3biv_R$`1A3biv_R_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#' 
#' ### Highways transport
#+ include = FALSE

sf.1A3biv_H <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3biv_H.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3biv_H <- sf.1A3biv_H %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3biv_H%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IA
#

he.1A3biv_H <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3biv_H = VA_H) %>%
  select(times, he_1A3biv_H)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3biv_H, aes(x = times, y = he_1A3biv_H)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3biv_H$he_1A3biv_H), max(he.1A3biv_H$he_1A3biv_H), sum(he.1A3biv_H$he_1A3biv_H))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3biv_H$sumF <- sum(he.1A3biv_H$he_1A3biv_H)
he.1A3biv_H %<>% 
  dplyr::mutate(NOx_1A3biv_H = (t.1A3biv_H$NOx/t.1A3biv_H$sumF)*he_1A3biv_H, 
                NOx_1A3biv_H_p = (NOx_1A3biv_H/sum(NOx_1A3biv_H))*100,
                SO2_1A3biv_H = (t.1A3biv_H$SO2/t.1A3biv_H$sumF)*he_1A3biv_H, 
                SO2_1A3biv_H_p = (SO2_1A3biv_H/sum(SO2_1A3biv_H))*100,
                PM10_1A3biv_H = (t.1A3biv_H$PM10/t.1A3biv_H$sumF)*he_1A3biv_H, 
                PM10_1A3biv_H_p = (PM10_1A3biv_H/sum(PM10_1A3biv_H))*100,
                PM2.5_1A3biv_H = (t.1A3biv_H$PM2.5/t.1A3biv_H$sumF)*he_1A3biv_H, 
                PM2.5_1A3biv_H_p = (PM2.5_1A3biv_H/sum(PM2.5_1A3biv_H))*100,
                NMVOC_1A3biv_H = (t.1A3biv_H$NMVOC/t.1A3biv_H$sumF)*he_1A3biv_H, 
                NMVOC_1A3biv_H_p = (NMVOC_1A3biv_H/sum(NMVOC_1A3biv_H))*100,
                NH3_1A3biv_H = (t.1A3biv_H$NH3/t.1A3biv_H$sumF)*he_1A3biv_H, 
                NH3_1A3biv_H_p = (NH3_1A3biv_H/sum(NH3_1A3biv_H))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3biv_H_p, SO2_1A3biv_H_p, PM10_1A3biv_H_p, PM2.5_1A3biv_H_p, NMVOC_1A3biv_H_p, NH3_1A3biv_H_p) %>%
  rename(`1A3biv_H_NOx` = NOx_1A3biv_H_p,
         `1A3biv_H_SO2` = SO2_1A3biv_H_p,
         `1A3biv_H_PM10` = PM10_1A3biv_H_p,
         `1A3biv_H_PM2.5` = PM2.5_1A3biv_H_p,
         `1A3biv_H_NMVOC` = NMVOC_1A3biv_H_p,
         `1A3biv_H_NH3` = NH3_1A3biv_H_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3biv_H$`1A3biv_H_NOx`), sum(he.1A3biv_H$`1A3biv_H_SO2`), sum(he.1A3biv_H$`1A3biv_H_PM10`), sum(he.1A3biv_H$`1A3biv_H_PM2.5`), sum(he.1A3biv_H$`1A3biv_H_NMVOC`), sum(he.1A3biv_H$`1A3biv_H_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )



#'
#'
#'
#'
#'
#'
#' ## 1A3bv-Road transport: Gasoline evaporation

#' ### Urban transport
#+ include = FALSE


sf.1A3bv_U <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bv_U.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bv_U <- sf.1A3bv_U %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bv_U%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA
#

he.1A3bv_U <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bv_U = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * (VA+0.5) * (RH0709+0.5) * (RH1517+0.5) + (TEMP+30) + SLP) %>%
  select(times, he_1A3bv_U)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bv_U, aes(x = times, y = he_1A3bv_U)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bv_U$he_1A3bv_U), max(he.1A3bv_U$he_1A3bv_U), sum(he.1A3bv_U$he_1A3bv_U))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bv_U$sumF <- sum(he.1A3bv_U$he_1A3bv_U)
he.1A3bv_U %<>% 
  dplyr::mutate(NOx_1A3bv_U = (t.1A3bv_U$NOx/t.1A3bv_U$sumF)*he_1A3bv_U, 
                NOx_1A3bv_U_p = (NOx_1A3bv_U/sum(NOx_1A3bv_U))*100,
                SO2_1A3bv_U = (t.1A3bv_U$SO2/t.1A3bv_U$sumF)*he_1A3bv_U, 
                SO2_1A3bv_U_p = (SO2_1A3bv_U/sum(SO2_1A3bv_U))*100,
                PM10_1A3bv_U = (t.1A3bv_U$PM10/t.1A3bv_U$sumF)*he_1A3bv_U, 
                PM10_1A3bv_U_p = (PM10_1A3bv_U/sum(PM10_1A3bv_U))*100,
                PM2.5_1A3bv_U = (t.1A3bv_U$PM2.5/t.1A3bv_U$sumF)*he_1A3bv_U, 
                PM2.5_1A3bv_U_p = (PM2.5_1A3bv_U/sum(PM2.5_1A3bv_U))*100,
                NMVOC_1A3bv_U = (t.1A3bv_U$NMVOC/t.1A3bv_U$sumF)*he_1A3bv_U, 
                NMVOC_1A3bv_U_p = (NMVOC_1A3bv_U/sum(NMVOC_1A3bv_U))*100,
                NH3_1A3bv_U = (t.1A3bv_U$NH3/t.1A3bv_U$sumF)*he_1A3bv_U, 
                NH3_1A3bv_U_p = (NH3_1A3bv_U/sum(NH3_1A3bv_U))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bv_U_p, SO2_1A3bv_U_p, PM10_1A3bv_U_p, PM2.5_1A3bv_U_p, NMVOC_1A3bv_U_p, NH3_1A3bv_U_p) %>%
  rename(`1A3bv_U_NOx` = NOx_1A3bv_U_p,
         `1A3bv_U_SO2` = SO2_1A3bv_U_p,
         `1A3bv_U_PM10` = PM10_1A3bv_U_p,
         `1A3bv_U_PM2.5` = PM2.5_1A3bv_U_p,
         `1A3bv_U_NMVOC` = NMVOC_1A3bv_U_p,
         `1A3bv_U_NH3` = NH3_1A3bv_U_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bv_U$`1A3bv_U_NOx`), sum(he.1A3bv_U$`1A3bv_U_SO2`), sum(he.1A3bv_U$`1A3bv_U_PM10`), sum(he.1A3bv_U$`1A3bv_U_PM2.5`), sum(he.1A3bv_U$`1A3bv_U_NMVOC`), sum(he.1A3bv_U$`1A3bv_U_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#' ### Rural transport
#+ include = FALSE


sf.1A3bv_R <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bv_R.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bv_R <- sf.1A3bv_R %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bv_R%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IIA + VA_IB
#

he.1A3bv_R <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bv_R = VA_R) %>%
  select(times, he_1A3bv_R)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bv_R, aes(x = times, y = he_1A3bv_R)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IIA + VA_IB")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bv_R$he_1A3bv_R), max(he.1A3bv_R$he_1A3bv_R), sum(he.1A3bv_R$he_1A3bv_R))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bv_R$sumF <- sum(he.1A3bv_R$he_1A3bv_R)
he.1A3bv_R %<>% 
  dplyr::mutate(NOx_1A3bv_R = (t.1A3bv_R$NOx/t.1A3bv_R$sumF)*he_1A3bv_R, 
                NOx_1A3bv_R_p = (NOx_1A3bv_R/sum(NOx_1A3bv_R))*100,
                SO2_1A3bv_R = (t.1A3bv_R$SO2/t.1A3bv_R$sumF)*he_1A3bv_R, 
                SO2_1A3bv_R_p = (SO2_1A3bv_R/sum(SO2_1A3bv_R))*100,
                PM10_1A3bv_R = (t.1A3bv_R$PM10/t.1A3bv_R$sumF)*he_1A3bv_R, 
                PM10_1A3bv_R_p = (PM10_1A3bv_R/sum(PM10_1A3bv_R))*100,
                PM2.5_1A3bv_R = (t.1A3bv_R$PM2.5/t.1A3bv_R$sumF)*he_1A3bv_R, 
                PM2.5_1A3bv_R_p = (PM2.5_1A3bv_R/sum(PM2.5_1A3bv_R))*100,
                NMVOC_1A3bv_R = (t.1A3bv_R$NMVOC/t.1A3bv_R$sumF)*he_1A3bv_R, 
                NMVOC_1A3bv_R_p = (NMVOC_1A3bv_R/sum(NMVOC_1A3bv_R))*100,
                NH3_1A3bv_R = (t.1A3bv_R$NH3/t.1A3bv_R$sumF)*he_1A3bv_R, 
                NH3_1A3bv_R_p = (NH3_1A3bv_R/sum(NH3_1A3bv_R))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bv_R_p, SO2_1A3bv_R_p, PM10_1A3bv_R_p, PM2.5_1A3bv_R_p, NMVOC_1A3bv_R_p, NH3_1A3bv_R_p) %>%
  rename(`1A3bv_R_NOx` = NOx_1A3bv_R_p,
         `1A3bv_R_SO2` = SO2_1A3bv_R_p,
         `1A3bv_R_PM10` = PM10_1A3bv_R_p,
         `1A3bv_R_PM2.5` = PM2.5_1A3bv_R_p,
         `1A3bv_R_NMVOC` = NMVOC_1A3bv_R_p,
         `1A3bv_R_NH3` = NH3_1A3bv_R_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bv_R$`1A3bv_R_NOx`), sum(he.1A3bv_R$`1A3bv_R_SO2`), sum(he.1A3bv_R$`1A3bv_R_PM10`), sum(he.1A3bv_R$`1A3bv_R_PM2.5`), sum(he.1A3bv_R$`1A3bv_R_NMVOC`), sum(he.1A3bv_R$`1A3bv_R_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#' 
#' ### Highways transport
#+ include = FALSE

sf.1A3bv_H <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bv_H.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bv_H <- sf.1A3bv_H %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bv_H%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = VA_IA
#

he.1A3bv_H <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bv_H = VA_H) %>%
  select(times, he_1A3bv_H)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bv_H, aes(x = times, y = he_1A3bv_H)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = VA_IA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bv_H$he_1A3bv_H), max(he.1A3bv_H$he_1A3bv_H), sum(he.1A3bv_H$he_1A3bv_H))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bv_H$sumF <- sum(he.1A3bv_H$he_1A3bv_H)
he.1A3bv_H %<>% 
  dplyr::mutate(NOx_1A3bv_H = (t.1A3bv_H$NOx/t.1A3bv_H$sumF)*he_1A3bv_H, 
                NOx_1A3bv_H_p = (NOx_1A3bv_H/sum(NOx_1A3bv_H))*100,
                SO2_1A3bv_H = (t.1A3bv_H$SO2/t.1A3bv_H$sumF)*he_1A3bv_H, 
                SO2_1A3bv_H_p = (SO2_1A3bv_H/sum(SO2_1A3bv_H))*100,
                PM10_1A3bv_H = (t.1A3bv_H$PM10/t.1A3bv_H$sumF)*he_1A3bv_H, 
                PM10_1A3bv_H_p = (PM10_1A3bv_H/sum(PM10_1A3bv_H))*100,
                PM2.5_1A3bv_H = (t.1A3bv_H$PM2.5/t.1A3bv_H$sumF)*he_1A3bv_H, 
                PM2.5_1A3bv_H_p = (PM2.5_1A3bv_H/sum(PM2.5_1A3bv_H))*100,
                NMVOC_1A3bv_H = (t.1A3bv_H$NMVOC/t.1A3bv_H$sumF)*he_1A3bv_H, 
                NMVOC_1A3bv_H_p = (NMVOC_1A3bv_H/sum(NMVOC_1A3bv_H))*100,
                NH3_1A3bv_H = (t.1A3bv_H$NH3/t.1A3bv_H$sumF)*he_1A3bv_H, 
                NH3_1A3bv_H_p = (NH3_1A3bv_H/sum(NH3_1A3bv_H))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bv_H_p, SO2_1A3bv_H_p, PM10_1A3bv_H_p, PM2.5_1A3bv_H_p, NMVOC_1A3bv_H_p, NH3_1A3bv_H_p) %>%
  rename(`1A3bv_H_NOx` = NOx_1A3bv_H_p,
         `1A3bv_H_SO2` = SO2_1A3bv_H_p,
         `1A3bv_H_PM10` = PM10_1A3bv_H_p,
         `1A3bv_H_PM2.5` = PM2.5_1A3bv_H_p,
         `1A3bv_H_NMVOC` = NMVOC_1A3bv_H_p,
         `1A3bv_H_NH3` = NH3_1A3bv_H_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bv_H$`1A3bv_H_NOx`), sum(he.1A3bv_H$`1A3bv_H_SO2`), sum(he.1A3bv_H$`1A3bv_H_PM10`), sum(he.1A3bv_H$`1A3bv_H_PM2.5`), sum(he.1A3bv_H$`1A3bv_H_NMVOC`), sum(he.1A3bv_H$`1A3bv_H_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


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


sf.1A3bvi <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bvi.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bvi <- sf.1A3bvi %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bvi%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA
#

he.1A3bvi <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bvi = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * (VA+0.5) * (RH0709+0.5) * (RH1517+0.5) + (TEMP+30) + SLP) %>%
  select(times, he_1A3bvi)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bvi, aes(x = times, y = he_1A3bvi)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bvi$he_1A3bvi), max(he.1A3bvi$he_1A3bvi), sum(he.1A3bvi$he_1A3bvi))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bvi$sumF <- sum(he.1A3bvi$he_1A3bvi)
he.1A3bvi %<>% 
  dplyr::mutate(NOx_1A3bvi = (t.1A3bvi$NOx/t.1A3bvi$sumF)*he_1A3bvi, 
                NOx_1A3bvi_p = (NOx_1A3bvi/sum(NOx_1A3bvi))*100,
                SO2_1A3bvi = (t.1A3bvi$SO2/t.1A3bvi$sumF)*he_1A3bvi, 
                SO2_1A3bvi_p = (SO2_1A3bvi/sum(SO2_1A3bvi))*100,
                PM10_1A3bvi = (t.1A3bvi$PM10/t.1A3bvi$sumF)*he_1A3bvi, 
                PM10_1A3bvi_p = (PM10_1A3bvi/sum(PM10_1A3bvi))*100,
                PM2.5_1A3bvi = (t.1A3bvi$PM2.5/t.1A3bvi$sumF)*he_1A3bvi, 
                PM2.5_1A3bvi_p = (PM2.5_1A3bvi/sum(PM2.5_1A3bvi))*100,
                NMVOC_1A3bvi = (t.1A3bvi$NMVOC/t.1A3bvi$sumF)*he_1A3bvi, 
                NMVOC_1A3bvi_p = (NMVOC_1A3bvi/sum(NMVOC_1A3bvi))*100,
                NH3_1A3bvi = (t.1A3bvi$NH3/t.1A3bvi$sumF)*he_1A3bvi, 
                NH3_1A3bvi_p = (NH3_1A3bvi/sum(NH3_1A3bvi))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bvi_p, SO2_1A3bvi_p, PM10_1A3bvi_p, PM2.5_1A3bvi_p, NMVOC_1A3bvi_p, NH3_1A3bvi_p) %>%
  rename(`1A3bvi_NOx` = NOx_1A3bvi_p,
         `1A3bvi_SO2` = SO2_1A3bvi_p,
         `1A3bvi_PM10` = PM10_1A3bvi_p,
         `1A3bvi_PM2.5` = PM2.5_1A3bvi_p,
         `1A3bvi_NMVOC` = NMVOC_1A3bvi_p,
         `1A3bvi_NH3` = NH3_1A3bvi_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bvi$`1A3bvi_NOx`), sum(he.1A3bvi$`1A3bvi_SO2`), sum(he.1A3bvi$`1A3bvi_PM10`), sum(he.1A3bvi$`1A3bvi_PM2.5`), sum(he.1A3bvi$`1A3bvi_NMVOC`), sum(he.1A3bvi$`1A3bvi_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )





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


sf.1A3bvii <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3bvii.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3bvii <- sf.1A3bvii %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3bvii%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA
#

he.1A3bvii <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3bvii = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * (VA+0.5) * (RH0709+0.5) * (RH1517+0.5) + (TEMP+30) + SLP) %>%
  select(times, he_1A3bvii)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3bvii, aes(x = times, y = he_1A3bvii)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WDWW + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + SLP + VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3bvii$he_1A3bvii), max(he.1A3bvii$he_1A3bvii), sum(he.1A3bvii$he_1A3bvii))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3bvii$sumF <- sum(he.1A3bvii$he_1A3bvii)
he.1A3bvii %<>% 
  dplyr::mutate(NOx_1A3bvii = (t.1A3bvii$NOx/t.1A3bvii$sumF)*he_1A3bvii, 
                NOx_1A3bvii_p = (NOx_1A3bvii/sum(NOx_1A3bvii))*100,
                SO2_1A3bvii = (t.1A3bvii$SO2/t.1A3bvii$sumF)*he_1A3bvii, 
                SO2_1A3bvii_p = (SO2_1A3bvii/sum(SO2_1A3bvii))*100,
                PM10_1A3bvii = (t.1A3bvii$PM10/t.1A3bvii$sumF)*he_1A3bvii, 
                PM10_1A3bvii_p = (PM10_1A3bvii/sum(PM10_1A3bvii))*100,
                PM2.5_1A3bvii = (t.1A3bvii$PM2.5/t.1A3bvii$sumF)*he_1A3bvii, 
                PM2.5_1A3bvii_p = (PM2.5_1A3bvii/sum(PM2.5_1A3bvii))*100,
                NMVOC_1A3bvii = (t.1A3bvii$NMVOC/t.1A3bvii$sumF)*he_1A3bvii, 
                NMVOC_1A3bvii_p = (NMVOC_1A3bvii/sum(NMVOC_1A3bvii))*100,
                NH3_1A3bvii = (t.1A3bvii$NH3/t.1A3bvii$sumF)*he_1A3bvii, 
                NH3_1A3bvii_p = (NH3_1A3bvii/sum(NH3_1A3bvii))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3bvii_p, SO2_1A3bvii_p, PM10_1A3bvii_p, PM2.5_1A3bvii_p, NMVOC_1A3bvii_p, NH3_1A3bvii_p) %>%
  rename(`1A3bvii_NOx` = NOx_1A3bvii_p,
         `1A3bvii_SO2` = SO2_1A3bvii_p,
         `1A3bvii_PM10` = PM10_1A3bvii_p,
         `1A3bvii_PM2.5` = PM2.5_1A3bvii_p,
         `1A3bvii_NMVOC` = NMVOC_1A3bvii_p,
         `1A3bvii_NH3` = NH3_1A3bvii_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3bvii$`1A3bvii_NOx`), sum(he.1A3bvii$`1A3bvii_SO2`), sum(he.1A3bvii$`1A3bvii_PM10`), sum(he.1A3bvii$`1A3bvii_PM2.5`), sum(he.1A3bvii$`1A3bvii_NMVOC`), sum(he.1A3bvii$`1A3bvii_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )





#'
#'
#'
#'
#'
#'
#' ## 1A3c-Railways
#+ include = FALSE


sf.1A3c <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3c.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3c <- sf.1A3c %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3c%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + WT1624 + WE + !PH + SA
#

he.1A3c <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3c = (WT0816+0.5) * (WT1624+0.5) * WE2 * PH2 * ((SA * (-1.5))+2.5)) %>%
  select(times, he_1A3c)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3c, aes(x = times, y = he_1A3c)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WDWW + WT0816 + WT1624 + WE + !PH + SA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3c$he_1A3c), max(he.1A3c$he_1A3c), sum(he.1A3c$he_1A3c))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3c$sumF <- sum(he.1A3c$he_1A3c)
he.1A3c %<>% 
  dplyr::mutate(NOx_1A3c = (t.1A3c$NOx/t.1A3c$sumF)*he_1A3c, 
                NOx_1A3c_p = (NOx_1A3c/sum(NOx_1A3c))*100,
                SO2_1A3c = (t.1A3c$SO2/t.1A3c$sumF)*he_1A3c, 
                SO2_1A3c_p = (SO2_1A3c/sum(SO2_1A3c))*100,
                PM10_1A3c = (t.1A3c$PM10/t.1A3c$sumF)*he_1A3c, 
                PM10_1A3c_p = (PM10_1A3c/sum(PM10_1A3c))*100,
                PM2.5_1A3c = (t.1A3c$PM2.5/t.1A3c$sumF)*he_1A3c, 
                PM2.5_1A3c_p = (PM2.5_1A3c/sum(PM2.5_1A3c))*100,
                NMVOC_1A3c = (t.1A3c$NMVOC/t.1A3c$sumF)*he_1A3c, 
                NMVOC_1A3c_p = (NMVOC_1A3c/sum(NMVOC_1A3c))*100,
                NH3_1A3c = (t.1A3c$NH3/t.1A3c$sumF)*he_1A3c, 
                NH3_1A3c_p = (NH3_1A3c/sum(NH3_1A3c))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3c_p, SO2_1A3c_p, PM10_1A3c_p, PM2.5_1A3c_p, NMVOC_1A3c_p, NH3_1A3c_p) %>%
  rename(`1A3c_NOx` = NOx_1A3c_p,
         `1A3c_SO2` = SO2_1A3c_p,
         `1A3c_PM10` = PM10_1A3c_p,
         `1A3c_PM2.5` = PM2.5_1A3c_p,
         `1A3c_NMVOC` = NMVOC_1A3c_p,
         `1A3c_NH3` = NH3_1A3c_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3c$`1A3c_NOx`), sum(he.1A3c$`1A3c_SO2`), sum(he.1A3c$`1A3c_PM10`), sum(he.1A3c$`1A3c_PM2.5`), sum(he.1A3c$`1A3c_NMVOC`), sum(he.1A3c$`1A3c_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 1A3dii-National navigation (shipping)
#+ include = FALSE

sf.1A3dii <- st_read("D:/R_projects/Spatialization/Products/1A3 - Transport/1A3dii.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A3dii <- sf.1A3dii %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A3dii%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL  + WE + !PH + SA
#

he.1A3dii <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A3dii = (DL+0.5) * WE2 * PH2 * ((SA * (-1.5))+2.5)) %>%
  select(times, he_1A3dii)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A3dii, aes(x = times, y = he_1A3dii)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WDWW + DL  + WE + !PH + SA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A3dii$he_1A3dii), max(he.1A3dii$he_1A3dii), sum(he.1A3dii$he_1A3dii))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A3dii$sumF <- sum(he.1A3dii$he_1A3dii)
he.1A3dii %<>% 
  dplyr::mutate(NOx_1A3dii = (t.1A3dii$NOx/t.1A3dii$sumF)*he_1A3dii, 
                NOx_1A3dii_p = (NOx_1A3dii/sum(NOx_1A3dii))*100,
                SO2_1A3dii = (t.1A3dii$SO2/t.1A3dii$sumF)*he_1A3dii, 
                SO2_1A3dii_p = (SO2_1A3dii/sum(SO2_1A3dii))*100,
                PM10_1A3dii = (t.1A3dii$PM10/t.1A3dii$sumF)*he_1A3dii, 
                PM10_1A3dii_p = (PM10_1A3dii/sum(PM10_1A3dii))*100,
                PM2.5_1A3dii = (t.1A3dii$PM2.5/t.1A3dii$sumF)*he_1A3dii, 
                PM2.5_1A3dii_p = (PM2.5_1A3dii/sum(PM2.5_1A3dii))*100,
                NMVOC_1A3dii = (t.1A3dii$NMVOC/t.1A3dii$sumF)*he_1A3dii, 
                NMVOC_1A3dii_p = (NMVOC_1A3dii/sum(NMVOC_1A3dii))*100,
                NH3_1A3dii = (t.1A3dii$NH3/t.1A3dii$sumF)*he_1A3dii, 
                NH3_1A3dii_p = (NH3_1A3dii/sum(NH3_1A3dii))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A3dii_p, SO2_1A3dii_p, PM10_1A3dii_p, PM2.5_1A3dii_p, NMVOC_1A3dii_p, NH3_1A3dii_p) %>%
  rename(`1A3dii_NOx` = NOx_1A3dii_p,
         `1A3dii_SO2` = SO2_1A3dii_p,
         `1A3dii_PM10` = PM10_1A3dii_p,
         `1A3dii_PM2.5` = PM2.5_1A3dii_p,
         `1A3dii_NMVOC` = NMVOC_1A3dii_p,
         `1A3dii_NH3` = NH3_1A3dii_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A3dii$`1A3dii_NOx`), sum(he.1A3dii$`1A3dii_SO2`), sum(he.1A3dii$`1A3dii_PM10`), sum(he.1A3dii$`1A3dii_PM2.5`), sum(he.1A3dii$`1A3dii_NMVOC`), sum(he.1A3dii$`1A3dii_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )



# temporalProfile_Transports<- activity.df$times %>% cbind(he.1A3ai[,1:6], 
#                                                            he.1A3aii[,1:6], 
#                                                            he.1A3bi_H[,1:6], 
#                                                            he.1A3bi_R[,1:6], 
#                                                            he.1A3bi_U[,1:6], 
#                                                            he.1A3bii_H[,1:6], 
#                                                            he.1A3bii_R[,1:6], 
#                                                            he.1A3bii_U[,1:6], 
#                                                            he.1A3biii_bc_H[,1:6], 
#                                                            he.1A3biii_bc_R[,1:6], 
#                                                            he.1A3biii_bc_U[,1:6], 
#                                                            he.1A3biii_H[,1:6], 
#                                                            he.1A3biii_R[,1:6], 
#                                                            he.1A3biii_U[,1:6], 
#                                                          he.1A3biv_H[,1:6], 
#                                                          he.1A3biv_R[,1:6], 
#                                                          he.1A3biv_U[,1:6], 
#                                                          he.1A3bv_H[,1:6], 
#                                                          he.1A3bv_R[,1:6], 
#                                                          he.1A3bv_U[,1:6], 
#                                                          he.1A3bvi[,1:6], 
#                                                          he.1A3bvii[,1:6], 
#                                                          he.1A3c[,1:6], 
#                                                          he.1A3dii[,1:6]) %>% 
#   as.data.frame()
# 
# writexl::write_xlsx(temporalProfile_Transports, path = 'Hourly_emissions/Products/TemporalProfile_Transports.xlsx')










