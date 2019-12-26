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
#' # 1A2 / 2 - Industrial processes
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
#' ## 1A2a / 2C1 - Iron and Steel
#+ include = FALSE

sf.1A2a <- st_read("D:/R_projects/Spatialization/Products/1A2 - Industry/1A2a.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A2a <- sf.1A2a %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A2a%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + !PH + !RP
#

he.1A2a <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A2a = (WDWW * (WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * WE2) %>%
  select(times, he_1A2a)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A2a, aes(x = times, y = he_1A2a)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A2a$he_1A2a), max(he.1A2a$he_1A2a), sum(he.1A2a$he_1A2a))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A2a$sumF <- sum(he.1A2a$he_1A2a)
he.1A2a %<>% 
  dplyr::mutate(NOx_1A2a = (t.1A2a$NOx/t.1A2a$sumF)*he_1A2a, 
                NOx_1A2a_p = (NOx_1A2a/sum(NOx_1A2a))*100,
                SO2_1A2a = (t.1A2a$SO2/t.1A2a$sumF)*he_1A2a, 
                SO2_1A2a_p = (SO2_1A2a/sum(SO2_1A2a))*100,
                PM10_1A2a = (t.1A2a$PM10/t.1A2a$sumF)*he_1A2a, 
                PM10_1A2a_p = (PM10_1A2a/sum(PM10_1A2a))*100,
                PM2.5_1A2a = (t.1A2a$PM2.5/t.1A2a$sumF)*he_1A2a, 
                PM2.5_1A2a_p = (PM2.5_1A2a/sum(PM2.5_1A2a))*100,
                NMVOC_1A2a = (t.1A2a$NMVOC/t.1A2a$sumF)*he_1A2a, 
                NMVOC_1A2a_p = (NMVOC_1A2a/sum(NMVOC_1A2a))*100,
                NH3_1A2a = (t.1A2a$NH3/t.1A2a$sumF)*he_1A2a, 
                NH3_1A2a_p = (NH3_1A2a/sum(NH3_1A2a))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A2a_p, SO2_1A2a_p, PM10_1A2a_p, PM2.5_1A2a_p, NMVOC_1A2a_p, NH3_1A2a_p) %>%
  rename(`1A2a_NOx` = NOx_1A2a_p,
         `1A2a_SO2` = SO2_1A2a_p,
         `1A2a_PM10` = PM10_1A2a_p,
         `1A2a_PM2.5` = PM2.5_1A2a_p,
         `1A2a_NMVOC` = NMVOC_1A2a_p,
         `1A2a_NH3` = NH3_1A2a_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A2a$`1A2a_NOx`), sum(he.1A2a$`1A2a_SO2`), sum(he.1A2a$`1A2a_PM10`), sum(he.1A2a$`1A2a_PM2.5`), sum(he.1A2a$`1A2a_NMVOC`), sum(he.1A2a$`1A2a_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#'
#'
#'
#'
#'
#'
#' ## 1A2b - Non-ferrous metals
#+ include = FALSE

sf.1A2b <- st_read("D:/R_projects/Spatialization/Products/1A2 - Industry/1A2b.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A2b <- sf.1A2b %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A2b%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + !PH + !RP
#

he.1A2b <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A2b = (WDWW * (WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * WE2) %>%
  select(times, he_1A2b)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-15 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A2b, aes(x = times, y = he_1A2b)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A2b$he_1A2b), max(he.1A2b$he_1A2b), sum(he.1A2b$he_1A2b))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A2b$sumF <- sum(he.1A2b$he_1A2b)
he.1A2b %<>% 
  dplyr::mutate(NOx_1A2b = (t.1A2b$NOx/t.1A2b$sumF)*he_1A2b, 
                NOx_1A2b_p = (NOx_1A2b/sum(NOx_1A2b))*100,
                SO2_1A2b = (t.1A2b$SO2/t.1A2b$sumF)*he_1A2b, 
                SO2_1A2b_p = (SO2_1A2b/sum(SO2_1A2b))*100,
                PM10_1A2b = (t.1A2b$PM10/t.1A2b$sumF)*he_1A2b, 
                PM10_1A2b_p = (PM10_1A2b/sum(PM10_1A2b))*100,
                PM2.5_1A2b = (t.1A2b$PM2.5/t.1A2b$sumF)*he_1A2b, 
                PM2.5_1A2b_p = (PM2.5_1A2b/sum(PM2.5_1A2b))*100,
                NMVOC_1A2b = (t.1A2b$NMVOC/t.1A2b$sumF)*he_1A2b, 
                NMVOC_1A2b_p = (NMVOC_1A2b/sum(NMVOC_1A2b))*100,
                NH3_1A2b = (t.1A2b$NH3/t.1A2b$sumF)*he_1A2b, 
                NH3_1A2b_p = (NH3_1A2b/sum(NH3_1A2b))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A2b_p, SO2_1A2b_p, PM10_1A2b_p, PM2.5_1A2b_p, NMVOC_1A2b_p, NH3_1A2b_p) %>%
  rename(`1A2b_NOx` = NOx_1A2b_p,
         `1A2b_SO2` = SO2_1A2b_p,
         `1A2b_PM10` = PM10_1A2b_p,
         `1A2b_PM2.5` = PM2.5_1A2b_p,
         `1A2b_NMVOC` = NMVOC_1A2b_p,
         `1A2b_NH3` = NH3_1A2b_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A2b$`1A2b_NOx`), sum(he.1A2b$`1A2b_SO2`), sum(he.1A2b$`1A2b_PM10`), sum(he.1A2b$`1A2b_PM2.5`), sum(he.1A2b$`1A2b_NMVOC`), sum(he.1A2b$`1A2b_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#'
#'
#'
#'
#'
#'
#' ## 1A2c - Chemicals
#+ include = FALSE

sf.1A2c <- st_read("D:/R_projects/Spatialization/Products/1A2 - Industry/1A2c.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A2c <- sf.1A2c %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A2c%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + !PH + !RP
#

he.1A2c <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A2c = (WDWW * (WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * WE2) %>%
  select(times, he_1A2c)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-15 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A2c, aes(x = times, y = he_1A2c)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A2c$he_1A2c), max(he.1A2c$he_1A2c), sum(he.1A2c$he_1A2c))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A2c$sumF <- sum(he.1A2c$he_1A2c)
he.1A2c %<>% 
  dplyr::mutate(NOx_1A2c = (t.1A2c$NOx/t.1A2c$sumF)*he_1A2c, 
                NOx_1A2c_p = (NOx_1A2c/sum(NOx_1A2c))*100,
                SO2_1A2c = (t.1A2c$SO2/t.1A2c$sumF)*he_1A2c, 
                SO2_1A2c_p = (SO2_1A2c/sum(SO2_1A2c))*100,
                PM10_1A2c = (t.1A2c$PM10/t.1A2c$sumF)*he_1A2c, 
                PM10_1A2c_p = (PM10_1A2c/sum(PM10_1A2c))*100,
                PM2.5_1A2c = (t.1A2c$PM2.5/t.1A2c$sumF)*he_1A2c, 
                PM2.5_1A2c_p = (PM2.5_1A2c/sum(PM2.5_1A2c))*100,
                NMVOC_1A2c = (t.1A2c$NMVOC/t.1A2c$sumF)*he_1A2c, 
                NMVOC_1A2c_p = (NMVOC_1A2c/sum(NMVOC_1A2c))*100,
                NH3_1A2c = (t.1A2c$NH3/t.1A2c$sumF)*he_1A2c, 
                NH3_1A2c_p = (NH3_1A2c/sum(NH3_1A2c))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A2c_p, SO2_1A2c_p, PM10_1A2c_p, PM2.5_1A2c_p, NMVOC_1A2c_p, NH3_1A2c_p) %>%
  rename(`1A2c_NOx` = NOx_1A2c_p,
         `1A2c_SO2` = SO2_1A2c_p,
         `1A2c_PM10` = PM10_1A2c_p,
         `1A2c_PM2.5` = PM2.5_1A2c_p,
         `1A2c_NMVOC` = NMVOC_1A2c_p,
         `1A2c_NH3` = NH3_1A2c_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A2c$`1A2c_NOx`), sum(he.1A2c$`1A2c_SO2`), sum(he.1A2c$`1A2c_PM10`), sum(he.1A2c$`1A2c_PM2.5`), sum(he.1A2c$`1A2c_NMVOC`), sum(he.1A2c$`1A2c_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
#'
#'
#'
#' ## 1A2d - Pulp, paper and print
#+ include = FALSE


sf.1A2d <- st_read("D:/R_projects/Spatialization/Products/1A2 - Industry/1A2d.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A2d <- sf.1A2d %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A2d%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + !PH + !RP
#

he.1A2d <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A2d = (WDWW * (WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * WE2) %>%
  select(times, he_1A2d)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-15 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A2d, aes(x = times, y = he_1A2d)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A2d$he_1A2d), max(he.1A2d$he_1A2d), sum(he.1A2d$he_1A2d))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A2d$sumF <- sum(he.1A2d$he_1A2d)
he.1A2d %<>% 
  dplyr::mutate(NOx_1A2d = (t.1A2d$NOx/t.1A2d$sumF)*he_1A2d, 
                NOx_1A2d_p = (NOx_1A2d/sum(NOx_1A2d))*100,
                SO2_1A2d = (t.1A2d$SO2/t.1A2d$sumF)*he_1A2d, 
                SO2_1A2d_p = (SO2_1A2d/sum(SO2_1A2d))*100,
                PM10_1A2d = (t.1A2d$PM10/t.1A2d$sumF)*he_1A2d, 
                PM10_1A2d_p = (PM10_1A2d/sum(PM10_1A2d))*100,
                PM2.5_1A2d = (t.1A2d$PM2.5/t.1A2d$sumF)*he_1A2d, 
                PM2.5_1A2d_p = (PM2.5_1A2d/sum(PM2.5_1A2d))*100,
                NMVOC_1A2d = (t.1A2d$NMVOC/t.1A2d$sumF)*he_1A2d, 
                NMVOC_1A2d_p = (NMVOC_1A2d/sum(NMVOC_1A2d))*100,
                NH3_1A2d = (t.1A2d$NH3/t.1A2d$sumF)*he_1A2d, 
                NH3_1A2d_p = (NH3_1A2d/sum(NH3_1A2d))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A2d_p, SO2_1A2d_p, PM10_1A2d_p, PM2.5_1A2d_p, NMVOC_1A2d_p, NH3_1A2d_p) %>%
  rename(`1A2d_NOx` = NOx_1A2d_p,
         `1A2d_SO2` = SO2_1A2d_p,
         `1A2d_PM10` = PM10_1A2d_p,
         `1A2d_PM2.5` = PM2.5_1A2d_p,
         `1A2d_NMVOC` = NMVOC_1A2d_p,
         `1A2d_NH3` = NH3_1A2d_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A2d$`1A2d_NOx`), sum(he.1A2d$`1A2d_SO2`), sum(he.1A2d$`1A2d_PM10`), sum(he.1A2d$`1A2d_PM2.5`), sum(he.1A2d$`1A2d_NMVOC`), sum(he.1A2d$`1A2d_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )



#'
#'
#'
#'
#'
#'
#' ## 1A2e - Food, beverages and tobacco
#+ include = FALSE

sf.1A2e <- st_read("D:/R_projects/Spatialization/Products/1A2 - Industry/1A2e.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A2e <- sf.1A2e %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A2e%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + !PH + !RP
#

he.1A2e <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A2e = (WDWW *(WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * WE2) %>%
  select(times, he_1A2e)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-15 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A2e, aes(x = times, y = he_1A2e)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A2e$he_1A2e), max(he.1A2e$he_1A2e), sum(he.1A2e$he_1A2e))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A2e$sumF <- sum(he.1A2e$he_1A2e)
he.1A2e %<>% 
  dplyr::mutate(NOx_1A2e = (t.1A2e$NOx/t.1A2e$sumF)*he_1A2e, 
                NOx_1A2e_p = (NOx_1A2e/sum(NOx_1A2e))*100,
                SO2_1A2e = (t.1A2e$SO2/t.1A2e$sumF)*he_1A2e, 
                SO2_1A2e_p = (SO2_1A2e/sum(SO2_1A2e))*100,
                PM10_1A2e = (t.1A2e$PM10/t.1A2e$sumF)*he_1A2e, 
                PM10_1A2e_p = (PM10_1A2e/sum(PM10_1A2e))*100,
                PM2.5_1A2e = (t.1A2e$PM2.5/t.1A2e$sumF)*he_1A2e, 
                PM2.5_1A2e_p = (PM2.5_1A2e/sum(PM2.5_1A2e))*100,
                NMVOC_1A2e = (t.1A2e$NMVOC/t.1A2e$sumF)*he_1A2e, 
                NMVOC_1A2e_p = (NMVOC_1A2e/sum(NMVOC_1A2e))*100,
                NH3_1A2e = (t.1A2e$NH3/t.1A2e$sumF)*he_1A2e, 
                NH3_1A2e_p = (NH3_1A2e/sum(NH3_1A2e))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A2e_p, SO2_1A2e_p, PM10_1A2e_p, PM2.5_1A2e_p, NMVOC_1A2e_p, NH3_1A2e_p) %>%
  rename(`1A2e_NOx` = NOx_1A2e_p,
         `1A2e_SO2` = SO2_1A2e_p,
         `1A2e_PM10` = PM10_1A2e_p,
         `1A2e_PM2.5` = PM2.5_1A2e_p,
         `1A2e_NMVOC` = NMVOC_1A2e_p,
         `1A2e_NH3` = NH3_1A2e_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A2e$`1A2e_NOx`), sum(he.1A2e$`1A2e_SO2`), sum(he.1A2e$`1A2e_PM10`), sum(he.1A2e$`1A2e_PM2.5`), sum(he.1A2e$`1A2e_NMVOC`), sum(he.1A2e$`1A2e_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#'
#'
#'
#'
#'
#'
#' ## 1A2f - Non-metallic minerals
#+ include = FALSE

sf.1A2f <- st_read("D:/R_projects/Spatialization/Products/1A2 - Industry/1A2f.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A2f <- sf.1A2f %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A2f%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + !PH + !RP
#

he.1A2f <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A2f = (WDWW * (WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * WE2) %>%
  select(times, he_1A2f)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-15 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A2f, aes(x = times, y = he_1A2f)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A2f$he_1A2f), max(he.1A2f$he_1A2f), sum(he.1A2f$he_1A2f))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A2f$sumF <- sum(he.1A2f$he_1A2f)
he.1A2f %<>% 
  dplyr::mutate(NOx_1A2f = (t.1A2f$NOx/t.1A2f$sumF)*he_1A2f, 
                NOx_1A2f_p = (NOx_1A2f/sum(NOx_1A2f))*100,
                SO2_1A2f = (t.1A2f$SO2/t.1A2f$sumF)*he_1A2f, 
                SO2_1A2f_p = (SO2_1A2f/sum(SO2_1A2f))*100,
                PM10_1A2f = (t.1A2f$PM10/t.1A2f$sumF)*he_1A2f, 
                PM10_1A2f_p = (PM10_1A2f/sum(PM10_1A2f))*100,
                PM2.5_1A2f = (t.1A2f$PM2.5/t.1A2f$sumF)*he_1A2f, 
                PM2.5_1A2f_p = (PM2.5_1A2f/sum(PM2.5_1A2f))*100,
                NMVOC_1A2f = (t.1A2f$NMVOC/t.1A2f$sumF)*he_1A2f, 
                NMVOC_1A2f_p = (NMVOC_1A2f/sum(NMVOC_1A2f))*100,
                NH3_1A2f = (t.1A2f$NH3/t.1A2f$sumF)*he_1A2f, 
                NH3_1A2f_p = (NH3_1A2f/sum(NH3_1A2f))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A2f_p, SO2_1A2f_p, PM10_1A2f_p, PM2.5_1A2f_p, NMVOC_1A2f_p, NH3_1A2f_p) %>%
  rename(`1A2f_NOx` = NOx_1A2f_p,
         `1A2f_SO2` = SO2_1A2f_p,
         `1A2f_PM10` = PM10_1A2f_p,
         `1A2f_PM2.5` = PM2.5_1A2f_p,
         `1A2f_NMVOC` = NMVOC_1A2f_p,
         `1A2f_NH3` = NH3_1A2f_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A2f$`1A2f_NOx`), sum(he.1A2f$`1A2f_SO2`), sum(he.1A2f$`1A2f_PM10`), sum(he.1A2f$`1A2f_PM2.5`), sum(he.1A2f$`1A2f_NMVOC`), sum(he.1A2f$`1A2f_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#'
#'
#'
#'
#'
#'
#' ## 1A2g - Other industries
#+ include = FALSE
sf.1A2g <- st_read("D:/R_projects/Spatialization/Products/1A2 - Industry/1A2g.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A2g <- sf.1A2g %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A2g%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + !PH + !RP
#

he.1A2g <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A2g = (WDWW * (WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * WE2) %>%
  select(times, he_1A2g)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-15 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A2g, aes(x = times, y = he_1A2g)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A2g$he_1A2g), max(he.1A2g$he_1A2g), sum(he.1A2g$he_1A2g))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A2g$sumF <- sum(he.1A2g$he_1A2g)
he.1A2g %<>% 
  dplyr::mutate(NOx_1A2g = (t.1A2g$NOx/t.1A2g$sumF)*he_1A2g, 
                NOx_1A2g_p = (NOx_1A2g/sum(NOx_1A2g))*100,
                SO2_1A2g = (t.1A2g$SO2/t.1A2g$sumF)*he_1A2g, 
                SO2_1A2g_p = (SO2_1A2g/sum(SO2_1A2g))*100,
                PM10_1A2g = (t.1A2g$PM10/t.1A2g$sumF)*he_1A2g, 
                PM10_1A2g_p = (PM10_1A2g/sum(PM10_1A2g))*100,
                PM2.5_1A2g = (t.1A2g$PM2.5/t.1A2g$sumF)*he_1A2g, 
                PM2.5_1A2g_p = (PM2.5_1A2g/sum(PM2.5_1A2g))*100,
                NMVOC_1A2g = (t.1A2g$NMVOC/t.1A2g$sumF)*he_1A2g, 
                NMVOC_1A2g_p = (NMVOC_1A2g/sum(NMVOC_1A2g))*100,
                NH3_1A2g = (t.1A2g$NH3/t.1A2g$sumF)*he_1A2g, 
                NH3_1A2g_p = (NH3_1A2g/sum(NH3_1A2g))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A2g_p, SO2_1A2g_p, PM10_1A2g_p, PM2.5_1A2g_p, NMVOC_1A2g_p, NH3_1A2g_p) %>%
  rename(`1A2g_NOx` = NOx_1A2g_p,
         `1A2g_SO2` = SO2_1A2g_p,
         `1A2g_PM10` = PM10_1A2g_p,
         `1A2g_PM2.5` = PM2.5_1A2g_p,
         `1A2g_NMVOC` = NMVOC_1A2g_p,
         `1A2g_NH3` = NH3_1A2g_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A2g$`1A2g_NOx`), sum(he.1A2g$`1A2g_SO2`), sum(he.1A2g$`1A2g_PM10`), sum(he.1A2g$`1A2g_PM2.5`), sum(he.1A2g$`1A2g_NMVOC`), sum(he.1A2g$`1A2g_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#'
#'
#'
#'
#'
#'
#' ## 1A2gvii - Auto-production
#+ include = FALSE
sf.1A2gvi <- st_read("D:/R_projects/Spatialization/Products/1A2 - Industry/1A2g-Auto-production.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A2gvi <- sf.1A2gvi %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A2gvi%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + !PH + !RP
#

he.1A2gvi <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A2gvi = (WDWW * (WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * WE2) %>%
  select(times, he_1A2gvi)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-15 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A2gvi, aes(x = times, y = he_1A2gvi)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A2gvi$he_1A2gvi), max(he.1A2gvi$he_1A2gvi), sum(he.1A2gvi$he_1A2gvi))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A2gvi$sumF <- sum(he.1A2gvi$he_1A2gvi)
he.1A2gvi %<>% 
  dplyr::mutate(NOx_1A2gvi = (t.1A2gvi$NOx/t.1A2gvi$sumF)*he_1A2gvi, 
                NOx_1A2gvi_p = (NOx_1A2gvi/sum(NOx_1A2gvi))*100,
                SO2_1A2gvi = (t.1A2gvi$SO2/t.1A2gvi$sumF)*he_1A2gvi, 
                SO2_1A2gvi_p = (SO2_1A2gvi/sum(SO2_1A2gvi))*100,
                PM10_1A2gvi = (t.1A2gvi$PM10/t.1A2gvi$sumF)*he_1A2gvi, 
                PM10_1A2gvi_p = (PM10_1A2gvi/sum(PM10_1A2gvi))*100,
                PM2.5_1A2gvi = (t.1A2gvi$PM2.5/t.1A2gvi$sumF)*he_1A2gvi, 
                PM2.5_1A2gvi_p = (PM2.5_1A2gvi/sum(PM2.5_1A2gvi))*100,
                NMVOC_1A2gvi = (t.1A2gvi$NMVOC/t.1A2gvi$sumF)*he_1A2gvi, 
                NMVOC_1A2gvi_p = (NMVOC_1A2gvi/sum(NMVOC_1A2gvi))*100,
                NH3_1A2gvi = (t.1A2gvi$NH3/t.1A2gvi$sumF)*he_1A2gvi, 
                NH3_1A2gvi_p = (NH3_1A2gvi/sum(NH3_1A2gvi))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A2gvi_p, SO2_1A2gvi_p, PM10_1A2gvi_p, PM2.5_1A2gvi_p, NMVOC_1A2gvi_p, NH3_1A2gvi_p) %>%
  rename(`1A2gvi_NOx` = NOx_1A2gvi_p,
         `1A2gvi_SO2` = SO2_1A2gvi_p,
         `1A2gvi_PM10` = PM10_1A2gvi_p,
         `1A2gvi_PM2.5` = PM2.5_1A2gvi_p,
         `1A2gvi_NMVOC` = NMVOC_1A2gvi_p,
         `1A2gvi_NH3` = NH3_1A2gvi_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A2gvi$`1A2gvi_NOx`), sum(he.1A2gvi$`1A2gvi_SO2`), sum(he.1A2gvi$`1A2gvi_PM10`), sum(he.1A2gvi$`1A2gvi_PM2.5`), sum(he.1A2gvi$`1A2gvi_NMVOC`), sum(he.1A2gvi$`1A2gvi_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#'
#'
#'
#'
#'
#'
#' ## 1A2gvii - Mobile combustion in manufacturing industries and construction
#+ include = FALSE

sf.1A2gvii <- st_read("D:/R_projects/Spatialization/Products/1A2 - Industry/1A2gvii.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A2gvii <- sf.1A2gvii %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A2gvii%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + !PH + !RP
#

he.1A2gvii <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A2gvii = (WDWW * (WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * WE2) %>%
  select(times, he_1A2gvii)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-15 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A2gvii, aes(x = times, y = he_1A2gvii)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A2gvii$he_1A2gvii), max(he.1A2gvii$he_1A2gvii), sum(he.1A2gvii$he_1A2gvii))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A2gvii$sumF <- sum(he.1A2gvii$he_1A2gvii)
he.1A2gvii %<>% 
  dplyr::mutate(NOx_1A2gvii = (t.1A2gvii$NOx/t.1A2gvii$sumF)*he_1A2gvii, 
                NOx_1A2gvii_p = (NOx_1A2gvii/sum(NOx_1A2gvii))*100,
                SO2_1A2gvii = (t.1A2gvii$SO2/t.1A2gvii$sumF)*he_1A2gvii, 
                SO2_1A2gvii_p = (SO2_1A2gvii/sum(SO2_1A2gvii))*100,
                PM10_1A2gvii = (t.1A2gvii$PM10/t.1A2gvii$sumF)*he_1A2gvii, 
                PM10_1A2gvii_p = (PM10_1A2gvii/sum(PM10_1A2gvii))*100,
                PM2.5_1A2gvii = (t.1A2gvii$PM2.5/t.1A2gvii$sumF)*he_1A2gvii, 
                PM2.5_1A2gvii_p = (PM2.5_1A2gvii/sum(PM2.5_1A2gvii))*100,
                NMVOC_1A2gvii = (t.1A2gvii$NMVOC/t.1A2gvii$sumF)*he_1A2gvii, 
                NMVOC_1A2gvii_p = (NMVOC_1A2gvii/sum(NMVOC_1A2gvii))*100,
                NH3_1A2gvii = (t.1A2gvii$NH3/t.1A2gvii$sumF)*he_1A2gvii, 
                NH3_1A2gvii_p = (NH3_1A2gvii/sum(NH3_1A2gvii))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A2gvii_p, SO2_1A2gvii_p, PM10_1A2gvii_p, PM2.5_1A2gvii_p, NMVOC_1A2gvii_p, NH3_1A2gvii_p) %>%
  rename(`1A2gvii_NOx` = NOx_1A2gvii_p,
         `1A2gvii_SO2` = SO2_1A2gvii_p,
         `1A2gvii_PM10` = PM10_1A2gvii_p,
         `1A2gvii_PM2.5` = PM2.5_1A2gvii_p,
         `1A2gvii_NMVOC` = NMVOC_1A2gvii_p,
         `1A2gvii_NH3` = NH3_1A2gvii_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A2gvii$`1A2gvii_NOx`), sum(he.1A2gvii$`1A2gvii_SO2`), sum(he.1A2gvii$`1A2gvii_PM10`), sum(he.1A2gvii$`1A2gvii_PM2.5`), sum(he.1A2gvii$`1A2gvii_NMVOC`), sum(he.1A2gvii$`1A2gvii_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


# temporalProfile_Industry <- activity.df$times %>% 
#   cbind(he.1A2a[,1:6], 
#         he.1A2b[,1:6], 
#         he.1A2c[,1:6], 
#         he.1A2d[,1:6], 
#         he.1A2e[,1:6], 
#         he.1A2f[,1:6], 
#         he.1A2g[,1:6], 
#         he.1A2gvi[,1:6], 
#         he.1A2gvii[,1:6]) %>% 
#   as.data.frame()
# 
# writexl::write_xlsx(temporalProfile_Industry, path = 'Hourly_emissions/Products/TemporalProfile_Industrial_ processes.xlsx')
