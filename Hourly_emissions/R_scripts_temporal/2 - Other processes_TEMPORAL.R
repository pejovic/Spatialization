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
library(data.table)
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
activity.df <- readRDS(file = "D:/R_projects/Spatialization/Version_2_update/Temporalization/activity_df_new.rds")
load(file = "D:/R_projects/Spatialization/Hourly_emissions/Data/counters_df.rds")
counters.df <- counters_df %>% 
  mutate(ALL_mean = (IA_mean + IIA_mean + IB_mean)/3)
activity.df$VA <- counters.df$ALL_mean

summary_tab <- data.frame(Label = c("WD", "WDWW", "WT0816", "WT1624", "WT0622", "DL", 
                                    "WE", "WW", "RH0709", "RH1517", "PH", "SA", "HS", "SAAG", "TEMP", "SLP", "VA", "NFH", "RP", "EC"),
                          Description = c("Working days", 
                                          "Working days, working weekends", 
                                          "Working time 08-16h",
                                          "working time 16-24h",
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
                                          "Repair - overhaul period", 
                                          "Electricity consumption"))
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
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_2A5a = ((WD+0.5)) * PH2 * (TEMP+30)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2A5a))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2A5a = he_sig) %>%
  dplyr::mutate(he_2A5a_n = he_sig/sum(he_sig))%>%
  select(times, he_2A5a, he_2A5a_n)
# ((SA * (-1.5))+2.5)
time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2A5a, aes(x = times, y = he_2A5a)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_2A5a = ((WT0024+0.5)) * PH2 * RP2 * (TEMP+30)")+
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
# sf.2A5a_df <- sf.2A5a %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2A5a.tl <- lapply(sf.2A5a_df[,-1], function(x) t((x %o% he.2A5a$he_2A5a_n)[,,1]))
# 
# sf.2A5a.tl <- lapply(sf.2A5a.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2A5a.tle, "sf.2A5a.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2A5a_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2A5a.tl[[i]], file = paste("sf.2A5a", paste(vars[i],"csv", sep = "."), sep = "_"))
# }

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
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_2A5b = ((WT0816+0.5) * (WT1624+0.5)) * PH2 * (TEMP+30))  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2A5b))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2A5b = he_sig) %>%
  dplyr::mutate(he_2A5b_n = he_sig/sum(he_sig))%>%
  select(times, he_2A5b, he_2A5b_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2A5b, aes(x = times, y = he_2A5b)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_2A5b = ((WT0816+0.5) * (WT1624+0.5)) * PH2 * (TEMP+30))")+
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
# sf.2A5b_df <- sf.2A5b %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2A5b.tl <- lapply(sf.2A5b_df[,-1], function(x) t((x %o% he.2A5b$he_2A5b_n)[,,1]))
# 
# sf.2A5b.tl <- lapply(sf.2A5b.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2A5b.tle, "sf.2A5b.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2A5b_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2A5b.tl[[i]], file = paste("sf.2A5b", paste(vars[i],"csv", sep = "."), sep = "_"))
# }

#'
#'
#'
#'
#'
#'
#' ## 2A5c - Storage, handling and transport of mineral products
#+ include = FALSE

sf.2A5c <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2A5c.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2A5c <- sf.2A5c %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2A5c%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WW + RH0709 + RH1517 + !PH + VA
#

he.2A5c <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2A5c = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2A5c))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2A5c = he_sig) %>%
  dplyr::mutate(he_2A5c_n = he_sig/sum(he_sig))%>%
  select(times, he_2A5c, he_2A5c_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2A5c, aes(x = times, y = he_2A5c)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_2A5c = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2A5c$he_2A5c), max(he.2A5c$he_2A5c), sum(he.2A5c$he_2A5c))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2A5c$sumF <- sum(he.2A5c$he_2A5c)
he.2A5c %<>% 
  dplyr::mutate(NOx_2A5c = (t.2A5c$NOx/t.2A5c$sumF)*he_2A5c, 
                NOx_2A5c_p = (NOx_2A5c/sum(NOx_2A5c))*100,
                SO2_2A5c = (t.2A5c$SO2/t.2A5c$sumF)*he_2A5c, 
                SO2_2A5c_p = (SO2_2A5c/sum(SO2_2A5c))*100,
                PM10_2A5c = (t.2A5c$PM10/t.2A5c$sumF)*he_2A5c, 
                PM10_2A5c_p = (PM10_2A5c/sum(PM10_2A5c))*100,
                PM2.5_2A5c = (t.2A5c$PM2.5/t.2A5c$sumF)*he_2A5c, 
                PM2.5_2A5c_p = (PM2.5_2A5c/sum(PM2.5_2A5c))*100,
                NMVOC_2A5c = (t.2A5c$NMVOC/t.2A5c$sumF)*he_2A5c, 
                NMVOC_2A5c_p = (NMVOC_2A5c/sum(NMVOC_2A5c))*100,
                NH3_2A5c = (t.2A5c$NH3/t.2A5c$sumF)*he_2A5c, 
                NH3_2A5c_p = (NH3_2A5c/sum(NH3_2A5c))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2A5c_p, SO2_2A5c_p, PM10_2A5c_p, PM2.5_2A5c_p, NMVOC_2A5c_p, NH3_2A5c_p) %>%
  rename(`2A5c_NOx` = NOx_2A5c_p,
         `2A5c_SO2` = SO2_2A5c_p,
         `2A5c_PM10` = PM10_2A5c_p,
         `2A5c_PM2.5` = PM2.5_2A5c_p,
         `2A5c_NMVOC` = NMVOC_2A5c_p,
         `2A5c_NH3` = NH3_2A5c_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2A5c$`2A5c_NOx`), sum(he.2A5c$`2A5c_SO2`), sum(he.2A5c$`2A5c_PM10`), sum(he.2A5c$`2A5c_PM2.5`), sum(he.2A5c$`2A5c_NMVOC`), sum(he.2A5c$`2A5c_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2A5c_df <- sf.2A5c %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2A5c.tl <- lapply(sf.2A5c_df[,-1], function(x) t((x %o% he.2A5c$he_2A5c_n)[,,1]))
# 
# sf.2A5c.tl <- lapply(sf.2A5c.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2A5c.tle, "sf.2A5c.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2A5c_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2A5c.tl[[i]], file = paste("sf.2A5c", paste(vars[i],"csv", sep = "."), sep = "_"))
# }

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
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3b = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * (VA*(-1)) * (TEMP+30))  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3b))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3b = he_sig) %>%
  dplyr::mutate(he_2D3b_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3b, he_2D3b_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3b, aes(x = times, y = he_2D3b)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_2D3b = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 * VA + (TEMP+30)")+
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
# sf.2D3b_df <- sf.2D3b %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3b.tl <- lapply(sf.2D3b_df[,-1], function(x) t((x %o% he.2D3b$he_2D3b_n)[,,1]))
# 
# sf.2D3b.tl <- lapply(sf.2D3b.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3b.tle, "sf.2D3b.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3b_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3b.tl[[i]], file = paste("sf.2D3b", paste(vars[i],"csv", sep = "."), sep = "_"))
# }
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
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3c = DL * WE2 * PH2 * (TEMP+30))  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3c))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3c = he_sig) %>%
  dplyr::mutate(he_2D3c_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3c, he_2D3c_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3c, aes(x = times, y = he_2D3c)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_2D3c = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 + (TEMP+30)")+
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
# sf.2D3c_df <- sf.2D3c %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3c.tl <- lapply(sf.2D3c_df[,-1], function(x) t((x %o% he.2D3c$he_2D3c_n)[,,1]))
# 
# sf.2D3c.tl <- lapply(sf.2D3c.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3c.tle, "sf.2D3c.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3c_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3c.tl[[i]], file = paste("sf.2D3c", paste(vars[i],"csv", sep = "."), sep = "_"))
# }




#'
#'
#'
#'
#'
#'
#' ## 2D3d - Coating application - paint for construction
#+ include = FALSE

sf.2D3d.pfc <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3d.gpkg")  # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3d.pfc <- sf.2D3d.pfc %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3d.pfc%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + VA 
#

he.2D3d.pfc <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3d.pfc = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 + (TEMP+30))  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3d.pfc))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3d.pfc = he_sig) %>%
  dplyr::mutate(he_2D3d.pfc_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3d.pfc, he_2D3d.pfc_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3d.pfc, aes(x = times, y = he_2D3d.pfc)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3d.pfc = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 + (TEMP+30)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3d.pfc$he_2D3d.pfc), max(he.2D3d.pfc$he_2D3d.pfc), sum(he.2D3d.pfc$he_2D3d.pfc))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3d.pfc$sumF <- sum(he.2D3d.pfc$he_2D3d.pfc)
he.2D3d.pfc %<>% 
  dplyr::mutate(NOx_2D3d.pfc = (t.2D3d.pfc$NOx/t.2D3d.pfc$sumF)*he_2D3d.pfc, 
                NOx_2D3d.pfc_p = (NOx_2D3d.pfc/sum(NOx_2D3d.pfc))*100,
                SO2_2D3d.pfc = (t.2D3d.pfc$SO2/t.2D3d.pfc$sumF)*he_2D3d.pfc, 
                SO2_2D3d.pfc_p = (SO2_2D3d.pfc/sum(SO2_2D3d.pfc))*100,
                PM10_2D3d.pfc = (t.2D3d.pfc$PM10/t.2D3d.pfc$sumF)*he_2D3d.pfc, 
                PM10_2D3d.pfc_p = (PM10_2D3d.pfc/sum(PM10_2D3d.pfc))*100,
                PM2.5_2D3d.pfc = (t.2D3d.pfc$PM2.5/t.2D3d.pfc$sumF)*he_2D3d.pfc, 
                PM2.5_2D3d.pfc_p = (PM2.5_2D3d.pfc/sum(PM2.5_2D3d.pfc))*100,
                NMVOC_2D3d.pfc = (t.2D3d.pfc$NMVOC/t.2D3d.pfc$sumF)*he_2D3d.pfc, 
                NMVOC_2D3d.pfc_p = (NMVOC_2D3d.pfc/sum(NMVOC_2D3d.pfc))*100,
                NH3_2D3d.pfc = (t.2D3d.pfc$NH3/t.2D3d.pfc$sumF)*he_2D3d.pfc, 
                NH3_2D3d.pfc_p = (NH3_2D3d.pfc/sum(NH3_2D3d.pfc))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3d.pfc_p, SO2_2D3d.pfc_p, PM10_2D3d.pfc_p, PM2.5_2D3d.pfc_p, NMVOC_2D3d.pfc_p, NH3_2D3d.pfc_p) %>%
  rename(`2D3d.pfc_NOx` = NOx_2D3d.pfc_p,
         `2D3d.pfc_SO2` = SO2_2D3d.pfc_p,
         `2D3d.pfc_PM10` = PM10_2D3d.pfc_p,
         `2D3d.pfc_PM2.5` = PM2.5_2D3d.pfc_p,
         `2D3d.pfc_NMVOC` = NMVOC_2D3d.pfc_p,
         `2D3d.pfc_NH3` = NH3_2D3d.pfc_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3d.pfc$`2D3d.pfc_NOx`), sum(he.2D3d.pfc$`2D3d.pfc_SO2`), sum(he.2D3d.pfc$`2D3d.pfc_PM10`), sum(he.2D3d.pfc$`2D3d.pfc_PM2.5`), sum(he.2D3d.pfc$`2D3d.pfc_NMVOC`), sum(he.2D3d.pfc$`2D3d.pfc_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3d.pfc_df <- sf.2D3d.pfc %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3d.pfc.tl <- lapply(sf.2D3d.pfc_df[,-1], function(x) t((x %o% he.2D3d.pfc$he_2D3d.pfc_n)[,,1]))
# 
# sf.2D3d.pfc.tl <- lapply(sf.2D3d.pfc.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3d.pfc.tle, "sf.2D3d.pfc.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3d.pfc_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3d.pfc.tl[[i]], file = paste("sf.2D3d.pfc", paste(vars[i],"csv", sep = "."), sep = "_"))
# }






#'
#'
#'
#'
#'
#'
#' ## 2D3d - Coating application - Car/bus/truck/van coating + leather finishing
#+ include = FALSE

sf.2D3d.cbtlf <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3d.gpkg") # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3d.cbtlf <- sf.2D3d.cbtlf %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3d.cbtlf%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + WE + RH0709 + RH1517 + !PH + TEMP + VA 
#

he.2D3d.cbtlf <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3d.cbtlf = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 + (TEMP+30))  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3d.cbtlf))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3d.cbtlf = he_sig) %>%
  dplyr::mutate(he_2D3d.cbtlf_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3d.cbtlf, he_2D3d.cbtlf_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3d.cbtlf, aes(x = times, y = he_2D3d.cbtlf)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3d.cbtlf = ((WT0816+0.5) * (WT1624+0.5)) * WE2 * PH2 + (TEMP+30)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3d.cbtlf$he_2D3d.cbtlf), max(he.2D3d.cbtlf$he_2D3d.cbtlf), sum(he.2D3d.cbtlf$he_2D3d.cbtlf))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3d.cbtlf$sumF <- sum(he.2D3d.cbtlf$he_2D3d.cbtlf)
he.2D3d.cbtlf %<>% 
  dplyr::mutate(NOx_2D3d.cbtlf = (t.2D3d.cbtlf$NOx/t.2D3d.cbtlf$sumF)*he_2D3d.cbtlf, 
                NOx_2D3d.cbtlf_p = (NOx_2D3d.cbtlf/sum(NOx_2D3d.cbtlf))*100,
                SO2_2D3d.cbtlf = (t.2D3d.cbtlf$SO2/t.2D3d.cbtlf$sumF)*he_2D3d.cbtlf, 
                SO2_2D3d.cbtlf_p = (SO2_2D3d.cbtlf/sum(SO2_2D3d.cbtlf))*100,
                PM10_2D3d.cbtlf = (t.2D3d.cbtlf$PM10/t.2D3d.cbtlf$sumF)*he_2D3d.cbtlf, 
                PM10_2D3d.cbtlf_p = (PM10_2D3d.cbtlf/sum(PM10_2D3d.cbtlf))*100,
                PM2.5_2D3d.cbtlf = (t.2D3d.cbtlf$PM2.5/t.2D3d.cbtlf$sumF)*he_2D3d.cbtlf, 
                PM2.5_2D3d.cbtlf_p = (PM2.5_2D3d.cbtlf/sum(PM2.5_2D3d.cbtlf))*100,
                NMVOC_2D3d.cbtlf = (t.2D3d.cbtlf$NMVOC/t.2D3d.cbtlf$sumF)*he_2D3d.cbtlf, 
                NMVOC_2D3d.cbtlf_p = (NMVOC_2D3d.cbtlf/sum(NMVOC_2D3d.cbtlf))*100,
                NH3_2D3d.cbtlf = (t.2D3d.cbtlf$NH3/t.2D3d.cbtlf$sumF)*he_2D3d.cbtlf, 
                NH3_2D3d.cbtlf_p = (NH3_2D3d.cbtlf/sum(NH3_2D3d.cbtlf))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3d.cbtlf_p, SO2_2D3d.cbtlf_p, PM10_2D3d.cbtlf_p, PM2.5_2D3d.cbtlf_p, NMVOC_2D3d.cbtlf_p, NH3_2D3d.cbtlf_p) %>%
  rename(`2D3d.cbtlf_NOx` = NOx_2D3d.cbtlf_p,
         `2D3d.cbtlf_SO2` = SO2_2D3d.cbtlf_p,
         `2D3d.cbtlf_PM10` = PM10_2D3d.cbtlf_p,
         `2D3d.cbtlf_PM2.5` = PM2.5_2D3d.cbtlf_p,
         `2D3d.cbtlf_NMVOC` = NMVOC_2D3d.cbtlf_p,
         `2D3d.cbtlf_NH3` = NH3_2D3d.cbtlf_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3d.cbtlf$`2D3d.cbtlf_NOx`), sum(he.2D3d.cbtlf$`2D3d.cbtlf_SO2`), sum(he.2D3d.cbtlf$`2D3d.cbtlf_PM10`), sum(he.2D3d.cbtlf$`2D3d.cbtlf_PM2.5`), sum(he.2D3d.cbtlf$`2D3d.cbtlf_NMVOC`), sum(he.2D3d.cbtlf$`2D3d.cbtlf_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3d.cbtlf_df <- sf.2D3d.cbtlf %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3d.cbtlf.tl <- lapply(sf.2D3d.cbtlf_df[,-1], function(x) t((x %o% he.2D3d.cbtlf$he_2D3d.cbtlf_n)[,,1]))
# 
# sf.2D3d.cbtlf.tl <- lapply(sf.2D3d.cbtlf.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3d.cbtlf.tle, "sf.2D3d.cbtlf.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3d.cbtlf_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3d.cbtlf.tl[[i]], file = paste("sf.2D3d.cbtlf", paste(vars[i],"csv", sep = "."), sep = "_"))
# }






#'
#'
#'
#'
#'
#'
#' ## 2D3g - Chemical products - - Rubber processing
#+ include = FALSE

sf.2D3g.rp <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3g.gpkg") # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3g.rp <- sf.2D3g.rp %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3g.rp%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3g.rp <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3g.rp = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3g.rp))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3g.rp = he_sig) %>%
  dplyr::mutate(he_2D3g.rp_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3g.rp, he_2D3g.rp_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3g.rp, aes(x = times, y = he_2D3g.rp)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3g.rp = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3g.rp$he_2D3g.rp), max(he.2D3g.rp$he_2D3g.rp), sum(he.2D3g.rp$he_2D3g.rp))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3g.rp$sumF <- sum(he.2D3g.rp$he_2D3g.rp)
he.2D3g.rp %<>% 
  dplyr::mutate(NOx_2D3g.rp = (t.2D3g.rp$NOx/t.2D3g.rp$sumF)*he_2D3g.rp, 
                NOx_2D3g.rp_p = (NOx_2D3g.rp/sum(NOx_2D3g.rp))*100,
                SO2_2D3g.rp = (t.2D3g.rp$SO2/t.2D3g.rp$sumF)*he_2D3g.rp, 
                SO2_2D3g.rp_p = (SO2_2D3g.rp/sum(SO2_2D3g.rp))*100,
                PM10_2D3g.rp = (t.2D3g.rp$PM10/t.2D3g.rp$sumF)*he_2D3g.rp, 
                PM10_2D3g.rp_p = (PM10_2D3g.rp/sum(PM10_2D3g.rp))*100,
                PM2.5_2D3g.rp = (t.2D3g.rp$PM2.5/t.2D3g.rp$sumF)*he_2D3g.rp, 
                PM2.5_2D3g.rp_p = (PM2.5_2D3g.rp/sum(PM2.5_2D3g.rp))*100,
                NMVOC_2D3g.rp = (t.2D3g.rp$NMVOC/t.2D3g.rp$sumF)*he_2D3g.rp, 
                NMVOC_2D3g.rp_p = (NMVOC_2D3g.rp/sum(NMVOC_2D3g.rp))*100,
                NH3_2D3g.rp = (t.2D3g.rp$NH3/t.2D3g.rp$sumF)*he_2D3g.rp, 
                NH3_2D3g.rp_p = (NH3_2D3g.rp/sum(NH3_2D3g.rp))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3g.rp_p, SO2_2D3g.rp_p, PM10_2D3g.rp_p, PM2.5_2D3g.rp_p, NMVOC_2D3g.rp_p, NH3_2D3g.rp_p) %>%
  rename(`2D3g.rp_NOx` = NOx_2D3g.rp_p,
         `2D3g.rp_SO2` = SO2_2D3g.rp_p,
         `2D3g.rp_PM10` = PM10_2D3g.rp_p,
         `2D3g.rp_PM2.5` = PM2.5_2D3g.rp_p,
         `2D3g.rp_NMVOC` = NMVOC_2D3g.rp_p,
         `2D3g.rp_NH3` = NH3_2D3g.rp_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3g.rp$`2D3g.rp_NOx`), sum(he.2D3g.rp$`2D3g.rp_SO2`), sum(he.2D3g.rp$`2D3g.rp_PM10`), sum(he.2D3g.rp$`2D3g.rp_PM2.5`), sum(he.2D3g.rp$`2D3g.rp_NMVOC`), sum(he.2D3g.rp$`2D3g.rp_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3g.rp_df <- sf.2D3g.rp %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3g.rp.tl <- lapply(sf.2D3g.rp_df[,-1], function(x) t((x %o% he.2D3g.rp$he_2D3g.rp_n)[,,1]))
# 
# sf.2D3g.rp.tl <- lapply(sf.2D3g.rp.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3g.rp.tle, "sf.2D3g.rp.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3g.rp_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3g.rp.tl[[i]], file = paste("sf.2D3g.rp", paste(vars[i],"csv", sep = "."), sep = "_"))
# }


#'
#'
#'
#'
#'
#'
#' ## 2D3g - Chemical products - Paint/ink/glue manufacturing
#+ include = FALSE

sf.2D3g.pigm <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3g.gpkg") # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3g.pigm <- sf.2D3g.pigm %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3g.pigm%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3g.pigm <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3g.pigm = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3g.pigm))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3g.pigm = he_sig) %>%
  dplyr::mutate(he_2D3g.pigm_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3g.pigm, he_2D3g.pigm_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3g.pigm, aes(x = times, y = he_2D3g.pigm)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3g.pigm = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3g.pigm$he_2D3g.pigm), max(he.2D3g.pigm$he_2D3g.pigm), sum(he.2D3g.pigm$he_2D3g.pigm))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3g.pigm$sumF <- sum(he.2D3g.pigm$he_2D3g.pigm)
he.2D3g.pigm %<>% 
  dplyr::mutate(NOx_2D3g.pigm = (t.2D3g.pigm$NOx/t.2D3g.pigm$sumF)*he_2D3g.pigm, 
                NOx_2D3g.pigm_p = (NOx_2D3g.pigm/sum(NOx_2D3g.pigm))*100,
                SO2_2D3g.pigm = (t.2D3g.pigm$SO2/t.2D3g.pigm$sumF)*he_2D3g.pigm, 
                SO2_2D3g.pigm_p = (SO2_2D3g.pigm/sum(SO2_2D3g.pigm))*100,
                PM10_2D3g.pigm = (t.2D3g.pigm$PM10/t.2D3g.pigm$sumF)*he_2D3g.pigm, 
                PM10_2D3g.pigm_p = (PM10_2D3g.pigm/sum(PM10_2D3g.pigm))*100,
                PM2.5_2D3g.pigm = (t.2D3g.pigm$PM2.5/t.2D3g.pigm$sumF)*he_2D3g.pigm, 
                PM2.5_2D3g.pigm_p = (PM2.5_2D3g.pigm/sum(PM2.5_2D3g.pigm))*100,
                NMVOC_2D3g.pigm = (t.2D3g.pigm$NMVOC/t.2D3g.pigm$sumF)*he_2D3g.pigm, 
                NMVOC_2D3g.pigm_p = (NMVOC_2D3g.pigm/sum(NMVOC_2D3g.pigm))*100,
                NH3_2D3g.pigm = (t.2D3g.pigm$NH3/t.2D3g.pigm$sumF)*he_2D3g.pigm, 
                NH3_2D3g.pigm_p = (NH3_2D3g.pigm/sum(NH3_2D3g.pigm))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3g.pigm_p, SO2_2D3g.pigm_p, PM10_2D3g.pigm_p, PM2.5_2D3g.pigm_p, NMVOC_2D3g.pigm_p, NH3_2D3g.pigm_p) %>%
  rename(`2D3g.pigm_NOx` = NOx_2D3g.pigm_p,
         `2D3g.pigm_SO2` = SO2_2D3g.pigm_p,
         `2D3g.pigm_PM10` = PM10_2D3g.pigm_p,
         `2D3g.pigm_PM2.5` = PM2.5_2D3g.pigm_p,
         `2D3g.pigm_NMVOC` = NMVOC_2D3g.pigm_p,
         `2D3g.pigm_NH3` = NH3_2D3g.pigm_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3g.pigm$`2D3g.pigm_NOx`), sum(he.2D3g.pigm$`2D3g.pigm_SO2`), sum(he.2D3g.pigm$`2D3g.pigm_PM10`), sum(he.2D3g.pigm$`2D3g.pigm_PM2.5`), sum(he.2D3g.pigm$`2D3g.pigm_NMVOC`), sum(he.2D3g.pigm$`2D3g.pigm_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3g.pigm_df <- sf.2D3g.pigm %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3g.pigm.tl <- lapply(sf.2D3g.pigm_df[,-1], function(x) t((x %o% he.2D3g.pigm$he_2D3g.pigm_n)[,,1]))
# 
# sf.2D3g.pigm.tl <- lapply(sf.2D3g.pigm.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3g.pigm.tle, "sf.2D3g.pigm.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3g.pigm_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3g.pigm.tl[[i]], file = paste("sf.2D3g.pigm", paste(vars[i],"csv", sep = "."), sep = "_"))
# }




#'
#'
#'
#'
#'
#'
#' ## 2D3g - Chemical products - Manufacture of shoes
#+ include = FALSE

sf.2D3g.ms <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3g.gpkg") # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3g.ms <- sf.2D3g.ms %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3g.ms%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3g.ms <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3g.ms = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3g.ms))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3g.ms = he_sig) %>%
  dplyr::mutate(he_2D3g.ms_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3g.ms, he_2D3g.ms_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3g.ms, aes(x = times, y = he_2D3g.ms)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3g.ms = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3g.ms$he_2D3g.ms), max(he.2D3g.ms$he_2D3g.ms), sum(he.2D3g.ms$he_2D3g.ms))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3g.ms$sumF <- sum(he.2D3g.ms$he_2D3g.ms)
he.2D3g.ms %<>% 
  dplyr::mutate(NOx_2D3g.ms = (t.2D3g.ms$NOx/t.2D3g.ms$sumF)*he_2D3g.ms, 
                NOx_2D3g.ms_p = (NOx_2D3g.ms/sum(NOx_2D3g.ms))*100,
                SO2_2D3g.ms = (t.2D3g.ms$SO2/t.2D3g.ms$sumF)*he_2D3g.ms, 
                SO2_2D3g.ms_p = (SO2_2D3g.ms/sum(SO2_2D3g.ms))*100,
                PM10_2D3g.ms = (t.2D3g.ms$PM10/t.2D3g.ms$sumF)*he_2D3g.ms, 
                PM10_2D3g.ms_p = (PM10_2D3g.ms/sum(PM10_2D3g.ms))*100,
                PM2.5_2D3g.ms = (t.2D3g.ms$PM2.5/t.2D3g.ms$sumF)*he_2D3g.ms, 
                PM2.5_2D3g.ms_p = (PM2.5_2D3g.ms/sum(PM2.5_2D3g.ms))*100,
                NMVOC_2D3g.ms = (t.2D3g.ms$NMVOC/t.2D3g.ms$sumF)*he_2D3g.ms, 
                NMVOC_2D3g.ms_p = (NMVOC_2D3g.ms/sum(NMVOC_2D3g.ms))*100,
                NH3_2D3g.ms = (t.2D3g.ms$NH3/t.2D3g.ms$sumF)*he_2D3g.ms, 
                NH3_2D3g.ms_p = (NH3_2D3g.ms/sum(NH3_2D3g.ms))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3g.ms_p, SO2_2D3g.ms_p, PM10_2D3g.ms_p, PM2.5_2D3g.ms_p, NMVOC_2D3g.ms_p, NH3_2D3g.ms_p) %>%
  rename(`2D3g.ms_NOx` = NOx_2D3g.ms_p,
         `2D3g.ms_SO2` = SO2_2D3g.ms_p,
         `2D3g.ms_PM10` = PM10_2D3g.ms_p,
         `2D3g.ms_PM2.5` = PM2.5_2D3g.ms_p,
         `2D3g.ms_NMVOC` = NMVOC_2D3g.ms_p,
         `2D3g.ms_NH3` = NH3_2D3g.ms_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3g.ms$`2D3g.ms_NOx`), sum(he.2D3g.ms$`2D3g.ms_SO2`), sum(he.2D3g.ms$`2D3g.ms_PM10`), sum(he.2D3g.ms$`2D3g.ms_PM2.5`), sum(he.2D3g.ms$`2D3g.ms_NMVOC`), sum(he.2D3g.ms$`2D3g.ms_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3g.ms_df <- sf.2D3g.ms %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3g.ms.tl <- lapply(sf.2D3g.ms_df[,-1], function(x) t((x %o% he.2D3g.ms$he_2D3g.ms_n)[,,1]))
# 
# sf.2D3g.ms.tl <- lapply(sf.2D3g.ms.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3g.ms.tle, "sf.2D3g.ms.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3g.ms_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3g.ms.tl[[i]], file = paste("sf.2D3g.ms", paste(vars[i],"csv", sep = "."), sep = "_"))
# }






#'
#'
#'
#'
#'
#'
#' ## 2D3g - Chemical products - Leather tanning
#+ include = FALSE

sf.2D3g.lt <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3g.gpkg") # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3g.lt <- sf.2D3g.lt %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3g.lt%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3g.lt <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3g.lt = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3g.lt))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3g.lt = he_sig) %>%
  dplyr::mutate(he_2D3g.lt_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3g.lt, he_2D3g.lt_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3g.lt, aes(x = times, y = he_2D3g.lt)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3g.lt = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3g.lt$he_2D3g.lt), max(he.2D3g.lt$he_2D3g.lt), sum(he.2D3g.lt$he_2D3g.lt))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3g.lt$sumF <- sum(he.2D3g.lt$he_2D3g.lt)
he.2D3g.lt %<>% 
  dplyr::mutate(NOx_2D3g.lt = (t.2D3g.lt$NOx/t.2D3g.lt$sumF)*he_2D3g.lt, 
                NOx_2D3g.lt_p = (NOx_2D3g.lt/sum(NOx_2D3g.lt))*100,
                SO2_2D3g.lt = (t.2D3g.lt$SO2/t.2D3g.lt$sumF)*he_2D3g.lt, 
                SO2_2D3g.lt_p = (SO2_2D3g.lt/sum(SO2_2D3g.lt))*100,
                PM10_2D3g.lt = (t.2D3g.lt$PM10/t.2D3g.lt$sumF)*he_2D3g.lt, 
                PM10_2D3g.lt_p = (PM10_2D3g.lt/sum(PM10_2D3g.lt))*100,
                PM2.5_2D3g.lt = (t.2D3g.lt$PM2.5/t.2D3g.lt$sumF)*he_2D3g.lt, 
                PM2.5_2D3g.lt_p = (PM2.5_2D3g.lt/sum(PM2.5_2D3g.lt))*100,
                NMVOC_2D3g.lt = (t.2D3g.lt$NMVOC/t.2D3g.lt$sumF)*he_2D3g.lt, 
                NMVOC_2D3g.lt_p = (NMVOC_2D3g.lt/sum(NMVOC_2D3g.lt))*100,
                NH3_2D3g.lt = (t.2D3g.lt$NH3/t.2D3g.lt$sumF)*he_2D3g.lt, 
                NH3_2D3g.lt_p = (NH3_2D3g.lt/sum(NH3_2D3g.lt))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3g.lt_p, SO2_2D3g.lt_p, PM10_2D3g.lt_p, PM2.5_2D3g.lt_p, NMVOC_2D3g.lt_p, NH3_2D3g.lt_p) %>%
  rename(`2D3g.lt_NOx` = NOx_2D3g.lt_p,
         `2D3g.lt_SO2` = SO2_2D3g.lt_p,
         `2D3g.lt_PM10` = PM10_2D3g.lt_p,
         `2D3g.lt_PM2.5` = PM2.5_2D3g.lt_p,
         `2D3g.lt_NMVOC` = NMVOC_2D3g.lt_p,
         `2D3g.lt_NH3` = NH3_2D3g.lt_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3g.lt$`2D3g.lt_NOx`), sum(he.2D3g.lt$`2D3g.lt_SO2`), sum(he.2D3g.lt$`2D3g.lt_PM10`), sum(he.2D3g.lt$`2D3g.lt_PM2.5`), sum(he.2D3g.lt$`2D3g.lt_NMVOC`), sum(he.2D3g.lt$`2D3g.lt_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3g.lt_df <- sf.2D3g.lt %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3g.lt.tl <- lapply(sf.2D3g.lt_df[,-1], function(x) t((x %o% he.2D3g.lt$he_2D3g.lt_n)[,,1]))
# 
# sf.2D3g.lt.tl <- lapply(sf.2D3g.lt.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3g.lt.tle, "sf.2D3g.lt.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3g.lt_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3g.lt.tl[[i]], file = paste("sf.2D3g.lt", paste(vars[i],"csv", sep = "."), sep = "_"))
# }



#'
#'
#'
#'
#'
#'
#' ## 2D3i - Other solvent and product use - Fat, edible and non-edible oil extraction (kg seed)
#+ include = FALSE

sf.2D3i.feneox <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3i.gpkg") # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3i.feneox <- sf.2D3i.feneox %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3i.feneox%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3i.feneox <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3i.feneox = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3i.feneox))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3i.feneox = he_sig) %>%
  dplyr::mutate(he_2D3i.feneox_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3i.feneox, he_2D3i.feneox_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3i.feneox, aes(x = times, y = he_2D3i.feneox)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3i.feneox = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3i.feneox$he_2D3i.feneox), max(he.2D3i.feneox$he_2D3i.feneox), sum(he.2D3i.feneox$he_2D3i.feneox))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3i.feneox$sumF <- sum(he.2D3i.feneox$he_2D3i.feneox)
he.2D3i.feneox %<>% 
  dplyr::mutate(NOx_2D3i.feneox = (t.2D3i.feneox$NOx/t.2D3i.feneox$sumF)*he_2D3i.feneox, 
                NOx_2D3i.feneox_p = (NOx_2D3i.feneox/sum(NOx_2D3i.feneox))*100,
                SO2_2D3i.feneox = (t.2D3i.feneox$SO2/t.2D3i.feneox$sumF)*he_2D3i.feneox, 
                SO2_2D3i.feneox_p = (SO2_2D3i.feneox/sum(SO2_2D3i.feneox))*100,
                PM10_2D3i.feneox = (t.2D3i.feneox$PM10/t.2D3i.feneox$sumF)*he_2D3i.feneox, 
                PM10_2D3i.feneox_p = (PM10_2D3i.feneox/sum(PM10_2D3i.feneox))*100,
                PM2.5_2D3i.feneox = (t.2D3i.feneox$PM2.5/t.2D3i.feneox$sumF)*he_2D3i.feneox, 
                PM2.5_2D3i.feneox_p = (PM2.5_2D3i.feneox/sum(PM2.5_2D3i.feneox))*100,
                NMVOC_2D3i.feneox = (t.2D3i.feneox$NMVOC/t.2D3i.feneox$sumF)*he_2D3i.feneox, 
                NMVOC_2D3i.feneox_p = (NMVOC_2D3i.feneox/sum(NMVOC_2D3i.feneox))*100,
                NH3_2D3i.feneox = (t.2D3i.feneox$NH3/t.2D3i.feneox$sumF)*he_2D3i.feneox, 
                NH3_2D3i.feneox_p = (NH3_2D3i.feneox/sum(NH3_2D3i.feneox))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3i.feneox_p, SO2_2D3i.feneox_p, PM10_2D3i.feneox_p, PM2.5_2D3i.feneox_p, NMVOC_2D3i.feneox_p, NH3_2D3i.feneox_p) %>%
  rename(`2D3i.feneox_NOx` = NOx_2D3i.feneox_p,
         `2D3i.feneox_SO2` = SO2_2D3i.feneox_p,
         `2D3i.feneox_PM10` = PM10_2D3i.feneox_p,
         `2D3i.feneox_PM2.5` = PM2.5_2D3i.feneox_p,
         `2D3i.feneox_NMVOC` = NMVOC_2D3i.feneox_p,
         `2D3i.feneox_NH3` = NH3_2D3i.feneox_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3i.feneox$`2D3i.feneox_NOx`), sum(he.2D3i.feneox$`2D3i.feneox_SO2`), sum(he.2D3i.feneox$`2D3i.feneox_PM10`), sum(he.2D3i.feneox$`2D3i.feneox_PM2.5`), sum(he.2D3i.feneox$`2D3i.feneox_NMVOC`), sum(he.2D3i.feneox$`2D3i.feneox_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3i.feneox_df <- sf.2D3i.feneox %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3i.feneox.tl <- lapply(sf.2D3i.feneox_df[,-1], function(x) t((x %o% he.2D3i.feneox$he_2D3i.feneox_n)[,,1]))
# 
# sf.2D3i.feneox.tl <- lapply(sf.2D3i.feneox.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3i.feneox.tle, "sf.2D3i.feneox.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3i.feneox_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3i.feneox.tl[[i]], file = paste("sf.2D3i.feneox", paste(vars[i],"csv", sep = "."), sep = "_"))
# }


#'
#'
#'
#'
#'
#'
#' ## 2D3i - Other solvent and product use - Preservation of wood
#+ include = FALSE

sf.2D3i.pow <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3i.gpkg") # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3i.pow <- sf.2D3i.pow %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3i.pow%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3i.pow <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3i.pow = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3i.pow))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3i.pow = he_sig) %>%
  dplyr::mutate(he_2D3i.pow_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3i.pow, he_2D3i.pow_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3i.pow, aes(x = times, y = he_2D3i.pow)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3i.pow = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3i.pow$he_2D3i.pow), max(he.2D3i.pow$he_2D3i.pow), sum(he.2D3i.pow$he_2D3i.pow))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3i.pow$sumF <- sum(he.2D3i.pow$he_2D3i.pow)
he.2D3i.pow %<>% 
  dplyr::mutate(NOx_2D3i.pow = (t.2D3i.pow$NOx/t.2D3i.pow$sumF)*he_2D3i.pow, 
                NOx_2D3i.pow_p = (NOx_2D3i.pow/sum(NOx_2D3i.pow))*100,
                SO2_2D3i.pow = (t.2D3i.pow$SO2/t.2D3i.pow$sumF)*he_2D3i.pow, 
                SO2_2D3i.pow_p = (SO2_2D3i.pow/sum(SO2_2D3i.pow))*100,
                PM10_2D3i.pow = (t.2D3i.pow$PM10/t.2D3i.pow$sumF)*he_2D3i.pow, 
                PM10_2D3i.pow_p = (PM10_2D3i.pow/sum(PM10_2D3i.pow))*100,
                PM2.5_2D3i.pow = (t.2D3i.pow$PM2.5/t.2D3i.pow$sumF)*he_2D3i.pow, 
                PM2.5_2D3i.pow_p = (PM2.5_2D3i.pow/sum(PM2.5_2D3i.pow))*100,
                NMVOC_2D3i.pow = (t.2D3i.pow$NMVOC/t.2D3i.pow$sumF)*he_2D3i.pow, 
                NMVOC_2D3i.pow_p = (NMVOC_2D3i.pow/sum(NMVOC_2D3i.pow))*100,
                NH3_2D3i.pow = (t.2D3i.pow$NH3/t.2D3i.pow$sumF)*he_2D3i.pow, 
                NH3_2D3i.pow_p = (NH3_2D3i.pow/sum(NH3_2D3i.pow))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3i.pow_p, SO2_2D3i.pow_p, PM10_2D3i.pow_p, PM2.5_2D3i.pow_p, NMVOC_2D3i.pow_p, NH3_2D3i.pow_p) %>%
  rename(`2D3i.pow_NOx` = NOx_2D3i.pow_p,
         `2D3i.pow_SO2` = SO2_2D3i.pow_p,
         `2D3i.pow_PM10` = PM10_2D3i.pow_p,
         `2D3i.pow_PM2.5` = PM2.5_2D3i.pow_p,
         `2D3i.pow_NMVOC` = NMVOC_2D3i.pow_p,
         `2D3i.pow_NH3` = NH3_2D3i.pow_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3i.pow$`2D3i.pow_NOx`), sum(he.2D3i.pow$`2D3i.pow_SO2`), sum(he.2D3i.pow$`2D3i.pow_PM10`), sum(he.2D3i.pow$`2D3i.pow_PM2.5`), sum(he.2D3i.pow$`2D3i.pow_NMVOC`), sum(he.2D3i.pow$`2D3i.pow_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3i.pow_df <- sf.2D3i.pow %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3i.pow.tl <- lapply(sf.2D3i.pow_df[,-1], function(x) t((x %o% he.2D3i.pow$he_2D3i.pow_n)[,,1]))
# 
# sf.2D3i.pow.tl <- lapply(sf.2D3i.pow.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3i.pow.tle, "sf.2D3i.pow.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3i.pow_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3i.pow.tl[[i]], file = paste("sf.2D3i.pow", paste(vars[i],"csv", sep = "."), sep = "_"))
# }






#'
#'
#'
#'
#'
#'
#' ## 2D3i - Other solvent and product use - Underseal treatment and conservation of vehicles
#+ include = FALSE

sf.2D3i.utcv <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3i.gpkg") # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3i.utcv <- sf.2D3i.utcv %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3i.utcv%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3i.utcv <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3i.utcv = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3i.utcv))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3i.utcv = he_sig) %>%
  dplyr::mutate(he_2D3i.utcv_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3i.utcv, he_2D3i.utcv_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3i.utcv, aes(x = times, y = he_2D3i.utcv)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3i.utcv = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3i.utcv$he_2D3i.utcv), max(he.2D3i.utcv$he_2D3i.utcv), sum(he.2D3i.utcv$he_2D3i.utcv))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3i.utcv$sumF <- sum(he.2D3i.utcv$he_2D3i.utcv)
he.2D3i.utcv %<>% 
  dplyr::mutate(NOx_2D3i.utcv = (t.2D3i.utcv$NOx/t.2D3i.utcv$sumF)*he_2D3i.utcv, 
                NOx_2D3i.utcv_p = (NOx_2D3i.utcv/sum(NOx_2D3i.utcv))*100,
                SO2_2D3i.utcv = (t.2D3i.utcv$SO2/t.2D3i.utcv$sumF)*he_2D3i.utcv, 
                SO2_2D3i.utcv_p = (SO2_2D3i.utcv/sum(SO2_2D3i.utcv))*100,
                PM10_2D3i.utcv = (t.2D3i.utcv$PM10/t.2D3i.utcv$sumF)*he_2D3i.utcv, 
                PM10_2D3i.utcv_p = (PM10_2D3i.utcv/sum(PM10_2D3i.utcv))*100,
                PM2.5_2D3i.utcv = (t.2D3i.utcv$PM2.5/t.2D3i.utcv$sumF)*he_2D3i.utcv, 
                PM2.5_2D3i.utcv_p = (PM2.5_2D3i.utcv/sum(PM2.5_2D3i.utcv))*100,
                NMVOC_2D3i.utcv = (t.2D3i.utcv$NMVOC/t.2D3i.utcv$sumF)*he_2D3i.utcv, 
                NMVOC_2D3i.utcv_p = (NMVOC_2D3i.utcv/sum(NMVOC_2D3i.utcv))*100,
                NH3_2D3i.utcv = (t.2D3i.utcv$NH3/t.2D3i.utcv$sumF)*he_2D3i.utcv, 
                NH3_2D3i.utcv_p = (NH3_2D3i.utcv/sum(NH3_2D3i.utcv))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3i.utcv_p, SO2_2D3i.utcv_p, PM10_2D3i.utcv_p, PM2.5_2D3i.utcv_p, NMVOC_2D3i.utcv_p, NH3_2D3i.utcv_p) %>%
  rename(`2D3i.utcv_NOx` = NOx_2D3i.utcv_p,
         `2D3i.utcv_SO2` = SO2_2D3i.utcv_p,
         `2D3i.utcv_PM10` = PM10_2D3i.utcv_p,
         `2D3i.utcv_PM2.5` = PM2.5_2D3i.utcv_p,
         `2D3i.utcv_NMVOC` = NMVOC_2D3i.utcv_p,
         `2D3i.utcv_NH3` = NH3_2D3i.utcv_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3i.utcv$`2D3i.utcv_NOx`), sum(he.2D3i.utcv$`2D3i.utcv_SO2`), sum(he.2D3i.utcv$`2D3i.utcv_PM10`), sum(he.2D3i.utcv$`2D3i.utcv_PM2.5`), sum(he.2D3i.utcv$`2D3i.utcv_NMVOC`), sum(he.2D3i.utcv$`2D3i.utcv_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3i.utcv_df <- sf.2D3i.utcv %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3i.utcv.tl <- lapply(sf.2D3i.utcv_df[,-1], function(x) t((x %o% he.2D3i.utcv$he_2D3i.utcv_n)[,,1]))
# 
# sf.2D3i.utcv.tl <- lapply(sf.2D3i.utcv.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3i.utcv.tle, "sf.2D3i.utcv.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3i.utcv_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3i.utcv.tl[[i]], file = paste("sf.2D3i.utcv", paste(vars[i],"csv", sep = "."), sep = "_"))
# }





#'
#'
#'
#'
#'
#'
#' ## 2D3i - Other solvent and product use - Tobacco (t tobacco)
#+ include = FALSE

sf.2D3i.t <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3i.gpkg") # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3i.t <- sf.2D3i.t %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3i.t%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3i.t <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3i.t = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3i.t))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3i.t = he_sig) %>%
  dplyr::mutate(he_2D3i.t_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3i.t, he_2D3i.t_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3i.t, aes(x = times, y = he_2D3i.t)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3i.t = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3i.t$he_2D3i.t), max(he.2D3i.t$he_2D3i.t), sum(he.2D3i.t$he_2D3i.t))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3i.t$sumF <- sum(he.2D3i.t$he_2D3i.t)
he.2D3i.t %<>% 
  dplyr::mutate(NOx_2D3i.t = (t.2D3i.t$NOx/t.2D3i.t$sumF)*he_2D3i.t, 
                NOx_2D3i.t_p = (NOx_2D3i.t/sum(NOx_2D3i.t))*100,
                SO2_2D3i.t = (t.2D3i.t$SO2/t.2D3i.t$sumF)*he_2D3i.t, 
                SO2_2D3i.t_p = (SO2_2D3i.t/sum(SO2_2D3i.t))*100,
                PM10_2D3i.t = (t.2D3i.t$PM10/t.2D3i.t$sumF)*he_2D3i.t, 
                PM10_2D3i.t_p = (PM10_2D3i.t/sum(PM10_2D3i.t))*100,
                PM2.5_2D3i.t = (t.2D3i.t$PM2.5/t.2D3i.t$sumF)*he_2D3i.t, 
                PM2.5_2D3i.t_p = (PM2.5_2D3i.t/sum(PM2.5_2D3i.t))*100,
                NMVOC_2D3i.t = (t.2D3i.t$NMVOC/t.2D3i.t$sumF)*he_2D3i.t, 
                NMVOC_2D3i.t_p = (NMVOC_2D3i.t/sum(NMVOC_2D3i.t))*100,
                NH3_2D3i.t = (t.2D3i.t$NH3/t.2D3i.t$sumF)*he_2D3i.t, 
                NH3_2D3i.t_p = (NH3_2D3i.t/sum(NH3_2D3i.t))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3i.t_p, SO2_2D3i.t_p, PM10_2D3i.t_p, PM2.5_2D3i.t_p, NMVOC_2D3i.t_p, NH3_2D3i.t_p) %>%
  rename(`2D3i.t_NOx` = NOx_2D3i.t_p,
         `2D3i.t_SO2` = SO2_2D3i.t_p,
         `2D3i.t_PM10` = PM10_2D3i.t_p,
         `2D3i.t_PM2.5` = PM2.5_2D3i.t_p,
         `2D3i.t_NMVOC` = NMVOC_2D3i.t_p,
         `2D3i.t_NH3` = NH3_2D3i.t_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3i.t$`2D3i.t_NOx`), sum(he.2D3i.t$`2D3i.t_SO2`), sum(he.2D3i.t$`2D3i.t_PM10`), sum(he.2D3i.t$`2D3i.t_PM2.5`), sum(he.2D3i.t$`2D3i.t_NMVOC`), sum(he.2D3i.t$`2D3i.t_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3i.t_df <- sf.2D3i.t %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3i.t.tl <- lapply(sf.2D3i.t_df[,-1], function(x) t((x %o% he.2D3i.t$he_2D3i.t_n)[,,1]))
# 
# sf.2D3i.t.tl <- lapply(sf.2D3i.t.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3i.t.tle, "sf.2D3i.t.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3i.t_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3i.t.tl[[i]], file = paste("sf.2D3i.t", paste(vars[i],"csv", sep = "."), sep = "_"))
# }







#'
#'
#'
#'
#'
#'
#' ## 2D3i - Other solvent and product use  - Use of shoes
#+ include = FALSE

sf.2D3i.uos <- st_read("D:/R_projects/Spatialization/Products/2 - Other processes/2D3i.gpkg") # PROMENIII

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.2D3i.uos <- sf.2D3i.uos %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.2D3i.uos%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + inverse(TEMP) + SLP + !PH
#

he.2D3i.uos <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3i.uos = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3i.uos))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3i.uos = he_sig) %>%
  dplyr::mutate(he_2D3i.uos_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3i.uos, he_2D3i.uos_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3i.uos, aes(x = times, y = he_2D3i.uos)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3i.uos = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.2D3i.uos$he_2D3i.uos), max(he.2D3i.uos$he_2D3i.uos), sum(he.2D3i.uos$he_2D3i.uos))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.2D3i.uos$sumF <- sum(he.2D3i.uos$he_2D3i.uos)
he.2D3i.uos %<>% 
  dplyr::mutate(NOx_2D3i.uos = (t.2D3i.uos$NOx/t.2D3i.uos$sumF)*he_2D3i.uos, 
                NOx_2D3i.uos_p = (NOx_2D3i.uos/sum(NOx_2D3i.uos))*100,
                SO2_2D3i.uos = (t.2D3i.uos$SO2/t.2D3i.uos$sumF)*he_2D3i.uos, 
                SO2_2D3i.uos_p = (SO2_2D3i.uos/sum(SO2_2D3i.uos))*100,
                PM10_2D3i.uos = (t.2D3i.uos$PM10/t.2D3i.uos$sumF)*he_2D3i.uos, 
                PM10_2D3i.uos_p = (PM10_2D3i.uos/sum(PM10_2D3i.uos))*100,
                PM2.5_2D3i.uos = (t.2D3i.uos$PM2.5/t.2D3i.uos$sumF)*he_2D3i.uos, 
                PM2.5_2D3i.uos_p = (PM2.5_2D3i.uos/sum(PM2.5_2D3i.uos))*100,
                NMVOC_2D3i.uos = (t.2D3i.uos$NMVOC/t.2D3i.uos$sumF)*he_2D3i.uos, 
                NMVOC_2D3i.uos_p = (NMVOC_2D3i.uos/sum(NMVOC_2D3i.uos))*100,
                NH3_2D3i.uos = (t.2D3i.uos$NH3/t.2D3i.uos$sumF)*he_2D3i.uos, 
                NH3_2D3i.uos_p = (NH3_2D3i.uos/sum(NH3_2D3i.uos))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_2D3i.uos_p, SO2_2D3i.uos_p, PM10_2D3i.uos_p, PM2.5_2D3i.uos_p, NMVOC_2D3i.uos_p, NH3_2D3i.uos_p) %>%
  rename(`2D3i.uos_NOx` = NOx_2D3i.uos_p,
         `2D3i.uos_SO2` = SO2_2D3i.uos_p,
         `2D3i.uos_PM10` = PM10_2D3i.uos_p,
         `2D3i.uos_PM2.5` = PM2.5_2D3i.uos_p,
         `2D3i.uos_NMVOC` = NMVOC_2D3i.uos_p,
         `2D3i.uos_NH3` = NH3_2D3i.uos_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.2D3i.uos$`2D3i.uos_NOx`), sum(he.2D3i.uos$`2D3i.uos_SO2`), sum(he.2D3i.uos$`2D3i.uos_PM10`), sum(he.2D3i.uos$`2D3i.uos_PM2.5`), sum(he.2D3i.uos$`2D3i.uos_NMVOC`), sum(he.2D3i.uos$`2D3i.uos_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.2D3i.uos_df <- sf.2D3i.uos %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3i.uos.tl <- lapply(sf.2D3i.uos_df[,-1], function(x) t((x %o% he.2D3i.uos$he_2D3i.uos_n)[,,1]))
# 
# sf.2D3i.uos.tl <- lapply(sf.2D3i.uos.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3i.uos.tle, "sf.2D3i.uos.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3i.uos_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3i.uos.tl[[i]], file = paste("sf.2D3i.uos", paste(vars[i],"csv", sep = "."), sep = "_"))
# }





















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
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3a = ((DL+0.5)) * (TEMP+30) * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3a))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3a = he_sig) %>%
  dplyr::mutate(he_2D3a_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3a, he_2D3a_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3a, aes(x = times, y = he_2D3a)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_2D3a = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
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
# sf.2D3a_df <- sf.2D3a %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3a.tl <- lapply(sf.2D3a_df[,-1], function(x) t((x %o% he.2D3a$he_2D3a_n)[,,1]))
# 
# sf.2D3a.tl <- lapply(sf.2D3a.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3a.tle, "sf.2D3a.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3a_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3a.tl[[i]], file = paste("sf.2D3a", paste(vars[i],"csv", sep = "."), sep = "_"))
# }
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
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3e = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3e))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3e = he_sig) %>%
  dplyr::mutate(he_2D3e_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3e, he_2D3e_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3e, aes(x = times, y = he_2D3e)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3e = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
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
# sf.2D3e_df <- sf.2D3e %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3e.tl <- lapply(sf.2D3e_df[,-1], function(x) t((x %o% he.2D3e$he_2D3e_n)[,,1]))
# 
# sf.2D3e.tl <- lapply(sf.2D3e.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3e.tle, "sf.2D3e.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3e_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3e.tl[[i]], file = paste("sf.2D3e", paste(vars[i],"csv", sep = "."), sep = "_"))
# }
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
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3f = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3f))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3f = he_sig) %>%
  dplyr::mutate(he_2D3f_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3f, he_2D3f_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3f, aes(x = times, y = he_2D3f)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3f = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
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
# sf.2D3f_df <- sf.2D3f %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3f.tl <- lapply(sf.2D3f_df[,-1], function(x) t((x %o% he.2D3f$he_2D3f_n)[,,1]))
# 
# sf.2D3f.tl <- lapply(sf.2D3f.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3f.tle, "sf.2D3f.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3f_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3f.tl[[i]], file = paste("sf.2D3f", paste(vars[i],"csv", sep = "."), sep = "_"))
# }
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
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2D3h = ((DL+0.5)) * (TEMP+30) * SLP * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2D3h))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2D3h = he_sig) %>%
  dplyr::mutate(he_2D3h_n = he_sig/sum(he_sig))%>%
  select(times, he_2D3h, he_2D3h_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2D3h, aes(x = times, y = he_2D3h)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2D3h = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
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
# sf.2D3h_df <- sf.2D3h %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2D3h.tl <- lapply(sf.2D3h_df[,-1], function(x) t((x %o% he.2D3h$he_2D3h_n)[,,1]))
# 
# sf.2D3h.tl <- lapply(sf.2D3h.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2D3h.tle, "sf.2D3h.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2D3h_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2D3h.tl[[i]], file = paste("sf.2D3h", paste(vars[i],"csv", sep = "."), sep = "_"))
# }
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
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+1)) %>%
  dplyr::mutate(he_2I = ((WT0816+0.5) * (DL+0.5)) * PH2 * (TEMP + 30) * WE2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_2I))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_2I = he_sig) %>%
  dplyr::mutate(he_2I_n = he_sig/sum(he_sig))%>%
  select(times, he_2I, he_2I_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.2I, aes(x = times, y = he_2I)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_2I = ((WT0816+0.5) * (DL+0.5)) * PH2 * (TEMP + 30) * WE2")+
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
#'
#'
#'
# sf.2I_df <- sf.2I %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.2I.tl <- lapply(sf.2I_df[,-1], function(x) t((x %o% he.2I$he_2I_n)[,,1]))
# 
# sf.2I.tl <- lapply(sf.2I.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.2I.tle, "sf.2I.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.2I_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.2I.tl[[i]], file = paste("sf.2I", paste(vars[i],"csv", sep = "."), sep = "_"))
# }




# temporalProfile_Other_processes <- activity.df$times %>% cbind(he.2A5a[,1:6], 
#                                                            he.2A5b[,1:6],
#                                                            he.2A5c[,1:6],
#                                                            he.2D3a[,1:6], 
#                                                            he.2D3b[,1:6], 
#                                                            he.2D3c[,1:6], 
#                                                            he.2D3d[,1:6], # DODATI
#                                                            he.2D3e[,1:6], 
#                                                            he.2D3f[,1:6], 
#                                                            he.2D3g[,1:6], # DODATI
#                                                            he.2D3h[,1:6], 
#                                                            he.2D3i[,1:6], # DODATI
#                                                            he.2I[,1:6]) %>% 
#   as.data.frame()
# 
# writexl::write_xlsx(temporalProfile_Other_processes, path = 'Hourly_emissions/Products/TemporalProfile_Other_processes.xlsx')

