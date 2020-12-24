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
#' # 1B - Fugitive emissions
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
activity.df <- readRDS(file = "D:/R_projects/Spatialization/Version_4_update/Temporalization/activity_df_new.rds")
load(file = "D:/R_projects/Spatialization/Hourly_emissions/Data/counters_df.rds")
counters.df <- counters_df %>% 
  mutate(ALL_mean = (IA_mean + IIA_mean + IB_mean)/3)
activity.df$VA <- counters.df$ALL_mean
activity.df$times_2030 <- seq.POSIXt(from = ymd_h("2030-01-01 00"),
                                     to   = ymd_h("2030-12-31 23"),
                                     by   = dhours(1)) 

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
#' ## 1B1a-Fugitive emission from solid fuels: Coal mining and handling 
#+ include = FALSE

sf.1B1a <- st_read("D:/R_projects/Spatialization/2030/Products_2030/1B - Fugitive emissions_2030/1B1a.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1B1a <- sf.1B1a %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1B1a%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + !PH + inverse(SA) + !RP
#

he.1B1a <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1B1a = EP*weekly.ep*hourly.ep) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1B1a))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1B1a = he_sig) %>%
  dplyr::mutate(he_1B1a_n = he_sig/sum(he_sig))%>%
  select(times, he_1B1a, he_1B1a_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1B1a, aes(x = times, y = he_1B1a)) +
  geom_point(size = 0.1) +
  geom_line(colour = "purple") + 
  # geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_1B1a = (WDWW * PH2 * (TEMP+30) * RP2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1B1a$he_1B1a), max(he.1B1a$he_1B1a), sum(he.1B1a$he_1B1a))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1B1a$sumF <- sum(he.1B1a$he_1B1a)
he.1B1a %<>% 
  dplyr::mutate(NOx_1B1a = (t.1B1a$NOx/t.1B1a$sumF)*he_1B1a, 
                NOx_1B1a_p = (NOx_1B1a/sum(NOx_1B1a))*100,
                SO2_1B1a = (t.1B1a$SO2/t.1B1a$sumF)*he_1B1a, 
                SO2_1B1a_p = (SO2_1B1a/sum(SO2_1B1a))*100,
                PM10_1B1a = (t.1B1a$PM10/t.1B1a$sumF)*he_1B1a, 
                PM10_1B1a_p = (PM10_1B1a/sum(PM10_1B1a))*100,
                PM2.5_1B1a = (t.1B1a$PM2.5/t.1B1a$sumF)*he_1B1a, 
                PM2.5_1B1a_p = (PM2.5_1B1a/sum(PM2.5_1B1a))*100,
                NMVOC_1B1a = (t.1B1a$NMVOC/t.1B1a$sumF)*he_1B1a, 
                NMVOC_1B1a_p = (NMVOC_1B1a/sum(NMVOC_1B1a))*100,
                NH3_1B1a = (t.1B1a$NH3/t.1B1a$sumF)*he_1B1a, 
                NH3_1B1a_p = (NH3_1B1a/sum(NH3_1B1a))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1B1a_p, SO2_1B1a_p, PM10_1B1a_p, PM2.5_1B1a_p, NMVOC_1B1a_p, NH3_1B1a_p) %>%
  rename(`1B1a_NOx` = NOx_1B1a_p,
         `1B1a_SO2` = SO2_1B1a_p,
         `1B1a_PM10` = PM10_1B1a_p,
         `1B1a_PM2.5` = PM2.5_1B1a_p,
         `1B1a_NMVOC` = NMVOC_1B1a_p,
         `1B1a_NH3` = NH3_1B1a_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1B1a$`1B1a_NOx`), sum(he.1B1a$`1B1a_SO2`), sum(he.1B1a$`1B1a_PM10`), sum(he.1B1a$`1B1a_PM2.5`), sum(he.1B1a$`1B1a_NMVOC`), sum(he.1B1a$`1B1a_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.1B1a_df <- sf.1B1a %>% st_drop_geometry() #%>% dplyr::select(NOx)
ids <- sf.1B1a$ID

sf.1B1a.tl <- lapply(sf.1B1a_df[,-1], function(x) t((x %o% he.1B1a$he_1B1a_n)[,,1]))

sf.1B1a.tl <- lapply(sf.1B1a.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.1B1a.tle, "sf.1B1a.tle.xlsx") # Mnogo traje...

vars <- names(sf.1B1a_df)[-1]

for(i in 1:length(vars)){
  fwrite(sf.1B1a.tl[[i]], file = paste("sf.1B1a", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 1B2ai-Fugitive emissions from liquid fuels: Exploration, production, transport
#+ include = FALSE

sf.1B2ai <- st_read("D:/R_projects/Spatialization/2030/Products_2030/1B - Fugitive emissions_2030/1B2ai.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1B2ai <- sf.1B2ai %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1B2ai%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WD + WT0816 + WT1624 + !PH 
#

he.1B2ai <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1B2ai = 1) %>%
  # dplyr::mutate(he_sig = sigmoid(scale(he_1B2ai))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  # dplyr::mutate(he_1B2ai = he_sig) %>%
  dplyr::mutate(he_1B2ai_n = as.matrix(he_1B2ai))%>%
  select(times, he_1B2ai, he_1B2ai_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1B2ai, aes(x = times, y = he_1B2ai)) +
  geom_point(size = 0.1) +
  geom_line(colour = "purple") + 
  # geom_smooth()+
  theme_bw() + 
  # ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_1B2ai = ((WT0816+0.5) * (WT1624+0.5)) * PH2 * VA")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1B2ai$he_1B2ai), max(he.1B2ai$he_1B2ai), sum(he.1B2ai$he_1B2ai))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1B2ai$sumF <- sum(he.1B2ai$he_1B2ai)
he.1B2ai %<>% 
  dplyr::mutate(NOx_1B2ai = (t.1B2ai$NOx/t.1B2ai$sumF)*he_1B2ai, 
                NOx_1B2ai_p = (NOx_1B2ai/sum(NOx_1B2ai))*100,
                SO2_1B2ai = (t.1B2ai$SO2/t.1B2ai$sumF)*he_1B2ai, 
                SO2_1B2ai_p = (SO2_1B2ai/sum(SO2_1B2ai))*100,
                PM10_1B2ai = (t.1B2ai$PM10/t.1B2ai$sumF)*he_1B2ai, 
                PM10_1B2ai_p = (PM10_1B2ai/sum(PM10_1B2ai))*100,
                PM2.5_1B2ai = (t.1B2ai$PM2.5/t.1B2ai$sumF)*he_1B2ai, 
                PM2.5_1B2ai_p = (PM2.5_1B2ai/sum(PM2.5_1B2ai))*100,
                NMVOC_1B2ai = (t.1B2ai$NMVOC/t.1B2ai$sumF)*he_1B2ai, 
                NMVOC_1B2ai_p = (NMVOC_1B2ai/sum(NMVOC_1B2ai))*100,
                NH3_1B2ai = (t.1B2ai$NH3/t.1B2ai$sumF)*he_1B2ai, 
                NH3_1B2ai_p = (NH3_1B2ai/sum(NH3_1B2ai))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1B2ai_p, SO2_1B2ai_p, PM10_1B2ai_p, PM2.5_1B2ai_p, NMVOC_1B2ai_p, NH3_1B2ai_p) %>%
  rename(`1B2ai_NOx` = NOx_1B2ai_p,
         `1B2ai_SO2` = SO2_1B2ai_p,
         `1B2ai_PM10` = PM10_1B2ai_p,
         `1B2ai_PM2.5` = PM2.5_1B2ai_p,
         `1B2ai_NMVOC` = NMVOC_1B2ai_p,
         `1B2ai_NH3` = NH3_1B2ai_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1B2ai$`1B2ai_NOx`), sum(he.1B2ai$`1B2ai_SO2`), sum(he.1B2ai$`1B2ai_PM10`), sum(he.1B2ai$`1B2ai_PM2.5`), sum(he.1B2ai$`1B2ai_NMVOC`), sum(he.1B2ai$`1B2ai_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.1B2ai_df <- sf.1B2ai %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.1B2ai.tl <- lapply(sf.1B2ai_df[,-1], function(x) t((x %o% (he.1B2ai$he_1B2ai_n/8760))[,,1]))

sf.1B2ai.tl <- lapply(sf.1B2ai.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.1B2ai.tle, "sf.1B2ai.tle.xlsx") # Mnogo traje...

vars <- names(sf.1B2ai_df)[-1]

for(i in 1:length(vars)){
  fwrite(sf.1B2ai.tl[[i]], file = paste("sf.1B2ai", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 1B2av-Fugitive emissions from liquid fuels: Distribution of oil products
#+ include = FALSE

sf.1B2av <- st_read("D:/R_projects/Spatialization/2030/Products_2030/1B - Fugitive emissions_2030/1B2av.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1B2av <- sf.1B2av %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1B2av%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + !PH
#

he.1B2av <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1B2av = (VA+0.5) * (DL+0.5)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1B2av))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1B2av = he_sig) %>%
  dplyr::mutate(he_1B2av_n = he_sig/sum(he_sig))%>%
  select(times, he_1B2av, he_1B2av_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1B2av, aes(x = times, y = he_1B2av)) +
  geom_point(size = 0.1) +
  geom_line(colour = "purple") + 
  # geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_1B2av = ((WT0024+0.5)) * PH2 * TEMP")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1B2av$he_1B2av), max(he.1B2av$he_1B2av), sum(he.1B2av$he_1B2av))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1B2av$sumF <- sum(he.1B2av$he_1B2av)
he.1B2av %<>% 
  dplyr::mutate(NOx_1B2av = (t.1B2av$NOx/t.1B2av$sumF)*he_1B2av, 
                NOx_1B2av_p = (NOx_1B2av/sum(NOx_1B2av))*100,
                SO2_1B2av = (t.1B2av$SO2/t.1B2av$sumF)*he_1B2av, 
                SO2_1B2av_p = (SO2_1B2av/sum(SO2_1B2av))*100,
                PM10_1B2av = (t.1B2av$PM10/t.1B2av$sumF)*he_1B2av, 
                PM10_1B2av_p = (PM10_1B2av/sum(PM10_1B2av))*100,
                PM2.5_1B2av = (t.1B2av$PM2.5/t.1B2av$sumF)*he_1B2av, 
                PM2.5_1B2av_p = (PM2.5_1B2av/sum(PM2.5_1B2av))*100,
                NMVOC_1B2av = (t.1B2av$NMVOC/t.1B2av$sumF)*he_1B2av, 
                NMVOC_1B2av_p = (NMVOC_1B2av/sum(NMVOC_1B2av))*100,
                NH3_1B2av = (t.1B2av$NH3/t.1B2av$sumF)*he_1B2av, 
                NH3_1B2av_p = (NH3_1B2av/sum(NH3_1B2av))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1B2av_p, SO2_1B2av_p, PM10_1B2av_p, PM2.5_1B2av_p, NMVOC_1B2av_p, NH3_1B2av_p) %>%
  rename(`1B2av_NOx` = NOx_1B2av_p,
         `1B2av_SO2` = SO2_1B2av_p,
         `1B2av_PM10` = PM10_1B2av_p,
         `1B2av_PM2.5` = PM2.5_1B2av_p,
         `1B2av_NMVOC` = NMVOC_1B2av_p,
         `1B2av_NH3` = NH3_1B2av_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1B2av$`1B2av_NOx`), sum(he.1B2av$`1B2av_SO2`), sum(he.1B2av$`1B2av_PM10`), sum(he.1B2av$`1B2av_PM2.5`), sum(he.1B2av$`1B2av_NMVOC`), sum(he.1B2av$`1B2av_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.1B2av_df <- sf.1B2av %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.1B2av.tl <- lapply(sf.1B2av_df[,-1], function(x) t((x %o% he.1B2av$he_1B2av_n)[,,1]))

sf.1B2av.tl <- lapply(sf.1B2av.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.1B2av.tle, "sf.1B2av.tle.xlsx") # Mnogo traje...

vars <- names(sf.1B2av_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.1B2av.tl[[i]], file = paste("sf.1B2av", paste(vars[i],"csv", sep = "."), sep = "_"))
}
#'
#'
#'
#'
#'
#'
#' ## 1B2b-Fugitive emissions from natural gas: Exploration, production, transport
#+ include = FALSE

sf.1B2b <- st_read("D:/R_projects/Spatialization/2030/Products_2030/1B - Fugitive emissions_2030/1B2b.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1B2b <- sf.1B2b %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1B2b%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + !PH
#

he.1B2b <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((pi)/24)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1B2b = 1) %>%
  # dplyr::mutate(he_sig = sigmoid(scale(he_1B2b))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  # dplyr::mutate(he_1B2b = he_sig) %>%
   dplyr::mutate(he_1B2b_n = as.matrix(he_1B2b))%>%
  select(times, he_1B2b, he_1B2b_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1B2b, aes(x = times, y = he_1B2b)) +
  geom_point(size = 0.1) +
  geom_line(colour = "purple") + 
  # geom_smooth()+
  theme_bw() + 
  # ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_1B2b = (WDWW * (WT0024+0.5)) * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1B2b$he_1B2b), max(he.1B2b$he_1B2b), sum(he.1B2b$he_1B2b))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1B2b$sumF <- sum(he.1B2b$he_1B2b)
he.1B2b %<>% 
  dplyr::mutate(NOx_1B2b = (t.1B2b$NOx/t.1B2b$sumF)*he_1B2b, 
                NOx_1B2b_p = (NOx_1B2b/sum(NOx_1B2b))*100,
                SO2_1B2b = (t.1B2b$SO2/t.1B2b$sumF)*he_1B2b, 
                SO2_1B2b_p = (SO2_1B2b/sum(SO2_1B2b))*100,
                PM10_1B2b = (t.1B2b$PM10/t.1B2b$sumF)*he_1B2b, 
                PM10_1B2b_p = (PM10_1B2b/sum(PM10_1B2b))*100,
                PM2.5_1B2b = (t.1B2b$PM2.5/t.1B2b$sumF)*he_1B2b, 
                PM2.5_1B2b_p = (PM2.5_1B2b/sum(PM2.5_1B2b))*100,
                NMVOC_1B2b = (t.1B2b$NMVOC/t.1B2b$sumF)*he_1B2b, 
                NMVOC_1B2b_p = (NMVOC_1B2b/sum(NMVOC_1B2b))*100,
                NH3_1B2b = (t.1B2b$NH3/t.1B2b$sumF)*he_1B2b, 
                NH3_1B2b_p = (NH3_1B2b/sum(NH3_1B2b))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1B2b_p, SO2_1B2b_p, PM10_1B2b_p, PM2.5_1B2b_p, NMVOC_1B2b_p, NH3_1B2b_p) %>%
  rename(`1B2b_NOx` = NOx_1B2b_p,
         `1B2b_SO2` = SO2_1B2b_p,
         `1B2b_PM10` = PM10_1B2b_p,
         `1B2b_PM2.5` = PM2.5_1B2b_p,
         `1B2b_NMVOC` = NMVOC_1B2b_p,
         `1B2b_NH3` = NH3_1B2b_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1B2b$`1B2b_NOx`), sum(he.1B2b$`1B2b_SO2`), sum(he.1B2b$`1B2b_PM10`), sum(he.1B2b$`1B2b_PM2.5`), sum(he.1B2b$`1B2b_NMVOC`), sum(he.1B2b$`1B2b_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.1B2b_df <- sf.1B2b %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.1B2b.tl <- lapply(sf.1B2b_df[,-1], function(x) t((x %o% (he.1B2b$he_1B2b_n/8760))[,,1]))

sf.1B2b.tl <- lapply(sf.1B2b.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.1B2b.tle, "sf.1B2b.tle.xlsx") # Mnogo traje...

vars <- names(sf.1B2b_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.1B2b.tl[[i]], file = paste("sf.1B2b", paste(vars[i],"csv", sep = "."), sep = "_"))
}

temporalProfile_Fugitive_emissions <- activity.df$times_2030 %>% cbind(he.1B1a[,1:6], 
                                                           he.1B2ai[,1:6], 
                                                           he.1B2av[,1:6], 
                                                           he.1B2b[,1:6]) %>% 
  as.data.frame()

writexl::write_xlsx(temporalProfile_Fugitive_emissions, path = '2030/Hourly_emissions_2030/Products_2030/TemporalProfile_Fugitive_emissions_2030.xlsx')





