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
#' # 1A4 - Residential/Tertiary
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
#' ## 1A4ai - Commercial/Institutional: Stationary Combustion
#+ include = FALSE

sf.1A4ai <- st_read("D:/R_projects/Spatialization/Products/1A4 - Residential-Tertiary/1A4ai.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A4ai <- sf.1A4ai %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A4ai%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  WDWW + WT0622 + k*PH + HS + inverse(TEMP)
#

he.1A4ai <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A4ai = ((WT0622+0.5)) / PH2 * (TEMP*(-1)+30)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1A4ai))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1A4ai = he_sig) %>%
  dplyr::mutate(he_1A4ai_n = he_sig/sum(he_sig))%>%
  select(times, he_1A4ai, he_1A4ai_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A4ai, aes(x = times, y = he_1A4ai)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_1A4ai = ((WT0622+0.5)) / PH2 * (TEMP*(-1)+30)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A4ai$he_1A4ai), max(he.1A4ai$he_1A4ai), sum(he.1A4ai$he_1A4ai))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A4ai$sumF <- sum(he.1A4ai$he_1A4ai)
he.1A4ai %<>% 
  dplyr::mutate(NOx_1A4ai = (t.1A4ai$NOx/t.1A4ai$sumF)*he_1A4ai, 
                NOx_1A4ai_p = (NOx_1A4ai/sum(NOx_1A4ai))*100,
                SO2_1A4ai = (t.1A4ai$SO2/t.1A4ai$sumF)*he_1A4ai, 
                SO2_1A4ai_p = (SO2_1A4ai/sum(SO2_1A4ai))*100,
                PM10_1A4ai = (t.1A4ai$PM10/t.1A4ai$sumF)*he_1A4ai, 
                PM10_1A4ai_p = (PM10_1A4ai/sum(PM10_1A4ai))*100,
                PM2.5_1A4ai = (t.1A4ai$PM2.5/t.1A4ai$sumF)*he_1A4ai, 
                PM2.5_1A4ai_p = (PM2.5_1A4ai/sum(PM2.5_1A4ai))*100,
                NMVOC_1A4ai = (t.1A4ai$NMVOC/t.1A4ai$sumF)*he_1A4ai, 
                NMVOC_1A4ai_p = (NMVOC_1A4ai/sum(NMVOC_1A4ai))*100,
                NH3_1A4ai = (t.1A4ai$NH3/t.1A4ai$sumF)*he_1A4ai, 
                NH3_1A4ai_p = (NH3_1A4ai/sum(NH3_1A4ai))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A4ai_p, SO2_1A4ai_p, PM10_1A4ai_p, PM2.5_1A4ai_p, NMVOC_1A4ai_p, NH3_1A4ai_p) %>%
  rename(`1A4ai_NOx` = NOx_1A4ai_p,
         `1A4ai_SO2` = SO2_1A4ai_p,
         `1A4ai_PM10` = PM10_1A4ai_p,
         `1A4ai_PM2.5` = PM2.5_1A4ai_p,
         `1A4ai_NMVOC` = NMVOC_1A4ai_p,
         `1A4ai_NH3` = NH3_1A4ai_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A4ai$`1A4ai_NOx`), sum(he.1A4ai$`1A4ai_SO2`), sum(he.1A4ai$`1A4ai_PM10`), sum(he.1A4ai$`1A4ai_PM2.5`), sum(he.1A4ai$`1A4ai_NMVOC`), sum(he.1A4ai$`1A4ai_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.1A4ai_df <- sf.1A4ai %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1A4ai.tl <- lapply(sf.1A4ai_df[,-1], function(x) t((x %o% he.1A4ai$he_1A4ai_n)[,,1]))
# 
# sf.1A4ai.tl <- lapply(sf.1A4ai.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.1A4ai.tle, "sf.1A4ai.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.1A4ai_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1A4ai.tl[[i]], file = paste("sf.1A4ai", paste(vars[i],"csv", sep = "."), sep = "_"))
# }


#'
#'
#'
#'
#'
#'
#' ## 1A4bi - Residential: Stationary combustion
#+ include = FALSE
sf.1A4bi <- st_read("D:/R_projects/Spatialization/Products/1A4 - Residential-Tertiary/1A4bi.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A4bi <- sf.1A4bi %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A4bi%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  WDWW + WT0622 + k*PH + HS + inverse(TEMP)
#

he.1A4bi <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_1A4bi = ((WT0622+0.5)) / PH2 * (TEMP*(-1)+30)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1A4bi))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1A4bi = he_sig) %>%
  dplyr::mutate(he_1A4bi_n = he_sig/sum(he_sig))%>%
  select(times, he_1A4bi, he_1A4bi_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A4bi, aes(x = times, y = he_1A4bi)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "HE = WT0622 + k*PH + HS + inverse(TEMP)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A4bi$he_1A4bi), max(he.1A4bi$he_1A4bi), sum(he.1A4bi$he_1A4bi))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A4bi$sumF <- sum(he.1A4bi$he_1A4bi)
he.1A4bi %<>% 
  dplyr::mutate(NOx_1A4bi = (t.1A4bi$NOx/t.1A4bi$sumF)*he_1A4bi, 
                NOx_1A4bi_p = (NOx_1A4bi/sum(NOx_1A4bi))*100,
                SO2_1A4bi = (t.1A4bi$SO2/t.1A4bi$sumF)*he_1A4bi, 
                SO2_1A4bi_p = (SO2_1A4bi/sum(SO2_1A4bi))*100,
                PM10_1A4bi = (t.1A4bi$PM10/t.1A4bi$sumF)*he_1A4bi, 
                PM10_1A4bi_p = (PM10_1A4bi/sum(PM10_1A4bi))*100,
                PM2.5_1A4bi = (t.1A4bi$PM2.5/t.1A4bi$sumF)*he_1A4bi, 
                PM2.5_1A4bi_p = (PM2.5_1A4bi/sum(PM2.5_1A4bi))*100,
                NMVOC_1A4bi = (t.1A4bi$NMVOC/t.1A4bi$sumF)*he_1A4bi, 
                NMVOC_1A4bi_p = (NMVOC_1A4bi/sum(NMVOC_1A4bi))*100,
                NH3_1A4bi = (t.1A4bi$NH3/t.1A4bi$sumF)*he_1A4bi, 
                NH3_1A4bi_p = (NH3_1A4bi/sum(NH3_1A4bi))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A4bi_p, SO2_1A4bi_p, PM10_1A4bi_p, PM2.5_1A4bi_p, NMVOC_1A4bi_p, NH3_1A4bi_p) %>%
  rename(`1A4bi_NOx` = NOx_1A4bi_p,
         `1A4bi_SO2` = SO2_1A4bi_p,
         `1A4bi_PM10` = PM10_1A4bi_p,
         `1A4bi_PM2.5` = PM2.5_1A4bi_p,
         `1A4bi_NMVOC` = NMVOC_1A4bi_p,
         `1A4bi_NH3` = NH3_1A4bi_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A4bi$`1A4bi_NOx`), sum(he.1A4bi$`1A4bi_SO2`), sum(he.1A4bi$`1A4bi_PM10`), sum(he.1A4bi$`1A4bi_PM2.5`), sum(he.1A4bi$`1A4bi_NMVOC`), sum(he.1A4bi$`1A4bi_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.1A4bi_df <- sf.1A4bi %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1A4bi.tl <- lapply(sf.1A4bi_df[,-1], function(x) t((x %o% he.1A4bi$he_1A4bi_n)[,,1]))
# 
# sf.1A4bi.tl <- lapply(sf.1A4bi.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.1A4bi.tle, "sf.1A4bi.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.1A4bi_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1A4bi.tl[[i]], file = paste("sf.1A4bi", paste(vars[i],"csv", sep = "."), sep = "_"))
# }


#'
#'
#'
#'
#'
#'
#' ## 1A4ci - Agriculture/Forestry/Fishing: Stationary combustion
#+ include = FALSE
sf.1A4ci <- st_read("D:/R_projects/Spatialization/Products/1A4 - Residential-Tertiary/1A4ci.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A4ci <- sf.1A4ci %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A4ci%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + !PH + SA + SAAG
#

he.1A4ci <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(SAAG1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(SAAG2 = (sin(((2*pi)/12)*(!SAAG1))+0.5)) %>%
  dplyr::mutate(he_1A4ci = ((DL+0.5)) * PH2 * (-TEMP+30) + SAAG.f + SAAG.fl) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1A4ci))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1A4ci = he_sig) %>%
  dplyr::mutate(he_1A4ci_n = he_sig/sum(he_sig))%>%
  select(times, he_1A4ci, he_1A4ci_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A4ci, aes(x = times, y = he_1A4ci)) +
  # geom_point(size = 0.1) +
  # geom_smooth() +
   geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_1A4ci = ((DL+0.5)) * PH2 * (-TEMP+30) + SAAG.f + SAAG.fl)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A4ci$he_1A4ci), max(he.1A4ci$he_1A4ci), sum(he.1A4ci$he_1A4ci))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A4ci$sumF <- sum(he.1A4ci$he_1A4ci)
he.1A4ci %<>% 
  dplyr::mutate(NOx_1A4ci = (t.1A4ci$NOx/t.1A4ci$sumF)*he_1A4ci, 
                NOx_1A4ci_p = (NOx_1A4ci/sum(NOx_1A4ci))*100,
                SO2_1A4ci = (t.1A4ci$SO2/t.1A4ci$sumF)*he_1A4ci, 
                SO2_1A4ci_p = (SO2_1A4ci/sum(SO2_1A4ci))*100,
                PM10_1A4ci = (t.1A4ci$PM10/t.1A4ci$sumF)*he_1A4ci, 
                PM10_1A4ci_p = (PM10_1A4ci/sum(PM10_1A4ci))*100,
                PM2.5_1A4ci = (t.1A4ci$PM2.5/t.1A4ci$sumF)*he_1A4ci, 
                PM2.5_1A4ci_p = (PM2.5_1A4ci/sum(PM2.5_1A4ci))*100,
                NMVOC_1A4ci = (t.1A4ci$NMVOC/t.1A4ci$sumF)*he_1A4ci, 
                NMVOC_1A4ci_p = (NMVOC_1A4ci/sum(NMVOC_1A4ci))*100,
                NH3_1A4ci = (t.1A4ci$NH3/t.1A4ci$sumF)*he_1A4ci, 
                NH3_1A4ci_p = (NH3_1A4ci/sum(NH3_1A4ci))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A4ci_p, SO2_1A4ci_p, PM10_1A4ci_p, PM2.5_1A4ci_p, NMVOC_1A4ci_p, NH3_1A4ci_p) %>%
  rename(`1A4ci_NOx` = NOx_1A4ci_p,
         `1A4ci_SO2` = SO2_1A4ci_p,
         `1A4ci_PM10` = PM10_1A4ci_p,
         `1A4ci_PM2.5` = PM2.5_1A4ci_p,
         `1A4ci_NMVOC` = NMVOC_1A4ci_p,
         `1A4ci_NH3` = NH3_1A4ci_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A4ci$`1A4ci_NOx`), sum(he.1A4ci$`1A4ci_SO2`), sum(he.1A4ci$`1A4ci_PM10`), sum(he.1A4ci$`1A4ci_PM2.5`), sum(he.1A4ci$`1A4ci_NMVOC`), sum(he.1A4ci$`1A4ci_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.1A4ci_df <- sf.1A4ci %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1A4ci.tl <- lapply(sf.1A4ci_df[,-1], function(x) t((x %o% he.1A4ci$he_1A4ci_n)[,,1]))
# 
# sf.1A4ci.tl <- lapply(sf.1A4ci.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.1A4ci.tle, "sf.1A4ci.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.1A4ci_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1A4ci.tl[[i]], file = paste("sf.1A4ci", paste(vars[i],"csv", sep = "."), sep = "_"))
# }


#'
#'
#'
#'
#'
#'
#' ## 1A4cii - Agriculture/Forestry/Fishing: Off-road vehicles and other machinery
#+ include = FALSE

sf.1A4cii <- st_read("D:/R_projects/Spatialization/Products/1A4 - Residential-Tertiary/1A4cii.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A4cii <- sf.1A4cii %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A4cii%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + DL + !PH + SA + SAAG
#

he.1A4cii <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+0.5)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(SAAG1 = dplyr::case_when(WE == TRUE ~ 1,
                                         WE == FALSE ~ 0)) %>%
  dplyr::mutate(SAAG2 = (sin(((2*pi)/12)*(!SAAG1))+0.5)) %>%
  dplyr::mutate(he_1A4cii = ((DL+0.5)) * PH2 * (-TEMP+30) + SAAG.f + SAAG.fl) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1A4cii))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1A4cii = he_sig) %>%
  dplyr::mutate(he_1A4cii_n = he_sig/sum(he_sig))%>%
  select(times, he_1A4cii, he_1A4cii_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A4cii, aes(x = times, y = he_1A4cii)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_1A4cii = ((DL+0.5)) * PH2 * (-TEMP+30) + SAAG.f + SAAG.fl)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A4cii$he_1A4cii), max(he.1A4cii$he_1A4cii), sum(he.1A4cii$he_1A4cii))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A4cii$sumF <- sum(he.1A4cii$he_1A4cii)
he.1A4cii %<>% 
  dplyr::mutate(NOx_1A4cii = (t.1A4cii$NOx/t.1A4cii$sumF)*he_1A4cii, 
                NOx_1A4cii_p = (NOx_1A4cii/sum(NOx_1A4cii))*100,
                SO2_1A4cii = (t.1A4cii$SO2/t.1A4cii$sumF)*he_1A4cii, 
                SO2_1A4cii_p = (SO2_1A4cii/sum(SO2_1A4cii))*100,
                PM10_1A4cii = (t.1A4cii$PM10/t.1A4cii$sumF)*he_1A4cii, 
                PM10_1A4cii_p = (PM10_1A4cii/sum(PM10_1A4cii))*100,
                PM2.5_1A4cii = (t.1A4cii$PM2.5/t.1A4cii$sumF)*he_1A4cii, 
                PM2.5_1A4cii_p = (PM2.5_1A4cii/sum(PM2.5_1A4cii))*100,
                NMVOC_1A4cii = (t.1A4cii$NMVOC/t.1A4cii$sumF)*he_1A4cii, 
                NMVOC_1A4cii_p = (NMVOC_1A4cii/sum(NMVOC_1A4cii))*100,
                NH3_1A4cii = (t.1A4cii$NH3/t.1A4cii$sumF)*he_1A4cii, 
                NH3_1A4cii_p = (NH3_1A4cii/sum(NH3_1A4cii))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A4cii_p, SO2_1A4cii_p, PM10_1A4cii_p, PM2.5_1A4cii_p, NMVOC_1A4cii_p, NH3_1A4cii_p) %>%
  rename(`1A4cii_NOx` = NOx_1A4cii_p,
         `1A4cii_SO2` = SO2_1A4cii_p,
         `1A4cii_PM10` = PM10_1A4cii_p,
         `1A4cii_PM2.5` = PM2.5_1A4cii_p,
         `1A4cii_NMVOC` = NMVOC_1A4cii_p,
         `1A4cii_NH3` = NH3_1A4cii_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A4cii$`1A4cii_NOx`), sum(he.1A4cii$`1A4cii_SO2`), sum(he.1A4cii$`1A4cii_PM10`), sum(he.1A4cii$`1A4cii_PM2.5`), sum(he.1A4cii$`1A4cii_NMVOC`), sum(he.1A4cii$`1A4cii_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
# sf.1A4cii_df <- sf.1A4cii %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1A4cii.tl <- lapply(sf.1A4cii_df[,-1], function(x) t((x %o% he.1A4cii$he_1A4cii_n)[,,1]))
# 
# sf.1A4cii.tl <- lapply(sf.1A4cii.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.1A4cii.tle, "sf.1A4cii.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.1A4cii_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1A4cii.tl[[i]], file = paste("sf.1A4cii", paste(vars[i],"csv", sep = "."), sep = "_"))
# }

#  temporalProfile_ResidentialTertiary <- activity.df$times %>% 
#    cbind(he.1A4ai[,1:6], 
#          he.1A4bi[,1:6], 
#          he.1A4ci[,1:6], 
#          he.1A4cii[,1:6]) %>% 
#    as.data.frame()
#  
#  writexl::write_xlsx(temporalProfile_ResidentialTertiary, path = 'Hourly_emissions/Products/TemporalProfile_Residential_Tertiary.xlsx')
