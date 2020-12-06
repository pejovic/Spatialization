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
#' # 3 - Agriculture
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
library(dublogistic)

Sys.setlocale("LC_ALL","English")

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
                                    "WE", "WW", "RH0709", "RH1517", "PH", "SA", "HS", "SAAG.f", "SAAG.fl", "TEMP", "SLP", "VA", "NFH", "RP", "EC"),
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
                                          "Agriculture Season - fertilizing",
                                          "Agriculture Season - farm level",
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
#' ## 3B3-Manure management - Swine
#+ include = FALSE

sf.3B3 <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3B3.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3B3 <- sf.3B3 %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3B3%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + (k+DL) + inverse(TEMP) + SLP
#

he.3B3 <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3B3 =(TEMP+30) * (DL+0.5)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3B3))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3B3 = he_sig) %>%
  dplyr::mutate(he_3B3_n = he_sig/sum(he_sig))%>%
  select(times, he_3B3, he_3B3_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3B3, aes(x = times, y = he_3B3)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_3B3 = (TEMP+30) * (DL+0.5)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3B3$he_3B3), max(he.3B3$he_3B3), sum(he.3B3$he_3B3))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3B3$sumF <- sum(he.3B3$he_3B3)
he.3B3 %<>% 
  dplyr::mutate(NOx_3B3 = (t.3B3$NOx/t.3B3$sumF)*he_3B3, 
                NOx_3B3_p = (NOx_3B3/sum(NOx_3B3))*100,
                SO2_3B3 = (t.3B3$SO2/t.3B3$sumF)*he_3B3, 
                SO2_3B3_p = (SO2_3B3/sum(SO2_3B3))*100,
                PM10_3B3 = (t.3B3$PM10/t.3B3$sumF)*he_3B3, 
                PM10_3B3_p = (PM10_3B3/sum(PM10_3B3))*100,
                PM2.5_3B3 = (t.3B3$PM2.5/t.3B3$sumF)*he_3B3, 
                PM2.5_3B3_p = (PM2.5_3B3/sum(PM2.5_3B3))*100,
                NMVOC_3B3 = (t.3B3$NMVOC/t.3B3$sumF)*he_3B3, 
                NMVOC_3B3_p = (NMVOC_3B3/sum(NMVOC_3B3))*100,
                NH3_3B3 = (t.3B3$NH3/t.3B3$sumF)*he_3B3, 
                NH3_3B3_p = (NH3_3B3/sum(NH3_3B3))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3B3_p, SO2_3B3_p, PM10_3B3_p, PM2.5_3B3_p, NMVOC_3B3_p, NH3_3B3_p) %>%
  rename(`3B3_NOx` = NOx_3B3_p,
         `3B3_SO2` = SO2_3B3_p,
         `3B3_PM10` = PM10_3B3_p,
         `3B3_PM2.5` = PM2.5_3B3_p,
         `3B3_NMVOC` = NMVOC_3B3_p,
         `3B3_NH3` = NH3_3B3_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3B3$`3B3_NOx`), sum(he.3B3$`3B3_SO2`), sum(he.3B3$`3B3_PM10`), sum(he.3B3$`3B3_PM2.5`), sum(he.3B3$`3B3_NMVOC`), sum(he.3B3$`3B3_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.3B3_df <- sf.3B3 %>% st_drop_geometry() #%>% dplyr::select(NOx)
ids <- sf.3B3$ID
sf.3B3.tl <- lapply(sf.3B3_df[,-1], function(x) t((x %o% he.3B3$he_3B3_n)[,,1]))

sf.3B3.tl <- lapply(sf.3B3.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3B3.tle, "sf.3B3.tle.xlsx") # Mnogo traje...

vars <- names(sf.3B3_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3B3.tl[[i]], file = paste("sf.3B3", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 3B4gi & 3B4gii-Laying hens & Broilers
#+ include = FALSE

sf.3B4gi_gii <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3B4gi_gii.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3B4gi_gii <- sf.3B4gi_gii %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3B4gi_gii%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + (k+DL) + inverse(TEMP) + SLP
#

he.3B4gi_gii <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3B4gi_gii = (TEMP+30) * (DL+0.5)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3B4gi_gii))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3B4gi_gii = he_sig) %>%
  dplyr::mutate(he_3B4gi_gii_n = he_sig/sum(he_sig))%>%
  select(times, he_3B4gi_gii, he_3B4gi_gii_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3B4gi_gii, aes(x = times, y = he_3B4gi_gii)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3B4gi_gii = (TEMP+30) * (DL+0.5)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3B4gi_gii$he_3B4gi_gii), max(he.3B4gi_gii$he_3B4gi_gii), sum(he.3B4gi_gii$he_3B4gi_gii))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3B4gi_gii$sumF <- sum(he.3B4gi_gii$he_3B4gi_gii)
he.3B4gi_gii %<>% 
  dplyr::mutate(NOx_3B4gi_gii = (t.3B4gi_gii$NOx/t.3B4gi_gii$sumF)*he_3B4gi_gii, 
                NOx_3B4gi_gii_p = (NOx_3B4gi_gii/sum(NOx_3B4gi_gii))*100,
                SO2_3B4gi_gii = (t.3B4gi_gii$SO2/t.3B4gi_gii$sumF)*he_3B4gi_gii, 
                SO2_3B4gi_gii_p = (SO2_3B4gi_gii/sum(SO2_3B4gi_gii))*100,
                PM10_3B4gi_gii = (t.3B4gi_gii$PM10/t.3B4gi_gii$sumF)*he_3B4gi_gii, 
                PM10_3B4gi_gii_p = (PM10_3B4gi_gii/sum(PM10_3B4gi_gii))*100,
                PM2.5_3B4gi_gii = (t.3B4gi_gii$PM2.5/t.3B4gi_gii$sumF)*he_3B4gi_gii, 
                PM2.5_3B4gi_gii_p = (PM2.5_3B4gi_gii/sum(PM2.5_3B4gi_gii))*100,
                NMVOC_3B4gi_gii = (t.3B4gi_gii$NMVOC/t.3B4gi_gii$sumF)*he_3B4gi_gii, 
                NMVOC_3B4gi_gii_p = (NMVOC_3B4gi_gii/sum(NMVOC_3B4gi_gii))*100,
                NH3_3B4gi_gii = (t.3B4gi_gii$NH3/t.3B4gi_gii$sumF)*he_3B4gi_gii, 
                NH3_3B4gi_gii_p = (NH3_3B4gi_gii/sum(NH3_3B4gi_gii))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3B4gi_gii_p, SO2_3B4gi_gii_p, PM10_3B4gi_gii_p, PM2.5_3B4gi_gii_p, NMVOC_3B4gi_gii_p, NH3_3B4gi_gii_p) %>%
  rename(`3B4gi_gii_NOx` = NOx_3B4gi_gii_p,
         `3B4gi_gii_SO2` = SO2_3B4gi_gii_p,
         `3B4gi_gii_PM10` = PM10_3B4gi_gii_p,
         `3B4gi_gii_PM2.5` = PM2.5_3B4gi_gii_p,
         `3B4gi_gii_NMVOC` = NMVOC_3B4gi_gii_p,
         `3B4gi_gii_NH3` = NH3_3B4gi_gii_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3B4gi_gii$`3B4gi_gii_NOx`), sum(he.3B4gi_gii$`3B4gi_gii_SO2`), sum(he.3B4gi_gii$`3B4gi_gii_PM10`), sum(he.3B4gi_gii$`3B4gi_gii_PM2.5`), sum(he.3B4gi_gii$`3B4gi_gii_NMVOC`), sum(he.3B4gi_gii$`3B4gi_gii_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#'
#'
#'
sf.3B4gi_gii_df <- sf.3B4gi_gii %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3B4gi_gii.tl <- lapply(sf.3B4gi_gii_df[,-1], function(x) t((x %o% he.3B4gi_gii$he_3B4gi_gii_n)[,,1]))

sf.3B4gi_gii.tl <- lapply(sf.3B4gi_gii.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3B4gi_gii.tle, "sf.3B4gi_gii.tle.xlsx") # Mnogo traje...

vars <- names(sf.3B4gi_gii_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3B4gi_gii.tl[[i]], file = paste("sf.3B4gi_gii", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 3B1a-Dairy cattle
#+ include = FALSE

sf.3B1a <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3B1a.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3B1a <- sf.3B1a %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3B1a%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + (k+DL) + inverse(TEMP) + SLP
#

he.3B1a <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3B1a = (TEMP+30) * (DL+0.5)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3B1a))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3B1a = he_sig) %>%
  dplyr::mutate(he_3B1a_n = he_sig/sum(he_sig))%>%
  select(times, he_3B1a, he_3B1a_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3B1a, aes(x = times, y = he_3B1a)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3B1a = (TEMP+30) * (DL+0.5)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3B1a$he_3B1a), max(he.3B1a$he_3B1a), sum(he.3B1a$he_3B1a))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3B1a$sumF <- sum(he.3B1a$he_3B1a)
he.3B1a %<>% 
  dplyr::mutate(NOx_3B1a = (t.3B1a$NOx/t.3B1a$sumF)*he_3B1a, 
                NOx_3B1a_p = (NOx_3B1a/sum(NOx_3B1a))*100,
                SO2_3B1a = (t.3B1a$SO2/t.3B1a$sumF)*he_3B1a, 
                SO2_3B1a_p = (SO2_3B1a/sum(SO2_3B1a))*100,
                PM10_3B1a = (t.3B1a$PM10/t.3B1a$sumF)*he_3B1a, 
                PM10_3B1a_p = (PM10_3B1a/sum(PM10_3B1a))*100,
                PM2.5_3B1a = (t.3B1a$PM2.5/t.3B1a$sumF)*he_3B1a, 
                PM2.5_3B1a_p = (PM2.5_3B1a/sum(PM2.5_3B1a))*100,
                NMVOC_3B1a = (t.3B1a$NMVOC/t.3B1a$sumF)*he_3B1a, 
                NMVOC_3B1a_p = (NMVOC_3B1a/sum(NMVOC_3B1a))*100,
                NH3_3B1a = (t.3B1a$NH3/t.3B1a$sumF)*he_3B1a, 
                NH3_3B1a_p = (NH3_3B1a/sum(NH3_3B1a))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3B1a_p, SO2_3B1a_p, PM10_3B1a_p, PM2.5_3B1a_p, NMVOC_3B1a_p, NH3_3B1a_p) %>%
  rename(`3B1a_NOx` = NOx_3B1a_p,
         `3B1a_SO2` = SO2_3B1a_p,
         `3B1a_PM10` = PM10_3B1a_p,
         `3B1a_PM2.5` = PM2.5_3B1a_p,
         `3B1a_NMVOC` = NMVOC_3B1a_p,
         `3B1a_NH3` = NH3_3B1a_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3B1a$`3B1a_NOx`), sum(he.3B1a$`3B1a_SO2`), sum(he.3B1a$`3B1a_PM10`), sum(he.3B1a$`3B1a_PM2.5`), sum(he.3B1a$`3B1a_NMVOC`), sum(he.3B1a$`3B1a_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
sf.3B1a_df <- sf.3B1a %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3B1a.tl <- lapply(sf.3B1a_df[,-1], function(x) t((x %o% he.3B1a$he_3B1a_n)[,,1]))

sf.3B1a.tl <- lapply(sf.3B1a.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids))%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3B1a.tle, "sf.3B1a.tle.xlsx") # Mnogo traje...

vars <- names(sf.3B1a_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3B1a.tl[[i]], file = paste("sf.3B1a", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 3B1b-Non-dairy cattle 
#+ include = FALSE

sf.3B1b <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3B1b.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3B1b <- sf.3B1b %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3B1b%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + (k+DL) + inverse(TEMP) + SLP
#

he.3B1b <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3B1b = (TEMP+30) * (DL+0.5)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3B1b))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3B1b = he_sig) %>%
  dplyr::mutate(he_3B1b_n = he_sig/sum(he_sig))%>%
  select(times, he_3B1b, he_3B1b_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3B1b, aes(x = times, y = he_3B1b)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3B1b = (TEMP+30) * (DL+0.5)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3B1b$he_3B1b), max(he.3B1b$he_3B1b), sum(he.3B1b$he_3B1b))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3B1b$sumF <- sum(he.3B1b$he_3B1b)
he.3B1b %<>% 
  dplyr::mutate(NOx_3B1b = (t.3B1b$NOx/t.3B1b$sumF)*he_3B1b, 
                NOx_3B1b_p = (NOx_3B1b/sum(NOx_3B1b))*100,
                SO2_3B1b = (t.3B1b$SO2/t.3B1b$sumF)*he_3B1b, 
                SO2_3B1b_p = (SO2_3B1b/sum(SO2_3B1b))*100,
                PM10_3B1b = (t.3B1b$PM10/t.3B1b$sumF)*he_3B1b, 
                PM10_3B1b_p = (PM10_3B1b/sum(PM10_3B1b))*100,
                PM2.5_3B1b = (t.3B1b$PM2.5/t.3B1b$sumF)*he_3B1b, 
                PM2.5_3B1b_p = (PM2.5_3B1b/sum(PM2.5_3B1b))*100,
                NMVOC_3B1b = (t.3B1b$NMVOC/t.3B1b$sumF)*he_3B1b, 
                NMVOC_3B1b_p = (NMVOC_3B1b/sum(NMVOC_3B1b))*100,
                NH3_3B1b = (t.3B1b$NH3/t.3B1b$sumF)*he_3B1b, 
                NH3_3B1b_p = (NH3_3B1b/sum(NH3_3B1b))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3B1b_p, SO2_3B1b_p, PM10_3B1b_p, PM2.5_3B1b_p, NMVOC_3B1b_p, NH3_3B1b_p) %>%
  rename(`3B1b_NOx` = NOx_3B1b_p,
         `3B1b_SO2` = SO2_3B1b_p,
         `3B1b_PM10` = PM10_3B1b_p,
         `3B1b_PM2.5` = PM2.5_3B1b_p,
         `3B1b_NMVOC` = NMVOC_3B1b_p,
         `3B1b_NH3` = NH3_3B1b_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3B1b$`3B1b_NOx`), sum(he.3B1b$`3B1b_SO2`), sum(he.3B1b$`3B1b_PM10`), sum(he.3B1b$`3B1b_PM2.5`), sum(he.3B1b$`3B1b_NMVOC`), sum(he.3B1b$`3B1b_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.3B1b_df <- sf.3B1b %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3B1b.tl <- lapply(sf.3B1b_df[,-1], function(x) t((x %o% he.3B1b$he_3B1b_n)[,,1]))

sf.3B1b.tl <- lapply(sf.3B1b.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3B1b.tle, "sf.3B1b.tle.xlsx") # Mnogo traje...

vars <- names(sf.3B1b_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3B1b.tl[[i]], file = paste("sf.3B1b", paste(vars[i],"csv", sep = "."), sep = "_"))
}


#'
#'
#'
#'
#'
#'
#' ## 3B2-Sheep
#+ include = FALSE

sf.3B2 <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3B2.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3B2 <- sf.3B2 %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3B2%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + (k+DL) + inverse(TEMP) + SLP
#

he.3B2 <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3B2 = (TEMP+30) * (DL+0.5)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3B2))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3B2 = he_sig) %>%
  dplyr::mutate(he_3B2_n = he_sig/sum(he_sig))%>%
  select(times, he_3B2, he_3B2_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3B2, aes(x = times, y = he_3B2)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3B2 = (TEMP+30) * (DL+0.5)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3B2$he_3B2), max(he.3B2$he_3B2), sum(he.3B2$he_3B2))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3B2$sumF <- sum(he.3B2$he_3B2)
he.3B2 %<>% 
  dplyr::mutate(NOx_3B2 = (t.3B2$NOx/t.3B2$sumF)*he_3B2, 
                NOx_3B2_p = (NOx_3B2/sum(NOx_3B2))*100,
                SO2_3B2 = (t.3B2$SO2/t.3B2$sumF)*he_3B2, 
                SO2_3B2_p = (SO2_3B2/sum(SO2_3B2))*100,
                PM10_3B2 = (t.3B2$PM10/t.3B2$sumF)*he_3B2, 
                PM10_3B2_p = (PM10_3B2/sum(PM10_3B2))*100,
                PM2.5_3B2 = (t.3B2$PM2.5/t.3B2$sumF)*he_3B2, 
                PM2.5_3B2_p = (PM2.5_3B2/sum(PM2.5_3B2))*100,
                NMVOC_3B2 = (t.3B2$NMVOC/t.3B2$sumF)*he_3B2, 
                NMVOC_3B2_p = (NMVOC_3B2/sum(NMVOC_3B2))*100,
                NH3_3B2 = (t.3B2$NH3/t.3B2$sumF)*he_3B2, 
                NH3_3B2_p = (NH3_3B2/sum(NH3_3B2))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3B2_p, SO2_3B2_p, PM10_3B2_p, PM2.5_3B2_p, NMVOC_3B2_p, NH3_3B2_p) %>%
  rename(`3B2_NOx` = NOx_3B2_p,
         `3B2_SO2` = SO2_3B2_p,
         `3B2_PM10` = PM10_3B2_p,
         `3B2_PM2.5` = PM2.5_3B2_p,
         `3B2_NMVOC` = NMVOC_3B2_p,
         `3B2_NH3` = NH3_3B2_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3B2$`3B2_NOx`), sum(he.3B2$`3B2_SO2`), sum(he.3B2$`3B2_PM10`), sum(he.3B2$`3B2_PM2.5`), sum(he.3B2$`3B2_NMVOC`), sum(he.3B2$`3B2_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.3B2_df <- sf.3B2 %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3B2.tl <- lapply(sf.3B2_df[,-1], function(x) t((x %o% he.3B2$he_3B2_n)[,,1]))

sf.3B2.tl <- lapply(sf.3B2.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3B2.tle, "sf.3B2.tle.xlsx") # Mnogo traje...

vars <- names(sf.3B2_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3B2.tl[[i]], file = paste("sf.3B2", paste(vars[i],"csv", sep = "."), sep = "_"))
}


#'
#'
#'
#'
#'
#'
#' ## 3B4d-Goats
#+ include = FALSE


sf.3B4d <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3B4d.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3B4d <- sf.3B4d %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3B4d%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + (k+DL) + inverse(TEMP) + SLP
#

he.3B4d <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3B4d = (TEMP+30) * (DL+0.5)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3B4d))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3B4d = he_sig) %>%
  dplyr::mutate(he_3B4d_n = he_sig/sum(he_sig))%>%
  select(times, he_3B4d, he_3B4d_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3B4d, aes(x = times, y = he_3B4d)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3B4d = (TEMP+30) * (DL+0.5)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3B4d$he_3B4d), max(he.3B4d$he_3B4d), sum(he.3B4d$he_3B4d))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3B4d$sumF <- sum(he.3B4d$he_3B4d)
he.3B4d %<>% 
  dplyr::mutate(NOx_3B4d = (t.3B4d$NOx/t.3B4d$sumF)*he_3B4d, 
                NOx_3B4d_p = (NOx_3B4d/sum(NOx_3B4d))*100,
                SO2_3B4d = (t.3B4d$SO2/t.3B4d$sumF)*he_3B4d, 
                SO2_3B4d_p = (SO2_3B4d/sum(SO2_3B4d))*100,
                PM10_3B4d = (t.3B4d$PM10/t.3B4d$sumF)*he_3B4d, 
                PM10_3B4d_p = (PM10_3B4d/sum(PM10_3B4d))*100,
                PM2.5_3B4d = (t.3B4d$PM2.5/t.3B4d$sumF)*he_3B4d, 
                PM2.5_3B4d_p = (PM2.5_3B4d/sum(PM2.5_3B4d))*100,
                NMVOC_3B4d = (t.3B4d$NMVOC/t.3B4d$sumF)*he_3B4d, 
                NMVOC_3B4d_p = (NMVOC_3B4d/sum(NMVOC_3B4d))*100,
                NH3_3B4d = (t.3B4d$NH3/t.3B4d$sumF)*he_3B4d, 
                NH3_3B4d_p = (NH3_3B4d/sum(NH3_3B4d))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3B4d_p, SO2_3B4d_p, PM10_3B4d_p, PM2.5_3B4d_p, NMVOC_3B4d_p, NH3_3B4d_p) %>%
  rename(`3B4d_NOx` = NOx_3B4d_p,
         `3B4d_SO2` = SO2_3B4d_p,
         `3B4d_PM10` = PM10_3B4d_p,
         `3B4d_PM2.5` = PM2.5_3B4d_p,
         `3B4d_NMVOC` = NMVOC_3B4d_p,
         `3B4d_NH3` = NH3_3B4d_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3B4d$`3B4d_NOx`), sum(he.3B4d$`3B4d_SO2`), sum(he.3B4d$`3B4d_PM10`), sum(he.3B4d$`3B4d_PM2.5`), sum(he.3B4d$`3B4d_NMVOC`), sum(he.3B4d$`3B4d_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
sf.3B4d_df <- sf.3B4d %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3B4d.tl <- lapply(sf.3B4d_df[,-1], function(x) t((x %o% he.3B4d$he_3B4d_n)[,,1]))

sf.3B4d.tl <- lapply(sf.3B4d.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3B4d.tle, "sf.3B4d.tle.xlsx") # Mnogo traje...

vars <- names(sf.3B4d_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3B4d.tl[[i]], file = paste("sf.3B4d", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 3B4e-Horses 
#+ include = FALSE

sf.3B4e <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3B4e.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3B4e <- sf.3B4e %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3B4e%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + (k+DL) + inverse(TEMP) + SLP
#

he.3B4e <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3B4e = (TEMP+30) * (DL+0.5)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3B4e))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3B4e = he_sig) %>%
  dplyr::mutate(he_3B4e_n = he_sig/sum(he_sig))%>%
  select(times, he_3B4e, he_3B4e_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3B4e, aes(x = times, y = he_3B4e)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3B4e = (TEMP+30) * (DL+0.5)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3B4e$he_3B4e), max(he.3B4e$he_3B4e), sum(he.3B4e$he_3B4e))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3B4e$sumF <- sum(he.3B4e$he_3B4e)
he.3B4e %<>% 
  dplyr::mutate(NOx_3B4e = (t.3B4e$NOx/t.3B4e$sumF)*he_3B4e, 
                NOx_3B4e_p = (NOx_3B4e/sum(NOx_3B4e))*100,
                SO2_3B4e = (t.3B4e$SO2/t.3B4e$sumF)*he_3B4e, 
                SO2_3B4e_p = (SO2_3B4e/sum(SO2_3B4e))*100,
                PM10_3B4e = (t.3B4e$PM10/t.3B4e$sumF)*he_3B4e, 
                PM10_3B4e_p = (PM10_3B4e/sum(PM10_3B4e))*100,
                PM2.5_3B4e = (t.3B4e$PM2.5/t.3B4e$sumF)*he_3B4e, 
                PM2.5_3B4e_p = (PM2.5_3B4e/sum(PM2.5_3B4e))*100,
                NMVOC_3B4e = (t.3B4e$NMVOC/t.3B4e$sumF)*he_3B4e, 
                NMVOC_3B4e_p = (NMVOC_3B4e/sum(NMVOC_3B4e))*100,
                NH3_3B4e = (t.3B4e$NH3/t.3B4e$sumF)*he_3B4e, 
                NH3_3B4e_p = (NH3_3B4e/sum(NH3_3B4e))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3B4e_p, SO2_3B4e_p, PM10_3B4e_p, PM2.5_3B4e_p, NMVOC_3B4e_p, NH3_3B4e_p) %>%
  rename(`3B4e_NOx` = NOx_3B4e_p,
         `3B4e_SO2` = SO2_3B4e_p,
         `3B4e_PM10` = PM10_3B4e_p,
         `3B4e_PM2.5` = PM2.5_3B4e_p,
         `3B4e_NMVOC` = NMVOC_3B4e_p,
         `3B4e_NH3` = NH3_3B4e_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3B4e$`3B4e_NOx`), sum(he.3B4e$`3B4e_SO2`), sum(he.3B4e$`3B4e_PM10`), sum(he.3B4e$`3B4e_PM2.5`), sum(he.3B4e$`3B4e_NMVOC`), sum(he.3B4e$`3B4e_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.3B4e_df <- sf.3B4e %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3B4e.tl <- lapply(sf.3B4e_df[,-1], function(x) t((x %o% he.3B4e$he_3B4e_n)[,,1]))

sf.3B4e.tl <- lapply(sf.3B4e.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3B4e.tle, "sf.3B4e.tle.xlsx") # Mnogo traje...

vars <- names(sf.3B4e_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3B4e.tl[[i]], file = paste("sf.3B4e", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#' ## 3B4giii-Turkeys 
#+ include = FALSE


sf.3B4giii <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3B4giii.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3B4giii <- sf.3B4giii %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3B4giii%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + (k+DL) + inverse(TEMP) + SLP
#

he.3B4giii <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3B4giii = (TEMP+30) * (DL+0.5)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3B4giii))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3B4giii = he_sig) %>%
  dplyr::mutate(he_3B4giii_n = he_sig/sum(he_sig))%>%
  select(times, he_3B4giii, he_3B4giii_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3B4giii, aes(x = times, y = he_3B4giii)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3B4giii = (TEMP+30) * (DL+0.5)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3B4giii$he_3B4giii), max(he.3B4giii$he_3B4giii), sum(he.3B4giii$he_3B4giii))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3B4giii$sumF <- sum(he.3B4giii$he_3B4giii)
he.3B4giii %<>% 
  dplyr::mutate(NOx_3B4giii = (t.3B4giii$NOx/t.3B4giii$sumF)*he_3B4giii, 
                NOx_3B4giii_p = (NOx_3B4giii/sum(NOx_3B4giii))*100,
                SO2_3B4giii = (t.3B4giii$SO2/t.3B4giii$sumF)*he_3B4giii, 
                SO2_3B4giii_p = (SO2_3B4giii/sum(SO2_3B4giii))*100,
                PM10_3B4giii = (t.3B4giii$PM10/t.3B4giii$sumF)*he_3B4giii, 
                PM10_3B4giii_p = (PM10_3B4giii/sum(PM10_3B4giii))*100,
                PM2.5_3B4giii = (t.3B4giii$PM2.5/t.3B4giii$sumF)*he_3B4giii, 
                PM2.5_3B4giii_p = (PM2.5_3B4giii/sum(PM2.5_3B4giii))*100,
                NMVOC_3B4giii = (t.3B4giii$NMVOC/t.3B4giii$sumF)*he_3B4giii, 
                NMVOC_3B4giii_p = (NMVOC_3B4giii/sum(NMVOC_3B4giii))*100,
                NH3_3B4giii = (t.3B4giii$NH3/t.3B4giii$sumF)*he_3B4giii, 
                NH3_3B4giii_p = (NH3_3B4giii/sum(NH3_3B4giii))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3B4giii_p, SO2_3B4giii_p, PM10_3B4giii_p, PM2.5_3B4giii_p, NMVOC_3B4giii_p, NH3_3B4giii_p) %>%
  rename(`3B4giii_NOx` = NOx_3B4giii_p,
         `3B4giii_SO2` = SO2_3B4giii_p,
         `3B4giii_PM10` = PM10_3B4giii_p,
         `3B4giii_PM2.5` = PM2.5_3B4giii_p,
         `3B4giii_NMVOC` = NMVOC_3B4giii_p,
         `3B4giii_NH3` = NH3_3B4giii_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3B4giii$`3B4giii_NOx`), sum(he.3B4giii$`3B4giii_SO2`), sum(he.3B4giii$`3B4giii_PM10`), sum(he.3B4giii$`3B4giii_PM2.5`), sum(he.3B4giii$`3B4giii_NMVOC`), sum(he.3B4giii$`3B4giii_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.3B4giii_df <- sf.3B4giii %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3B4giii.tl <- lapply(sf.3B4giii_df[,-1], function(x) t((x %o% he.3B4giii$he_3B4giii_n)[,,1]))

sf.3B4giii.tl <- lapply(sf.3B4giii.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3B4giii.tle, "sf.3B4giii.tle.xlsx") # Mnogo traje...

vars <- names(sf.3B4giii_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3B4giii.tl[[i]], file = paste("sf.3B4giii", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 3B4giv-Other poultry
#+ include = FALSE


sf.3B4giv <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3B4giv.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3B4giv <- sf.3B4giv %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3B4giv%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + (k+DL) + inverse(TEMP) + SLP
#

he.3B4giv <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3B4giv = (TEMP+30) * (DL+0.5)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3B4giv))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3B4giv = he_sig) %>%
  dplyr::mutate(he_3B4giv_n = he_sig/sum(he_sig))%>%
  select(times, he_3B4giv, he_3B4giv_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3B4giv, aes(x = times, y = he_3B4giv)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3B4giv = (TEMP+30) * (DL+0.5)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3B4giv$he_3B4giv), max(he.3B4giv$he_3B4giv), sum(he.3B4giv$he_3B4giv))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3B4giv$sumF <- sum(he.3B4giv$he_3B4giv)
he.3B4giv %<>% 
  dplyr::mutate(NOx_3B4giv = (t.3B4giv$NOx/t.3B4giv$sumF)*he_3B4giv, 
                NOx_3B4giv_p = (NOx_3B4giv/sum(NOx_3B4giv))*100,
                SO2_3B4giv = (t.3B4giv$SO2/t.3B4giv$sumF)*he_3B4giv, 
                SO2_3B4giv_p = (SO2_3B4giv/sum(SO2_3B4giv))*100,
                PM10_3B4giv = (t.3B4giv$PM10/t.3B4giv$sumF)*he_3B4giv, 
                PM10_3B4giv_p = (PM10_3B4giv/sum(PM10_3B4giv))*100,
                PM2.5_3B4giv = (t.3B4giv$PM2.5/t.3B4giv$sumF)*he_3B4giv, 
                PM2.5_3B4giv_p = (PM2.5_3B4giv/sum(PM2.5_3B4giv))*100,
                NMVOC_3B4giv = (t.3B4giv$NMVOC/t.3B4giv$sumF)*he_3B4giv, 
                NMVOC_3B4giv_p = (NMVOC_3B4giv/sum(NMVOC_3B4giv))*100,
                NH3_3B4giv = (t.3B4giv$NH3/t.3B4giv$sumF)*he_3B4giv, 
                NH3_3B4giv_p = (NH3_3B4giv/sum(NH3_3B4giv))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3B4giv_p, SO2_3B4giv_p, PM10_3B4giv_p, PM2.5_3B4giv_p, NMVOC_3B4giv_p, NH3_3B4giv_p) %>%
  rename(`3B4giv_NOx` = NOx_3B4giv_p,
         `3B4giv_SO2` = SO2_3B4giv_p,
         `3B4giv_PM10` = PM10_3B4giv_p,
         `3B4giv_PM2.5` = PM2.5_3B4giv_p,
         `3B4giv_NMVOC` = NMVOC_3B4giv_p,
         `3B4giv_NH3` = NH3_3B4giv_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3B4giv$`3B4giv_NOx`), sum(he.3B4giv$`3B4giv_SO2`), sum(he.3B4giv$`3B4giv_PM10`), sum(he.3B4giv$`3B4giv_PM2.5`), sum(he.3B4giv$`3B4giv_NMVOC`), sum(he.3B4giv$`3B4giv_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.3B4giv_df <- sf.3B4giv %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3B4giv.tl <- lapply(sf.3B4giv_df[,-1], function(x) t((x %o% he.3B4giv$he_3B4giv_n)[,,1]))

sf.3B4giv.tl <- lapply(sf.3B4giv.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3B4giv.tle, "sf.3B4giv.tle.xlsx") # Mnogo traje...

vars <- names(sf.3B4giv_df)[-1]

for(i in 1:length(vars)){
  fwrite(sf.3B4giv.tl[[i]], file = paste("sf.3B4giv", paste(vars[i],"csv", sep = "."), sep = "_"))
}


#'
#'
#'
#'
#'
#'
#' ## 3Da1 - Inorganic N-fertilizers (includes also urea application)
#+ include = FALSE

sf.3Da1 <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3Da1.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3Da1 <- sf.3Da1 %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3Da1%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + (k+DL) + !PH + (k+SAAG) + inverse(TEMP) + SLP
#

t1 <- seq.POSIXt(from = ymd_h("2015-03-01 00"),
                 to   = ymd_h("2015-05-01 24"),
                 by   = dhours(1)) 
t2 <- seq.POSIXt(from = ymd_h("2015-09-01 00"),
                 to   = ymd_h("2015-11-01 24"),
                 by   = dhours(1)) 

activity.df$indSAAG.fert[activity.df$times %in% t1] <- dublogistic.f(L=as.numeric(t1), inflection1=as.numeric(quantile(t1, probs = 0.1)), inflection2=as.numeric(quantile(t1, probs = 0.7)), slope1=0.000006, slope2=0.000002, max.sel=1, minsel.upper=0, plot=F)$selectivity
activity.df$indSAAG.fert[activity.df$times %in% t2] <- dublogistic.f(L=as.numeric(t2), inflection1=as.numeric(quantile(t2, probs = 0.2)), inflection2=as.numeric(quantile(t2, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=0.5, minsel.upper=0, plot=F)$selectivity 

activity.df %<>%
  dplyr::mutate(indSAAG.fert = tidyr::replace_na(indSAAG.fert, 0)) %>%
  dplyr::rename(SAAG.F = indSAAG.fert)



he.3Da1 <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3Da1 = ((DL+0.5)) * (TEMP+30) * PH2 * (SAAG.F+0.1)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3Da1))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3Da1 = he_sig) %>%
  dplyr::mutate(he_3Da1_n = he_sig/sum(he_sig))%>%
  select(times, he_3Da1, he_3Da1_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3Da1, aes(x = times, y = he_3Da1)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # stat_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_3Da1 = ((DL+0.5)) * (TEMP+30) * SLP * PH2 * (0.5+SAAG.f)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3Da1$he_3Da1), max(he.3Da1$he_3Da1), sum(he.3Da1$he_3Da1))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3Da1$sumF <- sum(he.3Da1$he_3Da1)
he.3Da1 %<>% 
  dplyr::mutate(NOx_3Da1 = (t.3Da1$NOx/t.3Da1$sumF)*he_3Da1, 
                NOx_3Da1_p = (NOx_3Da1/sum(NOx_3Da1))*100,
                SO2_3Da1 = (t.3Da1$SO2/t.3Da1$sumF)*he_3Da1, 
                SO2_3Da1_p = (SO2_3Da1/sum(SO2_3Da1))*100,
                PM10_3Da1 = (t.3Da1$PM10/t.3Da1$sumF)*he_3Da1, 
                PM10_3Da1_p = (PM10_3Da1/sum(PM10_3Da1))*100,
                PM2.5_3Da1 = (t.3Da1$PM2.5/t.3Da1$sumF)*he_3Da1, 
                PM2.5_3Da1_p = (PM2.5_3Da1/sum(PM2.5_3Da1))*100,
                NMVOC_3Da1 = (t.3Da1$NMVOC/t.3Da1$sumF)*he_3Da1, 
                NMVOC_3Da1_p = (NMVOC_3Da1/sum(NMVOC_3Da1))*100,
                NH3_3Da1 = (t.3Da1$NH3/t.3Da1$sumF)*he_3Da1, 
                NH3_3Da1_p = (NH3_3Da1/sum(NH3_3Da1))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3Da1_p, SO2_3Da1_p, PM10_3Da1_p, PM2.5_3Da1_p, NMVOC_3Da1_p, NH3_3Da1_p) %>%
  rename(`3Da1_NOx` = NOx_3Da1_p,
         `3Da1_SO2` = SO2_3Da1_p,
         `3Da1_PM10` = PM10_3Da1_p,
         `3Da1_PM2.5` = PM2.5_3Da1_p,
         `3Da1_NMVOC` = NMVOC_3Da1_p,
         `3Da1_NH3` = NH3_3Da1_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3Da1$`3Da1_NOx`), sum(he.3Da1$`3Da1_SO2`), sum(he.3Da1$`3Da1_PM10`), sum(he.3Da1$`3Da1_PM2.5`), sum(he.3Da1$`3Da1_NMVOC`), sum(he.3Da1$`3Da1_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
sf.3Da1_df <- sf.3Da1 %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3Da1.tl <- lapply(sf.3Da1_df[,-1], function(x) t((x %o% he.3Da1$he_3Da1_n)[,,1]))

sf.3Da1.tl <- lapply(sf.3Da1.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3Da1.tle, "sf.3Da1.tle.xlsx") # Mnogo traje...

vars <- names(sf.3Da1_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3Da1.tl[[i]], file = paste("sf.3Da1", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 3Da2a - Animal manure applied to soils 
#+ include = FALSE


sf.3Da2a <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3Da2a.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3Da2a <- sf.3Da2a %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3Da2a%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + (k+DL) + !PH + (k+SAAG) + inverse(TEMP) + SLP
#

he.3Da2a <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3Da2a = ((DL+0.5)) * (TEMP+30) * PH2 * (SAAG.F+0.1)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3Da2a))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3Da2a = he_sig) %>%
  dplyr::mutate(he_3Da2a_n = he_sig/sum(he_sig))%>%
  select(times, he_3Da2a, he_3Da2a_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3Da2a, aes(x = times, y = he_3Da2a)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3Da2a = ((DL+0.5)) * (TEMP+30) * SLP * PH2 * (0.5+SAAG.f)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3Da2a$he_3Da2a), max(he.3Da2a$he_3Da2a), sum(he.3Da2a$he_3Da2a))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3Da2a$sumF <- sum(he.3Da2a$he_3Da2a)
he.3Da2a %<>% 
  dplyr::mutate(NOx_3Da2a = (t.3Da2a$NOx/t.3Da2a$sumF)*he_3Da2a, 
                NOx_3Da2a_p = (NOx_3Da2a/sum(NOx_3Da2a))*100,
                SO2_3Da2a = (t.3Da2a$SO2/t.3Da2a$sumF)*he_3Da2a, 
                SO2_3Da2a_p = (SO2_3Da2a/sum(SO2_3Da2a))*100,
                PM10_3Da2a = (t.3Da2a$PM10/t.3Da2a$sumF)*he_3Da2a, 
                PM10_3Da2a_p = (PM10_3Da2a/sum(PM10_3Da2a))*100,
                PM2.5_3Da2a = (t.3Da2a$PM2.5/t.3Da2a$sumF)*he_3Da2a, 
                PM2.5_3Da2a_p = (PM2.5_3Da2a/sum(PM2.5_3Da2a))*100,
                NMVOC_3Da2a = (t.3Da2a$NMVOC/t.3Da2a$sumF)*he_3Da2a, 
                NMVOC_3Da2a_p = (NMVOC_3Da2a/sum(NMVOC_3Da2a))*100,
                NH3_3Da2a = (t.3Da2a$NH3/t.3Da2a$sumF)*he_3Da2a, 
                NH3_3Da2a_p = (NH3_3Da2a/sum(NH3_3Da2a))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3Da2a_p, SO2_3Da2a_p, PM10_3Da2a_p, PM2.5_3Da2a_p, NMVOC_3Da2a_p, NH3_3Da2a_p) %>%
  rename(`3Da2a_NOx` = NOx_3Da2a_p,
         `3Da2a_SO2` = SO2_3Da2a_p,
         `3Da2a_PM10` = PM10_3Da2a_p,
         `3Da2a_PM2.5` = PM2.5_3Da2a_p,
         `3Da2a_NMVOC` = NMVOC_3Da2a_p,
         `3Da2a_NH3` = NH3_3Da2a_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3Da2a$`3Da2a_NOx`), sum(he.3Da2a$`3Da2a_SO2`), sum(he.3Da2a$`3Da2a_PM10`), sum(he.3Da2a$`3Da2a_PM2.5`), sum(he.3Da2a$`3Da2a_NMVOC`), sum(he.3Da2a$`3Da2a_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
sf.3Da2a_df <- sf.3Da2a %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3Da2a.tl <- lapply(sf.3Da2a_df[,-1], function(x) t((x %o% he.3Da2a$he_3Da2a_n)[,,1]))

sf.3Da2a.tl <- lapply(sf.3Da2a.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3Da2a.tle, "sf.3Da2a.tle.xlsx") # Mnogo traje...

vars <- names(sf.3Da2a_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3Da2a.tl[[i]], file = paste("sf.3Da2a", paste(vars[i],"csv", sep = "."), sep = "_"))
}


#'
#'
#'
#'
#'
#'
#' ## 3Da3 - Urine and dung deposited by grazing animals
#+ include = FALSE

sf.3Da3 <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3Da3.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3Da3 <- sf.3Da3 %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3Da3%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = (k+DL) + (k+SA) + inverse(TEMP) + SLP
#


he.3Da3 <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3Da3 = (DL+0.5) * (TEMP+30) * SLP ) %>%
  #dplyr::mutate(he_sig = sigmoid(scale(he_3Da3))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  #dplyr::mutate(he_sign = 100*he_sig/sum(he_sig)) %>% # OVO je normalizovano i prebaceno u procente
  dplyr::mutate(he_sig = sigmoid(scale(he_3Da3))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3Da3 = he_sig) %>%
  dplyr::mutate(he_3Da3_n = he_sig/sum(he_sig))%>%
  dplyr::select(times, he_3Da3, he_3Da3_n)
# * (0.5+SA)
time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3Da3, aes(x = times, y = he_3Da3)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3Da3 = (DL+0.5) * (TEMP+30) * SLP")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3Da3$he_3Da3), max(he.3Da3$he_3Da3), sum(he.3Da3$he_3Da3))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3Da3$sumF <- sum(he.3Da3$he_3Da3)
he.3Da3 %<>% 
  dplyr::mutate(NOx_3Da3 = (t.3Da3$NOx/t.3Da3$sumF)*he_3Da3, 
                NOx_3Da3_p = (NOx_3Da3/sum(NOx_3Da3))*100,
                SO2_3Da3 = (t.3Da3$SO2/t.3Da3$sumF)*he_3Da3, 
                SO2_3Da3_p = (SO2_3Da3/sum(SO2_3Da3))*100,
                PM10_3Da3 = (t.3Da3$PM10/t.3Da3$sumF)*he_3Da3, 
                PM10_3Da3_p = (PM10_3Da3/sum(PM10_3Da3))*100,
                PM2.5_3Da3 = (t.3Da3$PM2.5/t.3Da3$sumF)*he_3Da3, 
                PM2.5_3Da3_p = (PM2.5_3Da3/sum(PM2.5_3Da3))*100,
                NMVOC_3Da3 = (t.3Da3$NMVOC/t.3Da3$sumF)*he_3Da3, 
                NMVOC_3Da3_p = (NMVOC_3Da3/sum(NMVOC_3Da3))*100,
                NH3_3Da3 = (t.3Da3$NH3/t.3Da3$sumF)*he_3Da3, 
                NH3_3Da3_p = (NH3_3Da3/sum(NH3_3Da3))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3Da3_p, SO2_3Da3_p, PM10_3Da3_p, PM2.5_3Da3_p, NMVOC_3Da3_p, NH3_3Da3_p) %>%
  rename(`3Da3_NOx` = NOx_3Da3_p,
         `3Da3_SO2` = SO2_3Da3_p,
         `3Da3_PM10` = PM10_3Da3_p,
         `3Da3_PM2.5` = PM2.5_3Da3_p,
         `3Da3_NMVOC` = NMVOC_3Da3_p,
         `3Da3_NH3` = NH3_3Da3_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3Da3$`3Da3_NOx`), sum(he.3Da3$`3Da3_SO2`), sum(he.3Da3$`3Da3_PM10`), sum(he.3Da3$`3Da3_PM2.5`), sum(he.3Da3$`3Da3_NMVOC`), sum(he.3Da3$`3Da3_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.3Da3_df <- sf.3Da3 %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3Da3.tl <- lapply(sf.3Da3_df[,-1], function(x) t((x %o% he.3Da3$he_3Da3_n)[,,1]))

sf.3Da3.tl <- lapply(sf.3Da3.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3Da3.tle, "sf.3Da3.tle.xlsx") # Mnogo traje...

vars <- names(sf.3Da3_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3Da3.tl[[i]], file = paste("sf.3Da3", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 3Dc - Farm-level agricultural operations including storage, handling and transport of agricultural products
#+ include = FALSE


sf.3Dc <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3Dc.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3Dc <- sf.3Dc %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3Dc%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + (k+DL) + (k+SAAG) + inverse(TEMP) + SLP
#

he.3Dc <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3Dc = ((DL+0.5)) * (TEMP+30) * SLP * (0.5+SAAG.fl)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3Dc))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3Dc = he_sig) %>%
  dplyr::mutate(he_3Dc_n = he_sig/sum(he_sig))%>%
  select(times, he_3Dc, he_3Dc_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3Dc, aes(x = times, y = he_3Dc)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3Dc = ((DL+0.5)) * (TEMP+30) * SLP * (0.5+SAAG.fl)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3Dc$he_3Dc), max(he.3Dc$he_3Dc), sum(he.3Dc$he_3Dc))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3Dc$sumF <- sum(he.3Dc$he_3Dc)
he.3Dc %<>% 
  dplyr::mutate(NOx_3Dc = (t.3Dc$NOx/t.3Dc$sumF)*he_3Dc, 
                NOx_3Dc_p = (NOx_3Dc/sum(NOx_3Dc))*100,
                SO2_3Dc = (t.3Dc$SO2/t.3Dc$sumF)*he_3Dc, 
                SO2_3Dc_p = (SO2_3Dc/sum(SO2_3Dc))*100,
                PM10_3Dc = (t.3Dc$PM10/t.3Dc$sumF)*he_3Dc, 
                PM10_3Dc_p = (PM10_3Dc/sum(PM10_3Dc))*100,
                PM2.5_3Dc = (t.3Dc$PM2.5/t.3Dc$sumF)*he_3Dc, 
                PM2.5_3Dc_p = (PM2.5_3Dc/sum(PM2.5_3Dc))*100,
                NMVOC_3Dc = (t.3Dc$NMVOC/t.3Dc$sumF)*he_3Dc, 
                NMVOC_3Dc_p = (NMVOC_3Dc/sum(NMVOC_3Dc))*100,
                NH3_3Dc = (t.3Dc$NH3/t.3Dc$sumF)*he_3Dc, 
                NH3_3Dc_p = (NH3_3Dc/sum(NH3_3Dc))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3Dc_p, SO2_3Dc_p, PM10_3Dc_p, PM2.5_3Dc_p, NMVOC_3Dc_p, NH3_3Dc_p) %>%
  rename(`3Dc_NOx` = NOx_3Dc_p,
         `3Dc_SO2` = SO2_3Dc_p,
         `3Dc_PM10` = PM10_3Dc_p,
         `3Dc_PM2.5` = PM2.5_3Dc_p,
         `3Dc_NMVOC` = NMVOC_3Dc_p,
         `3Dc_NH3` = NH3_3Dc_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3Dc$`3Dc_NOx`), sum(he.3Dc$`3Dc_SO2`), sum(he.3Dc$`3Dc_PM10`), sum(he.3Dc$`3Dc_PM2.5`), sum(he.3Dc$`3Dc_NMVOC`), sum(he.3Dc$`3Dc_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#'
sf.3Dc_df <- sf.3Dc %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3Dc.tl <- lapply(sf.3Dc_df[,-1], function(x) t((x %o% he.3Dc$he_3Dc_n)[,,1]))

sf.3Dc.tl <- lapply(sf.3Dc.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3Dc.tle, "sf.3Dc.tle.xlsx") # Mnogo traje...

vars <- names(sf.3Dc_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3Dc.tl[[i]], file = paste("sf.3Dc", paste(vars[i],"csv", sep = "."), sep = "_"))
}
#'
#'
#'
#'
#'
#'
#' ## 3De - Cultivated crops  
#+ include = FALSE

sf.3De <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3De.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3De <- sf.3De %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3De%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + (k+DL) + !PH + (k+SAAG) + inverse(TEMP) + SLP
#

he.3De <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_3De = ((DL+0.5)) * (TEMP+30) * SLP * PH2 * (0.5+SAAG.f) * (0.5+SAAG.fl)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3De))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3De = he_sig) %>%
  dplyr::mutate(he_3De_n = he_sig/sum(he_sig))%>%
  select(times, he_3De, he_3De_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3De, aes(x = times, y = he_3De)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_3De = ((DL+0.5)) * (TEMP+30) * SLP * PH2 * (0.5+SAAG.f) * (0.5+SAAG.fl)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3De$he_3De), max(he.3De$he_3De), sum(he.3De$he_3De))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3De$sumF <- sum(he.3De$he_3De)
he.3De %<>% 
  dplyr::mutate(NOx_3De = (t.3De$NOx/t.3De$sumF)*he_3De, 
                NOx_3De_p = (NOx_3De/sum(NOx_3De))*100,
                SO2_3De = (t.3De$SO2/t.3De$sumF)*he_3De, 
                SO2_3De_p = (SO2_3De/sum(SO2_3De))*100,
                PM10_3De = (t.3De$PM10/t.3De$sumF)*he_3De, 
                PM10_3De_p = (PM10_3De/sum(PM10_3De))*100,
                PM2.5_3De = (t.3De$PM2.5/t.3De$sumF)*he_3De, 
                PM2.5_3De_p = (PM2.5_3De/sum(PM2.5_3De))*100,
                NMVOC_3De = (t.3De$NMVOC/t.3De$sumF)*he_3De, 
                NMVOC_3De_p = (NMVOC_3De/sum(NMVOC_3De))*100,
                NH3_3De = (t.3De$NH3/t.3De$sumF)*he_3De, 
                NH3_3De_p = (NH3_3De/sum(NH3_3De))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3De_p, SO2_3De_p, PM10_3De_p, PM2.5_3De_p, NMVOC_3De_p, NH3_3De_p) %>%
  rename(`3De_NOx` = NOx_3De_p,
         `3De_SO2` = SO2_3De_p,
         `3De_PM10` = PM10_3De_p,
         `3De_PM2.5` = PM2.5_3De_p,
         `3De_NMVOC` = NMVOC_3De_p,
         `3De_NH3` = NH3_3De_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3De$`3De_NOx`), sum(he.3De$`3De_SO2`), sum(he.3De$`3De_PM10`), sum(he.3De$`3De_PM2.5`), sum(he.3De$`3De_NMVOC`), sum(he.3De$`3De_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.3De_df <- sf.3De %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3De.tl <- lapply(sf.3De_df[,-1], function(x) t((x %o% he.3De$he_3De_n)[,,1]))

sf.3De.tl <- lapply(sf.3De.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3De.tle, "sf.3De.tle.xlsx") # Mnogo traje...

vars <- names(sf.3De_df)[-1]

for(i in 1:length(vars)){
 fwrite(sf.3De.tl[[i]], file = paste("sf.3De", paste(vars[i],"csv", sep = "."), sep = "_"))
}


#'
#'
#'
#'
#'
#'
#' ## 3F - Field burning of agricultural residues   
#+ include = FALSE

sf.3F <- st_read("D:/R_projects/Spatialization/Products/3 - Agriculture/3F.gpkg")


tBurning <- seq.POSIXt(from = ymd_h("2015-10-01 00"),
                  to   = ymd_h("2015-11-30 23"),
                  by   = dhours(1)) 

activity.df$indBurning[activity.df$times %in% tBurning] <- TRUE 
activity.df$indBurning[!(activity.df$times %in% tBurning)] <- FALSE 

activity.df %<>% 
  dplyr::mutate(AS = sin(((2*pi)/4)*(indBurning)))


p <- ggplot(activity.df, aes(x = times, y = AS, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-10-01 00"),
                       to   = ymd_h("2015-10-30 23"),
                       by   = dhours(1))


p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)






# during dry days - veca temperatura
# less air humidity - As humidity increases pressure decreases, znaci sa vecim vazdusnim pritiskom
# without rain or no rain foreseen - bez padavina 

load("Hourly_emissions/Data/Perticipation/ogimet_serbia08_prcp.rda")
ogimet_serbia

prcp <- as.data.frame(ogimet_serbia)
prcp$time1 <- as.Date.POSIXct(prcp$date)

time_per_day <- seq(from = ymd('2015-01-01'),
                    to   = ymd('2015-12-31'),
                    by   = 'day')



prcp_2015 <- prcp %>% dplyr::filter(date %in% time_per_day)

prcp_2015 %<>% 
  tidyr::drop_na(prcp) %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(meanPrcp = mean(prcp))



prcp_2015 <- prcp_2015[rep(seq.int(1,nrow(prcp_2015)), each = 24),]

activity.df$PRCP <- prcp_2015$meanPrcp

p <- ggplot(activity.df, aes(x = times, y = PRCP)) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw() +
  geom_smooth(formula =  ~ TEMP, colour = "orange")

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-02-28 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)


# 


#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.3F <- sf.3F %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  dplyr::select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.3F%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + (k+DL) + !PH + (k+SAAG) + inverse(TEMP) + SLP
#

unique(activity.df$AS)
activity.df %<>% dplyr::mutate(TEMP = TEMP + 10, TEMP = case_when(TEMP >= 35 ~ 0, 
                                               TEMP < 35 ~ TEMP))

?scale

activity.df %<>% dplyr::mutate(WDWW = -1)

he.3F <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(AS2 = (sin(((2*pi)/12)*(AS)))) %>%
  dplyr::mutate(he_3F = AS * (TEMP) * SLP * (PRCP*(-1)+23.238)* (DL)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_3F))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_3F = he_sig) %>%
  dplyr::mutate(he_3F_n = he_sig/sum(he_sig))%>%
  dplyr::select(times, he_3F, he_3F_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-10-01 00"),
                       to   = ymd_h("2015-10-31 24"),
                       by   = dhours(1)) 

#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.3F, aes(x = times, y = he_3F)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  # geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  #labs( caption = "he_3F = ((DL+0.5)) * (TEMP+30) * SLP * PH2 * (0.5+SAAG.f) * (0.5+SAAG.fl)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.3F$he_3F), max(he.3F$he_3F), sum(he.3F$he_3F))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.3F$sumF <- sum(he.3F$he_3F)
he.3F %<>% 
  dplyr::mutate(NOx_3F = (t.3F$NOx/t.3F$sumF)*he_3F, 
                NOx_3F_p = (NOx_3F/sum(NOx_3F))*100,
                SO2_3F = (t.3F$SO2/t.3F$sumF)*he_3F, 
                SO2_3F_p = (SO2_3F/sum(SO2_3F))*100,
                PM10_3F = (t.3F$PM10/t.3F$sumF)*he_3F, 
                PM10_3F_p = (PM10_3F/sum(PM10_3F))*100,
                PM2.5_3F = (t.3F$PM2.5/t.3F$sumF)*he_3F, 
                PM2.5_3F_p = (PM2.5_3F/sum(PM2.5_3F))*100,
                NMVOC_3F = (t.3F$NMVOC/t.3F$sumF)*he_3F, 
                NMVOC_3F_p = (NMVOC_3F/sum(NMVOC_3F))*100,
                NH3_3F = (t.3F$NH3/t.3F$sumF)*he_3F, 
                NH3_3F_p = (NH3_3F/sum(NH3_3F))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_3F_p, SO2_3F_p, PM10_3F_p, PM2.5_3F_p, NMVOC_3F_p, NH3_3F_p) %>%
  rename(`3F_NOx` = NOx_3F_p,
         `3F_SO2` = SO2_3F_p,
         `3F_PM10` = PM10_3F_p,
         `3F_PM2.5` = PM2.5_3F_p,
         `3F_NMVOC` = NMVOC_3F_p,
         `3F_NH3` = NH3_3F_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.3F$`3F_NOx`), sum(he.3F$`3F_SO2`), sum(he.3F$`3F_PM10`), sum(he.3F$`3F_PM2.5`), sum(he.3F$`3F_NMVOC`), sum(he.3F$`3F_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.3F_df <- sf.3F %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.3F.tl <- lapply(sf.3F_df[,-1], function(x) t((x %o% he.3F$he_3F_n)[,,1]))

sf.3F.tl <- lapply(sf.3F.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything())%>% dplyr::rename_at(vars(-1), ~ paste(ids)))

# writexl::write_xlsx(sf.3F.tle, "sf.3F.tle.xlsx") # Mnogo traje...

vars <- names(sf.3F_df)[-1]

for(i in 1:length(vars)){
  fwrite(sf.3F.tl[[i]], file = paste("sf.3F", paste(vars[i],"csv", sep = "."), sep = "_"))
}










####  DODATI za filed burning --- dodato

temporalProfile_Agriculture <- activity.df$times %>% cbind(he.3B1a[,1:6], 
                                                          he.3B1b[,1:6], 
                                                          he.3B2[,1:6], 
                                                          he.3B3[,1:6], 
                                                          he.3B4d[,1:6], 
                                                          he.3B4e[,1:6], 
                                                          he.3B4gi_gii[,1:6], 
                                                          he.3B4giii[,1:6], 
                                                          he.3B4giv[,1:6], 
                                                          he.3Da1[,1:6], 
                                                          he.3Da2a[,1:6], 
                                                          he.3Da3[,1:6], 
                                                          he.3Dc[,1:6], 
                                                          he.3De[,1:6],
                                                          he.3F[,1:6]) %>% 
 as.data.frame()

writexl::write_xlsx(temporalProfile_Agriculture, path = 'Hourly_emissions/Products/TemporalProfile_Agriculture.xlsx')


