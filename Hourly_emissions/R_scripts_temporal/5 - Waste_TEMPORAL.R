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
#' # 5 - Waste
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
#' ## 5A-Solid waste disposal on land
#+ include = FALSE

sf.5A <- st_read("D:/R_projects/Spatialization/Products/5 - Waste/5A.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.5A <- sf.5A %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.5A%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + !PH 
#

he.5A <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_5A = (WDWW * (WT0816+0.5)) * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_5A))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_5A = he_sig) %>%
  dplyr::mutate(he_5A_n = he_sig/sum(he_sig))%>%
  select(times, he_5A, he_5A_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.5A, aes(x = times, y = he_5A)) +
  geom_point(size = 0.1) +
  geom_line(colour = "lightblue") + 
  #geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_5A = (WDWW * (WT0816+0.5)) * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.5A$he_5A), max(he.5A$he_5A), sum(he.5A$he_5A))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.5A$sumF <- sum(he.5A$he_5A)
he.5A %<>% 
  dplyr::mutate(NOx_5A = (t.5A$NOx/t.5A$sumF)*he_5A, 
                NOx_5A_p = (NOx_5A/sum(NOx_5A))*100,
                SO2_5A = (t.5A$SO2/t.5A$sumF)*he_5A, 
                SO2_5A_p = (SO2_5A/sum(SO2_5A))*100,
                PM10_5A = (t.5A$PM10/t.5A$sumF)*he_5A, 
                PM10_5A_p = (PM10_5A/sum(PM10_5A))*100,
                PM2.5_5A = (t.5A$PM2.5/t.5A$sumF)*he_5A, 
                PM2.5_5A_p = (PM2.5_5A/sum(PM2.5_5A))*100,
                NMVOC_5A = (t.5A$NMVOC/t.5A$sumF)*he_5A, 
                NMVOC_5A_p = (NMVOC_5A/sum(NMVOC_5A))*100,
                NH3_5A = (t.5A$NH3/t.5A$sumF)*he_5A, 
                NH3_5A_p = (NH3_5A/sum(NH3_5A))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_5A_p, SO2_5A_p, PM10_5A_p, PM2.5_5A_p, NMVOC_5A_p, NH3_5A_p) %>%
  rename(`5A_NOx` = NOx_5A_p,
         `5A_SO2` = SO2_5A_p,
         `5A_PM10` = PM10_5A_p,
         `5A_PM2.5` = PM2.5_5A_p,
         `5A_NMVOC` = NMVOC_5A_p,
         `5A_NH3` = NH3_5A_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.5A$`5A_NOx`), sum(he.5A$`5A_SO2`), sum(he.5A$`5A_PM10`), sum(he.5A$`5A_PM2.5`), sum(he.5A$`5A_NMVOC`), sum(he.5A$`5A_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.5A_df <- sf.5A %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.5A.tl <- lapply(sf.5A_df[,-1], function(x) t((x %o% he.5A$he_5A_n)[,,1]))

sf.5A.tl <- lapply(sf.5A.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))

# writexl::write_xlsx(sf.5A.tle, "sf.5A.tle.xlsx") # Mnogo traje...

vars <- names(sf.5A_df)[-1]

for(i in 1:length(vars)){
  fwrite(sf.5A.tl[[i]], file = paste("sf.5A", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 5C1bv-Cremation
#+ include = FALSE

sf.5C1bv <- st_read("D:/R_projects/Spatialization/Products/5 - Waste/5C1bv.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.5C1bv <- sf.5C1bv %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.5C1bv%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0816 + !PH 
#

he.5C1bv <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_5C1bv = (WDWW * (WT0816+0.5)) * PH2)  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_5C1bv))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_5C1bv = he_sig) %>%
  dplyr::mutate(he_5C1bv_n = he_sig/sum(he_sig))%>%
  select(times, he_5C1bv, he_5C1bv_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.5C1bv, aes(x = times, y = he_5C1bv)) +
  geom_point(size = 0.1) +
  geom_line(colour = "lightblue") + 
  #geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_5C1bv = (WDWW * (WT0816+0.5)) * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.5C1bv$he_5C1bv), max(he.5C1bv$he_5C1bv), sum(he.5C1bv$he_5C1bv))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.5C1bv$sumF <- sum(he.5C1bv$he_5C1bv)
he.5C1bv %<>% 
  dplyr::mutate(NOx_5C1bv = (t.5C1bv$NOx/t.5C1bv$sumF)*he_5C1bv, 
                NOx_5C1bv_p = (NOx_5C1bv/sum(NOx_5C1bv))*100,
                SO2_5C1bv = (t.5C1bv$SO2/t.5C1bv$sumF)*he_5C1bv, 
                SO2_5C1bv_p = (SO2_5C1bv/sum(SO2_5C1bv))*100,
                PM10_5C1bv = (t.5C1bv$PM10/t.5C1bv$sumF)*he_5C1bv, 
                PM10_5C1bv_p = (PM10_5C1bv/sum(PM10_5C1bv))*100,
                PM2.5_5C1bv = (t.5C1bv$PM2.5/t.5C1bv$sumF)*he_5C1bv, 
                PM2.5_5C1bv_p = (PM2.5_5C1bv/sum(PM2.5_5C1bv))*100,
                NMVOC_5C1bv = (t.5C1bv$NMVOC/t.5C1bv$sumF)*he_5C1bv, 
                NMVOC_5C1bv_p = (NMVOC_5C1bv/sum(NMVOC_5C1bv))*100,
                NH3_5C1bv = (t.5C1bv$NH3/t.5C1bv$sumF)*he_5C1bv, 
                NH3_5C1bv_p = (NH3_5C1bv/sum(NH3_5C1bv))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_5C1bv_p, SO2_5C1bv_p, PM10_5C1bv_p, PM2.5_5C1bv_p, NMVOC_5C1bv_p, NH3_5C1bv_p) %>%
  rename(`5C1bv_NOx` = NOx_5C1bv_p,
         `5C1bv_SO2` = SO2_5C1bv_p,
         `5C1bv_PM10` = PM10_5C1bv_p,
         `5C1bv_PM2.5` = PM2.5_5C1bv_p,
         `5C1bv_NMVOC` = NMVOC_5C1bv_p,
         `5C1bv_NH3` = NH3_5C1bv_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.5C1bv$`5C1bv_NOx`), sum(he.5C1bv$`5C1bv_SO2`), sum(he.5C1bv$`5C1bv_PM10`), sum(he.5C1bv$`5C1bv_PM2.5`), sum(he.5C1bv$`5C1bv_NMVOC`), sum(he.5C1bv$`5C1bv_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.5C1bv_df <- sf.5C1bv %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.5C1bv.tl <- lapply(sf.5C1bv_df[,-1], function(x) t((x %o% he.5C1bv$he_5C1bv_n)[,,1]))

sf.5C1bv.tl <- lapply(sf.5C1bv.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))

# writexl::write_xlsx(sf.5C1bv.tle, "sf.5C1bv.tle.xlsx") # Mnogo traje...

vars <- names(sf.5C1bv_df)[-1]

for(i in 1:length(vars)){
  fwrite(sf.5C1bv.tl[[i]], file = paste("sf.5C1bv", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 5D1-Domestic wastewater handling
#+ include = FALSE


sf.5D1 <- st_read("D:/R_projects/Spatialization/Products/5 - Waste/5D1.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.5D1 <- sf.5D1 %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.5D1%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + inverse(TEMP) + SLP
#

he.5D1 <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_5D1 = (WDWW * (WT0024+0.5)) * (TEMP*(-1)+30) * SLP )  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_5D1))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_5D1 = he_sig) %>%
  dplyr::mutate(he_5D1_n = he_sig/sum(he_sig))%>%
  select(times, he_5D1, he_5D1_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.5D1, aes(x = times, y = he_5D1)) +
  geom_point(size = 0.1) +
  geom_line(colour = "lightblue") + 
  #geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_5D1 = (WDWW * (WT0024+0.5)) * (TEMP*(-1)+30) * SLP")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.5D1$he_5D1), max(he.5D1$he_5D1), sum(he.5D1$he_5D1))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.5D1$sumF <- sum(he.5D1$he_5D1)
he.5D1 %<>% 
  dplyr::mutate(NOx_5D1 = (t.5D1$NOx/t.5D1$sumF)*he_5D1, 
                NOx_5D1_p = (NOx_5D1/sum(NOx_5D1))*100,
                SO2_5D1 = (t.5D1$SO2/t.5D1$sumF)*he_5D1, 
                SO2_5D1_p = (SO2_5D1/sum(SO2_5D1))*100,
                PM10_5D1 = (t.5D1$PM10/t.5D1$sumF)*he_5D1, 
                PM10_5D1_p = (PM10_5D1/sum(PM10_5D1))*100,
                PM2.5_5D1 = (t.5D1$PM2.5/t.5D1$sumF)*he_5D1, 
                PM2.5_5D1_p = (PM2.5_5D1/sum(PM2.5_5D1))*100,
                NMVOC_5D1 = (t.5D1$NMVOC/t.5D1$sumF)*he_5D1, 
                NMVOC_5D1_p = (NMVOC_5D1/sum(NMVOC_5D1))*100,
                NH3_5D1 = (t.5D1$NH3/t.5D1$sumF)*he_5D1, 
                NH3_5D1_p = (NH3_5D1/sum(NH3_5D1))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_5D1_p, SO2_5D1_p, PM10_5D1_p, PM2.5_5D1_p, NMVOC_5D1_p, NH3_5D1_p) %>%
  rename(`5D1_NOx` = NOx_5D1_p,
         `5D1_SO2` = SO2_5D1_p,
         `5D1_PM10` = PM10_5D1_p,
         `5D1_PM2.5` = PM2.5_5D1_p,
         `5D1_NMVOC` = NMVOC_5D1_p,
         `5D1_NH3` = NH3_5D1_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.5D1$`5D1_NOx`), sum(he.5D1$`5D1_SO2`), sum(he.5D1$`5D1_PM10`), sum(he.5D1$`5D1_PM2.5`), sum(he.5D1$`5D1_NMVOC`), sum(he.5D1$`5D1_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.5D1_df <- sf.5D1 %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.5D1.tl <- lapply(sf.5D1_df[,-1], function(x) t((x %o% he.5D1$he_5D1_n)[,,1]))

sf.5D1.tl <- lapply(sf.5D1.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))

# writexl::write_xlsx(sf.5D1.tle, "sf.5D1.tle.xlsx") # Mnogo traje...

vars <- names(sf.5D1_df)[-1]

for(i in 1:length(vars)){
  fwrite(sf.5D1.tl[[i]], file = paste("sf.5D1", paste(vars[i],"csv", sep = "."), sep = "_"))
}

#'
#'
#'
#'
#'
#'
#' ## 5D2-Industrial wastewater handling
#+ include = FALSE

sf.5D2 <- st_read("D:/R_projects/Spatialization/Products/5 - Waste/5D2.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.5D2 <- sf.5D2 %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.5D2%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
# ---  HE = WDWW + WT0024 + inverse(TEMP) + SLP
#

he.5D2 <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((2*pi)/12)*(!PH1))+1)) %>%
  dplyr::mutate(WE1 = dplyr::case_when(WE == TRUE ~ 1,
                                       WE == FALSE ~ 0)) %>%
  dplyr::mutate(WE2 = (sin(((2*pi)/12)*(!WE1))+0.5)) %>%
  dplyr::mutate(he_5D2 = (WDWW * (WT0024+0.5)) * (TEMP*(-1)+30) * SLP )  %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_5D2))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_5D2 = he_sig) %>%
  dplyr::mutate(he_5D2_n = he_sig/sum(he_sig))%>%
  select(times, he_5D2, he_5D2_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-06 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.5D2, aes(x = times, y = he_5D2)) +
  geom_point(size = 0.1) +
  geom_line(colour = "lightblue") + 
  #geom_smooth() +
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_5D2 = (WDWW * (WT0024+0.5)) * (TEMP*(-1)+30) * SLP")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.5D2$he_5D2), max(he.5D2$he_5D2), sum(he.5D2$he_5D2))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.5D2$sumF <- sum(he.5D2$he_5D2)
he.5D2 %<>% 
  dplyr::mutate(NOx_5D2 = (t.5D2$NOx/t.5D2$sumF)*he_5D2, 
                NOx_5D2_p = (NOx_5D2/sum(NOx_5D2))*100,
                SO2_5D2 = (t.5D2$SO2/t.5D2$sumF)*he_5D2, 
                SO2_5D2_p = (SO2_5D2/sum(SO2_5D2))*100,
                PM10_5D2 = (t.5D2$PM10/t.5D2$sumF)*he_5D2, 
                PM10_5D2_p = (PM10_5D2/sum(PM10_5D2))*100,
                PM2.5_5D2 = (t.5D2$PM2.5/t.5D2$sumF)*he_5D2, 
                PM2.5_5D2_p = (PM2.5_5D2/sum(PM2.5_5D2))*100,
                NMVOC_5D2 = (t.5D2$NMVOC/t.5D2$sumF)*he_5D2, 
                NMVOC_5D2_p = (NMVOC_5D2/sum(NMVOC_5D2))*100,
                NH3_5D2 = (t.5D2$NH3/t.5D2$sumF)*he_5D2, 
                NH3_5D2_p = (NH3_5D2/sum(NH3_5D2))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_5D2_p, SO2_5D2_p, PM10_5D2_p, PM2.5_5D2_p, NMVOC_5D2_p, NH3_5D2_p) %>%
  rename(`5D2_NOx` = NOx_5D2_p,
         `5D2_SO2` = SO2_5D2_p,
         `5D2_PM10` = PM10_5D2_p,
         `5D2_PM2.5` = PM2.5_5D2_p,
         `5D2_NMVOC` = NMVOC_5D2_p,
         `5D2_NH3` = NH3_5D2_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.5D2$`5D2_NOx`), sum(he.5D2$`5D2_SO2`), sum(he.5D2$`5D2_PM10`), sum(he.5D2$`5D2_PM2.5`), sum(he.5D2$`5D2_NMVOC`), sum(he.5D2$`5D2_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
sf.5D2_df <- sf.5D2 %>% st_drop_geometry() #%>% dplyr::select(NOx)

sf.5D2.tl <- lapply(sf.5D2_df[,-1], function(x) t((x %o% he.5D2$he_5D2_n)[,,1]))

sf.5D2.tl <- lapply(sf.5D2.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))

# writexl::write_xlsx(sf.5D2.tle, "sf.5D2.tle.xlsx") # Mnogo traje...

vars <- names(sf.5D2_df)[-1]

for(i in 1:length(vars)){
  fwrite(sf.5D2.tl[[i]], file = paste("sf.5D2", paste(vars[i],"csv", sep = "."), sep = "_"))
}


#  temporalProfile_Waste <- activity.df$times %>% cbind(he.5A[,1:6], 
#                                                             he.5C1bv[,1:6], 
#                                                             he.5D1[,1:6], 
#                                                             he.5D2[,1:6]) %>% 
#    as.data.frame()
#  
#  writexl::write_xlsx(temporalProfile_Waste, path = 'Hourly_emissions/Products/TemporalProfile_Waste.xlsx')






