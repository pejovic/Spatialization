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
#' # 1A1 - Energy industries
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

#'
#'
#+ include = FALSE

sigmoid = function(x) {
  1 / (1 + exp(-x))
}
#'
#'
#'
#'
#'
#'
#' ## 1A1a - Public electricity production
#+ include = FALSE
sf.1A1a_ep <- st_read("D:/R_projects/Spatialization/Products/1A1 - Energy/1A1a.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A1a_ep <- sf.1A1a_ep %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  dplyr::select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A1a_ep%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
#
# ---  HE = WDWW + WT0024 + WT0622 + !PH + k*SA + k*HS + TEMP + !RP
#

he.1A1a_ep <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(PH1))+0.5)) %>%
  dplyr::mutate(HS1 = dplyr::case_when(HS == TRUE ~ 1,
                                       HS == FALSE ~ 0)) %>%
  dplyr::mutate(HS2 = (sin(((2*pi)/12)*(HS1))+0.5)) %>%
  # dplyr::mutate(he_1A1a_ep = ((WT0622+0.5))  * (HS2) * RP2 * (PH2) * EP) %>%
  dplyr::mutate(he_1A1a_ep = EP) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1A1a_ep))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1A1a_ep_n = he_sig/sum(he_sig)) %>% # OVO je normalizovano i prebaceno u procente
  dplyr::mutate(he_1A1a_ep = he_sig) %>%
  dplyr::select(times, he_1A1a_ep, he_1A1a_ep_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-04-05 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A1a_ep, aes(x = times, y = he_1A1a_ep)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  #labs(caption = "he_1A1a_ep = EP")+
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", colour = "black")
  )


#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A1a_ep$he_1A1a_ep), max(he.1A1a_ep$he_1A1a_ep), sum(he.1A1a_ep$he_1A1a_ep))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A1a_ep$sumF <- sum(he.1A1a_ep$he_1A1a_ep)
he.1A1a_ep %<>% 
  dplyr::mutate(NOx_1A1a_ep = (t.1A1a_ep$NOx/t.1A1a_ep$sumF)*he_1A1a_ep, 
                NOx_1A1a_ep_p = (NOx_1A1a_ep/sum(NOx_1A1a_ep))*100,
                SO2_1A1a_ep = (t.1A1a_ep$SO2/t.1A1a_ep$sumF)*he_1A1a_ep, 
                SO2_1A1a_ep_p = (SO2_1A1a_ep/sum(SO2_1A1a_ep))*100,
                PM10_1A1a_ep = (t.1A1a_ep$PM10/t.1A1a_ep$sumF)*he_1A1a_ep, 
                PM10_1A1a_ep_p = (PM10_1A1a_ep/sum(PM10_1A1a_ep))*100,
                PM2.5_1A1a_ep = (t.1A1a_ep$PM2.5/t.1A1a_ep$sumF)*he_1A1a_ep, 
                PM2.5_1A1a_ep_p = (PM2.5_1A1a_ep/sum(PM2.5_1A1a_ep))*100,
                NMVOC_1A1a_ep = (t.1A1a_ep$NMVOC/t.1A1a_ep$sumF)*he_1A1a_ep, 
                NMVOC_1A1a_ep_p = (NMVOC_1A1a_ep/sum(NMVOC_1A1a_ep))*100,
                NH3_1A1a_ep = (t.1A1a_ep$NH3/t.1A1a_ep$sumF)*he_1A1a_ep, 
                NH3_1A1a_ep_p = (NH3_1A1a_ep/sum(NH3_1A1a_ep))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A1a_ep_p, SO2_1A1a_ep_p, PM10_1A1a_ep_p, PM2.5_1A1a_ep_p, NMVOC_1A1a_ep_p, NH3_1A1a_ep_p) %>%
  rename(`1A1a_ep_NOx` = NOx_1A1a_ep_p,
         `1A1a_ep_SO2` = SO2_1A1a_ep_p,
         `1A1a_ep_PM10` = PM10_1A1a_ep_p,
         `1A1a_ep_PM2.5` = PM2.5_1A1a_ep_p,
         `1A1a_ep_NMVOC` = NMVOC_1A1a_ep_p,
         `1A1a_ep_NH3` = NH3_1A1a_ep_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A1a_ep$`1A1a_ep_NOx`), sum(he.1A1a_ep$`1A1a_ep_SO2`), sum(he.1A1a_ep$`1A1a_ep_PM10`), sum(he.1A1a_ep$`1A1a_ep_PM2.5`), sum(he.1A1a_ep$`1A1a_ep_NMVOC`), sum(he.1A1a_ep$`1A1a_ep_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
#'
#+
# sf.1A1a_ep_df <- sf.1A1a_ep %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1A1a_ep.tl <- lapply(sf.1A1a_ep_df[,-1], function(x) t((x %o% he.1A1a_ep$he_1A1a_ep_n)[,,1]))
# 
# sf.1A1a_ep.tl <- lapply(sf.1A1a_ep.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.1A1a_ep.tle, "sf.1A1a_ep.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.1A1a_ep_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1A1a_ep.tl[[i]], file = paste("sf.1A1a_ep", paste(vars[i],"csv", sep = "."), sep = "_"))
# }




#'
#'
#'
#'
#'
#'
#' ## 1A1a - Public heat production
#+ include = FALSE
sf.1A1a_hp <- st_read("D:/R_projects/Spatialization/Products/1A1 - Energy/1A1a.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A1a_hp <- sf.1A1a_hp %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A1a_hp%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
#
# ---  HE = WDWW + WT0024 + WT0622 + !PH + k*SA + k*HS + TEMP + !RP
#

he.1A1a_hp <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((2*pi)/12)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(PH1))+0.5)) %>%
  dplyr::mutate(HS1 = dplyr::case_when(HS == TRUE ~ 1,
                                       HS == FALSE ~ 0)) %>%
  dplyr::mutate(HS2 = (sin(((2*pi)/12)*(HS1))+0.5)) %>%
  # dplyr::mutate(he_1A1a_hp = ((WT0622+0.5))  * (HS2) * RP2 * (PH2) * EP) %>%
  dplyr::mutate(he_1A1a_hp = (WT0622*HS2)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1A1a_hp))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1A1a_hp_n = he_sig/sum(he_sig)) %>% # OVO je normalizovano i prebaceno u procente
  dplyr::mutate(he_1A1a_hp = he_sig) %>%
  dplyr::select(times, he_1A1a_hp, he_1A1a_hp_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-04-05 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A1a_hp, aes(x = times, y = he_1A1a_hp)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs(caption = "he_1A1a_hp = ((WT0622+0.5))  * (HS2) * RP2 * (PH2) * EP)")+
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", colour = "black")
  )


#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A1a_hp$he_1A1a_hp), max(he.1A1a_hp$he_1A1a_hp), sum(he.1A1a_hp$he_1A1a_hp))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A1a_hp$sumF <- sum(he.1A1a_hp$he_1A1a_hp)
he.1A1a_hp %<>% 
  dplyr::mutate(NOx_1A1a_hp = (t.1A1a_hp$NOx/t.1A1a_hp$sumF)*he_1A1a_hp, 
                NOx_1A1a_hp_p = (NOx_1A1a_hp/sum(NOx_1A1a_hp))*100,
                SO2_1A1a_hp = (t.1A1a_hp$SO2/t.1A1a_hp$sumF)*he_1A1a_hp, 
                SO2_1A1a_hp_p = (SO2_1A1a_hp/sum(SO2_1A1a_hp))*100,
                PM10_1A1a_hp = (t.1A1a_hp$PM10/t.1A1a_hp$sumF)*he_1A1a_hp, 
                PM10_1A1a_hp_p = (PM10_1A1a_hp/sum(PM10_1A1a_hp))*100,
                PM2.5_1A1a_hp = (t.1A1a_hp$PM2.5/t.1A1a_hp$sumF)*he_1A1a_hp, 
                PM2.5_1A1a_hp_p = (PM2.5_1A1a_hp/sum(PM2.5_1A1a_hp))*100,
                NMVOC_1A1a_hp = (t.1A1a_hp$NMVOC/t.1A1a_hp$sumF)*he_1A1a_hp, 
                NMVOC_1A1a_hp_p = (NMVOC_1A1a_hp/sum(NMVOC_1A1a_hp))*100,
                NH3_1A1a_hp = (t.1A1a_hp$NH3/t.1A1a_hp$sumF)*he_1A1a_hp, 
                NH3_1A1a_hp_p = (NH3_1A1a_hp/sum(NH3_1A1a_hp))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A1a_hp_p, SO2_1A1a_hp_p, PM10_1A1a_hp_p, PM2.5_1A1a_hp_p, NMVOC_1A1a_hp_p, NH3_1A1a_hp_p) %>%
  rename(`1A1a_hp_NOx` = NOx_1A1a_hp_p,
         `1A1a_hp_SO2` = SO2_1A1a_hp_p,
         `1A1a_hp_PM10` = PM10_1A1a_hp_p,
         `1A1a_hp_PM2.5` = PM2.5_1A1a_hp_p,
         `1A1a_hp_NMVOC` = NMVOC_1A1a_hp_p,
         `1A1a_hp_NH3` = NH3_1A1a_hp_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A1a_hp$`1A1a_hp_NOx`), sum(he.1A1a_hp$`1A1a_hp_SO2`), sum(he.1A1a_hp$`1A1a_hp_PM10`), sum(he.1A1a_hp$`1A1a_hp_PM2.5`), sum(he.1A1a_hp$`1A1a_hp_NMVOC`), sum(he.1A1a_hp$`1A1a_hp_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )
#'
#'
#'
#'
#+
# sf.1A1a_hp_df <- sf.1A1a_hp %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1A1a_hp.tl <- lapply(sf.1A1a_hp_df[,-1], function(x) t((x %o% he.1A1a_hp$he_1A1a_hp_n)[,,1]))
# 
# sf.1A1a_hp.tl <- lapply(sf.1A1a_hp.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # writexl::write_xlsx(sf.1A1a_hp.tle, "sf.1A1a_hp.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.1A1a_hp_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1A1a_hp.tl[[i]], file = paste("sf.1A1a_hp", paste(vars[i],"csv", sep = "."), sep = "_"))
# }



#'
#'
#'
#'
#'
#' ## 1A1b - Refineries
#+ include = FALSE
sf.1A1b <- st_read("D:/R_projects/Spatialization/Products/1A1 - Energy/1A1b.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A1b <- sf.1A1b %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A1b%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
#
# ---  HE = WDWW + WT0024 + !RP
#
activity.df$WDWW <- 1
he.1A1b <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  # dplyr::mutate(he_1A1b =   RP2 * (TEMP+30)) %>%
  dplyr::mutate(he_1A1b =   WDWW) %>%
  #dplyr::mutate(he_sig = sigmoid(scale(he_1A1b))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  #dplyr::mutate(he_1A1b = he_sig) %>%
  #dplyr::mutate(he_1A1b_n = he_sig/sum(he_sig)) %>% # OVO je normalizovano i prebaceno u procente
  select(times, he_1A1b) # , he_1A1b_n

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A1b, aes(x = times, y = he_1A1b)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  #ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6) + 
  # labs( caption = "he_1A1b = (WT0024+0.5) * RP2 * (TEMP+30)")+
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", colour = "black")
  )

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A1b$he_1A1b), max(he.1A1b$he_1A1b), sum(he.1A1b$he_1A1b))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A1b$sumF <- sum(he.1A1b$he_1A1b)
he.1A1b %<>% 
  dplyr::mutate(NOx_1A1b = (t.1A1b$NOx/t.1A1b$sumF)*he_1A1b, 
                NOx_1A1b_p = (NOx_1A1b/sum(NOx_1A1b))*100,
                SO2_1A1b = (t.1A1b$SO2/t.1A1b$sumF)*he_1A1b, 
                SO2_1A1b_p = (SO2_1A1b/sum(SO2_1A1b))*100,
                PM10_1A1b = (t.1A1b$PM10/t.1A1b$sumF)*he_1A1b, 
                PM10_1A1b_p = (PM10_1A1b/sum(PM10_1A1b))*100,
                PM2.5_1A1b = (t.1A1b$PM2.5/t.1A1b$sumF)*he_1A1b, 
                PM2.5_1A1b_p = (PM2.5_1A1b/sum(PM2.5_1A1b))*100,
                NMVOC_1A1b = (t.1A1b$NMVOC/t.1A1b$sumF)*he_1A1b, 
                NMVOC_1A1b_p = (NMVOC_1A1b/sum(NMVOC_1A1b))*100,
                NH3_1A1b = (t.1A1b$NH3/t.1A1b$sumF)*he_1A1b, 
                NH3_1A1b_p = (NH3_1A1b/sum(NH3_1A1b))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A1b_p, SO2_1A1b_p, PM10_1A1b_p, PM2.5_1A1b_p, NMVOC_1A1b_p, NH3_1A1b_p) %>%
  rename(`1A1b_NOx` = NOx_1A1b_p,
         `1A1b_SO2` = SO2_1A1b_p,
         `1A1b_PM10` = PM10_1A1b_p,
         `1A1b_PM2.5` = PM2.5_1A1b_p,
         `1A1b_NMVOC` = NMVOC_1A1b_p,
         `1A1b_NH3` = NH3_1A1b_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A1b$`1A1b_NOx`), sum(he.1A1b$`1A1b_SO2`), sum(he.1A1b$`1A1b_PM10`), sum(he.1A1b$`1A1b_PM2.5`), sum(he.1A1b$`1A1b_NMVOC`), sum(he.1A1b$`1A1b_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )


#'
#+
# sf.1A1b_df <- sf.1A1b %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1A1b.tl <- lapply(sf.1A1b_df[,-1], function(x) t((x %o% he.1A1b$he_1A1b_n)[,,1]))
# 
# sf.1A1b.tl <- lapply(sf.1A1b.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # str(sf.1A1b.tl)
# 
# # writexl::write_xlsx(sf.1A1b.tle, "sf.1A1b.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.1A1b_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1A1b.tl[[i]], file = paste("sf.1A1b", paste(vars[i],"csv", sep = "."), sep = "_"))
# }

#'
#'
#'
#'
#' ## 1B2aiv - Fugitive emissions from liquid fuels: Refining, storage
#+ include = FALSE
sf.1B2aiv <- st_read("D:/R_projects/Spatialization/Products/1A1 - Energy/1B2aiv.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1B2aiv <- sf.1B2aiv %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1B2aiv%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
#
# ---  HE = WDWW + WT0024 + !RP
#

he.1B2aiv <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  # dplyr::mutate(he_1B2aiv = RP2 * (TEMP+30)) %>%
  dplyr::mutate(he_1B2aiv = WDWW) %>%
  #dplyr::mutate(he_sig = sigmoid(scale(he_1B2aiv))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  #dplyr::mutate(he_1B2aiv = he_sig) %>%
  #dplyr::mutate(he_1B2aiv_n = he_sig/sum(he_sig)) %>% # OVO je normalizovano i prebaceno u procente
  select(times, he_1B2aiv)#, he_1B2aiv_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1B2aiv, aes(x = times, y = he_1B2aiv)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  # ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "RP2 * (TEMP+30)")+
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", colour = "black")
  )

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1B2aiv$he_1B2aiv), max(he.1B2aiv$he_1B2aiv), sum(he.1B2aiv$he_1B2aiv))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1B2aiv$sumF <- sum(he.1B2aiv$he_1B2aiv)
he.1B2aiv %<>% 
  dplyr::mutate(NOx_1B2aiv = (t.1B2aiv$NOx/t.1B2aiv$sumF)*he_1B2aiv, 
                NOx_1B2aiv_p = (NOx_1B2aiv/sum(NOx_1B2aiv))*100,
                SO2_1B2aiv = (t.1B2aiv$SO2/t.1B2aiv$sumF)*he_1B2aiv, 
                SO2_1B2aiv_p = (SO2_1B2aiv/sum(SO2_1B2aiv))*100,
                PM10_1B2aiv = (t.1B2aiv$PM10/t.1B2aiv$sumF)*he_1B2aiv, 
                PM10_1B2aiv_p = (PM10_1B2aiv/sum(PM10_1B2aiv))*100,
                PM2.5_1B2aiv = (t.1B2aiv$PM2.5/t.1B2aiv$sumF)*he_1B2aiv, 
                PM2.5_1B2aiv_p = (PM2.5_1B2aiv/sum(PM2.5_1B2aiv))*100,
                NMVOC_1B2aiv = (t.1B2aiv$NMVOC/t.1B2aiv$sumF)*he_1B2aiv, 
                NMVOC_1B2aiv_p = (NMVOC_1B2aiv/sum(NMVOC_1B2aiv))*100,
                NH3_1B2aiv = (t.1B2aiv$NH3/t.1B2aiv$sumF)*he_1B2aiv, 
                NH3_1B2aiv_p = (NH3_1B2aiv/sum(NH3_1B2aiv))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1B2aiv_p, SO2_1B2aiv_p, PM10_1B2aiv_p, PM2.5_1B2aiv_p, NMVOC_1B2aiv_p, NH3_1B2aiv_p) %>%
  rename(`1B2aiv_NOx` = NOx_1B2aiv_p,
         `1B2aiv_SO2` = SO2_1B2aiv_p,
         `1B2aiv_PM10` = PM10_1B2aiv_p,
         `1B2aiv_PM2.5` = PM2.5_1B2aiv_p,
         `1B2aiv_NMVOC` = NMVOC_1B2aiv_p,
         `1B2aiv_NH3` = NH3_1B2aiv_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1B2aiv$`1B2aiv_NOx`), sum(he.1B2aiv$`1B2aiv_SO2`), sum(he.1B2aiv$`1B2aiv_PM10`), sum(he.1B2aiv$`1B2aiv_PM2.5`), sum(he.1B2aiv$`1B2aiv_NMVOC`), sum(he.1B2aiv$`1B2aiv_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#'
#+
# sf.1B2aiv_df <- sf.1B2aiv %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1B2aiv.tl <- lapply(sf.1B2aiv_df[,-1], function(x) t((x %o% he.1B2aiv$he_1B2aiv_n)[,,1]))
# 
# sf.1B2aiv.tl <- lapply(sf.1B2aiv.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # str(sf.1B2aiv.tl)
# 
# # writexl::write_xlsx(sf.1B2aiv.tle, "sf.1B2aiv.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.1B2aiv_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1B2aiv.tl[[i]], file = paste("sf.1B2aiv", paste(vars[i],"csv", sep = "."), sep = "_"))
# }
#'
#'
#'
#' ## 1B2c - Fugitive emissions: Venting and flaring
#+ include = FALSE
sf.1B2c <- st_read("D:/R_projects/Spatialization/Products/1A1 - Energy/1B2c.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1B2c <- sf.1B2c %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1B2c%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
#
# ---  HE = WDWW + WT0024 + !RP
#

he.1B2c <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  # dplyr::mutate(he_1B2c = RP2 * (TEMP + 30)) %>%
  dplyr::mutate(he_1B2c = WDWW) %>%
  #dplyr::mutate(he_sig = sigmoid(scale(he_1B2c))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  #dplyr::mutate(he_1B2c = he_sig) %>%
  #dplyr::mutate(he_1B2c_n = he_sig/sum(he_sig)) %>% # OVO je normalizovano i prebaceno u procente
  select(times, he_1B2c)#, he_1B2c_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1B2c, aes(x = times, y = he_1B2c)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  # ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_1B2c = RP2 * (TEMP+30)")+
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", colour = "black")
  )

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1B2c$he_1B2c), max(he.1B2c$he_1B2c), sum(he.1B2c$he_1B2c))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1B2c$sumF <- sum(he.1B2c$he_1B2c)
he.1B2c %<>% 
  dplyr::mutate(NOx_1B2c = (t.1B2c$NOx/t.1B2c$sumF)*he_1B2c, 
                NOx_1B2c_p = (NOx_1B2c/sum(NOx_1B2c))*100,
                SO2_1B2c = (t.1B2c$SO2/t.1B2c$sumF)*he_1B2c, 
                SO2_1B2c_p = (SO2_1B2c/sum(SO2_1B2c))*100,
                PM10_1B2c = (t.1B2c$PM10/t.1B2c$sumF)*he_1B2c, 
                PM10_1B2c_p = (PM10_1B2c/sum(PM10_1B2c))*100,
                PM2.5_1B2c = (t.1B2c$PM2.5/t.1B2c$sumF)*he_1B2c, 
                PM2.5_1B2c_p = (PM2.5_1B2c/sum(PM2.5_1B2c))*100,
                NMVOC_1B2c = (t.1B2c$NMVOC/t.1B2c$sumF)*he_1B2c, 
                NMVOC_1B2c_p = (NMVOC_1B2c/sum(NMVOC_1B2c))*100,
                NH3_1B2c = (t.1B2c$NH3/t.1B2c$sumF)*he_1B2c, 
                NH3_1B2c_p = (NH3_1B2c/sum(NH3_1B2c))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1B2c_p, SO2_1B2c_p, PM10_1B2c_p, PM2.5_1B2c_p, NMVOC_1B2c_p, NH3_1B2c_p) %>%
  rename(`1B2c_NOx` = NOx_1B2c_p,
         `1B2c_SO2` = SO2_1B2c_p,
         `1B2c_PM10` = PM10_1B2c_p,
         `1B2c_PM2.5` = PM2.5_1B2c_p,
         `1B2c_NMVOC` = NMVOC_1B2c_p,
         `1B2c_NH3` = NH3_1B2c_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1B2c$`1B2c_NOx`), sum(he.1B2c$`1B2c_SO2`), sum(he.1B2c$`1B2c_PM10`), sum(he.1B2c$`1B2c_PM2.5`), sum(he.1B2c$`1B2c_NMVOC`), sum(he.1B2c$`1B2c_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#+
# sf.1B2c_df <- sf.1B2c %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1B2c.tl <- lapply(sf.1B2c_df[,-1], function(x) t((x %o% he.1B2c$he_1B2c_n)[,,1]))
# 
# sf.1B2c.tl <- lapply(sf.1B2c.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # str(sf.1B2c.tl)
# 
# # writexl::write_xlsx(sf.1B2c.tle, "sf.1B2c.tle.xlsx") # Mnogo traje...
# 
# vars <- names(sf.1B2c_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1B2c.tl[[i]], file = paste("sf.1B2c", paste(vars[i],"csv", sep = "."), sep = "_"))
# }

#'
#'
#'
#'
#' ## 1A1c - Manufacturing of solid fuels
#+ include = FALSE
sf.1A1c <- st_read("D:/R_projects/Spatialization/Products/1A1 - Energy/1A1c.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1A1c <- sf.1A1c %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1A1c%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
#
# ---  HE = WDWW + WT0816 + WT1624 + !PH + !RP
#

he.1A1c <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+0.5)) %>%
  # dplyr::mutate(he_1A1c = ((WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2* (TEMP+30)) %>%
  dplyr::mutate(he_1A1c = WDWW) %>%
  #dplyr::mutate(he_sig = sigmoid(scale(he_1A1c))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  #dplyr::mutate(he_1A1c = he_sig) %>%
  #dplyr::mutate(he_1A1c_n = he_sig/sum(he_sig)) %>% # OVO je normalizovano i prebaceno u procente
  select(times, he_1A1c)#, he_1A1c_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1A1c, aes(x = times, y = he_1A1c)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_1A1c = ((WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2* (TEMP+30)")+
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", colour = "black")
  )

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1A1c$he_1A1c), max(he.1A1c$he_1A1c), sum(he.1A1c$he_1A1c))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1A1c$sumF <- sum(he.1A1c$he_1A1c)
he.1A1c %<>% 
  dplyr::mutate(NOx_1A1c = (t.1A1c$NOx/t.1A1c$sumF)*he_1A1c, 
                NOx_1A1c_p = (NOx_1A1c/sum(NOx_1A1c))*100,
                SO2_1A1c = (t.1A1c$SO2/t.1A1c$sumF)*he_1A1c, 
                SO2_1A1c_p = (SO2_1A1c/sum(SO2_1A1c))*100,
                PM10_1A1c = (t.1A1c$PM10/t.1A1c$sumF)*he_1A1c, 
                PM10_1A1c_p = (PM10_1A1c/sum(PM10_1A1c))*100,
                PM2.5_1A1c = (t.1A1c$PM2.5/t.1A1c$sumF)*he_1A1c, 
                PM2.5_1A1c_p = (PM2.5_1A1c/sum(PM2.5_1A1c))*100,
                NMVOC_1A1c = (t.1A1c$NMVOC/t.1A1c$sumF)*he_1A1c, 
                NMVOC_1A1c_p = (NMVOC_1A1c/sum(NMVOC_1A1c))*100,
                NH3_1A1c = (t.1A1c$NH3/t.1A1c$sumF)*he_1A1c, 
                NH3_1A1c_p = (NH3_1A1c/sum(NH3_1A1c))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1A1c_p, SO2_1A1c_p, PM10_1A1c_p, PM2.5_1A1c_p, NMVOC_1A1c_p, NH3_1A1c_p) %>%
  rename(`1A1c_NOx` = NOx_1A1c_p,
         `1A1c_SO2` = SO2_1A1c_p,
         `1A1c_PM10` = PM10_1A1c_p,
         `1A1c_PM2.5` = PM2.5_1A1c_p,
         `1A1c_NMVOC` = NMVOC_1A1c_p,
         `1A1c_NH3` = NH3_1A1c_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1A1c$`1A1c_NOx`), sum(he.1A1c$`1A1c_SO2`), sum(he.1A1c$`1A1c_PM10`), sum(he.1A1c$`1A1c_PM2.5`), sum(he.1A1c$`1A1c_NMVOC`), sum(he.1A1c$`1A1c_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#+
# sf.1A1c_df <- sf.1A1c %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1A1c.tl <- lapply(sf.1A1c_df[,-1], function(x) t((x %o% he.1A1c$he_1A1c_n)[,,1]))
# 
# sf.1A1c.tl <- lapply(sf.1A1c.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # str(sf.1A1c.tl)
# 
# # writexl::write_xlsx(sf.1A1c.tle, "sf.1A1c.tle.xlsx") # Mnogo traje...
# 
# # vars <- names(sf.1A1c_df)[-1]
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1A1c.tl[[i]], file = paste("sf.1A1c", paste(vars[i],"csv", sep = "."), sep = "_"))
# }

#'
#'
#'
#'
#' ## 1B1b - Fugitive emissions from solid fuels: Solid fuel transformation
#'
#+ include = FALSE

sf.1B1b <- st_read("D:/R_projects/Spatialization/Products/1A1 - Energy/1B1b.gpkg")

#'
#+ echo = FALSE, result = TRUE, eval = TRUE
t.1B1b <- sf.1B1b %>%
  summarize(NOx = sum(NOx),
            SO2 = sum(SO2),
            PM10 = sum(PM10),
            PM2.5 = sum(PM2.5),
            NMVOC = sum(NMVOC),
            NH3 = sum(NH3)) %>%
  select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
  st_drop_geometry()

data.frame(t.1B1b%>%
             dplyr::mutate_if(is.numeric, round, 2)) %>%
  datatable(., caption = 'Table 1: Total spatialized inventory',
            options = list(pageLength = 5)
  )

#+ include = FALSE
# Building function for Hourly emissions - HE:
#
# ---  HE = WDWW + WT0816 + WT1624 + !PH + !RP
#

he.1B1b <- activity.df %>%
  dplyr::mutate(RP1 = dplyr::case_when(RP == TRUE ~ 1,
                                       RP == FALSE ~ 0)) %>%
  dplyr::mutate(RP2 = (sin(((pi)/24)*(!RP1))+0.5)) %>%
  dplyr::mutate(PH1 = dplyr::case_when(PH == TRUE ~ 1,
                                       PH == FALSE ~ 0)) %>%
  dplyr::mutate(PH2 = (sin(((pi)/24)*(!PH1))+0.5)) %>%
  # dplyr::mutate(he_1B1b = ((WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * (TEMP+30)) %>%
  dplyr::mutate(he_1B1b = WDWW) %>%
  # dplyr::mutate(he_sig = sigmoid(scale(he_1B1b))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  # dplyr::mutate(he_1B1b = he_sig) %>%
  # dplyr::mutate(he_1B1b_n = he_sig/sum(he_sig)) %>% # OVO je normalizovano i prebaceno u procente
  select(times, he_1B1b)#, he_1B1b_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he.1B1b, aes(x = times, y = he_1B1b)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  labs( caption = "he_1B1b = ((WT0816+0.5) + (WT1624+0.5)) * RP2 * PH2 * (TEMP+30)")+
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", colour = "black")
  )
#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(sum = c("Function - min", "Function - max", "Function - sum"), Stat = rbind(min(he.1B1b$he_1B1b), max(he.1B1b$he_1B1b), sum(he.1B1b$he_1B1b))) %>%
  datatable(., caption = 'Table 2: Function summary',
            options = list(pageLength = 5)
  ) # min mora biti veci od 0 !!!!!

#'
#+ include = FALSE
t.1B1b$sumF <- sum(he.1B1b$he_1B1b)
he.1B1b %<>% 
  dplyr::mutate(NOx_1B1b = (t.1B1b$NOx/t.1B1b$sumF)*he_1B1b, 
                NOx_1B1b_p = (NOx_1B1b/sum(NOx_1B1b))*100,
                SO2_1B1b = (t.1B1b$SO2/t.1B1b$sumF)*he_1B1b, 
                SO2_1B1b_p = (SO2_1B1b/sum(SO2_1B1b))*100,
                PM10_1B1b = (t.1B1b$PM10/t.1B1b$sumF)*he_1B1b, 
                PM10_1B1b_p = (PM10_1B1b/sum(PM10_1B1b))*100,
                PM2.5_1B1b = (t.1B1b$PM2.5/t.1B1b$sumF)*he_1B1b, 
                PM2.5_1B1b_p = (PM2.5_1B1b/sum(PM2.5_1B1b))*100,
                NMVOC_1B1b = (t.1B1b$NMVOC/t.1B1b$sumF)*he_1B1b, 
                NMVOC_1B1b_p = (NMVOC_1B1b/sum(NMVOC_1B1b))*100,
                NH3_1B1b = (t.1B1b$NH3/t.1B1b$sumF)*he_1B1b, 
                NH3_1B1b_p = (NH3_1B1b/sum(NH3_1B1b))*100) %>%
  #replace_all(., is.na(.), 0) %>%
  select(NOx_1B1b_p, SO2_1B1b_p, PM10_1B1b_p, PM2.5_1B1b_p, NMVOC_1B1b_p, NH3_1B1b_p) %>%
  rename(`1B1b_NOx` = NOx_1B1b_p,
         `1B1b_SO2` = SO2_1B1b_p,
         `1B1b_PM10` = PM10_1B1b_p,
         `1B1b_PM2.5` = PM2.5_1B1b_p,
         `1B1b_NMVOC` = NMVOC_1B1b_p,
         `1B1b_NH3` = NH3_1B1b_p) %>% 
  mutate_all(~replace_na(., 0))

#+ echo = FALSE, result = TRUE, eval = TRUE
data.frame(Emission = c("NOx [%]", "SO2 [%]", "PM10 [%]", "PM2.5 [%]","NMVOC [%]", "NH3 [%]"), 
           Sum = rbind(sum(he.1B1b$`1B1b_NOx`), sum(he.1B1b$`1B1b_SO2`), sum(he.1B1b$`1B1b_PM10`), sum(he.1B1b$`1B1b_PM2.5`), sum(he.1B1b$`1B1b_NMVOC`), sum(he.1B1b$`1B1b_NH3`))) %>%
  datatable(., caption = 'Table 3: Summary',
            options = list(pageLength = 5)
  )

#'
#+
# sf.1B1b_df <- sf.1B1b %>% st_drop_geometry() #%>% dplyr::select(NOx)
# 
# sf.1B1b.tl <- lapply(sf.1B1b_df[,-1], function(x) t((x %o% he.1B1b$he_1B1b_n)[,,1]))
# 
# sf.1B1b.tl <- lapply(sf.1B1b.tl, function(x) data.frame(x) %>% mutate(Time = activity.df$times) %>% dplyr::select(Time, everything()))
# 
# # str(sf.1B1b.tl)
# 
# # writexl::write_xlsx(sf.1B1b.tle, "sf.1B1b.tle.xlsx") # Mnogo traje...
# 
# # vars <- names(sf.1B1b_df)[-1]
# 
# # for(i in 1:length(vars)){
# #   fwrite(sf.1B1b.tl[[i]], file = here::here("Hourly_emissions", "Products", "Energy_TemporalByCell", paste("1B1b", paste(vars[i],"csv", sep = "."), sep = "_")))
# # }
# 
# for(i in 1:length(vars)){
#   fwrite(sf.1B1b.tl[[i]], file = paste("sf.1B1b", paste(vars[i],"csv", sep = "."), sep = "_"))
# }



# temporalProfile_Energy <- activity.df$times %>% 
#  cbind(he.1A1a[,1:6], 
#        he.1A1b[,1:6], 
#        he.1A1c[,1:6], 
#        he.1B1b[,1 :6], 
#        he.1B2aiv[,1:6], 
#        he.1B2c[,1:6]) %>% 
#   as.data.frame()
# 
# writexl::write_xlsx(temporalProfile_Energy, path = 'Hourly_emissions/Products/TemporalProfile_Energy_industries.xlsx')






