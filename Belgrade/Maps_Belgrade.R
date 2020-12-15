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
library(stringr)
library(s2)
library(units)
library(ggspatial)
library(ggrepel)
library(ggsflabel)

my_theme <- function(base_size = 10, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fffcfc"),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "bottom",
      #legend.justification = "center",
      #legend.background = element_blank(),
      #panel.border = element_rect(color = "grey30", fill = NA, size = 0.5)
    )
}
theme_set(my_theme())





sf.grid.4326 <- st_read(dsn = "Grid/Grid_Serbia_0.05deg.gpkg")




# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Belgrade roads
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_roads_urban <- st_read(dsn = "Data/putevi/OSM_putevi_urbana_podrucja.gpkg") %>%
  st_transform(4326)

#sf_granica <- st_read(dsn = "Data/Granica_SRB.gpkg")
#sf_urb <- st_read(dsn = "Products/urban_areas.gpkg")

sf_bel <- st_read(dsn = "Data/Mun_Belgrade_urban.gpkg")
#sf_bel_roads <- sf_roads_urban %>% st_intersection(sf_bel)


opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

sf_opstine_bg <- sf_opstine %>% dplyr::filter(NAME_1 == "Grad Beograd")
sf_bel_roads <- sf_roads_urban %>% st_intersection(sf_opstine_bg)

ggplot()+
  geom_sf(data = sf_opstine_bg, fill = NA)+
  geom_sf(data = sf_bel_roads)+
  labs(x = NULL, y = NULL,
       title = "OSM road network for urban areas",
       subtitle = "Territory of the Repubic of Serbia",
       caption = "© GiLab (2019/20)")+
  theme_bw()

sf_roads <- st_read(dsn = "Data/Roads_PGDS_intersected_with_SRB_boundary.gpkg")
sf_roads.4326 <- st_transform(sf_roads, 4326)
vcds <- st_read(dsn = "Data/brojaci/VCDs.gpkg")
vcds %<>% st_zm(., drop = TRUE)
vcds %<>% dplyr::filter(Kategorija != "nije u mrezi") %>%
  dplyr::mutate(Category = Kategorija)

vcds  %<>% st_intersection(sf_opstine_bg)
sf_roads.4326 %<>% st_intersection(sf_opstine_bg)

g1 <- ggplot(data = vcds)+
  labs(#x = "Longitude [deg]", y="Latitude [deg]",
       caption = "© UBFCE (2020)",
       #subtitle = "Territory of the Belgrade metropolitan area",
       title = "Network of main state roads")+#,
  geom_sf(aes(color = Category), size= 2.5)+
  theme_bw()+
  theme(#panel.grid = element_line(color = "black"), 
    #panel.background = element_rect(fill = "white"), 
    #axis.text.x = element_text(colour = "black"), 
    #axis.text.y = element_text(colour = "black")
    axis.title = element_blank(),legend.position = c(0.85, 0.85))+
  #annotation_scale(location = "bl", width_hint = 0.5) +
  #annotation_north_arrow(location = "bl", which_north = "true",
  #                       pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                       style = north_arrow_fancy_orienteering)+
  geom_sf(data = sf_roads.4326)+
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "brown")+
  geom_sf_text_repel(data = sf_opstine_bg, aes(label = NAME_2),
                #nudge_x = 0.025,
                #nudge_y = 0.025,
                colour = "brown")+ #, force = 5
  coord_sf(xlim = c(19.96119, 20.83907), ylim = c(44.24046, 45.08233))
g1


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Zvezdara roads
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

mapview(sf_opstine_bg)
vr_bg <- sf_opstine_bg %>% dplyr::filter(NAME_2 == "Zvezdara") 

sf_vr_roads <- sf_roads_urban %>% st_intersection(vr_bg)
sf_vr_roads %<>% dplyr::mutate(Length = drop_units(st_length(.)))
unique(sf_vr_roads$fclass)


# g2 <- ggplot()+
#   geom_sf(data = vr_bg, fill = NA, colour = "brown")+
#   geom_sf(data = sf_vr_roads, aes(colour = fclass))+
#   labs(x = NULL, y = NULL,
#        title = "Dense network of roads in urban areas",
#        caption = "© UBFCE (2020)")+
#   theme_bw()+
#   theme(legend.position = "none",
#         panel.grid = element_blank(), 
#         panel.background = element_rect(fill = "transparent", color = NA), 
#         plot.background = element_rect(fill = "transparent", color = NA),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.background = element_rect(fill = "transparent", color = NA),
#         axis.text = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title = element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_blank(),
#         axis.ticks = element_blank())+
#   geom_sf_label(data = vr_bg, aes(label = NAME_2),
#                 nudge_x = 0.025,
#                 nudge_y = 0.025,
#                 fontface = "bold", colour = "brown")
# 
# g2

sf_roads_urban <- sf_roads_urban %>% st_intersection(sf_opstine_bg)

g2 <- ggplot()+
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "brown")+
  geom_sf(data = sf_roads_urban, aes(colour = fclass))+
  labs(x = NULL, y = NULL,
       title = "Dense network of roads in urban areas",
       caption = "© UBFCE (2020)")+
  theme_bw()+
  theme(legend.position = "None")+
  geom_sf(data = sf_roads.4326)+
  coord_sf(xlim = c(19.96119, 20.83907), ylim = c(44.24046, 45.08233))

g2








library(gridExtra)
gr1 <- grid.arrange(g1, g2, ncol = 2)

ggsave(plot = gr1, filename = "Belgrade/Maps/map1.jpg", width = 40, height = 30, units = "cm", device = "jpeg", dpi = 600)



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Population density grid
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(raster)
library(stars)
library(viridis)

pop_raster <- raster("Version_3_update/Spatialization/New_data/pop_dens_4326.tif")

pop_star <- st_as_stars(pop_raster)
pop_star_bel <- raster::mask(pop_raster, sf_opstine_bg)

#mapview(pop_star_bel) + mapview(sf_opstine_bg)
pop_star_bel <- st_as_stars(pop_star_bel)
popdens.map <- ggplot()+
  geom_stars(data = pop_star_bel)+
  labs(x = NULL, y = NULL,
       title = "Population density map",
       #subtitle = "Territory of the Belgrade metropolitan area, [0.006°/pix]",
       caption = "© UBFCE (2020)")+
  theme_bw()+
  theme(legend.position = c(0.85, 0.85))+
  scale_fill_viridis(option = "B", limits = c(0,250), na.value = NA)+
  guides(fill=guide_legend(title="Density"))+
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "brown")+
  coord_sf(xlim = c(19.96119, 20.83907), ylim = c(44.24046, 45.08233))

popdens.map


#extent(sf_opstine_bg)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# CLC map
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sf_clc <- st_read(dsn = "GIS_layers/CLC_12-18.gpkg")


clc_legend <- read.csv(file="Data/clc_legend.csv", header=TRUE, sep=",") %>% as.data.frame()
sf_clc$RGB <- as.character(clc_legend$RGB[match(as.character(sf_clc$CODE_18),as.character(clc_legend$CLC_CODE))])
sf_clc %<>% dplyr::mutate(RGB = str_replace_all(RGB, "-", ","))
sf_clc %<>% tidyr::separate(RGB, c("R", "G", "B"), sep = ",") 
sf_clc$rgb <- rgb(as.numeric(sf_clc$R)/255, as.numeric(sf_clc$G)/255, as.numeric(sf_clc$B)/255)

colors <- distinct(sf_clc, CODE_18, rgb)
pal <- colors$rgb
names(pal) <- colors$CODE_18


sf_clc_4326 <- st_transform(sf_clc, 4326)
# sf_clc_4326 <- sf_clc_4326[sf_bel %>% st_transform(4326), ]
sf_clc_4326  %<>% st_intersection(sf_opstine_bg )

CLC_map <- ggplot() +
  geom_sf(data = sf_clc_4326, aes(fill = CODE_18)) + 
  labs(x = NULL, y = NULL,
       title = "Corine Land Cover map [2012-2018]",
       #subtitle = "Territory of the Belgrade metropolitan area",
       caption = "© UBFCE (2020)")+
  scale_fill_manual(values = pal)+
  theme_bw()+
  theme(legend.position = c(0.85, 0.8))+
  coord_sf(xlim = c(19.96119, 20.83907), ylim = c(44.24046, 45.08233))
CLC_map


gr2 <- grid.arrange(popdens.map, CLC_map, ncol = 2 )
ggsave(plot = gr2, filename = "Belgrade/Maps/map2.jpg", width = 40, height = 30, units = "cm", device = "jpeg", dpi = 300)




gr3 <- grid.arrange(g1, g2, popdens.map, CLC_map, ncol = 2)
ggsave(plot = gr3, filename = "Belgrade/Maps/map3 200dpi.jpg", width = 40, height = 50, units = "cm", device = "jpeg", dpi = 200)







# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# TEMPORAL PROFILES
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
Sys.setlocale("LC_ALL","English")


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
activity.df <- readRDS(file = "D:/R_projects/Spatialization/Version_2_update/Temporalization/activity_df_new.rds")
load(file = "D:/R_projects/Spatialization/Hourly_emissions/Data/counters_df.rds")
counters.df <- counters_df %>% 
  mutate(ALL_mean = (IA_mean + IIA_mean + IB_mean)/3)
activity.df$VA <- counters.df$ALL_mean


sigmoid = function(x) {
  1 / (1 + exp(-x))
}


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
  dplyr::select(times, he_2D3a, he_2D3a_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
gt1 <- ggplot(he.2D3a, aes(x = times, y = he_2D3a)) +
  geom_point(size = 0.1) +
  geom_line(colour = "violet") + 
  labs(y = "Temporal profile 2D3a", title = "Industrial processes: 2D3a-Domestic solvent use including fungicides: f(WDWW,DL,TEMP,PH)"  )+
  # geom_smooth() +
  theme_bw() + 
  #ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_2D3a = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"),
        axis.title.x = element_blank())+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1, by=0.25)) 
gt1

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
  dplyr::select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
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
  # dplyr::mutate(he_1A4ai = (WT0622) * (DL) * HS) %>%
  dplyr::mutate(he_1A4ai = ((WT0622+0.5)) / PH2 * (TEMP*(-1)+30)) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1A4ai))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1A4ai = he_sig) %>%
  dplyr::mutate(he_1A4ai_n = he_sig/sum(he_sig))%>%
  dplyr::select(times, he_1A4ai, he_1A4ai_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
gt2 <- ggplot(he.1A4ai, aes(x = times, y = he_1A4ai)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  labs(y = "Temporal profile 1A4ai", title = "Residential/Tertiary: 1A4ai-Commercial/Institutional: Stationary Combustion: f(WT0622,PH,TEMP)"  )+
  #geom_smooth(se=FALSE, colour = "deepskyblue") +
  theme_bw() + 
  #ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_2D3a = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"),
        axis.title.x = element_blank())+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1, by=0.25))

gt2



activity.df <- readRDS(file = "D:/R_projects/Spatialization/Version_4_update/Temporalization/activity_df_new.rds")
load(file = "D:/R_projects/Spatialization/Hourly_emissions/Data/counters_df.rds")
counters.df <- counters_df %>% 
  mutate(ALL_mean = (IA_mean + IIA_mean + IB_mean)/3,
         RURAL_mean = (IIA_mean + IB_mean)/2)
activity.df$VA <- counters.df$ALL_mean
activity.df$VA_H <- counters.df$IA_mean # Highway
activity.df$VA_R <- counters.df$RURAL_mean # Rural



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
  dplyr::select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
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
  dplyr::mutate(he_sig = sigmoid(scale(he_1A3bi_H))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1A3bi_H = he_sig) %>%
  dplyr::mutate(he_1A3bi_H_n = he_sig/sum(he_sig))%>%
  dplyr::select(times, he_1A3bi_H, he_1A3bi_H_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
gt3 <- ggplot(he.1A3bi_H, aes(x = times, y = he_1A3bi_H)) +
  geom_point(size = 0.1) +
  geom_line(colour = "coral") + 
  labs(y = "Temporal profile 1A3bi_H", title = "Transports: 1A3bi-Road Transport: Passengers cars - Highways transport: f(VA_IA)"  )+
  #geom_smooth(se=FALSE, colour = "coral") +
  theme_bw() + 
  #ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_2D3a = ((DL+0.5)) * (TEMP+30) * SLP * PH2")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"),
        axis.title.x = element_blank())+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1, by=0.25))
gt3



activity.df <- readRDS(file = "D:/R_projects/Spatialization/Version_2_update/Temporalization/activity_df_new.rds")

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
  dplyr::select(NOx, SO2, PM10, PM2.5, NMVOC, NH3) %>%
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
  dplyr::select(times, he_3De, he_3De_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
gt4 <- ggplot(he.3De, aes(x = times, y = he_3De)) +
  geom_point(size = 0.1) +
  geom_line(colour = "red") + 
  labs(x = "Time", y = "Temporal profile 3De", title = "Agriculture: 3De-Cultivated crops: f(SAAG.f,SAAG.fl,SLP,TEMP,DL)"  )+
  #geom_smooth(se=FALSE, colour = "coral") +
  theme_bw() + 
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1, by=0.25))
gt4


gr4 <- grid.arrange(gt1, gt2, gt3, gt4, nrow = 4)
ggsave(plot = gr4, filename = "Belgrade/Maps/profile 300dpi.jpg", width = 38, height = 25, units = "cm", device = "jpeg", dpi = 300)






# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# FINAL MAPS
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

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
library(stringr)
library(classInt)
library(viridis)
library(gridExtra)
# zoomed maps per city
# Belgrade, Bor, Novi Sad, Pančevo, Smederevo, Užice

# Belgrade

sf_data <- st_read(dsn = "Products/Sum_up_by_cell_by_pollutant.gpkg")

opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)
unique(sf_opstine$NAME_1)

sf_opstine_bg <- sf_opstine %>% dplyr::filter(NAME_1 == "Grad Beograd")

sf_data_beograd <- sf_data[sf_opstine_bg, ] 

bg_classes.NOx <- classIntervals(sf_data_beograd$NOx, n = 12, style = "fisher")
bg_classes.SO2 <- classIntervals(sf_data_beograd$SO2, n = 12, style = "fisher")
bg_classes.PM10 <- classIntervals(sf_data_beograd$PM10, n = 12, style = "fisher")
bg_classes.PM2.5 <- classIntervals(sf_data_beograd$PM2.5, n = 12, style = "fisher")
bg_classes.NMVOC <- classIntervals(sf_data_beograd$NMVOC, n = 12, style = "fisher")
bg_classes.NH3 <- classIntervals(sf_data_beograd$NH3, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(NOx, bg_classes.NOx$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(SO2, bg_classes.SO2$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, bg_classes.PM10$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, bg_classes.PM2.5$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, bg_classes.NMVOC$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(NH3, bg_classes.NH3$brks, include.lowest = T,dig.lab=7)
  )

pal1 <- viridisLite::viridis(12, direction = -1)



vec <- c(sf_data_beograd$NOx, sf_data_beograd$SO2, sf_data_beograd$PM10, sf_data_beograd$PM2.5, sf_data_beograd$NMVOC, sf_data_beograd$NH3)

bg_classes <- classIntervals(vec, n = 12, style = "fisher")

sf_data_beograd <- sf_data_beograd %>%
  mutate(percent_class_NOx = cut(NOx, bg_classes$brks, include.lowest = T, dig.lab=7),
         percent_class_SO2 = cut(SO2, bg_classes$brks, include.lowest = T,dig.lab=7),
         percent_class_PM10 = cut(PM10, bg_classes$brks, include.lowest = T,dig.lab=7),
         percent_class_PM2.5 = cut(PM2.5, bg_classes$brks, include.lowest = T,dig.lab=7),
         percent_class_NMVOC = cut(NMVOC, bg_classes$brks, include.lowest = T,dig.lab=7),
         percent_class_NH3 = cut(NH3, bg_classes$brks, include.lowest = T,dig.lab=7)
  )

pal1 <- viridisLite::viridis(12, direction = -1)






#+ include = FALSE 
a.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "NOx", #Pollutant inventory spatialization - 
       #subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme_bw()+
  theme(#line = element_blank(),
        #axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right" #c(0.85, 0.9)
        #legend.position = "None", ###################### legend
        #panel.background = element_blank()
        ) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))#+


a.bg



b.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "SO2",
       #subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme_bw()+
  theme(#line = element_blank(),
    #axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right" #c(0.85, 0.9)
    #legend.position = "None", ###################### legend
    #panel.background = element_blank()
  ) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))


b.bg


c.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "PM10",
      #subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme_bw()+
  theme(#line = element_blank(),
    #axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right" #c(0.85, 0.9)
    #legend.position = "None", ###################### legend
    #panel.background = element_blank()
  ) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))

c.bg


d.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "PM2.5",
       #subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme_bw()+
  theme(#line = element_blank(),
    #axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right" #c(0.85, 0.9)
    #legend.position = "None", ###################### legend
    #panel.background = element_blank()
  ) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))


d.bg


e.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "NMVOC",
       #subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme_bw()+
  theme(#line = element_blank(),
    #axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right" #c(0.85, 0.9)
    #legend.position = "None", ###################### legend
    #panel.background = element_blank()
  ) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))


e.bg



f.bg<-ggplot() +
  geom_sf(data = sf_data_beograd,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "NH3",
       #subtitle = "Spatial resolution 0.05°x0.05°, Territory of the City of Belgrade",
       caption = "UBFCE (2020)") +
  theme_bw()+
  theme(#line = element_blank(),
    #axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right" #c(0.85, 0.9)

    #legend.position = "None", ###################### legend
    #panel.background = element_blank()
  ) +
  geom_sf(data = sf_opstine_bg, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))#+
  #guides(fill = guide_legend(override.aes = list(size = 0.01)))


f.bg



bg <- grid.arrange(a.bg, b.bg, c.bg, d.bg, e.bg, f.bg, ncol = 2)


ggsave(plot = bg, 
       filename = "Belgrade/Maps/BG_map 300dpi.jpg", 
       width = 25, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=300)
 



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# MAPS 3 TYPES 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(ggsflabel)

sf.grid.4326 <- st_read(dsn = "Grid/Grid_Serbia_0.05deg.gpkg")

mapview(sf.grid.4326)
extents <- c(xmin = 20.37689, xmax = 20.52555, ymin = 44.73405, ymax = 44.87534)

# Point
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#point <- mapedit::editMap()
#point %<>% dplyr::mutate(Name = "Point 1")
point <- st_read(dsn = "Belgrade/Maps/point.gpkg")

gp1 <- ggplot()+
  #geom_sf(data = sf.grid.4326, fill = NA, colour = "red")+
  geom_sf(data = point, colour = "red", size = 3.5, pch = 19)+
  geom_sf_text(data = point, aes(label = Name, colour = "red"), nudge_x = -0.01, nudge_y = 0.01)+
  coord_sf(datum = sf::st_crs(4326), xlim = c(20.37689, 20.52555), ylim = c(44.73405, 44.87534))+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "None",
        #panel.grid = element_blank(), 
        #panel.background = element_rect(fill = "transparent", color = NA), 
        #plot.background = element_rect(fill = "transparent", color = NA),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #legend.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        #axis.text.x = element_blank(),
        #axis.title = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_blank())

gp1
gp2 <- ggplot()+
  geom_sf(data = sf.grid.4326, fill = NA, colour = "red")+
  geom_sf(data = point, colour = "red", size = 3.5, pch = 19)+
  geom_sf_text(data = point, aes(label = Name), colour = "red", nudge_x = -0.01, nudge_y = 0.01)+
  geom_sf_text(data = sf.grid.4326, aes(label = id), colour = "black", nudge_x = -0.01, nudge_y = -0.02)+
  coord_sf(datum = sf::st_crs(4326), xlim = c(20.37689, 20.52555), ylim = c(44.73405, 44.87534))+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "None",
        #panel.grid = element_blank(), 
        #panel.background = element_rect(fill = "transparent", color = NA), 
        #plot.background = element_rect(fill = "transparent", color = NA),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #legend.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        #axis.text.x = element_blank(),
        #axis.title = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_blank())

gp3 <- ggplot()+
  #geom_sf(data = sf.grid.4326, fill = NA, colour = "red")+
  geom_sf(data = point, colour = "NA")+
  #geom_sf(data = line_int, colour = "NA", size = 1)+
  scale_size_identity()+
  #geom_sf_text(data = line_int, aes(label = Name, colour = "NA"), nudge_x = -0.02, nudge_y = 0.03)+
  coord_sf(datum = sf::st_crs(4326), xlim = c(20.37689, 20.52555), ylim = c(44.73405, 44.87534))+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "None",
        panel.grid = element_blank(), 
        #panel.background = element_rect(fill = "transparent", color = NA), 
        #plot.background = element_rect(fill = "transparent", color = NA),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #legend.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        #axis.text.x = element_blank(),
        #axis.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_blank())

gpgr <- grid.arrange(gp1, gp2, gp3, ncol = 3)

ggsave(plot = gpgr, 
       filename = "Belgrade/Maps/point_source.jpg", 
       width = 30, 
       height = 10, 
       units = "cm", 
       device = "jpeg",
       dpi=400)

#st_write(point, dsn="Belgrade/Maps/point.gpkg", layer='point')

# Line
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(units)
mapview(sf.grid.4326)
#linee <- mapedit::editMap() 
#st_write(linee, dsn="Belgrade/Maps/line.gpkg", layer='linee')
st_read(dsn = "Belgrade/Maps/line.gpkg")


line_int <- st_intersection(linee, sf.grid.4326)
line_int %<>% dplyr::mutate(Length = drop_units(st_length(.)))
line_int$sizee <- 1
line_int$sizee[line_int$id == 2908] <- 2

line_int$Name <- NA
line_int$Name[line_int$id == 2908] <- "Line 1"

line_int$Name2 <- NA
line_int$Name2[line_int$id == 2908] <- "Line 1-1"


line_int$Col <- "blue"
line_int$Col[line_int$id == 2908] <- "orange"


mapview(line_int)

gl1 <- ggplot()+
  #geom_sf(data = sf.grid.4326, fill = NA, colour = "red")+
  #geom_sf(data = point, colour = "red")+
  geom_sf(data = line_int, colour = "blue", size = 1)+
  scale_size_identity()+
  geom_sf_text(data = line_int, aes(label = Name, colour = "brown"), nudge_x = -0.02, nudge_y = 0.03)+
  coord_sf(datum = sf::st_crs(4326), xlim = c(20.37689, 20.52555), ylim = c(44.73405, 44.87534))+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "None",
        #panel.grid = element_blank(), 
        #panel.background = element_rect(fill = "transparent", color = NA), 
        #plot.background = element_rect(fill = "transparent", color = NA),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #legend.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        #axis.text.x = element_blank(),
        #axis.title = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_blank())

#sf_roads <- st_read(dsn = "Data/Roads_PGDS_intersected_with_SRB_boundary.gpkg")
#sf_roads.4326 <- st_transform(sf_roads, 4326)

gl2 <- ggplot()+
  geom_sf(data = sf.grid.4326, fill = NA, colour = "red")+
  #geom_sf(data = point, colour = "red")+
  geom_sf(data = line_int, aes(size = sizee, colour = Col))+
  scale_color_manual(values = c("blue","red"))+
  scale_size_identity()+
  #scale_color_discrete()+
  geom_sf_text(data = line_int, aes(label = Name2), colour = "brown", nudge_x = -0.02, nudge_y = 0.03)+
  coord_sf(datum = sf::st_crs(4326), xlim = c(20.37689, 20.52555), ylim = c(44.73405, 44.87534))+
  geom_sf_text(data = sf.grid.4326, aes(label = id), colour = "black", nudge_x = -0.01, nudge_y = -0.02)+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "None",
        #panel.grid = element_blank(), 
        #panel.background = element_rect(fill = "transparent", color = NA), 
        #plot.background = element_rect(fill = "transparent", color = NA),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #legend.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        #axis.text.x = element_blank(),
        #axis.title = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_blank())

gl3 <- ggplot()+
  #geom_sf(data = sf.grid.4326, fill = NA, colour = "red")+
  #geom_sf(data = point, colour = "red")+
  geom_sf(data = line_int, colour = "NA", size = 1)+
  scale_size_identity()+
  #geom_sf_text(data = line_int, aes(label = Name, colour = "NA"), nudge_x = -0.02, nudge_y = 0.03)+
  coord_sf(datum = sf::st_crs(4326), xlim = c(20.37689, 20.52555), ylim = c(44.73405, 44.87534))+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "None",
        panel.grid = element_blank(), 
        #panel.background = element_rect(fill = "transparent", color = NA), 
        #plot.background = element_rect(fill = "transparent", color = NA),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #legend.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        #axis.text.x = element_blank(),
        #axis.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_blank())




glpr <- grid.arrange(gl1, gl2, gl3, ncol = 3)

ggsave(plot = glpr, 
       filename = "Belgrade/Maps/line_source.jpg", 
       width = 30, 
       height = 10, 
       units = "cm", 
       device = "jpeg",
       dpi=400)



# Polygon
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#polyg <- mapedit::editMap() 
#mapview(polyg) + mapview(sf.grid.4326)
#st_write(polyg, dsn="Belgrade/Maps/polygon.gpkg", layer='polyg')
polyg <- st_read(dsn = "Belgrade/Maps/polygon.gpkg")



polyg_int <- st_intersection(polyg, sf.grid.4326)
mapview(polyg_int)

polyg_int$Name <- NA
polyg_int$Name[polyg_int$id == 2908] <- "Polygon 1"

polyg_int$Col <- "blue"
polyg_int$Col[polyg_int$id == 2908] <- "orange"

polyg_int$Name_2 <- NA
polyg_int$Name_2[polyg_int$id == 2908] <- "Polygon 1-1"


gpo1 <- ggplot()+
  #geom_sf(data = sf.grid.4326, fill = NA, colour = "red")+
  #geom_sf(data = point, colour = "red")+
  geom_sf(data = polyg_int, colour = "NA", fill = "blue")+
  geom_sf_text(data = polyg_int, aes(label = Name), colour = "white")+#, nudge_x = -0.02, nudge_y = 0.03)+
  coord_sf(datum = sf::st_crs(4326), xlim = c(20.37689, 20.52555), ylim = c(44.73405, 44.87534))+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "None",
        #panel.grid = element_blank(), 
        #panel.background = element_rect(fill = "transparent", color = NA), 
        #plot.background = element_rect(fill = "transparent", color = NA),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #legend.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        #axis.text.x = element_blank(),
        #axis.title = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_blank())
gpo1


gpo2 <- ggplot()+
  #geom_sf(data = point, colour = "red")+
  geom_sf(data = polyg_int, colour = "NA", aes(fill = Col))+
  scale_fill_manual(values = c("blue", "orange"))+
  geom_sf_text(data = polyg_int, aes(label = Name_2), colour = "white")+#, nudge_x = -0.02, nudge_y = 0.03)+
  geom_sf(data = sf.grid.4326, fill = NA, colour = "red")+
  geom_sf_text(data = sf.grid.4326, aes(label = id), colour = "black", nudge_x = -0.01, nudge_y = -0.02)+
  coord_sf(datum = sf::st_crs(4326), xlim = c(20.37689, 20.52555), ylim = c(44.73405, 44.87534))+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "None",
        #panel.grid = element_blank(), 
        #panel.background = element_rect(fill = "transparent", color = NA), 
        #plot.background = element_rect(fill = "transparent", color = NA),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #legend.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        #axis.text.x = element_blank(),
        #axis.title = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_blank())
gpo2

gpo3 <- ggplot()+
  #geom_sf(data = point, colour = "red")+
  geom_sf(data = polyg_int, colour = "NA", fill = "NA")+
  coord_sf(datum = sf::st_crs(4326), xlim = c(20.37689, 20.52555), ylim = c(44.73405, 44.87534))+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "None",
        panel.grid = element_blank(), 
        #panel.background = element_rect(fill = "transparent", color = NA), 
        #plot.background = element_rect(fill = "transparent", color = NA),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #legend.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        #axis.text.x = element_blank(),
        #axis.title = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_blank())
gpo3


gpogr <- grid.arrange(gpo1, gpo2, gpo3, ncol = 3)

ggsave(plot = gpogr, 
       filename = "Belgrade/Maps/polygon_source.jpg", 
       width = 30, 
       height = 10, 
       units = "cm", 
       device = "jpeg",
       dpi=400)

gpgr <- grid.arrange(gp1, gp2, gp3, ncol = 3)
glpr <- grid.arrange(gl1, gl2, gl3, ncol = 3)
gpogr <- grid.arrange(gpo1, gpo2, gpo3, ncol = 3)

fgr <- grid.arrange(gp1, gp2, gp3, gl1, gl2, gl3, gpo1, gpo2, gpo3, nrow = 3, ncol = 3)

ggsave(plot = fgr, 
       filename = "Belgrade/Maps/final_source.jpg", 
       width = 28, 
       height = 30, 
       units = "cm", 
       device = "jpeg",
       dpi=400)
