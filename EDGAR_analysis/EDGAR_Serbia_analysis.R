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
library(readr)
library(mapedit)
library(magrittr)


RS_grid_0.1_spated <- st_read(dsn = "EDGAR_analysis/RS_Grid_0.1_RF2015_bez_KiM.gpkg")
RS_grid_0.1_cent <- st_read(dsn = "CEIP_Grid_0.1/CEIP/tacke_ccentroidi_2015.gpkg")

# RS_grid_0.1_spated_n <- RS_grid_0.1_spated %>%
#   st_join(RS_grid_0.1_cent, join = st_contains) %>%
#   group_by(ID.x) %>%
#   dplyr::summarize(N = n()) %>%
#   dplyr::rename(ID = ID.x)
# 
# summary(RS_grid_0.1_spated_n)
# mapview(RS_grid_0.1_spated_n, zcol = "pt_count") + mapview(RS_grid_0.1_cent)


RS_grid_0.1_spated$N_pt <- lengths(st_intersects(RS_grid_0.1_spated, RS_grid_0.1_cent))
summary(RS_grid_0.1_spated$N_pt)


# NH3

edg_NH3 <- read_delim(file = "D:/air-15/Edgar/v50_NH3_2015/v50_NH3_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_NH3_mov <- edg_NH3 %>% dplyr::mutate(lat = (lat - 0.05), 
                                         lon = (lon + 0.05))
sf_edg_NH3_mov <- st_as_sf(edg_NH3_mov, 
                           coords = c("lon", "lat"), 
                           crs = 4326)

RS_grid_0.1_spated <- st_join(RS_grid_0.1_spated, sf_edg_NH3_mov, join = st_contains) 

RS_grid_0.1_spated %<>% dplyr::rename(NH3_edgar = `emission 2015 (tons)`) 

mapview(RS_grid_0.1_spated, zcol = "NH3")

# gde je nula tu nula i u edgaru i sada sracunati cetvrtine celija
RS_grid_0.1_spated %<>% dplyr::mutate(NH3_edgar_sa_0 = case_when((NH3 == 0 | NOx == 0 | SO2 == 0 | PM2_5 == 0 | PM10 == 0 | NMVOC == 0) ~ 0, 
                                                           (NH3 != 0 & NOx != 0 & SO2 != 0 & PM2_5 != 0 & PM10 != 0 & NMVOC != 0) ~ NH3_edgar))

RS_grid_0.1_spated %<>% dplyr::mutate(NH3_edgar_f = case_when(N_pt == 0 ~ 0,
                                                              N_pt == 1 ~ (NH3_edgar_sa_0/4)*1,
                                                              N_pt == 2 ~ (NH3_edgar_sa_0/4)*2,
                                                              N_pt == 3 ~ (NH3_edgar_sa_0/4)*3,
                                                              N_pt == 4 ~ (NH3_edgar_sa_0/4)*4,
                                                              N_pt == 5 ~ (NH3_edgar_sa_0/4)*5,
                                                              N_pt == 6 ~ (NH3_edgar_sa_0/4)*6))

RS_grid_0.1_spated %<>% dplyr::mutate(NH3_diff = NH3 - NH3_edgar_f, NH3_relative =  ((NH3 - NH3_edgar_f)/NH3_edgar_f)*100) 
summary(RS_grid_0.1_spated)

mean(RS_grid_0.1_spated$NH3_diff)
mapview(RS_grid_0.1_spated , zcol = "NH3_diff")


# NOx

edg_NOx <- read_delim(file = "D:/air-15/Edgar/v50_NOx_2015/v50_NOx_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_NOx_mov <- edg_NOx %>% dplyr::mutate(lat = (lat - 0.05), 
                                         lon = (lon + 0.05))
sf_edg_NOx_mov <- st_as_sf(edg_NOx_mov, 
                           coords = c("lon", "lat"), 
                           crs = 4326)
RS_grid_0.1_spated <- st_join(RS_grid_0.1_spated, sf_edg_NOx_mov, join = st_contains) 
RS_grid_0.1_spated %<>% dplyr::rename(NOx_edgar = `emission 2015 (tons)`) 

RS_grid_0.1_spated %<>% dplyr::mutate(NOx_edgar_sa_0 = case_when((NH3 == 0 | NOx == 0 | SO2 == 0 | PM2_5 == 0 | PM10 == 0 | NMVOC == 0) ~ 0, 
                                                                 (NH3 != 0 & NOx != 0 & SO2 != 0 & PM2_5 != 0 & PM10 != 0 & NMVOC != 0) ~ NOx_edgar))
RS_grid_0.1_spated %<>% dplyr::mutate(NOx_edgar_f = case_when(N_pt == 0 ~ 0,
                                                              N_pt == 1 ~ (NOx_edgar_sa_0/4)*1,
                                                              N_pt == 2 ~ (NOx_edgar_sa_0/4)*2,
                                                              N_pt == 3 ~ (NOx_edgar_sa_0/4)*3,
                                                              N_pt == 4 ~ (NOx_edgar_sa_0/4)*4,
                                                              N_pt == 5 ~ (NOx_edgar_sa_0/4)*5,
                                                              N_pt == 6 ~ (NOx_edgar_sa_0/4)*6))

RS_grid_0.1_spated %<>% dplyr::mutate(NOx_diff = NOx-NOx_edgar_f, NOx_relative =  ((NOx - NOx_edgar_f)/NOx_edgar_f)*100) 
summary(RS_grid_0.1_spated$NOx_diff)


# SO2

edg_SO2 <- read_delim(file = "D:/air-15/Edgar/v50_SO2_2015/v50_SO2_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_SO2_mov <- edg_SO2 %>% dplyr::mutate(lat = (lat - 0.05), 
                                         lon = (lon + 0.05))
sf_edg_SO2_mov <- st_as_sf(edg_SO2_mov, 
                           coords = c("lon", "lat"), 
                           crs = 4326)
RS_grid_0.1_spated <- st_join(RS_grid_0.1_spated, sf_edg_SO2_mov, join = st_contains) 
RS_grid_0.1_spated %<>% dplyr::rename(SO2_edgar = `emission 2015 (tons)`) 

RS_grid_0.1_spated %<>% dplyr::mutate(SO2_edgar_sa_0 = case_when((NH3 == 0 | NOx == 0 | SO2 == 0 | PM2_5 == 0 | PM10 == 0 | NMVOC == 0) ~ 0, 
                                                                 (NH3 != 0 & NOx != 0 & SO2 != 0 & PM2_5 != 0 & PM10 != 0 & NMVOC != 0) ~ SO2_edgar))
RS_grid_0.1_spated %<>% dplyr::mutate(SO2_edgar_f = case_when(N_pt == 0 ~ 0,
                                                              N_pt == 1 ~ (SO2_edgar_sa_0/4)*1,
                                                              N_pt == 2 ~ (SO2_edgar_sa_0/4)*2,
                                                              N_pt == 3 ~ (SO2_edgar_sa_0/4)*3,
                                                              N_pt == 4 ~ (SO2_edgar_sa_0/4)*4,
                                                              N_pt == 5 ~ (SO2_edgar_sa_0/4)*5,
                                                              N_pt == 6 ~ (SO2_edgar_sa_0/4)*6))

RS_grid_0.1_spated %<>% dplyr::mutate(SO2_diff = SO2-SO2_edgar_f, SO2_relative =  ((SO2 - SO2_edgar_f)/SO2_edgar_f)*100) 
summary(RS_grid_0.1_spated$SO2_diff)

# PM10

edg_PM10 <- read_delim(file = "D:/air-15/Edgar/v50_PM10_2015/v50_PM10_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_PM10_mov <- edg_PM10 %>% dplyr::mutate(lat = (lat - 0.05), 
                                         lon = (lon + 0.05))
sf_edg_PM10_mov <- st_as_sf(edg_PM10_mov, 
                           coords = c("lon", "lat"), 
                           crs = 4326)
RS_grid_0.1_spated <- st_join(RS_grid_0.1_spated, sf_edg_PM10_mov, join = st_contains) 
RS_grid_0.1_spated %<>% dplyr::rename(PM10_edgar = `emission 2015 (tons)`) 

RS_grid_0.1_spated %<>% dplyr::mutate(PM10_edgar_sa_0 = case_when((NH3 == 0 | NOx == 0 | SO2 == 0 | PM2_5 == 0 | PM10 == 0 | NMVOC == 0) ~ 0, 
                                                                 (NH3 != 0 & NOx != 0 & SO2 != 0 & PM2_5 != 0 & PM10 != 0 & NMVOC != 0) ~ PM10_edgar))
RS_grid_0.1_spated %<>% dplyr::mutate(PM10_edgar_f = case_when(N_pt == 0 ~ 0,
                                                              N_pt == 1 ~ (PM10_edgar_sa_0/4)*1,
                                                              N_pt == 2 ~ (PM10_edgar_sa_0/4)*2,
                                                              N_pt == 3 ~ (PM10_edgar_sa_0/4)*3,
                                                              N_pt == 4 ~ (PM10_edgar_sa_0/4)*4,
                                                              N_pt == 5 ~ (PM10_edgar_sa_0/4)*5,
                                                              N_pt == 6 ~ (PM10_edgar_sa_0/4)*6))

RS_grid_0.1_spated %<>% dplyr::mutate(PM10_diff = PM10_edgar_f - PM10, PM10_relative =  ((PM10 - PM10_edgar_f)/PM10_edgar_f)*100) 
summary(RS_grid_0.1_spated$PM10_diff)


# PM2.5

edg_PM2.5 <- read_delim(file = "D:/air-15/Edgar/v50_PM2.5_2015/v50_PM2.5_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_PM2.5_mov <- edg_PM2.5 %>% dplyr::mutate(lat = (lat - 0.05), 
                                           lon = (lon + 0.05))
sf_edg_PM2.5_mov <- st_as_sf(edg_PM2.5_mov, 
                            coords = c("lon", "lat"), 
                            crs = 4326)
RS_grid_0.1_spated <- st_join(RS_grid_0.1_spated, sf_edg_PM2.5_mov, join = st_contains) 
RS_grid_0.1_spated %<>% dplyr::rename(PM2.5_edgar = `emission 2015 (tons)`) 

RS_grid_0.1_spated %<>% dplyr::mutate(PM2.5_edgar_sa_0 = case_when((NH3 == 0 | NOx == 0 | SO2 == 0 | PM2_5 == 0 | PM10 == 0 | NMVOC == 0) ~ 0, 
                                                                  (NH3 != 0 & NOx != 0 & SO2 != 0 & PM2_5 != 0 & PM10 != 0 & NMVOC != 0) ~ PM2.5_edgar))
RS_grid_0.1_spated %<>% dplyr::mutate(PM2.5_edgar_f = case_when(N_pt == 0 ~ 0,
                                                               N_pt == 1 ~ (PM2.5_edgar_sa_0/4)*1,
                                                               N_pt == 2 ~ (PM2.5_edgar_sa_0/4)*2,
                                                               N_pt == 3 ~ (PM2.5_edgar_sa_0/4)*3,
                                                               N_pt == 4 ~ (PM2.5_edgar_sa_0/4)*4,
                                                               N_pt == 5 ~ (PM2.5_edgar_sa_0/4)*5,
                                                               N_pt == 6 ~ (PM2.5_edgar_sa_0/4)*6))

RS_grid_0.1_spated %<>% dplyr::mutate(PM2.5_diff = PM2.5_edgar_f - PM2_5, PM2.5_relative =  ((PM2_5 - PM2.5_edgar_f)/PM2.5_edgar_f)*100) 
summary(RS_grid_0.1_spated$PM2.5_diff)


# NMVOC

edg_NMVOC <- read_delim(file = "D:/air-15/Edgar/v50_NMVOC_2015/v50_NMVOC_2015.txt", col_names = TRUE, col_types = "ddd", delim = ";")
edg_NMVOC_mov <- edg_NMVOC %>% dplyr::mutate(lat = (lat - 0.05), 
                                             lon = (lon + 0.05))
sf_edg_NMVOC_mov <- st_as_sf(edg_NMVOC_mov, 
                             coords = c("lon", "lat"), 
                             crs = 4326)
RS_grid_0.1_spated <- st_join(RS_grid_0.1_spated, sf_edg_NMVOC_mov, join = st_contains) 
RS_grid_0.1_spated %<>% dplyr::rename(NMVOC_edgar = `emission 2015 (tons)`) 

RS_grid_0.1_spated %<>% dplyr::mutate(NMVOC_edgar_sa_0 = case_when((NH3 == 0 | NOx == 0 | SO2 == 0 | PM2_5 == 0 | PM10 == 0 | NMVOC == 0) ~ 0, 
                                                                   (NH3 != 0 & NOx != 0 & SO2 != 0 & PM2_5 != 0 & PM10 != 0 & NMVOC != 0) ~ NMVOC_edgar))
RS_grid_0.1_spated %<>% dplyr::mutate(NMVOC_edgar_f = case_when(N_pt == 0 ~ 0,
                                                                N_pt == 1 ~ (NMVOC_edgar_sa_0/4)*1,
                                                                N_pt == 2 ~ (NMVOC_edgar_sa_0/4)*2,
                                                                N_pt == 3 ~ (NMVOC_edgar_sa_0/4)*3,
                                                                N_pt == 4 ~ (NMVOC_edgar_sa_0/4)*4,
                                                                N_pt == 5 ~ (NMVOC_edgar_sa_0/4)*5,
                                                                N_pt == 6 ~ (NMVOC_edgar_sa_0/4)*6))

RS_grid_0.1_spated %<>% dplyr::mutate(NMVOC_diff = NMVOC_edgar_f - NMVOC, NMVOC_relative =  ((NMVOC - NMVOC_edgar_f)/NMVOC_edgar_f)*100) 
summary(RS_grid_0.1_spated$NMVOC_diff)


# -----------------------------------------------------------------------------------------------------------
# Analysis
# -----------------------------------------------------------------------------------------------------------

summary(RS_grid_0.1_spated %>% dplyr::select(NOx_relative, SO2_relative, PM10_relative, PM2.5_relative, NMVOC_relative, NH3_relative)) 

#install.packages('qwraps2')
library(qwraps2)
options(qwraps2_markup = "markdown")

relatives <- RS_grid_0.1_spated %>% dplyr::select(NOx_relative, SO2_relative, PM10_relative, PM2.5_relative, NMVOC_relative, NH3_relative) %>% st_drop_geometry()
t1 <- summary_table(relatives) %>% DT::datatable()
summary(relatives)
our_summary1 <-
  list("NOx_relative" =
         list("min"       = ~ min(NOx_relative),
              "max"       = ~ max(NOx_relative),
              "median"    = ~ median(NOx_relative),
              "mean (sd)" = ~ qwraps2::mean_sd(NOx_relative)),
       "SO2_relative" =
         list("min"       = ~ min(disp),
              "median"    = ~ median(disp),
              "max"       = ~ max(disp),
              "mean (sd)" = ~ qwraps2::mean_sd(disp)),
       "Weight (1000 lbs)" =
         list("min"       = ~ min(wt),
              "max"       = ~ max(wt),
              "mean (sd)" = ~ qwraps2::mean_sd(wt)),
       "Forward Gears" =
         list("Three" = ~ qwraps2::n_perc0(gear == 3),
              "Four"  = ~ qwraps2::n_perc0(gear == 4),
              "Five"  = ~ qwraps2::n_perc0(gear == 5))
  )

# Iskljuciti ekstremume
# https://www.epfl.ch/labs/aprl/wp-content/uploads/2018/08/09_extremevals.html#background

WinsorizedMean <- function (x, frac=.05, ...) {
  ## x is a vector of values
  ## frac is the fraction to replace at each end
  lim <- quantile(x, c(frac/2, 1-frac/2), ...)
  x[x < lim[1]] <- lim[1]
  x[x > lim[2]] <- lim[2]
  mean(x, ...)
}

ComputeCentral <- function(x) {
  ## x is a vector of values
  metric <- c("mean"            = mean(x, na.rm=TRUE),
              "median"          = median(x, na.rm=TRUE),
              "trimmed.mean"    = mean(x, trim=0.05, na.rm=TRUE),
              "Winsorized.mean" = WinsorizedMean(x, frac=0.05, na.rm=TRUE))
  data.frame(metric=factor(names(metric), names(metric)), value=metric)
}

ComputeDispersion <- function(x) {
  ## x is a vector of values
  metric <- c("sd"    = sd(x, na.rm=TRUE),
              "IQR"   = IQR(x, na.rm=TRUE),
              "MAD"   = mad(x, na.rm=TRUE))
  data.frame(metric=factor(names(metric), names(metric)), value=metric)
}

ComputeCentral(x = relatives$NOx_relative)

?mean()

names(RS_grid_0.1_spated)
summary(RS_grid_0.1_spated)

NOX_out <- RS_grid_0.1_spated %>% 
  dplyr::filter(NOx_diff > -37 & NOx_diff < 10) %>% 
  dplyr::select(NOx, NOx_edgar_f) %>%
  dplyr::mutate(NOx_relative =  ((NOx - NOx_edgar_f)/NOx_edgar_f)*100)

summary(NOX_out)



NOX_out <- RS_grid_0.1_spated %>% 
  dplyr::arrange(., NOx_diff) %>%
  dplyr::select(NOx, NOx_edgar_f, NOx_diff) %>%
  dplyr::slice(100:955) %>%
  dplyr::mutate(NOx_relative =  ((NOx - NOx_edgar_f)/NOx_edgar_f)*100)

summary(NOX_out)


mapview(NOX_out, zcol = "NOx_diff")
summary(RS_grid_0.1_spated$NOx_relative)

dim(NOX_out)

head(NOX_out$NOx_diff, 60)  
hist(RS_grid_0.1_spated$NOx_diff, breaks = 200)

SO2_out <- RS_grid_0.1_spated %>% 
  dplyr::filter(SO2_diff > -9 & SO2_diff < 6) %>% 
  dplyr::select(SO2, SO2_edgar_f) %>%
  dplyr::mutate(SO2_relative =  ((SO2 - SO2_edgar_f)/SO2_edgar_f)*100)

summary(SO2_out)



NOX_out







# -----------------------------------------------------------------------------------------------------------
# Diff Maps
# -----------------------------------------------------------------------------------------------------------


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
library(ggspatial)


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


sf_data <- RS_grid_0.1_spated



classes.NOx <- classIntervals(sf_data$NOx_diff, n = 12, style = "fisher")
classes.SO2 <- classIntervals(sf_data$SO2_diff, n = 12, style = "fisher")
classes.PM10 <- classIntervals(sf_data$PM10_diff, n = 12, style = "fisher")
classes.PM2.5 <- classIntervals(sf_data$PM2.5_diff, n = 12, style = "fisher")
classes.NMVOC <- classIntervals(sf_data$NMVOC_diff, n = 12, style = "fisher")
classes.NH3 <- classIntervals(sf_data$NH3_diff, n = 12, style = "fisher")

sf_data <- sf_data %>%
  mutate(percent_class_NOx = cut(NOx_diff, classes.NOx$brks, include.lowest = T,dig.lab=5),
    percent_class_SO2 = cut(SO2_diff, classes.SO2$brks, include.lowest = T,dig.lab=7),
    percent_class_PM10 = cut(PM10_diff, classes.PM10$brks, include.lowest = T,dig.lab=5),
    percent_class_PM2.5 = cut(PM2.5_diff, classes.PM2.5$brks, include.lowest = T,dig.lab=5),
    percent_class_NMVOC = cut(NMVOC_diff, classes.NMVOC$brks, include.lowest = T,dig.lab=5),
    percent_class_NH3 = cut(NH3_diff, classes.NH3$brks, include.lowest = T,dig.lab=5)
  )

pal1 <- viridisLite::viridis(12, direction = -1)
# pal2 <- viridisLite::cividis(40)
# pal3 <- viridisLite::inferno(40)
# pal4 <- viridisLite::magma(40)
# pal5 <- viridisLite::plasma(40)
# pal6 <- viridisLite::viridis(40)

opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)


a <- ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NOx)) +
  scale_fill_manual(values = pal1,
                    name = "NOx [t]") +
  
  labs(x = NULL, y = NULL,
       title = "Pollutant Verification with EDGAR database - NOx",
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

a
# ggsave(plot = a, 
#        filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_NOx.jpg", 
#        width = 30, 
#        height = 30, 
#        units = "cm", 
#        device = "jpeg",
#        dpi=600)


b <- ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_SO2)) +
  scale_fill_manual(values = pal1,
                    name = "SO2 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant Verification with EDGAR database - SO2",
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

b
# ggsave(plot = b, 
#        filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_SO2.jpg", 
#        width = 30, 
#        height = 30, 
#        units = "cm", 
#        device = "jpeg",
#        dpi=600)


c <- ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_PM10)) +
  scale_fill_manual(values = pal1,
                    name = "PM10 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant Verification with EDGAR database - PM10",
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

c

# ggsave(plot = c, 
#        filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_PM10.jpg", 
#        width = 30, 
#        height = 30, 
#        units = "cm", 
#        device = "jpeg",
#        dpi=600)

d <- ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_PM2.5)) +
  scale_fill_manual(values = pal1,
                    name = "PM2.5 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant Verification with EDGAR database - PM2.5",
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

d

# ggsave(plot = d, 
#        filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_PM2.5.jpg", 
#        width = 30, 
#        height = 30, 
#        units = "cm", 
#        device = "jpeg",
#        dpi=600)

e <- ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NMVOC)) +
  scale_fill_manual(values = pal1,
                    name = "NMVOC [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant Verification with EDGAR database - NMVOC",
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

e

# ggsave(plot = e, 
#        filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_NMVOC.jpg", 
#        width = 30, 
#        height = 30, 
#        units = "cm", 
#        device = "jpeg",
#        dpi=600)

f <- ggplot() +
  geom_sf(data = sf_data,
          aes(fill = percent_class_NH3)) +
  scale_fill_manual(values = pal1,
                    name = "NH3 [t]") +
  labs(x = NULL, y = NULL,
       title = "Pollutant Verification with EDGAR database - NH3 ",
       subtitle = "Spatial resolution 0.1°x0.1°, Teritory of the Republic of Serbia",
       caption = "UBFCE (2021)") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank()) +
  geom_sf(data = sf_opstine, fill = NA, colour = "black", lwd = 0.6)+
  coord_sf(datum = sf::st_crs(4326))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

f

# ggsave(plot = f, 
#        filename = "CEIP_Grid_0.1/Predaja/Maps/Serbia_RF2015_0.1deg_NH3.jpg", 
#        width = 30, 
#        height = 30, 
#        units = "cm", 
#        device = "jpeg",
#        dpi=600)



grid.arrange(a, b, c, d, e, f, ncol = 3, nrow = 2)










