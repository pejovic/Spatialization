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
library(nngeo)
# devtools::install_github("basarabam/SerbianCyrLat")

Sys.setlocale(locale = 'Serbian (Latin)')

# ::::::::::::::::::::::::::::::::::
# Opstine + indikator urbana-ruralna
# ::::::::::::::::::::::::::::::::::

opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

opstine_ind <- readOGR("Data/opstine/tfr_mac_bom.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine_ind <- st_as_sf(opstine_ind)

sf_opstine$ind <- sf_opstine_ind$type[match(sf_opstine$NAME_2, sf_opstine_ind$Name_Mun)]

sf_opstine$ind[sf_opstine$NAME_2 == "Kosjerić"] <- 0
sf_opstine$ind[sf_opstine$NAME_2 == "Novi Sad"] <- 1
sf_opstine$ind[sf_opstine$NAME_2 == "Surčin"] <- 1
sf_opstine$ind[sf_opstine$NAME_2 == "Kragujevac"] <- 1
sf_opstine$ind[sf_opstine$NAME_2 == "Žitorađa"] <- 0

mapview(sf_opstine, zcol = "ind")

# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
# Opstine + broj registrovanih vozila 2015 godine
# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;

reg_vozila <- readxl::read_xls(path = "Data/Reg_vozila_2015.xls", sheet = "Saobracaj3")

lcl(loc = "C")
reg_vozila <- cyr_lat(reg_vozila)
names(reg_vozila)
names(reg_vozila) <- cyr_lat(names(reg_vozila)) 

reg_vozila <- reg_vozila %>%
  mutate(Mopedi = as.numeric(Mopedi),
         Motocikli = as.numeric(Motocikli),
         `Putnički automobili` = as.numeric(`Putnički automobili`),
         Autobusi = as.numeric(Autobusi),
         `Teretna vozila` = as.numeric(`Teretna vozila`),
         `Radna vozila` = as.numeric(`Radna vozila`),
         `Drumski tegljači` = as.numeric(`Drumski tegljači`),
         `Priključna vozila` = as.numeric(`Priključna vozila`))

# Replace Na with 0
reg_vozila <- reg_vozila %>%
  mutate_all(~replace_na(., 0))
# Suma broja vozila po opstinama
reg_vozila$Broj_reg_vozila <- rowSums(reg_vozila[,2:9]) 

reg_vozila$Opština[reg_vozila$Opština == "Indjija"] <- "Inđija"
reg_vozila$Opština[reg_vozila$Opština == "LJubovija"] <- "Ljubovija"
reg_vozila$Opština[reg_vozila$Opština == "Mali Idjoš"] <- "Mali Iđoš"
reg_vozila$Opština[reg_vozila$Opština == "Savski venac"] <- "Savski Venac"
reg_vozila$Opština[reg_vozila$Opština == "Stari grad"] <- "Stari Grad"
reg_vozila$Opština[reg_vozila$Opština == "Petrovac na Mlavi"] <- "Petrovac"
reg_vozila$Opština[reg_vozila$Opština == "Arandjelovac"] <- "Aranđelovac"
reg_vozila$Opština[reg_vozila$Opština == "LJig"] <- "Ljig"
reg_vozila$Opština[reg_vozila$Opština == "Žitoradja"] <- "Žitorađa"
reg_vozila$Opština[reg_vozila$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_reg_vozila <- reg_vozila$Broj_reg_vozila[match(sf_opstine$NAME_2, reg_vozila$Opština)]

mapview(sf_opstine, zcol = "Br_reg_vozila")

# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
# Brojaci i putevi
# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
brojaci <- readOGR("Data/brojaci/Polozaj_automatskih_brojaca_bez_duplikata.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8",
                   stringsAsFactors = FALSE)
sf_brojaci <- st_as_sf(brojaci) %>% mutate_at(vars(starts_with("PGDS")), .funs = as.numeric)

putevi <- readOGR("Data/putevi/Saobracajne_deonice_i_odseci_sa_brojaca.shp", 
                       use_iconv=TRUE,  
                       encoding = "UTF-8",
                       stringsAsFactors = FALSE)
sf_putevi <- st_as_sf(putevi)

pIA <- readOGR("Data/putevi/pIA_bez_duplikata.shp", 
                  use_iconv=TRUE,  
                  encoding = "UTF-8",
                  stringsAsFactors = FALSE)

pIA <- st_as_sf(pIA) %>%
  select(-PGDS_2015)

mapview(sf_putevi, zcol = "Kategorija") + mapview(sf_brojaci, zcol = "Kategorija")

# Kategorije puteva i brojaca
unique(sf_putevi$Kategorija)
unique(sf_brojaci$Kategorija)

# Putevi
#pIA <- subset(sf_putevi, Kategorija == "IA") %>%
#  st_transform(crs = "+init=epsg:32634") 
pIIA <- subset(sf_putevi, Kategorija == "IIA") %>%
  st_transform(crs = "+init=epsg:32634") 
pIB <- subset(sf_putevi, Kategorija == "IB") %>%
  st_transform(crs = "+init=epsg:32634") 
pIIB <- subset(sf_putevi, Kategorija == "IIB") %>%
  st_transform(crs = "+init=epsg:32634") 

# Brojaci
bIA <- subset(sf_brojaci, Kategorija == "IA") %>%
  st_transform(crs = "+init=epsg:32634")
bIIA <- subset(sf_brojaci, Kategorija == "IIA") %>%
  st_transform(crs = "+init=epsg:32634")
bIB <- subset(sf_brojaci, Kategorija == "IB") %>%
  st_transform(crs = "+init=epsg:32634")
bIIB <- subset(sf_brojaci, Kategorija == "IIB") %>%
  st_transform(crs = "+init=epsg:32634")
bostalo <- subset(sf_brojaci, Kategorija == "nije u mrezi" | Kategorija == "-") %>%
  st_transform(crs = "+init=epsg:32634")

# Buffer-i
buf_bIA <- st_buffer(bIA$geometry, dist = 100)
buf_bIIA <- st_buffer(bIIA$geometry, dist = 100)
buf_bIB <- st_buffer(bIB$geometry, dist = 100)
buf_bIIB <- st_buffer(bIIB$geometry, dist = 100)
buf_bostalo <- st_buffer(bostalo$geometry, dist = 100)


mapview(pIA, zcol = "Kategorija") + 
  mapview(bIA, zcol = "Kategorija") + 
  mapview(buf_bIA, col.regions = "red")

mapview(pIIA, zcol = "Kategorija") + 
  mapview(sf_grid, col.regions = "red", legend = F)+
  mapview(bIIA, zcol = "Kategorija") + 
  mapview(buf_bIIA, col.regions = "red")

mapview(pIB, zcol = "Kategorija") + 
  mapview(bIB, zcol = "Kategorija") + 
  mapview(buf_bIB, col.regions = "red")

mapview(pIIB, zcol = "Kategorija") + 
  mapview(bIIB, zcol = "Kategorija") + 
  mapview(buf_bIIB, col.regions = "red") + 
  mapview(bostalo, zcol = "Kategorija") + 
  mapview(buf_bostalo, col.regions = "orange")


# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
# CLC i urbana podrucja
# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;

clc_12 <- readOGR("Data/clc/CLC12_RS.shp")
sf_clc12 <- st_as_sf(clc_12)

clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)

sf_clc12_urb <- subset(sf_clc12, CODE_12 == "111" | CODE_12 == "112")

sf_clc18_urb <- subset(sf_clc18, CODE_18 == "111" | CODE_18 == "112") %>%
  st_transform(crs = "+init=epsg:32634")

clc121 <- subset(sf_clc18, CODE_18 == "121") %>%
  st_transform(crs = "+init=epsg:32634")

mapview(clc121, zcol = "CODE_18")


# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
# GRID_5km
# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;

grid <- readOGR("Grid/Polygons_5km_UTM_34N.shp")
sf_grid <- st_as_sf(grid)

mapview(sf_grid, col.regions = "red", legend = F)


# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
# Mid_points deoinca
# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
mapview(buf_bIA) + mapview(bIA)

buf_bIA <- st_join(st_sf(buf_bIA), bIA, join = st_intersects) %>% dplyr::select(PGDS_2015 = PGDS_2015_) %>% mutate_at(.vars = "PGDS_2015", .funs = as.numeric)
buf_bIIA <- st_join(st_sf(buf_bIIA), bIIA, join = st_intersects) %>% dplyr::select(PGDS_2015 = PGDS_2015_) %>% mutate_at(.vars = "PGDS_2015", .funs = as.numeric)
buf_bIB <- st_join(st_sf(buf_bIB), bIB, join = st_intersects) %>% dplyr::select(PGDS_2015 = PGDS_2015_) %>% mutate_at(.vars = "PGDS_2015", .funs = as.numeric)
buf_bIIB <- st_join(st_sf(buf_bIIB), bIIB, join = st_intersects) %>% dplyr::select(PGDS_2015 = PGDS_2015_) %>% mutate_at(.vars = "PGDS_2015", .funs = as.numeric)

pIA <- st_join(pIA, buf_bIA, join = st_intersects)
pIIA <- st_join(pIIA, buf_bIIA, join = st_intersects)
pIB <- st_join(pIB, buf_bIB, join = st_intersects)
pIIB <- st_join(pIIB, buf_bIIB, join = st_intersects)

# Mid points

st_line_midpoints <- function(sf_lines = NULL) {
  
  g <- st_geometry(sf_lines)
  
  g_mids <- lapply(g, function(x) {
    
    coords <- as.matrix(x)
    
    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }
    
    mids <- st_point(get_mids(coords))
  })
  
  out <- st_sfc(g_mids, crs = st_crs(sf_lines))
  out <- st_sf(out)
}


pIA_midp <- st_line_midpoints(pIA) %>%
  st_join(., pIA, dist = 1, join = st_is_within_distance) %>%
  select(PGDS_2015_) %>%
  rename(PGDS_2015 = PGDS_2015_) %>%
  mutate(PGDS_2015 = as.numeric(PGDS_2015)) %>%
  mutate_all(~replace_na(., 0))

pIIA_midp <- st_line_midpoints(pIIA) %>% 
  st_join(., pIIA, dist = 1, join = st_is_within_distance) %>%
  select(PGDS_2015_) %>%
  rename(PGDS_2015 = PGDS_2015_)%>%
  mutate(PGDS_2015 = as.numeric(PGDS_2015)) %>%
  mutate_all(~replace_na(., 0))

pIB_midp <- st_line_midpoints(pIB) %>% 
  st_join(., pIB, dist = 1, join = st_is_within_distance)  %>%
  select(PGDS_2015_) %>%
  rename(PGDS_2015 = PGDS_2015_) %>%
  mutate(PGDS_2015 = as.numeric(PGDS_2015)) %>%
  mutate_all(~replace_na(., 0))

pIIB_midp <- st_line_midpoints(pIIB) %>% 
  st_join(., pIIB, dist = 1, join = st_is_within_distance) %>%
  select(PGDS_2015_) %>%
  rename(PGDS_2015 = PGDS_2015_)%>%
  mutate(PGDS_2015 = as.numeric(PGDS_2015)) %>%
  mutate_all(~replace_na(., 0))

# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
# PGDS deonica puteva
# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
lcl(loc = "C")

pgds.deonica.pIA <- readxl::read_xls(path = "Data/PGDS_deonica_puteva_2015.xls", sheet = "IA") %>%
  cyr_lat()
pgds.deonica.pIIA <- readxl::read_xls(path = "Data/PGDS_deonica_puteva_2015.xls", sheet = "IIA") %>%
  cyr_lat()
pgds.deonica.pIB <- readxl::read_xls(path = "Data/PGDS_deonica_puteva_2015.xls", sheet = "IB") %>%
  cyr_lat()
pgds.deonica.pIIB <- readxl::read_xls(path = "Data/PGDS_deonica_puteva_2015.xls", sheet = "IIB") %>%
  cyr_lat()

names(pgds.deonica.pIA) <- cyr_lat(names(pgds.deonica.pIA)) 
names(pgds.deonica.pIIA) <- cyr_lat(names(pgds.deonica.pIIA)) 
names(pgds.deonica.pIB) <- cyr_lat(names(pgds.deonica.pIB)) 
names(pgds.deonica.pIIB) <- cyr_lat(names(pgds.deonica.pIIB)) 

pIA$PGDS_2015_deonica <- pgds.deonica.pIA$PGDS[match(pIA$Oznaka_deo, pgds.deonica.pIA$`Oznaka deonice`)]
pIIA$PGDS_2015_deonica <- pgds.deonica.pIIA$PGDS[match(pIIA$Oznaka_deo, pgds.deonica.pIIA$`Oznaka deonice`)]
pIB$PGDS_2015_deonica <- pgds.deonica.pIB$PGDS[match(pIB$Oznaka_deo, pgds.deonica.pIB$`Oznaka deonice`)]
pIIB$PGDS_2015_deonica <- pgds.deonica.pIIB$PGDS[match(pIIB$Oznaka_deo, pgds.deonica.pIIB$`Oznaka deonice`)]

# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
# Saobracajni cvorovi
# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
cvorovi <- readOGR("Data/cvorovi/Saobracajni_cvorovi.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_cvorovi <- st_as_sf(cvorovi) %>%
  st_transform(crs = "+init=epsg:32634") 

mapview(sf_cvorovi) + mapview(pIIA) + mapview(pIB) + mapview(pIIB)

# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
# OSM putevi unutar urbanih podrucja
# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;

#osm_roads <- readOGR("d:/Projekti/Spatialisation/Transport/Data/osm_roads_Serbia_epsg4326.shp", 
#                   use_iconv=TRUE,  
#                   encoding = "UTF-8")
#sf_osm <- st_as_sf(osm_roads) %>%
#  st_transform(crs = "+init=epsg:32634")
#
#sf_osm_urb <- st_intersection(sf_osm, sf_clc18_urb)
#st_write(sf_osm_urb, dsn="Data/putevi/OSM_putevi_urbana_podrucja.gpkg", layer='osm_urb')

osm_urb <- st_read("Data/putevi/OSM_putevi_urbana_podrucja.gpkg")
mapview(osm_urb)


#############################################################################

road.net = pIIA; max.dist = 50000; use.est = TRUE
i = 56

pgds <- function(road.net, max.dist, use.est = FALSE){
  road.net <- road.net %>% mutate(ID = seq(1:dim(road.net)[1]), is.PGDS = !is.na(PGDS_2015), PGDS_2015.est = NA) %>% select(ID, is.PGDS, PGDS_2015.est, everything())
  for(i in 1:dim(road.net)[1]){
    #if(!is.na(road.net$PGDS_2015.est)) next
    #if(road.net$is.PGDS[i]) next
    road.net.nn.ind <- st_nn(road.net[i, ], road.net[!is.na(road.net$PGDS_2015) & road.net$ID != i, ], k = 5, maxdist = max.dist, returnDist = TRUE, progress = FALSE)
    if(identical(road.net.nn.ind$nn[[1]], integer(0))) next
    dists <- data.frame(road.net.nn.ind$dist)[, !is.na(road.net.nn.ind$dist)]
    dists[dists == 0] <- 10^-10
    weigths <- sum(dists)/dists
    weigths <- weigths/sum(weigths)
    road.net$PGDS_2015.est[i] <- road.net[!is.na(road.net$PGDS_2015) & road.net$ID != i, ][road.net.nn.ind$nn[[1]],] %>% st_drop_geometry() %>% .$PGDS_2015 %>% weighted.mean(., na.rm = TRUE, w = weigths)
    if(use.est & !road.net$is.PGDS[i]){
      road.net$PGDS_2015[i] <- road.net$PGDS_2015.est[i]
    }
  }
  return(road.net)
}

rn <- pgds(road.net = pIIA, max.dist = 50000)

save(rn, file = "pIIA_est.RDS")
load(file = "pIIA_est.RDS")

#######################################################################################
rn.IA %<>% mutate_all(~replace_na(., 0))
mapview(rn.IA, zcol = "PGDS_2015.est") + mapview(buf_bIA) + mapview(bIA) + mapview(pIA)

pIA.L <- pIA %>% subset(., Smer == "L")
pIA.D <- pIA %>% subset(., Smer == "D")

#st_write(pIA.L, dsn="Products/pIA_L.gpkg", layer='pIA.L')
#st_write(pIA.D, dsn="Products/pIA_D.gpkg", layer='pIA.D')

#######################################################################################

rn.IA <- pgds(road.net = pIA, max.dist = 50000, use.est = TRUE)
rn.IIA <- pgds(road.net = pIIA, max.dist = 50000, use.est = TRUE)
rn.IB <- pgds(road.net = pIB, max.dist = 50000, use.est = TRUE)
rn.IIB <- pgds(road.net = pIIB, max.dist = 50000, use.est = TRUE)


save(rn.IA, file = "rn_IA.RDS")
save(rn.IIA, file = "rn_IIA.RDS")
save(rn.IB, file = "rn_IB.RDS")
save(rn.IIB, file = "rn_IIB.RDS")

rn.IA$PGDS_2015.est
rn.IIA$PGDS_2015.est
rn.IB$PGDS_2015.est

names(rn.IA)
names(rn.IIA)
rn.IA %<>% select(-fid)
es_pgds <- rbind(rn.IA, rn.IIA, rn.IB)
es_pgds %<>% mutate(lwd  = PGDS_2015.est/1000)

# st_write(es_pgds, dsn="Products/es_pgds.gpkg", layer='es_pgds')

es_pgds %>% dplyr::filter(is.PGDS) %>%
  dplyr::group_by(., Kategorija) %>%
  dplyr::summarise(nn = n(), n.counters = sum(is.PGDS), mape = (1/nn)*(sum(abs(PGDS_2015.est - PGDS_2015)/PGDS_2015)), 
                   RMSE = sqrt((1/nn)*sum((PGDS_2015.est - PGDS_2015)^2)), mean.PGDS = mean(PGDS_2015), mean.PGDS.est = mean(PGDS_2015.est))


# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
# CLC waste klasa
# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)

sf_clc18_waste <- subset(sf_clc18, CODE_18 == "132") %>%
  st_transform(crs = "+init=epsg:32634")


mapview(sf_clc18_waste, zcol = "CODE_18")


# ::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;























































































































