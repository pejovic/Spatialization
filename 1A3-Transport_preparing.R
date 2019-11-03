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
brojaci <- readOGR("Data/brojaci/Polozaj_automatskih_brojaca.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_brojaci <- st_as_sf(brojaci)

putevi <- readOGR("Data/putevi/Saobracajne_deonice_i_odseci_sa_brojaca.shp", 
                       use_iconv=TRUE,  
                       encoding = "UTF-8")
sf_putevi <- st_as_sf(putevi)

mapview(sf_putevi, zcol = "Kategorija") + mapview(sf_brojaci, zcol = "Kategorija")

# Kategorije puteva i brojaca
unique(sf_putevi$Kategorija)
unique(sf_brojaci$Kategorija)

pIA <- subset(sf_putevi, Kategorija == "IA") %>%
  st_transform(crs = "+init=epsg:3857")
pIIA <- subset(sf_putevi, Kategorija == "IIA") %>%
  st_transform(crs = "+init=epsg:3857")
pIB <- subset(sf_putevi, Kategorija == "IB") %>%
  st_transform(crs = "+init=epsg:3857")
pIIB <- subset(sf_putevi, Kategorija == "IIB") %>%
  st_transform(crs = "+init=epsg:3857")

bIA <- subset(sf_brojaci, Kategorija == "IA") %>%
  st_transform(crs = "+init=epsg:3857")
bIIA <- subset(sf_brojaci, Kategorija == "IIA") %>%
  st_transform(crs = "+init=epsg:3857")
bIB <- subset(sf_brojaci, Kategorija == "IB") %>%
  st_transform(crs = "+init=epsg:3857")
bIIB <- subset(sf_brojaci, Kategorija == "IIB") %>%
  st_transform(crs = "+init=epsg:3857")


buf_bIA <- st_buffer(bIA$geometry, dist = 1000)
buf_bIIA <- st_buffer(bIIA$geometry, dist = 1000)
buf_bIB <- st_buffer(bIB$geometry, dist = 1000)
buf_bIIB <- st_buffer(bIIB$geometry, dist = 1000)

mapview(pIIA, zcol = "Kategorija") + mapview(bIIA, zcol = "Kategorija") + mapview(buf_bIIA, col.regions = "red")



