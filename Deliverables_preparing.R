grid.5km <- readOGR("Grid/Polygons_5km_UTM_34N.shp")
sf.grid.5km <- st_as_sf(grid.5km) 
st_write(sf.grid.5km, dsn="Grid/Grid_5km_Serbia.gpkg", layer='Grid_5km_Serbia')


# Temporal profiles all in one
data.tprofiles <- list.files('Hourly_emissions/Products/')

data.temp.list <- list()                                                   
for(i in 1:length(data.tprofiles)){                                             
  data.temp.list[[i]] <- readxl::read_xlsx(path = paste("Hourly_emissions/Products/",data.tprofiles[i], sep = ""))
}


length(data.temp.list[[8]])


temporal_Profiles <- data.temp.list[[1]]$Time %>%
  cbind(
    data.temp.list[[1]][,2:85],
    data.temp.list[[2]][,2:37],
    data.temp.list[[3]][,2:25],
    data.temp.list[[4]][,2:55],
    data.temp.list[[5]][,2:73],
    data.temp.list[[6]][,2:25],
    data.temp.list[[7]][,2:145],
    data.temp.list[[8]][,2:25]
  ) %>%
  as_data_frame()

writexl::write_xlsx(temporal_Profiles, path = 'Hourly_emissions/Products/TemporalProfiles_All_in_one.xlsx')



###########################################################

clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
st_write(sf_clc18, dsn="GIS_layers/CLC_12-18.gpkg", layer='CLC_12-18_complete')

###########################################################

clc131 <- subset(sf_clc18, CODE_18 == "131") %>% # Mine, dump and construction sites
  st_set_crs(32634)

clc131 %<>% dplyr::mutate(Area = st_area(.)) %>%
  dplyr::select(Area)

st_write(clc131, dsn="GIS_layers/CLC_class_131.gpkg", layer='CLC_class_131')


clc131.int <- st_intersection(clc131, sf.grid.5km) %>%
  dplyr::mutate(Area = st_area(.)) %>%
  select(Area)
st_write(clc131.int, dsn="GIS_layers/CLC_class_131_intersected.gpkg", layer='CLC_class_131_intersected')

###########################################################

Sys.setlocale(locale = 'Serbian (Latin)')
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

sf_opstine %<>% select(NAME_2, ind) %>%
  st_transform(crs = "+init=epsg:32634") %>%
  dplyr::rename(Name = NAME_2) 
  
sf_opstine %<>% dplyr::mutate(Area = st_area(.))

st_write(sf_opstine, dsn="GIS_layers/Municipalities.gpkg", layer='Municipalities')


#+ include = FALSE
stanovnistvo <- readxl::read_xls(path = "Data/Stanovnistvo_2015.xls", sheet = "OpstiPodaci")
lcl(loc = "C")
stanovnistvo <- cyr_lat(stanovnistvo)
names(stanovnistvo) <- cyr_lat(names(stanovnistvo)) 
stanovnistvo <- stanovnistvo %>%
  mutate_all(~replace_na(., 0))
stanovnistvo$Opština[stanovnistvo$Opština == "Indjija"] <- "Inđija"
stanovnistvo$Opština[stanovnistvo$Opština == "LJubovija"] <- "Ljubovija"
stanovnistvo$Opština[stanovnistvo$Opština == "Mali Idjoš"] <- "Mali Iđoš"
stanovnistvo$Opština[stanovnistvo$Opština == "Savski venac"] <- "Savski Venac"
stanovnistvo$Opština[stanovnistvo$Opština == "Stari grad"] <- "Stari Grad"
stanovnistvo$Opština[stanovnistvo$Opština == "Petrovac na Mlavi"] <- "Petrovac"
stanovnistvo$Opština[stanovnistvo$Opština == "Arandjelovac"] <- "Aranđelovac"
stanovnistvo$Opština[stanovnistvo$Opština == "LJig"] <- "Ljig"
stanovnistvo$Opština[stanovnistvo$Opština == "Žitoradja"] <- "Žitorađa"
stanovnistvo$Opština[stanovnistvo$Opština == "Medvedja"] <- "Medveđa"
sf_opstine$Br_stanovnistvo <- stanovnistvo$Stanovništvo[match(sf_opstine$Name, stanovnistvo$Opština)]

st_write(sf_opstine, dsn="GIS_layers/Municipalities_Number_of_habitants.gpkg", layer='Municipalities_Number_of_habitants')



sf_opstine.int <- st_intersection(sf_opstine, sf.grid.5km)%>%
  dplyr::mutate(Area = st_area(.)) %>%
  dplyr::rename(No_habitants = Br_stanovnistvo)
st_write(sf_opstine.int, dsn="GIS_layers/Municipalities_Number_of_habitants_intersected.gpkg", layer='Municipalities_Number_of_habitants_intersected')

###########################################################

load("./Data/Putevi/rn_IIA.RDS")
load("./Data/Putevi/rn_IB.RDS")

rn.IB 
rn.IIA
rn.IB <- dplyr::mutate(rn.IB, PGDS = PGDS_2015.est)
rn.IB$PGDS[rn.IB$is.PGDS] <- rn.IB$PGDS_2015[rn.IB$is.PGDS]

rn.IIA <- dplyr::mutate(rn.IIA, PGDS = PGDS_2015.est)
rn.IIA$PGDS[rn.IIA$is.PGDS] <- rn.IIA$PGDS_2015[rn.IIA$is.PGDS]


st_write(rn.IB, dsn="GIS_layers/Roads_with_PGDS2015_IB.gpkg", layer='Roads_with_PGDS2015_IB')
st_write(rn.IIA, dsn="GIS_layers/Roads_with_PGDS2015_IIA.gpkg", layer='Roads_with_PGDS2015_IIA')

sf_IB.int <- st_intersection(rn.IB, sf.grid.5km) %>%
  dplyr::select(., PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(., Length, PGDSL,PGDS)

sf_IIA.int <- st_intersection(rn.IIA, sf.grid.5km) %>%
  dplyr::select(., PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(., Length, PGDSL, PGDS)

st_write(sf_IB.int, dsn="GIS_layers/Roads_with_PGDS2015_IB_intersected.gpkg", layer='Roads_with_PGDS2015_IB_intersected')
st_write(sf_IIA.int, dsn="GIS_layers/Roads_with_PGDS2015_IIA_intersected.gpkg", layer='Roads_with_PGDS2015_IIA_intersected')



load("./Data/Putevi/rn_IA.RDS")
rn.IA
rn.IA <- dplyr::mutate(rn.IA, PGDS = PGDS_2015.est)
rn.IA$PGDS[rn.IA$is.PGDS] <- rn.IA$PGDS_2015[rn.IA$is.PGDS] 

rn.IA %<>% dplyr::select(-fid)

st_write(rn.IA, dsn="GIS_layers/Roads_with_PGDS2015_IA.gpkg", layer='Roads_with_PGDS2015_IA')

sf_IA.int <- st_intersection(rn.IA, sf.grid.5km) %>%
  dplyr::select(., PGDS) %>%
  mutate(Length = st_length(.), PGDSL = PGDS*Length/1000) %>%
  dplyr::select(., Length, PGDSL, PGDS)

st_write(sf_IA.int, dsn="GIS_layers/Roads_with_PGDS2015_IA_intersected.gpkg", layer='Roads_with_PGDS2015_IA_intersected')



###########################################################


urban_roads <- readOGR("Data/putevi/OSM_putevi_urbana_podrucja.gpkg", 
                       use_iconv=TRUE,  
                       encoding = "UTF-8",
                       stringsAsFactors = FALSE)
sf_urban_roads <- st_as_sf(urban_roads) %>%
  st_transform(crs = "+init=epsg:32634") 

sf_urban_roads %<>% dplyr::mutate(Length = st_length(.)) %>% 
  dplyr::select(Length)



st_write(sf_urban_roads, dsn="GIS_layers/Roads_in_urban_areas.gpkg", layer='Roads_in_urban_areas')


sf_urban_roads.int <- st_intersection(sf_urban_roads, sf.grid.5km) %>%
  mutate(Length = st_length(.)) %>%
  select(Length) 


st_write(sf_urban_roads.int, dsn="GIS_layers/Roads_in_urban_areas_intersected.gpkg", layer='Roads_in_urban_areas_intersected')


###########################################################


railways <- readOGR("Data/pruge/Pruge_osm_32634.shp", 
                    use_iconv=TRUE,  
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)
sf_railways <- st_as_sf(railways) %>%
  st_transform(crs = "+init=epsg:32634") 


sf_railways %<>% mutate(Length = st_length(.)) %>%
  select(Length)
st_write(sf_railways, dsn="GIS_layers/Railways.gpkg", layer='Railways')


sf_railways.int <- st_intersection(sf_railways, sf.grid.5km) %>%
  mutate(Length = st_length(.))%>%
  select(Length) 
st_write(sf_railways.int, dsn="GIS_layers/Railways_intersected.gpkg", layer='Railways_intersected')

###########################################################

n.navigation <- readOGR("Data/plovni_putevi/Plovni_putevi_32634.shp", 
                        use_iconv=TRUE,  
                        encoding = "UTF-8",
                        stringsAsFactors = FALSE)
sf_navigation <- st_as_sf(n.navigation) %>%
  st_transform(crs = "+init=epsg:32634")

sf_navigation %<>% dplyr::mutate(Area = st_area(.)) %>%
  dplyr::select(Area)

st_write(sf_navigation, dsn="GIS_layers/Navigable_rivers.gpkg", layer='Navigable_rivers')



sf_navigation.int <- st_intersection(sf_navigation, sf.grid.5km) %>%
  dplyr::mutate(Area = st_area(.)) %>%
  dplyr::select(Area)
st_write(sf_navigation, dsn="GIS_layers/Navigable_rivers_intersected.gpkg", layer='Navigable_rivers_intersected')


###########################################################

clc_18 <- readOGR("Data/clc/CLC18_RS.shp")
sf_clc18 <- st_as_sf(clc_18)
sf_clc18_urb <- subset(sf_clc18, CODE_18 == "111" | CODE_18 == "112") %>% # CLC urban zones
  st_transform(crs = "+init=epsg:32634") %>%
  dplyr::mutate(Area = st_area(.)) %>%
  dplyr::select(Area)

st_write(sf_clc18_urb, dsn="GIS_layers/Urban_areas.gpkg", layer='Urban_areas')

sf_clc18_urb.int <- st_intersection(sf_clc18_urb, sf.grid.5km) %>%
  dplyr::mutate(Area = st_area(.)) %>%
  dplyr::select(Area)

st_write(sf_clc18_urb.int, dsn="GIS_layers/Urban_areas_intersected.gpkg", layer='Urban_areas_intersected')

#+ include = FALSE
Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
toplane <- readxl::read_xls(path = "Data/toplane/Toplane_2015.xls") %>%
  mutate(GRAD = str_to_title(GRAD))

sf_opstine$Toplane <- toplane$`Ukupna grejna povrsina (m2)`[match(sf_opstine$NAME_2, toplane$GRAD)] 

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634") %>% 
  mutate_all(~replace(., is.na(.), 0))

sf_clc18_urb <- st_join(sf_clc18_urb, sf_opstine, largest = TRUE) 

sf_clc18_urb %<>% dplyr::select(.,Toplane, NAME_2) %>%
  dplyr::rename(Heating_area = Toplane, Name = NAME_2)

st_write(sf_clc18_urb, dsn="GIS_layers/Urban_areas_Toplane_Srbije_Heating_area.gpkg", layer='Urban_areas_Toplane_Srbije_Heating_area')



sf_clc18_urb.int <- st_intersection(sf_clc18_urb, sf.grid.5km) %>% 
  filter(!is.na(Heating_area)) 

st_write(sf_clc18_urb.int, dsn="GIS_layers/Urban_areas_Toplane_Srbije_Heating_area_intersected.gpkg", layer='Urban_areas_Toplane_Srbije_Heating_area_intersected')

###########################################################

sf_rur <- st_read(dsn = "Products/rural_areas.gpkg", layer = "rural_areas")

sf_rur %<>% dplyr::mutate(Area = st_area(.)) %>%
  select(NAME_2, Area) %>%
  dplyr::rename(Name = NAME_2)


st_write(sf_rur, dsn="GIS_layers/Rural_areas.gpkg", layer='Rural_areas')

sf_rur.int <- st_intersection(sf_rur, sf.grid.5km) %>%
  dplyr::mutate(Area = st_area(.)) %>%
  dplyr::select(Name, Area)

st_write(sf_rur.int, dsn="GIS_layers/Rural_areas_intersected.gpkg", layer='Rural_areas_intersected')




sf_rur <- st_read(dsn = "Products/rural_areas.gpkg", layer = "rural_areas")

sf_rur %<>% dplyr::mutate(Area = st_area(.)) %>%
  select(NAME_2, Area, Br_traktori) %>%
  dplyr::rename(Name = NAME_2, No_tractors = Br_traktori)


st_write(sf_rur, dsn="GIS_layers/Rural_areas_with_Number_of_tractors.gpkg", layer='Rural_areas_with_Number_of_tractors')

sf_rur.int <- st_intersection(sf_rur, sf.grid.5km) %>%
  dplyr::mutate(Area = st_area(.)) %>%
  dplyr::select(Name, Area, No_tractors) %>%
  mutate_all(~replace(., is.na(.), 0))

st_write(sf_rur.int, dsn="GIS_layers/Rural_areas_with_Number_of_tractors_intersected.gpkg", layer='Rural_areas_with_Number_of_tractors_intersected')

###########################################################

sf_clc18_polj <- subset(sf_clc18, CODE_18 == "211" | CODE_18 == "221" | CODE_18 == "222" | CODE_18 == "231" | CODE_18 == "242" | CODE_18 == "243") %>% # CLC agricultural areas
  st_transform(crs = "+init=epsg:32634")

sf_clc18_polj %<>% dplyr::mutate(Area = st_area(.)) %>%
  select(Area)
st_write(sf_clc18_polj, dsn="GIS_layers/Agricultural_areas.gpkg", layer='Agricultural_areas')


sf_clc18_polj.int <- st_intersection(sf_clc18_polj, sf.grid.5km)

sf_clc18_polj.int %<>% dplyr::mutate(Area = st_area(.)) %>%
  select(Area)
st_write(sf_clc18_polj.int, dsn="GIS_layers/Agricultural_areas_intersected.gpkg", layer='Agricultural_areas_intersected')

###########################################################

clc133 <- subset(sf_clc18, CODE_18 == "133" | CODE_18 == "121") %>% # Construction sites and industrial sites
  st_set_crs(32634)
clc133 %<>% dplyr::mutate(Area = st_area(.)) %>%
  select(Area)

st_write(clc133, dsn="GIS_layers/Construction_and_industrial_sites.gpkg", layer='Construction_and_industrial_sites')


clc133.int <- st_intersection(clc133, sf.grid.5km) %>%
  dplyr::mutate(Area = st_area(.)) %>%
  select(Area)

st_write(clc133.int, dsn="GIS_layers/Construction_and_industrial_sites_intersected.gpkg", layer='Construction_and_industrial_sites_intersected')



###########################################################


clc121 <- subset(sf_clc18, CODE_18 == "121") %>% # Industrial sites
  st_transform(32634) # Transform to UTM projection

clc121 %<>% dplyr::mutate(Area = st_area(.)) %>%
  select(Area)
st_write(clc121, dsn="GIS_layers/Industrial_sites.gpkg", layer='Industrial_sites')



clc121.int <- st_intersection(clc121, sf.grid.5km) %>%
  dplyr::mutate(Area = st_area(.)) %>%
  select(Area)

st_write(clc121.int, dsn="GIS_layers/Industrial_sites_intersected.gpkg", layer='Industrial_sites_intersected')

###########################################################

Sys.setlocale(locale = 'Serbian (Latin)') # Reading data
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = TRUE, message = FALSE, warning = FALSE
otpadne_vode <- readxl::read_xls(path = "Data/Vodosnabdevanje_2015.xls", sheet = "Vodosnabdevanje")
lcl(loc = "C")
otpadne_vode <- cyr_lat(otpadne_vode)
names(otpadne_vode) <- cyr_lat(names(otpadne_vode)) 
otpadne_vode <- otpadne_vode %>%
  mutate_all(~replace_na(., 0))
otpadne_vode$Opština[otpadne_vode$Opština == "Indjija"] <- "Inđija"
otpadne_vode$Opština[otpadne_vode$Opština == "LJubovija"] <- "Ljubovija"
otpadne_vode$Opština[otpadne_vode$Opština == "Mali Idjoš"] <- "Mali Iđoš"
otpadne_vode$Opština[otpadne_vode$Opština == "Savski venac"] <- "Savski Venac"
otpadne_vode$Opština[otpadne_vode$Opština == "Stari grad"] <- "Stari Grad"
otpadne_vode$Opština[otpadne_vode$Opština == "Petrovac na Mlavi"] <- "Petrovac"
otpadne_vode$Opština[otpadne_vode$Opština == "Arandjelovac"] <- "Aranđelovac"
otpadne_vode$Opština[otpadne_vode$Opština == "LJig"] <- "Ljig"
otpadne_vode$Opština[otpadne_vode$Opština == "Žitoradja"] <- "Žitorađa"
otpadne_vode$Opština[otpadne_vode$Opština == "Medvedja"] <- "Medveđa"
sf_opstine$Otpadne_vode <- otpadne_vode$`Ukupne ispuštene otpadne vode`[match(sf_opstine$NAME_2, otpadne_vode$Opština)] # Matching data 

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634")
sf_opstine %<>% dplyr::select(.,Otpadne_vode, NAME_2) %>%
  dplyr::rename(Waste_water = Otpadne_vode, Name = NAME_2)


st_write(sf_opstine, dsn="GIS_layers/Municipalities_Waste_water.gpkg", layer='Municipalities_Waste_water')

sf_opstine.int <- st_intersection(sf_opstine, sf.grid.5km) %>%
  select(Waste_water, Name) 

st_write(sf_opstine.int, dsn="GIS_layers/Municipalities_Waste_water_intersected.gpkg", layer='Municipalities_Waste_water_intersected')

###########################################################

clc132 <- subset(sf_clc18, CODE_18 == "132") %>% # Dump sites
  st_transform(32634)

clc132 %<>% dplyr::mutate(Area = st_area(.)) %>%
  select(Area) 

st_write(clc132, dsn="GIS_layers/Dump_sites.gpkg", layer='Dump_sites')



clc132.int <- st_intersection(clc132, sf.grid.5km) %>% dplyr::mutate(Area = st_area(.)) %>%
  select(Area)
st_write(clc132.int, dsn="GIS_layers/Dump_sites_intersected.gpkg", layer='Dump_sites_intersected')

###########################################################


sf_clc18_242 <- subset(sf_clc18, CODE_18 == "242") %>% # CLC complex cultivated areas (including farms)
  st_transform(crs = "+init=epsg:32634") %>%
  dplyr::mutate(Area = st_area(.)) %>%
  select(Area) 
st_write(sf_clc18_242, dsn="GIS_layers/Complex_cultivated_areas.gpkg", layer='complex_cultivated_areas')


sf_clc18_242.int <- st_intersection(sf_clc18_242, sf.grid.5km) %>% dplyr::mutate(Area = st_area(.)) %>%
  select(Area)

st_write(sf_clc18_242.int, dsn="GIS_layers/Complex_cultivated_areas_intersected.gpkg", layer='complex_cultivated_areas_intersected')


###########################################################

sf_clc18_pasnjaci <- subset(sf_clc18, CODE_18 == "231") %>% # CLC pastures
  st_transform(crs = "+init=epsg:32634")%>%
  dplyr::mutate(Area = st_area(.)) %>%
  select(Area) 

st_write(sf_clc18_pasnjaci, dsn="GIS_layers/Pastures.gpkg", layer='Pastures')


sf_clc18_pasnjaci.int <- st_intersection(sf_clc18_pasnjaci, sf.grid.5km)

st_write(sf_clc18_pasnjaci.int, dsn="GIS_layers/Pastures_intersected.gpkg", layer='Pastures_intersected')


###########################################################

zivina <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
zivina <- cyr_lat(zivina)
names(zivina) <- cyr_lat(names(zivina)) 
zivina <- zivina %>%
  mutate_all(~replace_na(., 0))
zivina$Opština[zivina$Opština == "Indjija"] <- "Inđija"
zivina$Opština[zivina$Opština == "LJubovija"] <- "Ljubovija"
zivina$Opština[zivina$Opština == "Mali Idjoš"] <- "Mali Iđoš"
zivina$Opština[zivina$Opština == "Savski venac"] <- "Savski Venac"
zivina$Opština[zivina$Opština == "Stari grad"] <- "Stari Grad"
zivina$Opština[zivina$Opština == "Petrovac na Mlavi"] <- "Petrovac"
zivina$Opština[zivina$Opština == "Arandjelovac"] <- "Aranđelovac"
zivina$Opština[zivina$Opština == "LJig"] <- "Ljig"
zivina$Opština[zivina$Opština == "Žitoradja"] <- "Žitorađa"
zivina$Opština[zivina$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_zivina <- zivina$Živina[match(sf_opstine$Name, zivina$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634") %>%
  select(Name, Br_zivina) %>%
  rename(No_polutry = Br_zivina)


st_write(sf_opstine, dsn="GIS_layers/Municipalities_Number_of_polutry.gpkg", layer='Municipalities_Number_of_polutry')

sf_opstine.int <- st_intersection(sf_opstine, sf.grid.5km) %>%
  select(No_polutry, Name) 

st_write(sf_opstine.int, dsn="GIS_layers/Municipalities_Number_of_polutry_intersected.gpkg", layer='Municipalities_Number_of_polutry_intersected')


###########################################################

konji <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
konji <- cyr_lat(konji)
names(konji) <- cyr_lat(names(konji)) 
konji <- konji %>%
  mutate_all(~replace_na(., 0))
konji$Opština[konji$Opština == "Indjija"] <- "Inđija"
konji$Opština[konji$Opština == "LJubovija"] <- "Ljubovija"
konji$Opština[konji$Opština == "Mali Idjoš"] <- "Mali Iđoš"
konji$Opština[konji$Opština == "Savski venac"] <- "Savski Venac"
konji$Opština[konji$Opština == "Stari grad"] <- "Stari Grad"
konji$Opština[konji$Opština == "Petrovac na Mlavi"] <- "Petrovac"
konji$Opština[konji$Opština == "Arandjelovac"] <- "Aranđelovac"
konji$Opština[konji$Opština == "LJig"] <- "Ljig"
konji$Opština[konji$Opština == "Žitoradja"] <- "Žitorađa"
konji$Opština[konji$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$No_farms <- konji$`Broj gazdinstava`[match(sf_opstine$Name, konji$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634") %>%
  select(Name, No_farms) 

st_write(sf_opstine, dsn="GIS_layers/Municipalities_Number_of_farms.gpkg", layer='Municipalities_Number_of_farms')

sf_opstine.int <- st_intersection(sf_opstine, sf.grid.5km) %>%
  select(No_farms, Name) 

st_write(sf_opstine.int, dsn="GIS_layers/Municipalities_Number_of_farms_intersected.gpkg", layer='Municipalities_Number_of_farms_intersected')


###########################################################



ovce <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
ovce <- cyr_lat(ovce)
names(ovce) <- cyr_lat(names(ovce)) 
ovce <- ovce %>%
  mutate_all(~replace_na(., 0))
ovce$Opština[ovce$Opština == "Indjija"] <- "Inđija"
ovce$Opština[ovce$Opština == "LJubovija"] <- "Ljubovija"
ovce$Opština[ovce$Opština == "Mali Idjoš"] <- "Mali Iđoš"
ovce$Opština[ovce$Opština == "Savski venac"] <- "Savski Venac"
ovce$Opština[ovce$Opština == "Stari grad"] <- "Stari Grad"
ovce$Opština[ovce$Opština == "Petrovac na Mlavi"] <- "Petrovac"
ovce$Opština[ovce$Opština == "Arandjelovac"] <- "Aranđelovac"
ovce$Opština[ovce$Opština == "LJig"] <- "Ljig"
ovce$Opština[ovce$Opština == "Žitoradja"] <- "Žitorađa"
ovce$Opština[ovce$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$No_sheeps <- ovce$Ovce[match(sf_opstine$Name, ovce$Opština)]


sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634") %>%
  select(Name, No_sheeps) 

st_write(sf_opstine, dsn="GIS_layers/Municipalities_Number_of_sheeps.gpkg", layer='Municipalities_Number_of_sheeps')

sf_opstine.int <- st_intersection(sf_opstine, sf.grid.5km) %>%
  select(No_sheeps, Name) 

st_write(sf_opstine.int, dsn="GIS_layers/Municipalities_Number_of_sheeps.gpkg", layer='Municipalities_Number_of_sheeps_intersected')

###########################################################

Sys.setlocale(locale = 'Serbian (Latin)')
opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)

#+ include = FALSE
goveda <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
goveda <- cyr_lat(goveda)
names(goveda) <- cyr_lat(names(goveda)) 
goveda <- goveda %>%
  mutate_all(~replace_na(., 0))
goveda$Opština[goveda$Opština == "Indjija"] <- "Inđija"
goveda$Opština[goveda$Opština == "LJubovija"] <- "Ljubovija"
goveda$Opština[goveda$Opština == "Mali Idjoš"] <- "Mali Iđoš"
goveda$Opština[goveda$Opština == "Savski venac"] <- "Savski Venac"
goveda$Opština[goveda$Opština == "Stari grad"] <- "Stari Grad"
goveda$Opština[goveda$Opština == "Petrovac na Mlavi"] <- "Petrovac"
goveda$Opština[goveda$Opština == "Arandjelovac"] <- "Aranđelovac"
goveda$Opština[goveda$Opština == "LJig"] <- "Ljig"
goveda$Opština[goveda$Opština == "Žitoradja"] <- "Žitorađa"
goveda$Opština[goveda$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$No_cattle <- goveda$Goveda[match(sf_opstine$NAME_2, goveda$Opština)]


sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634") %>%
  dplyr::rename(Name = NAME_2) %>%
  select(Name, No_cattle) 

st_write(sf_opstine, dsn="GIS_layers/Municipalities_Number_of_cattle.gpkg", layer='Municipalities_Number_of_cattle')

sf_opstine.int <- st_intersection(sf_opstine, sf.grid.5km) %>%
  select(No_cattle, Name) 

st_write(sf_opstine.int, dsn="GIS_layers/Municipalities_Number_of_cattle_intersected.gpkg", layer='Municipalities_Number_of_cattle_intersected')

###########################################################

#+ include = FALSE
svinje <- readxl::read_xls(path = "Data/Poljoprivreda_2015.xls", sheet = "Poljoprivreda") 
lcl(loc = "C")
svinje <- cyr_lat(svinje)
names(svinje) <- cyr_lat(names(svinje)) 
svinje <- svinje %>%
  mutate_all(~replace_na(., 0))
svinje$Opština[svinje$Opština == "Indjija"] <- "Inđija"
svinje$Opština[svinje$Opština == "LJubovija"] <- "Ljubovija"
svinje$Opština[svinje$Opština == "Mali Idjoš"] <- "Mali Iđoš"
svinje$Opština[svinje$Opština == "Savski venac"] <- "Savski Venac"
svinje$Opština[svinje$Opština == "Stari grad"] <- "Stari Grad"
svinje$Opština[svinje$Opština == "Petrovac na Mlavi"] <- "Petrovac"
svinje$Opština[svinje$Opština == "Arandjelovac"] <- "Aranđelovac"
svinje$Opština[svinje$Opština == "LJig"] <- "Ljig"
svinje$Opština[svinje$Opština == "Žitoradja"] <- "Žitorađa"
svinje$Opština[svinje$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$Br_svinje <- svinje$Svinje[match(sf_opstine$Name, svinje$Opština)]

sf_opstine %<>% 
  st_transform(crs = "+init=epsg:32634") %>%
  dplyr::rename(No_swine = Br_svinje) %>%
  select(Name, No_swine) 

st_write(sf_opstine, dsn="GIS_layers/Municipalities_Number_of_swine.gpkg", layer='Municipalities_Number_of_swine')

sf_opstine.int <- st_intersection(sf_opstine, sf.grid.5km) %>%
  select(No_swine, Name) 

st_write(sf_opstine.int, dsn="GIS_layers/Municipalities_Number_of_swine_intersected.gpkg", layer='Municipalities_Number_of_swine_intersected')

###########################################################


wood <- readxl::read_xls(path = "Data/Tehnicko_drvo_2015.xls", sheet = "Sumarstvo")
lcl(loc = "C")
wood <- cyr_lat(wood)
names(wood) <- cyr_lat(names(wood)) 
wood <- wood  %>% 
  mutate_at(.vars = 7:9, .funs = as.numeric) %>%
  mutate_all(~replace_na(., 0))

wood$Opština[wood$Opština == "Indjija"] <- "Inđija"
wood$Opština[wood$Opština == "LJubovija"] <- "Ljubovija"
wood$Opština[wood$Opština == "Mali Idjoš"] <- "Mali Iđoš"
wood$Opština[wood$Opština == "Savski venac"] <- "Savski Venac"
wood$Opština[wood$Opština == "Stari grad"] <- "Stari Grad"
wood$Opština[wood$Opština == "Petrovac na Mlavi"] <- "Petrovac"
wood$Opština[wood$Opština == "Arandjelovac"] <- "Aranđelovac"
wood$Opština[wood$Opština == "LJig"] <- "Ljig"
wood$Opština[wood$Opština == "Žitoradja"] <- "Žitorađa"
wood$Opština[wood$Opština == "Medvedja"] <- "Medveđa"

sf_opstine$pl <- wood$`Posečena drvna zapremina lišćara`[match(sf_opstine$Name, wood$Opština)]
sf_opstine$pc <- wood$`Posečena drvna zapremina četinara`[match(sf_opstine$Name, wood$Opština)]
sf_opstine$tlp <- wood$`Tehničko_lišćara_procenat`[match(sf_opstine$Name, wood$Opština)]
sf_opstine$tcp <- wood$`Tehničko_četinara_procenat`[match(sf_opstine$Name, wood$Opština)]


sf_opstine %<>%
  mutate(Wood_stock = ((pl/100)*tlp) + ((pc/100)*tcp)) %>%
  select(Wood_stock, Name)


st_write(sf_opstine, dsn="GIS_layers/Municipalities_Volume_of_industrial_woods.gpkg", layer='Municipalities_Volume_of_industrial_woods')

sf_opstine.int <- st_intersection(sf_opstine, sf.grid.5km) %>%
  select(Wood_stock, Name) 

st_write(sf_opstine.int, dsn="GIS_layers/Municipalities_Volume_of_industrial_woods_intersected.gpkg", layer='Municipalities_Volume_of_industrial_woods_intersected')

###########################################################




