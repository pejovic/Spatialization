
# Data per NFR category

# Uzice, Valjevo, Kragujevac, Pancevo and Nis

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
library(data.table)

opstine <- readOGR("Data/opstine/gadm36_SRB_2.shp", 
                   use_iconv=TRUE,  
                   encoding = "UTF-8")
sf_opstine <- st_as_sf(opstine)
unique(sf_opstine$NAME_1)


sf_opstine_pa <- sf_opstine %>% dplyr::filter(NAME_2 == "Pančevo")
sf_opstine_uz <- sf_opstine %>% dplyr::filter(NAME_2 == "Užice")

sf_opstine_va <- sf_opstine %>% dplyr::filter(NAME_2 == "Valjevo")
sf_opstine_kg <- sf_opstine %>% dplyr::filter(NAME_2 == "Kragujevac")
sf_opstine_ni <- sf_opstine %>% dplyr::filter(NAME_2 == "Niš")



# 2015

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_pa, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Pancevo",".csv", sep = ""), sep = ",")
}

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_uz, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Uzice",".csv", sep = ""), sep = ",")
}
# ======================================================================================

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_va, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Valjevo",".csv", sep = ""), sep = ",")
}
# ======================================================================================


# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_kg, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Kragujevac",".csv", sep = ""), sep = ",")
}
# ======================================================================================

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_ni, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Nis",".csv", sep = ""), sep = ",")
}
# ======================================================================================





# WEM 2030

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_pa, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WEM_Pancevo",".csv", sep = ""), sep = ",")
}

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_uz, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WEM_Uzice",".csv", sep = ""), sep = ",")
}
# ======================================================================================

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_va, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WEM_Valjevo",".csv", sep = ""), sep = ",")
}
# ======================================================================================


# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_kg, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WEM_Kragujevac",".csv", sep = ""), sep = ",")
}
# ======================================================================================


# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_ni, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WEM_Nis",".csv", sep = ""), sep = ",")
}
# ======================================================================================




# WAM A 2030

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_pa, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WAM_A_Pancevo",".csv", sep = ""), sep = ",")
}

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_uz, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WAM_A_Uzice",".csv", sep = ""), sep = ",")
}
# ======================================================================================

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_va, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WAM_A_Valjevo",".csv", sep = ""), sep = ",")
}
# ======================================================================================


# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_kg, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WAM_A_Kragujevac",".csv", sep = ""), sep = ",")
}
# ======================================================================================


# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_ni, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WAM_A_Nis",".csv", sep = ""), sep = ",")
}
# ======================================================================================





# WAM B 2030

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_pa, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WAM_B_Pancevo",".csv", sep = ""), sep = ",")
}

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_uz, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WAM_B_Uzice",".csv", sep = ""), sep = ",")
}
# ======================================================================================

# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_va, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WAM_B_Valjevo",".csv", sep = ""), sep = ",")
}
# ======================================================================================


# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_kg, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WAM_B_Kragujevac",".csv", sep = ""), sep = ",")
}
# ======================================================================================


# ======================================================================================
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_ni, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2030_WAM_B_Nis",".csv", sep = ""), sep = ",")
}
# ======================================================================================







# ======================================================================================
# Data you used for spatialisation of activity 1A4bi (residential combustion)

# pokrenuta skripta za generisanje podataka 1A4-Residential... .R

# district heating system - DHS
# own  heating system -  OHS

mapview(sf_opstine, zcol = "Br_domacinstva_SDG")
names(sf_opstine)

sf_municipalities <- sf_opstine %>% dplyr::select(NAME_2, SDG, Br_domacinstva, Br_domacinstva_SDG) %>%
  dplyr::rename(NAME = NAME_2, No_houeses_DHS = SDG, No_houses = Br_domacinstva, No_houses_OHS = Br_domacinstva_SDG)

mapview(sf_municipalities, zcol = "No_houses_OHS")

sf_municipalities %<>% st_transform(4326)
st_write(sf_municipalities,  dsn = "E:/Deliverables_25042021/Data_used_for_spatialisation_1A4bi/NOVO/1A4bi_proxy_data_Municipalities.gpkg")

mapview(sf.1A4bi, zcol = "Br_domacinstva") 

# 1       114.5628 [1]
# 2       254.9083 [1]
# 3       112.2872 [1]
# 4       274.5678 [1]
# 5       566.8860 [1]
# 6       566.5721 [1]
# 7       303.1007 [1]
# 8       115.6506 [1]

sf_municipalities %<>% st_transform(4326)
sf_municipalities %<>% mutate(Area_mun = st_area(.))

sf_clc18_urb <- st_join(sf_clc18_urb, sf_opstine, largest = TRUE) 
sf_clc18_urb %<>% st_transform(4326)

mapview(sf_clc18_urb) + mapview(sf_municipalities)

st_intersection(sf_clc18_urb, sf_municipalities)

sf_municipalities.int <- st_intersection(sf_municipalities, sf.grid.5km)

sf_municipalities.int %<>% mutate(Area_int = st_area(.)) %>%
  mutate(No_houses_OHS_int = (No_houses_OHS/Area_mun)* Area_int)

sum(sf_municipalities.int$Area_int[sf_municipalities.int$NAME == "Sombor"])


p.1A4bi <- sf.grid.5km %>%
  st_join(sf_municipalities.int, join = st_contains) %>% 
  group_by(ID.x) %>%
  summarize(No_houses_OHS = sum(No_houses_OHS_int, na.rm = TRUE)) %>% 
  mutate(ID = as.numeric(ID.x)) %>%
  dplyr::select(ID, No_houses_OHS)
library(units)
p.1A4bi %<>% drop_units()

grid_SRB <- p.1A4bi

mapview(grid_SRB, zcol = "No_houses_OHS")

st_write(grid_SRB, dsn = "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Serbia.gpkg")
writexl::write_xlsx(grid_SRB %>% st_drop_geometry(), "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Serbia.xlsx")


grid_SRB_pa <- grid_SRB[sf_opstine_pa, ]
grid_SRB_uz <- grid_SRB[sf_opstine_uz, ]
grid_SRB_va <- grid_SRB[sf_opstine_va, ]
grid_SRB_kg <- grid_SRB[sf_opstine_kg, ]
grid_SRB_ni <- grid_SRB[sf_opstine_ni, ]


st_write(grid_SRB_pa, dsn = "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Pancevo.gpkg")
st_write(grid_SRB_uz, dsn = "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Uzice.gpkg")
st_write(grid_SRB_va, dsn = "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Valjevo.gpkg")
st_write(grid_SRB_kg, dsn = "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Kragujevac.gpkg")
st_write(grid_SRB_ni, dsn = "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Nis.gpkg")

writexl::write_xlsx(grid_SRB_pa %>% st_drop_geometry(), "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Pancevo.xlsx")
writexl::write_xlsx(grid_SRB_uz %>% st_drop_geometry(), "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Uzice.xlsx")
writexl::write_xlsx(grid_SRB_va %>% st_drop_geometry(), "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Valjevo.xlsx")
writexl::write_xlsx(grid_SRB_kg %>% st_drop_geometry(), "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Kragujevac.xlsx")
writexl::write_xlsx(grid_SRB_ni %>% st_drop_geometry(), "E:/Deliverables_26042021/Data_used_for_spatialisation_1A4bi/1A4bi_proxy_data_Nis.xlsx")


















