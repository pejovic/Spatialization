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

sf_opstine_bg <- sf_opstine %>% dplyr::filter(NAME_1 == "Grad Beograd")
sf_opstine_bo <- sf_opstine %>% dplyr::filter(NAME_2 == "Bor")
sf_opstine_ns <- sf_opstine %>% dplyr::filter(NAME_2 == "Novi Sad")
sf_opstine_pa <- sf_opstine %>% dplyr::filter(NAME_2 == "Pančevo")
sf_opstine_sd <- sf_opstine %>% dplyr::filter(NAME_2 == "Smederevo")
sf_opstine_uz <- sf_opstine %>% dplyr::filter(NAME_2 == "Užice")



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Sum-up by GNFR category per cities
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#######################################################################################
#### 2015

data.spat <- list.files('D:/R_projects/Spatialization/Products/Sum-up_By_category/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/Sum-up_By_category/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_bg, ]
}


for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Belgrade",".csv", sep = ""), sep = ",")
}

###################################################

data.spat <- list.files('D:/R_projects/Spatialization/Products/Sum-up_By_category/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/Sum-up_By_category/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_bo, ]
}


for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Bor",".csv", sep = ""), sep = ",")
}

###################################################

data.spat <- list.files('D:/R_projects/Spatialization/Products/Sum-up_By_category/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/Sum-up_By_category/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_ns, ]
}


for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Novi Sad",".csv", sep = ""), sep = ",")
}

###################################################

data.spat <- list.files('D:/R_projects/Spatialization/Products/Sum-up_By_category/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/Sum-up_By_category/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_pa, ]
}


for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Pancevo",".csv", sep = ""), sep = ",")
}

###################################################

data.spat <- list.files('D:/R_projects/Spatialization/Products/Sum-up_By_category/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/Sum-up_By_category/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_sd, ]
}


for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Smederevo",".csv", sep = ""), sep = ",")
}

###################################################

data.spat <- list.files('D:/R_projects/Spatialization/Products/Sum-up_By_category/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/Sum-up_By_category/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_uz, ]
}


for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_2015_Uzice",".csv", sep = ""), sep = ",")
}









#######################################################################################
#### WEM

data.spat <- list.files('D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_bg, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_WEM_Belgrade",".csv", sep = ""), sep = ",")
}
###################################################


data.spat <- list.files('D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_bo, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_WEM_Bor",".csv", sep = ""), sep = ",")
}
###################################################


data.spat <- list.files('D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_ns, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_WEM_Novi Sad",".csv", sep = ""), sep = ",")
}
###################################################


data.spat <- list.files('D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_pa, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_WEM_Pancevo",".csv", sep = ""), sep = ",")
}
###################################################

data.spat <- list.files('D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_sd, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_WEM_Smederevo",".csv", sep = ""), sep = ",")
}
###################################################

data.spat <- list.files('D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030/Products_2030/Sum-up_By_category_2030/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_uz, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_WEM_Uzice",".csv", sep = ""), sep = ",")
}
###################################################


#######################################################################################
#### WAM A

data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_bg, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Belgrade",".csv", sep = ""), sep = ",")
}

###################################################



data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_bo, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Bor",".csv", sep = ""), sep = ",")
}

###################################################

data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_ns, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Novi Sad",".csv", sep = ""), sep = ",")
}

###################################################

data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_pa, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Pancevo",".csv", sep = ""), sep = ",")
}

###################################################

data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_sd, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Smederevo",".csv", sep = ""), sep = ",")
}

###################################################

data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_A/Products_2030_WAM_A/Sum-up_By_category_2030_WAM_A/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_uz, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Uzice",".csv", sep = ""), sep = ",")
}

###################################################


#######################################################################################
#### WAM B

data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_bg, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Belgrade",".csv", sep = ""), sep = ",")
}

###################################################


data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_bo, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Bor",".csv", sep = ""), sep = ",")
}

###################################################



data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_ns, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Novi Sad",".csv", sep = ""), sep = ",")
}

###################################################



data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_pa, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Pancevo",".csv", sep = ""), sep = ",")
}

###################################################



data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_sd, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Smederevo",".csv", sep = ""), sep = ",")
}

###################################################

data.spat <- list.files('D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/2030_WAM_B/Products_2030_WAM_B/Sum-up_By_category_2030_WAM_B/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_uz, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Uzice",".csv", sep = ""), sep = ",")
}

###################################################





