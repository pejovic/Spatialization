
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

sf_opstine_bg <- sf_opstine %>% dplyr::filter(NAME_1 == "Grad Beograd")
sf_opstine_ns <- sf_opstine %>% dplyr::filter(NAME_2 == "Novi Sad")






# ======================================================================================
data.spat <- list.files('D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_pa, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] 
  #fwrite(dataa, file = paste(data.spat[i],"_Pancevo",".csv", sep = ""), sep = ",")
  st_write(dataa, dsn = paste(data.spat[i],"_Pancevo",".gpkg", sep = ""))
}


for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Pancevo",".csv", sep = ""), sep = ",")
}




# ======================================================================================
data.spat <- list.files('D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_uz, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] 
  st_write(dataa, dsn = paste(data.spat[i],"_Uzice",".gpkg", sep = ""))
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Uzice",".csv", sep = ""), sep = ",")
}
# ======================================================================================

# ======================================================================================
data.spat <- list.files('D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_va, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] 
  st_write(dataa, dsn = paste(data.spat[i],"_Valjevo",".gpkg", sep = ""))
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Valjevo",".csv", sep = ""), sep = ",")
}
# ======================================================================================


# ======================================================================================
data.spat <- list.files('D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_kg, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] 
  st_write(dataa, dsn = paste(data.spat[i],"_Kragujevac",".gpkg", sep = ""))
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Kragujevac",".csv", sep = ""), sep = ",")
}
# ======================================================================================

# ======================================================================================
data.spat <- list.files('D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_ni, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] 
  st_write(dataa, dsn = paste(data.spat[i],"_Nis",".gpkg", sep = ""))
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Nis",".csv", sep = ""), sep = ",")
}
# ======================================================================================


# ======================================================================================
data.spat <- list.files('D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/gpkg all/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/gpkg all/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_bg, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] 
  st_write(dataa, dsn = paste(data.spat[i],"_Beograd",".gpkg", sep = ""))
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_Beograd",".csv", sep = ""), sep = ",")
}
# ======================================================================================


# ======================================================================================
data.spat <- list.files('D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/gpkg all/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/NOVO_25052021/1A4 - Residential-Tertiary/gpkg all/",data.spat[i], sep = ""))
}

data.spat %<>% str_remove(., ".gpkg")

for(i in 1:length(data.spat)){
  data.spat.list[[i]] <- data.spat.list[[i]][sf_opstine_ns, ]
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] 
  st_write(dataa, dsn = paste(data.spat[i],"_NoviSad",".gpkg", sep = ""))
}

for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"_NoviSad",".csv", sep = ""), sep = ",")
}
# ======================================================================================
