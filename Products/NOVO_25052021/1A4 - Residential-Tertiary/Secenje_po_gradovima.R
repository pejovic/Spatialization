
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


