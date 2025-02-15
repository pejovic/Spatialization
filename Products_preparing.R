# Products preparing


### Spatialisation 

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# From geopackeges to CSV
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(data.table)
data.spat <- list.files('D:/R_projects/Spatialization/Products/5 - Waste/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/5 - Waste/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")



for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"csv", sep = "."), sep = ",")
}



##### Rename files
old_files <- list.files("D:/R_projects/Spatialization/Products/! CSVs/5 - Waste/")
old_files <- paste("D:/R_projects/Spatialization/Products/! CSVs/5 - Waste/", old_files, sep = "")

# Create vector of new files

new_files <- old_files %>% str_remove(., ".gpkg")
new_files

# Rename from old files to new files

file.rename(from = old_files, to = new_files)



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Sum-up by category
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data.spat <- list.files('D:/R_projects/Spatialization/Products/5 - Waste/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
 data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/5 - Waste/",data.spat[i], sep = ""))
}


sf_data <- data.spat.list[[1]]
for(i in 2:length(data.spat)){                                              
 sf_data <- st_join(sf_data, data.spat.list[[i]], join = st_equals) %>% 
   group_by(ID.x) %>%
   summarize(NOx = sum(as.numeric(NOx.x), as.numeric(NOx.y)),
             SO2 = sum(SO2.x, SO2.y),
             PM10 = sum(PM10.x, PM10.y),
             PM2.5 = sum(PM2.5.x, PM2.5.y),
             NMVOC = sum(NMVOC.x, NMVOC.y),
             NH3 = sum(NH3.x + NH3.y)) %>%
   mutate(ID = ID.x) %>%
   dplyr::select(ID, NOx, SO2, PM10, PM2.5, NMVOC, NH3)
 print(paste("NOx:",sum(sf_data$NOx))) 
 print(paste("SO2:",sum(sf_data$SO2)))
 print(paste("PM10:",sum(sf_data$PM10)))
 print(paste("PM2.5:",sum(sf_data$PM2.5)))
 print(paste("NMVOC:",sum(sf_data$NMVOC)))
 print(paste("NH3:",sum(sf_data$NH3)))
}

sf_data

st_write(sf_data, dsn="Products/Sum-up_By_category/5 - Waste.gpkg", layer='5 - Waste')

#### CSvs

data.spat <- list.files('D:/R_projects/Spatialization/Products/Sum-up_By_category/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("D:/R_projects/Spatialization/Products/Sum-up_By_category/",data.spat[i], sep = ""))
}


data.spat %<>% str_remove(., ".gpkg")



for(i in 1:length(data.spat)){                                             
  dataa <- data.spat.list[[i]] %>% st_drop_geometry()
  fwrite(dataa, file = paste(data.spat[i],"csv", sep = "."), sep = ",")
}


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Sum_up_by_cell_by_pollutant
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
data.spat <- list.files('C:/Users/Petar/Desktop/data/') # C:/Users/pbursac/Desktop/data/

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
  data.spat.list[[i]] <- st_read(paste("C:/Users/Petar/Desktop/data/",data.spat[i], sep = ""))
}

sf_data <- data.spat.list[[1]]
for(i in 2:length(data.spat)){                                             
  sf_data <- st_join(sf_data, data.spat.list[[i]], join = st_equals) %>% 
    group_by(ID.x) %>%
    summarize(NOx = sum(NOx.x, NOx.y),
              SO2 = sum(SO2.x, SO2.y),
              PM10 = sum(PM10.x, PM10.y),
              PM2.5 = sum(PM2.5.x, PM2.5.y),
              NMVOC = sum(NMVOC.x, NMVOC.y),
              NH3 = sum(NH3.x + NH3.y)) %>%
    mutate(ID = ID.x) %>%
    dplyr::select(ID, NOx, SO2, PM10, PM2.5, NMVOC, NH3)
  print(paste("NOx:",sum(sf_data$NOx))) 
  print(paste("SO2:",sum(sf_data$SO2)))
  print(paste("PM10:",sum(sf_data$PM10)))
  print(paste("PM2.5:",sum(sf_data$PM2.5)))
  print(paste("NMVOC:",sum(sf_data$NMVOC)))
  print(paste("NH3:",sum(sf_data$NH3)))
}

sf_data

mapview(sf_data, zcol = "NMVOC")
st_write(sf_data, dsn="Products/Sum_up_by_cell_by_pollutant.gpkg", layer='Sum_up_by_cell_by_pollutant')
writexl::write_xlsx(sf_data %>% st_drop_geometry(), "Products/Sum_up_by_cell_by_pollutant.xlsx")

#sf_data <- st_read(dsn = "Products/sf_data.gpkg", layer = "sf_data")



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Temporalization
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Temporal profiles all in one
data.tprofiles <- list.files('Hourly_emissions/Products/')

data.temp.list <- list()                                                   
for(i in 1:length(data.tprofiles)){                                             
  data.temp.list[[i]] <- readxl::read_xlsx(path = paste("Hourly_emissions/Products/",data.tprofiles[i], sep = ""))
}


for(i in 1:length(data.temp.list)){
  print(length(data.temp.list[[i]]))
}


data.temp.list[[1]]$.

temporal_Profiles <- data.temp.list[[1]]$. %>%
  cbind(
    data.temp.list[[1]][,2:91],
    data.temp.list[[2]][,2:31],
    data.temp.list[[3]][,2:25],
    data.temp.list[[4]][,2:73],
    data.temp.list[[5]][,2:127],
    data.temp.list[[6]][,2:25],
    data.temp.list[[7]][,2:145],
    data.temp.list[[8]][,2:25]
  ) %>%
  as_data_frame()


writexl::write_xlsx(temporal_Profiles, "Hourly_emissions/TemporalProfiles_by_pollutant_and_sub-categories.xlsx")






# nesto staro neka kontrola

data.temp.all <- readxl::read_xlsx(path = "Hourly_emissions/Products/TemporalProfiles_All_in_one.xlsx")
# 
NOx.temp <- temporal_Profiles %>% dplyr::select(ends_with("NOx")) %>% rowwise() %>%
  do( (.) %>% as.data.frame %>% mutate(NOx_temp = sum(.)) ) %>%
  ungroup() %>% mutate(Time = temporal_Profiles$.) %>% dplyr::select(Time, NOx_temp)

NOx.temp$Time <- data.temp.all$Time
sum(NOx.temp$NOx_temp)


