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

