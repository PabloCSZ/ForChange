
# Pablo Salazar Z
# Universidad de Córdoba
# 08/04/2020
# Spatial projection
# Spanish National Forestry inventory
# Andalucia subset

# DataMp2 from "2_0_data_loading.R" is required

## Finding a good projection -----------
# DataMp does not have UTM zone, but location in the National topographic map.
# Loading the Spanish topographic map
pm_shape <- st_read("data/projection/E50_area.shp") # projected in ED50
pm_shape <- st_set_crs(pm_shape, 4258) # projected in wgs84
pm_shape$MTN50_CLAS <- as.factor(pm_shape$MTN50_CLAS)
# Loading the world UTM grid 
utm_shape <- st_read("data/World_UTM/World_UTM_Grid.shp")
utm_shape <- st_set_crs(utm_shape, 4258)

# Filtering the UTM grid before spatial operation
# H29
utm_shape_f <- utm_shape %>%
  filter(ZONE == "29") %>%
  filter(ROW_ == "S" | ROW_ == "T") %>%
  group_by(ZONE) %>%
  dplyr::summarise()
# Spatial operation 
# National cartographic sheets within the 29 UTM zone
touch <- st_contains(utm_shape_f, pm_shape)
pm_shape_29<- pm_shape[unlist(touch),] 
pm_shape_29 <-  st_drop_geometry(pm_shape_29)

# National topographic sheets that intersect with UTM line
#2930_o

# Spatial operation 
# National cartographic sheets that touch but no contain the 29 UTM zone
touch <- st_overlaps(utm_shape_f, pm_shape) 
pm_shape_2930_o<- pm_shape[unlist(touch),] 
pm_shape_2930_o <-  st_drop_geometry(pm_shape_2930_o)

# H30
utm_shape_f <- utm_shape %>%
  filter(ZONE == "30") %>%
  filter(ROW_ == "S" | ROW_ == "T") %>%
  group_by(ZONE) %>%
  dplyr::summarise()
# Spatial operation 
# National cartographic sheets within the 29 UTM zone
touch <- st_contains(utm_shape_f, pm_shape)
pm_shape_30 <- pm_shape[unlist(touch),] 
pm_shape_30 <-  st_drop_geometry(pm_shape_30)


# Merging pm_shape with DataMp2 to assign a UTM Zone 
# 29
DataMp_29<- merge(pm_shape_29[,c(1,2)], DataMp2, by.y = "Hoja50" , by.x = "MTN50_CLAS", all = FALSE)

# Merging the pm_shape intersection and assign UTM Zone
# 29-5
DataMp_29_o<- merge(pm_shape_2930_o[,c(1,2)], DataMp2, by.y = "Hoja50" , by.x = "MTN50_CLAS", all = FALSE)
DataMp_29_o$zone <- 0
for(i in 1:nrow(DataMp_29_o)){
  if(DataMp_29_o[i,11] > 600000){
    DataMp_29_o$zone[i] <- paste0("29")
  } else {DataMp_29_o$zone[i] <- paste0("30")}
}
DataMp_29_o$zone <- as.factor(DataMp_29_o$zone)

# Binding all Data from 29 zone and 29 intersecpt, and two weird plots that should 29 but somehow are not
DataMp_29 <- rbind.fill(DataMp_29,subset(DataMp_29_o, zone == "29"),subset(DataMp2, Provincia == "Huelva" & Estadillo == "1623"),subset(DataMp2, Provincia == "Huelva" & Estadillo == "1954"))
# converting to sp before sf because dont know how
DataMp_sp_29 <- SpatialPoints(DataMp_29[,c(11,12)], proj4string=CRS("+proj=utm +North +init=epsg:4230 +zone=29"))
DataMp_sp_29 <- SpatialPointsDataFrame(DataMp_sp_29,DataMp_29, coords.nrs = c(11,12))
# converting to sf object
DataMp_sf_29 <- st_as_sf(DataMp_sp_29)
DataMp_sf_29 <- DataMp_sf_29[,-14]

# Merging pm_shape with DataMp2 to assign a UTM Zone 
# 30 
DataMp_30 <- merge(pm_shape_30[,c(1,2)], DataMp2, by.y = "Hoja50" , by.x = "MTN50_CLAS", all = FALSE)
DataMp_30 <- DataMp_30[complete.cases(DataMp_30$CoorXp),]
# Binding Data from 30 zone and the  remaining intercept, except three plot from Huelva that should not be here
DataMp_30 <- rbind.fill(droplevels(DataMp_30[-which(DataMp_30$Provincia == "Huelva"), ] ),subset(DataMp_29_o, zone == "30"))
# converting to sp before sf because dont know how
DataMp_sp_30 <- SpatialPoints(DataMp_30[,c(11,12)], proj4string=CRS("+proj=utm +North +init=epsg:4230 +zone=30"))
DataMp_sp_30 <- SpatialPointsDataFrame(DataMp_sp_30,DataMp_30, coords.nrs = c(11,12))
# converting to sf object
DataMp_sf_30 <- st_as_sf(DataMp_sp_30)

# Project both dataframes from UTM to longlat
DataMp_sf_29 <- st_transform(DataMp_sf_29,4258)
DataMp_sf_30 <- st_transform(DataMp_sf_30,4258)
# Binding both dataframes
DataMp_sf <- do.call(rbind,list(DataMp_sf_29, DataMp_sf_30))
st_geometry(DataMp_sf)

# mapview(DataMp_sf)
#st_write(DataMp_sf, "Andalucia.shp")
