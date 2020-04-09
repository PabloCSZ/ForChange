
# Pablo Salazar Z
# Universidad de Córdoba
# 08/04/2020
# Climate data exctraction
# Spanish National Forestry inventory
# Andalucia subset

# DataMp_sf from "3_0_data_projection.R" is required 

## getting data -----------------
# Spanish administrative borders
Spain <- getData("GADM", country = "ESP", level = 2, path = "data")

#downloading WorlClim
climate <-getData(name = "worldclim", var="bio", res = 0.5, lon=-5, lat = 48, path = "data")

Aridity1 <- climate$bio12_15/(climate$bio1_15+33)
Aridity2 <- climate$bio12_15/(climate$bio1_15+10)+(((12*climate$bio17_15)/(climate$bio9_15+10))/2)
Aridity3 <- (100*climate$bio12_15)/(climate$bio10_15*climate$bio10_15-climate$bio11_15*climate$bio11_15)

# Loading PET and aridity index (cgiarCSI)
PET <- raster(file.path("Data/Climate/pet_1_0/PET_he_annual/pet_he_yr"))
AI <- raster(file.path("Data/Climate/pet_1_0/AI_annual/ai_yr"))

# CRU Palmer aridity data 
Palmer <- raster(file.path("Data/Climate/palmer_cru/scPDSI.cru_ts4.03early1.1901.2018.cal_1901_18.bams.2019.GLOBAL.1901.2018.nc"))

## Data extraction -------------------
# Re project DataMp_sf
DataMp_sf <- st_transform(DataMp_sf,4326)
#raster extraction

Arid_Koppen <- raster::extract(Aridity1,DataMp_sf)
Arid_Emberg <- raster::extract(Aridity2,DataMp_sf)
Arid_Dm <- raster::extract(Aridity3,DataMp_sf)
MAT <- raster::extract(climate$bio1_15,DataMp_sf)
MAP <- raster::extract(climate$bio12_15,DataMp_sf)
T_Cold_M <- raster::extract(climate$bio6_15,DataMp_sf)
PET <- raster::extract(PET,DataMp_sf)
Aridity4 <- raster::extract(AI,DataMp_sf)
Arid_Pal <- raster::extract(Palmer,DataMp_sf)

# Binding climate data to DataMp_sf
DataMp_sf <-cbind(DataMp_sf, Arid_Koppen, Arid_Emberg, Arid_Dm,
                  MAT, MAP, T_Cold_M, PET, Aridity4, Arid_Pal)
# Creating climate levels
DataMp_sf$grad_Kop <-as.numeric(cut(DataMp_sf$Arid_Koppen,10))
DataMp_sf$grad_Emb <-as.numeric(cut(DataMp_sf$Arid_Emberg,10))
DataMp_sf$grad_Dm <-as.numeric(cut(DataMp_sf$Arid_Dm,10))
DataMp_sf$grad_MAT <-as.numeric(cut(DataMp_sf$MAT,10))
DataMp_sf$grad_MAP <-as.numeric(cut(DataMp_sf$MAP,10))
DataMp_sf$grad_TCM <-as.numeric(cut(DataMp_sf$T_Cold_M,10))
DataMp_sf$grad_pet <-as.numeric(cut(DataMp_sf$PET,10))
DataMp_sf$grad4 <-as.numeric(cut(DataMp_sf$Aridity4,10))
DataMp_sf$grad_Pal <-as.numeric(cut(DataMp_sf$Arid_Pal,10))

#some testing graphs
Koppen<-tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + tm_fill() + tm_borders() +
  tm_shape(DataMp_sf) + tm_dots(col = "grad1", size = 1, n = 10) + tm_layout(title = "Koppen")

#beware that worldclim have some zeros
blanks <- DataMp_sf[is.na(DataMp_sf$Arid_Koppen),]
blanks_geo <- tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + tm_fill() + tm_borders() +
  tm_shape(blanks) + tm_dots(size = 1) + tm_layout(title = "WorldClim blanks")
#tmap_save(blanks_geo, "Blanks.jpg")
