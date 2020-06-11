
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

## Getting Chelsea data ------------------

## Run ONLY if you need to download Chelsa current conditions (1979-2013) ####
# y <- "1979-2013"
# m <- 12
# 
# for (m in 1:12){
#   mm <- ifelse(nchar(m) < 2,
#              paste("0", m, sep=""),m)
#   #mean
#   file <- paste("CHELSA_temp10_", mm,
#                 "_", y, "_V1.2_land.tif", sep="")
#   download.file(paste("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/temp/integer/temp/", file,sep=""),
#                 destfile=paste("data/ChelsaClimate/", file,sep=""), mode="wb")
#   #min temp
#   file <- paste("CHELSA_tmin10_", mm,
#                 "_", y, "_V1.2_land.tif", sep="")
#   download.file(paste("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/temp/integer/tmin/", file,sep=""),
#                 destfile=paste("data/ChelsaClimate/", file,sep=""), mode="wb")
#   #max temp
#   file <- paste("CHELSA_tmax10_", mm,
#                 "_", y, "_V1.2_land.tif", sep="")
#   download.file(paste("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/temp/integer/tmax/", file,sep=""),
#                 destfile=paste("data/ChelsaClimate/", file,sep=""), mode="wb")
#   #precip
#   file <- paste("CHELSA_prec_", mm,
#                 "_V1.2_land.tif", sep="")
#   download.file(paste("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/prec/", file,sep=""),
#                 destfile=paste("data/ChelsaClimate/", file,sep=""), mode="wb")
# }

## Load raster of current conditions (1996) for the Chelsa data nd stack them ####
# Create empty raster objects first
ta1996 <- raster()
maxta1996 <- raster()
minta1996 <- raster()
pp1996 <- raster()

# Now fill them in and stack 
y <- "1979-2013"
for (m in 1:12){
  mm<-ifelse(nchar(m) < 2,
             paste("0", m, sep=""),m)
  file <- paste("data/ChelsaClimate/CHELSA_temp10_", mm, "_", y, "_V1.2_land.tif", sep="")
  ta1996 <- stack(ta1996,file)
  file <- paste("data/ChelsaClimate/CHELSA_tmax10_", mm, "_", y, "_V1.2_land.tif", sep="")
  maxta1996 <- stack(maxta1996, file)
  file <- paste("data/ChelsaClimate/CHELSA_tmin10_", mm, "_", y, "_V1.2_land.tif", sep="")
  minta1996 <- stack(minta1996, file)
  file <- paste("data/ChelsaClimate/CHELSA_prec_", mm,  "_V1.2_land.tif", sep="")
  pp1996 <- stack(pp1996, file)
}

# Croping the rasterstacks to Andalucia extent
ta1996 <- crop(ta1996, Spain) 
maxta1996 <- crop(maxta1996, Spain) 
minta1996 <- crop(minta1996, Spain) 
pp1996 <- crop(pp1996, Spain) 


## computting Chelsa PET and Aridity ####
# Below are lines of codes for computting PET at the original resolution, 30-arc second

ta1996 <- ta1996/10
maxta1996 <- maxta1996/10
minta1996 <- minta1996/10
trang1996 <- abs(maxta1996-minta1996)
solar1996 <- ETsolradRasters(ta1996[[1]], year=46)#1996-1950 = 46, default 0 is for 1950
# Why does it says 1 if it looks like it process the 12 months??

pet1996 <- monthlyPET(ta1996, solar1996, trang1996)
annualPET1996 <- sum(pet1996)

# set up naming scheme - only precip is different from default
names(pp1996)<-c("precip_01","precip_02","precip_03","precip_04","precip_05","precip_06",
                 "precip_07","precip_08","precip_09","precip_10","precip_11","precip_12")

ai1996<-aridityIndexThornthwaite(pp1996, pet1996, precipScale = 1)
# plot(ai1996)

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
ai_chelsa <- raster::extract(ai1996,DataMp_sf)

# Binding climate data to DataMp_sf---------------
DataMp_sf <-cbind(DataMp_sf, Arid_Koppen, Arid_Emberg, Arid_Dm,
                  MAT, MAP, T_Cold_M, PET, Aridity4, Arid_Pal, ai_chelsa)
# Creating climate levels
DataMp_sf$grad_Kop <-as.numeric(cut(DataMp_sf$Arid_Koppen,10))
DataMp_sf$grad_Emb <-as.numeric(cut(DataMp_sf$Arid_Emberg,10))
DataMp_sf$grad_Dm <-as.numeric(cut(DataMp_sf$Arid_Dm,10))
DataMp_sf$grad_MAT <-as.numeric(cut(DataMp_sf$MAT,10))
DataMp_sf$grad_MAP <-as.numeric(cut(DataMp_sf$MAP,10))
DataMp_sf$grad_TCM <-as.numeric(cut(DataMp_sf$T_Cold_M,10))
DataMp_sf$grad_pet <-as.numeric(cut(DataMp_sf$PET,10))
DataMp_sf$grad4 <- as.numeric(cut(DataMp_sf$Aridity4,10))
DataMp_sf$grad_Pal <- as.numeric(cut(DataMp_sf$Arid_Pal,10))
DataMp_sf$grad_chelsa <- as.numeric(cut(DataMp_sf$ai_chelsa,10))

#some testing graphs
Koppen <- tm_shape(Spain) + tm_fill() + tm_borders() +
  tm_shape(DataMp_sf) + tm_dots(col = "grad_Kop", size = 1, n = 10) + tm_layout(title = "Koppen")
tmap_save(Koppen, "koppen.jpg")
#beware that worldclim have some zeros
blanks <- DataMp_sf[is.na(DataMp_sf$Arid_Koppen),]
blanks_geo <- tm_shape(Spain) + tm_fill() + tm_borders() +
  tm_shape(blanks) + tm_dots(size = 1) + tm_layout(title = "WorldClim blanks")
#tmap_save(blanks_geo, "Blanks.jpg")
