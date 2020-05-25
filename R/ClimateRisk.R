#### Initial settings ####

library(envirem)
library(raster)
e <- extent(-9, -7, 43, 44)
# rm(e)

#########################
#### CURRENT CLIMATE ####
#########################
  ## Run ONLY if you need to download Chelsa current conditions (1979-2013) ####
  y <- "1979-2013"
  m <- 12
  
  for (m in 1:12){
    mm<-ifelse(nchar(m) < 2,
               paste("0", m, sep=""),m)
    #mean
    file <- paste("CHELSA_temp10_", mm,
                  "_", y, "_V1.2_land.tif", sep="")
    download.file(paste("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/temp/integer/temp/", file,sep=""),
                  destfile=paste("data/ChelsaClimate/", file,sep=""), mode="wb")
    #min temp
    file <- paste("CHELSA_tmin10_", mm,
                  "_", y, "_V1.2_land.tif", sep="")
    download.file(paste("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/temp/integer/tmin/", file,sep=""),
                  destfile=paste("data/ChelsaClimate/", file,sep=""), mode="wb")

    #max temp
    file <- paste("CHELSA_tmax10_", mm,
                  "_", y, "_V1.2_land.tif", sep="")
    download.file(paste("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/temp/integer/tmax/", file,sep=""),
                  destfile=paste("data/ChelsaClimate/", file,sep=""), mode="wb")

    #precip
    file <- paste("CHELSA_prec_", mm,
                  "_V1.2_land.tif", sep="")
    download.file(paste("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/prec/", file,sep=""),
                  destfile=paste("data/ChelsaClimate/", file,sep=""), mode="wb")
  }

  ## Make raster stacks of current conditions (1996) ####
  ta1996_original <- raster()
  maxta1996_original <- raster()
  minta1996_original <- raster()
  pp1996_original <- raster()

  y <- "1979-2013"
  for (m in 1:12){
    mm<-ifelse(nchar(m) < 2,
               paste("0", m, sep=""),m)
    file <- paste("data/ChelsaClimate/CHELSA_temp10_", mm, "_", y, "_V1.2_land.tif", sep="")
    ta1996_original <- stack(ta1996_original, file)
    file <- paste("data/ChelsaClimate/CHELSA_tmax10_", mm, "_", y, "_V1.2_land.tif", sep="")
    maxta1996_original <- stack(maxta1996_original, file)
    file <- paste("data/ChelsaClimate/CHELSA_tmin10_", mm, "_", y, "_V1.2_land.tif", sep="")
    minta1996_original <- stack(minta1996_original, file)
    file <- paste("data/ChelsaClimate/CHELSA_prec_", mm,  "_V1.2_land.tif", sep="")
    pp1996_original <- stack(pp1996_original, file)
  }
  ta1996<- ta1996_original
  maxta1996<- maxta1996_original
  minta1996<- minta1996_original
  pp1996<- pp1996_original

  ## crop to a small region ONLY for testing ####
  #To be able to work with the rasters I will set as the extent part of the PI
  # ta1996<- crop(ta1996_original, e)
  # maxta1996<- crop(maxta1996_original, e)
  # minta1996<- crop(minta1996_original, e)
  # pp1996<- crop(pp1996_original, e)

  ## computting PET and Aridity ####
  # Below are lines of codes for computting PET at the original resolution, 30-arc second
  
  ta1996 <- ta1996/10
  solar1996 <- ETsolradRasters(ta1996[[1]], year=46)#1996-1950 = 46, default 0 is for 1950 # Why is this only [1]??
  maxta1996 <- maxta1996/10
  minta1996 <- minta1996/10
  trang1996 <- maxta1996-minta1996
  trang1996 <- abs(maxta1996-minta1996)
  
  # plot(ta1996[[2]])
  # plot(maxta1996[[1]])
  # plot(minta1996[[1]])
  # plot(trang1996[[1]])
  # plot(solar1996[[1]])
  # plot(pp1996[[1]])
  
  pet1996 <- monthlyPET(ta1996, solar1996, trang1996)
  annualPET1996 <- sum(pet1996)
  # plot(pet1996)
  # plot(pp1996)
  # plot(annualPET1996[[1]])
  
  # set up naming scheme - only precip is different from default
  names(pp1996)<-c("prec_01","prec_02","prec_03","prec_04","prec_05","prec_06",
                   "prec_07","prec_08","prec_09","prec_10","prec_11","prec_12")
  ai1996<-aridityIndexThornthwaite(pp1996, pet1996, precipScale = 1)
  
  # plot(maxta1996[[1]])
  # plot(minta1996[[1]])
  # plot(trang1996[[1]])
  # plot(pet1996[[1]])
  # plot(ai1996)
  
  ## Save the output rasters -> ONLY if they are the good ones ####
  # writeRaster(annualPET1996, "annualPET1996.grd", overwrite=TRUE) 
  # writeRaster(maxta1996, "maxta1996.grd", overwrite=TRUE) 
  # writeRaster(minta1996, "minta1996.grd", overwrite=TRUE) 
  # writeRaster(ai1996, "ai1996.grd", overwrite=TRUE) 
  #
  # writeRaster(annualPET1996, "annualPET1996.tif", overwrite=TRUE) 
  # writeRaster(maxta1996, "maxta1996.tif", overwrite=TRUE) 
  # writeRaster(minta1996, "minta1996.tif", overwrite=TRUE) 
  # writeRaster(ai1996, "ai1996.tif", overwrite=TRUE) 
  ## Aggregate at 50km and save ####
  
  annualPET1996 <- raster("annualPET1996.grd") 
  maxta1996 <- stack("maxta1996.grd") 
  minta1996 <- stack("minta1996.grd") 
  ai1996 <- raster("ai1996.grd") 
  
    # # crop to a small region ONLY for testing ####
    # 
    # annualPET1996 <- crop(annualPET1996, e)
    # maxta1996 <- crop(maxta1996, e)
    # minta1996 <- crop(minta1996, e)
    # ai1996 <- crop(ai1996, e)

    # Aggregating ####
    rastemp <- raster("global_land_surface_50km.tif") 
    
    #current conditions
    minta1996_50 <- aggregate(minta1996, fact=50, fun=mean)
    minta1996_50 <- projectRaster(minta1996_50, crs=projection(rastemp))
    minta1996_50 <- resample(minta1996_50, rastemp, method="bilinear")
    writeRaster(minta1996_50, filename="minta1996_50.tif",
                options="INTERLEAVE=BAND", overwrite=TRUE)
    maxta1996_50 <- aggregate(maxta1996, fact=50, fun=mean)
    maxta1996_50 <- projectRaster(maxta1996_50, crs=projection(rastemp))
    maxta1996_50 <- resample(maxta1996_50, rastemp, method="bilinear")
    writeRaster(maxta1996_50, filename="maxta1996_50.tif",
                options="INTERLEAVE=BAND", overwrite=TRUE)
    annualPET1996_50 <- aggregate(annualPET1996, fact=50, fun=mean)
    annualPET1996_50 <- projectRaster(annualPET1996_50, crs=projection(rastemp))
    annualPET1996_50 <- resample(annualPET1996_50, rastemp, method="bilinear")
    writeRaster(annualPET1996_50, filename="annualPET1996_50.tif",
                options="INTERLEAVE=BAND", overwrite=TRUE)
    ai1996_50 <- aggregate(ai1996, fact=50, fun=mean)
    ai1996_50 <- projectRaster(ai1996_50, crs=projection(rastemp))
    ai1996_50 <- resample(ai1996_50, rastemp, methods="bilinear")
    writeRaster(ai1996_50, filename="ai1996_50.tif",
                options="INTERLEAVE=BAND", overwrite=TRUE)
    
########################
#### FUTURE CLIMATE ####
########################
  ## Function to download chelsa data for future scenarios ####  
  download.chelsa.future<-function(yr, scenario,model){
    for (m in 1:12){
      #mean 
      file <- paste("CHELSA_tas_mon_",model,"_",scenario,"_r1i1p1_g025.nc_", m, "_", yr, "_V1.2.tif", sep="")
      download.file(paste("https://www.wsl.ch/lud/chelsa/data/cmip5/2041-2060/temp/", file,sep=""), 
                    destfile=paste("ChelsaClimate/", file,sep=""), mode="wb")
      #min temp
      file <- paste("CHELSA_tasmin_mon_",model,"_",scenario,"_r1i1p1_g025.nc_", m, "_", yr, "_V1.2.tif", sep="")
      download.file(paste("https://www.wsl.ch/lud/chelsa/data/cmip5/2041-2060/tmin/", file,sep=""), 
                    destfile=paste("ChelsaClimate/", file,sep=""), mode="wb")
      #max temp
      file <- paste("CHELSA_tasmax_mon_",model,"_",scenario,"_r1i1p1_g025.nc_", m, "_", yr, "_V1.2.tif", sep="")
      download.file(paste("https://www.wsl.ch/lud/chelsa/data/cmip5/2041-2060/tmax/", file,sep=""), 
                    destfile=paste("ChelsaClimate/", file,sep=""), mode="wb")
      #precip
      file <- paste("CHELSA_pr_mon_",model,"_",scenario,"_r1i1p1_g025.nc_", m, "_", yr, ".tif", sep="")
      download.file(paste("https://www.wsl.ch/lud/chelsa/data/cmip5/2041-2060/prec/", file,sep=""), 
                    destfile=paste("ChelsaClimate/", file,sep=""), mode="wb")
    }
  }
  
  ## Download future data ####
  download.chelsa.future(yr="2041-2060",scenario="rcp26",model="MPI-ESM-MR")
  download.chelsa.future(yr="2041-2060",scenario="rcp45",model="MPI-ESM-MR")
  download.chelsa.future(yr="2041-2060",scenario="rcp85",model="MPI-ESM-MR")

  download.chelsa.future(yr="2041-2060",scenario="rcp26",model="MIROC5")
  download.chelsa.future(yr="2041-2060",scenario="rcp45",model="MIROC5")
  download.chelsa.future(yr="2041-2060",scenario="rcp85",model="MIROC5")

  download.chelsa.future(yr="2041-2060",scenario="rcp26",model="CESM1-CAM5")
  download.chelsa.future(yr="2041-2060",scenario="rcp45",model="CESM1-CAM5")
  download.chelsa.future(yr="2041-2060",scenario="rcp85",model="CESM1-CAM5")

  download.chelsa.future(yr="2041-2060",scenario="rcp26",model="IPSL-CM5A-MR")
  download.chelsa.future(yr="2041-2060",scenario="rcp45",model="IPSL-CM5A-MR")
  download.chelsa.future(yr="2041-2060",scenario="rcp85",model="IPSL-CM5A-MR")

  download.chelsa.future(yr="2041-2060",scenario="rcp26",model="FIO-ESM")
  download.chelsa.future(yr="2041-2060",scenario="rcp45",model="FIO-ESM")
  download.chelsa.future(yr="2041-2060",scenario="rcp85",model="FIO-ESM")

  ## Function to Make raster stacks of future conditions (2070) and mask the sea ####
  create.raster.future<-function(model, scenario, yr){
    annualPET1996 <- raster("annualPET1996.grd") 
    fmask <- annualPET1996[[1]]
    # fmask <- crop(fmask, e)#mask this to run for the entire world

    # Stacking ####
    ta2050 <- raster()
    maxta2050 <- raster()
    minta2050 <- raster()
    pp2050 <- raster()
    m <- 1
    for (m in 1:12){
      
      file <- paste("ChelsaClimate/CHELSA_tas_mon_",model,"_",scenario,"_r1i1p1_g025.nc_", m, "_", yr, "_V1.2.tif", sep="")
      file <- raster(file)
      # file <- crop(file, e)#mask this to run for the entire world
      file <- mask(file, fmask)
      ta2050 <- stack(ta2050, file)
       
      file <- paste("ChelsaClimate/CHELSA_tasmax_mon_",model,"_",scenario,"_r1i1p1_g025.nc_", m, "_", yr, "_V1.2.tif", sep="")
      file <- raster(file)
      # file <- crop(file, e)#mask this to run for the entire world
      file <- mask(file, fmask)
      maxta2050 <- stack(maxta2050, file)
  
      file <- paste("ChelsaClimate/CHELSA_tasmin_mon_",model,"_",scenario,"_r1i1p1_g025.nc_", m, "_", yr, "_V1.2.tif", sep="")
      file <- raster(file)
      # file <- crop(file, e)#mask this to run for the entire world
      file <- mask(file, fmask)
      minta2050 <- stack(minta2050, file)
      
      file <- paste("ChelsaClimate/CHELSA_pr_mon_",model,"_",scenario,"_r1i1p1_g025.nc_", m, "_", yr, ".tif", sep="")
      file <- raster(file)
      # file <- crop(file, e)#mask this to run for the entire world
      file <- mask(file, fmask)
      pp2050 <- stack(pp2050, file)
    }
    writeRaster(ta2050, filename=paste("ta",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    writeRaster(maxta2050, filename=paste("maxta",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    writeRaster(minta2050, filename=paste("minta",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    writeRaster(pp2050, filename=paste("pr",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)

    ##Here we calculate aet and save the file at the original resolution
    ta2050 <- ta2050/10
    maxta2050 <- maxta2050/10
    minta2050 <- minta2050/10
    trang2050 <- abs(maxta2050-minta2050)
    solar2050 <- ETsolradRasters(ta2050[[1]], year=2070-1950)#1996-1950 = 46, default 0 is for 1950
    pet2050 <- monthlyPET(ta2050, solar2050, trang2050)
    annualPET2050 <- sum(pet2050)
    
    # set up naming scheme - only precip is different from default
    names(pp2050)<-c("prec_01","prec_02","prec_03","prec_04","prec_05","prec_06",
                     "prec_07","prec_08","prec_09","prec_10","prec_11","prec_12")
    ai2050 <- aridityIndexThornthwaite(pp2050, pet2050)
    
    writeRaster(annualPET2050, filename=paste("pet",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    writeRaster(ai2050, filename=paste("ai",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    
    # Aggregate at 50km ####
    rastemp <- raster("global_land_surface_50km.tif") 
    
    minta2050_50 <- aggregate(minta2050, fact=50, fun=mean)
    minta2050_50 <- projectRaster(minta2050_50, crs=projection(rastemp))
    minta2050_50 <- resample(minta2050_50, rastemp, method="bilinear")
    writeRaster(minta2050_50, filename=paste("minta2050_50",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    maxta2050_50 <- aggregate(maxta2050, fact=50, fun=mean)
    maxta2050_50 <- projectRaster(maxta2050_50, crs=projection(rastemp))
    maxta2050_50 <- resample(maxta2050_50, rastemp, method="bilinear")
    writeRaster(maxta2050_50, filename=paste("maxta2050_50",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    annualPET2050_50 <- aggregate(annualPET2050, fact=50, fun=mean)
    annualPET2050_50 <- projectRaster(annualPET2050_50, crs=projection(rastemp))
    annualPET2050_50 <- resample(annualPET2050_50, rastemp, method="bilinear")
    writeRaster(annualPET2050_50, filename=paste("annualPET2050_50",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    ai2050_50 <- aggregate(ai2050, fact=50, fun=mean)
    ai2050_50 <- projectRaster(ai2050_50, crs=projection(rastemp))
    ai2050_50 <- resample(ai2050_50, rastemp, methods="bilinear")
    writeRaster(ai2050_50, filename=paste("ai2050_50",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)


    minta1996_50 <- stack("minta1996_50.tif")
    maxta1996_50 <- stack("maxta1996_50.tif")
    annualPET1996_50 <- raster("annualPET1996_50.tif")
    ai1996_50 <- raster("ai1996_50.tif")

    minta_dif50 <- minta2050_50 - minta1996_50
    maxta_dif50 <- maxta2050_50 - maxta1996_50
    pet_dif50 <- annualPET2050_50 - annualPET1996_50
    ai_dif50 <- ai2050_50 - ai1996_50

    writeRaster(ai_dif50, filename=paste("ai_dif50",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    writeRaster(pet_dif50, filename=paste("pet_dif50",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    writeRaster(minta_dif50, filename=paste("minta_dif50",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    writeRaster(maxta_dif50, filename=paste("maxta_dif50",scenario,model,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
  }

  ## Create individual raster future ####
  
  rm(e)
  # create.raster.future(yr="2041-2060",scenario="rcp26",model="MPI-ESM-MR") 
  # create.raster.future(yr="2041-2060",scenario="rcp45",model="MPI-ESM-MR") 
  create.raster.future(yr="2041-2060",scenario="rcp85",model="MPI-ESM-MR") # Falta media creo
  
  # create.raster.future(yr="2041-2060",scenario="rcp26",model="MIROC5")
  # create.raster.future(yr="2041-2060",scenario="rcp45",model="MIROC5")
  # create.raster.future(yr="2041-2060",scenario="rcp85",model="MIROC5")
  
  # create.raster.future(yr="2041-2060",scenario="rcp26",model="CESM1-CAM5")
  # create.raster.future(yr="2041-2060",scenario="rcp45",model="CESM1-CAM5")
  # create.raster.future(yr="2041-2060",scenario="rcp85",model="CESM1-CAM5")
  
  # create.raster.future(yr="2041-2060",scenario="rcp26",model="IPSL-CM5A-MR")
  # create.raster.future(yr="2041-2060",scenario="rcp45",model="IPSL-CM5A-MR")
  create.raster.future(yr="2041-2060",scenario="rcp85",model="IPSL-CM5A-MR") # Falta pet ai y medias
  
  # create.raster.future(yr="2041-2060",scenario="rcp26",model="FIO-ESM")
  # create.raster.future(yr="2041-2060",scenario="rcp45",model="FIO-ESM")
  create.raster.future(yr="2041-2060",scenario="rcp85",model="FIO-ESM") # Falta pet ai y medias
  
  ## Average per variable and rcp for the five models ####
  average.raster.future<-function(scenario, yr){
    model1<-"MPI-ESM-MR"
    model2<-"MIROC5"
    model3<-"CESM1-CAM5"
    model4<-"IPSL-CM5A-MR"
    model5<-"FIO-ESM"
    
    proj1 <-raster(paste("Worked/ai_dif50",scenario,model1,yr,".tif", sep="_"))
    proj2 <-raster(paste("Worked/ai_dif50",scenario,model2,yr,".tif", sep="_"))
    proj3 <-raster(paste("Worked/ai_dif50",scenario,model3,yr,".tif", sep="_"))
    proj4 <-raster(paste("Worked/ai_dif50",scenario,model4,yr,".tif", sep="_"))
    proj5 <-raster(paste("Worked/ai_dif50",scenario,model5,yr,".tif", sep="_"))
    
    projections.ai.mean <-
      mean(proj1, proj2, proj3, proj4, proj5)
    
    writeRaster(
      projections.ai.mean,
      filename = paste0(scenario, "_", "ai50_ensemble.tif"),
      format = "GTiff",
      overwrite = TRUE)
    
    proj1 <-raster(paste("Worked/pet_dif50",scenario,model1,yr,".tif", sep="_"))
    proj2 <-raster(paste("Worked/pet_dif50",scenario,model2,yr,".tif", sep="_"))
    proj3 <-raster(paste("Worked/pet_dif50",scenario,model3,yr,".tif", sep="_"))
    proj4 <-raster(paste("Worked/pet_dif50",scenario,model4,yr,".tif", sep="_"))
    proj5 <-raster(paste("Worked/pet_dif50",scenario,model5,yr,".tif", sep="_"))
    
    projections.pet.mean <-
      mean(proj1, proj2, proj3, proj4, proj5)
    
    writeRaster(
      projections.pet.mean,
      filename = paste0(scenario, "_", "pet50_ensemble.tif"),
      format = "GTiff",
      overwrite = TRUE)
    
    proj1 <-stack(paste("Worked/maxta_dif50",scenario,model1,yr,".tif", sep="_"))
    proj2 <-stack(paste("Worked/maxta_dif50",scenario,model2,yr,".tif", sep="_"))
    proj3 <-stack(paste("Worked/maxta_dif50",scenario,model3,yr,".tif", sep="_"))
    proj4 <-stack(paste("Worked/maxta_dif50",scenario,model4,yr,".tif", sep="_"))
    proj5 <-stack(paste("Worked/maxta_dif50",scenario,model5,yr,".tif", sep="_"))
    
    projections.maxta.mean <-
      mean(proj1, proj2, proj3, proj4, proj5)
    
    writeRaster(
      projections.maxta.mean,
      filename = paste0(scenario, "_", "maxta50_ensemble.tif"),
      format = "GTiff",
      overwrite = TRUE)
    
    proj1 <-stack(paste("Worked/minta_dif50",scenario,model1,yr,".tif", sep="_"))
    proj2 <-stack(paste("Worked/minta_dif50",scenario,model2,yr,".tif", sep="_"))
    proj3 <-stack(paste("Worked/minta_dif50",scenario,model3,yr,".tif", sep="_"))
    proj4 <-stack(paste("Worked/minta_dif50",scenario,model4,yr,".tif", sep="_"))
    proj5 <-stack(paste("Worked/minta_dif50",scenario,model5,yr,".tif", sep="_"))
    
    projections.minta.mean <-
      mean(proj1, proj2, proj3, proj4, proj5)
    
    writeRaster(
      projections.minta.mean,
      filename = paste0(scenario, "_", "minta50_ensemble.tif"),
      format = "GTiff",
      overwrite = TRUE)
    
    minta1996_50 <- stack("minta1996_50.tif")
    maxta1996_50 <- stack("maxta1996_50.tif")
    annualPET1996_50 <- raster("annualPET1996_50.tif")
    ai1996_50 <- raster("ai1996_50.tif")
    
    ai_ensemble_dif50 <- projections.ai.mean - ai1996_50
    pet_ensemble_dif50 <- projections.pet.mean - annualPET1996_50
    maxta_ensemble_dif50 <- projections.maxta.mean - maxta1996_50
    minta_ensemble_dif50 <- projections.minta.mean - minta1996_50
    
    writeRaster(ai_ensemble_dif50, filename=paste("ai_ensemble_dif50",scenario,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    writeRaster(pet_ensemble_dif50, filename=paste("pet_ensemble_dif50",scenario,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    writeRaster(minta_ensemble_dif50, filename=paste("minta_dif50",scenario,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
    writeRaster(maxta_ensemble_dif50, filename=paste("maxta_dif50",scenario,yr,".tif", sep="_"),
                options="INTERLEAVE=BAND", overwrite=TRUE)
  }
  
  rm(e)
  
  average.raster.future(scenario="rcp26", yr="2041-2060")
  average.raster.future(scenario="rcp45", yr="2041-2060")
  average.raster.future(scenario="rcp85", yr="2041-2060")