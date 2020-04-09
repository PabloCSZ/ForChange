# Pablo Salazar Z
# Universidad de Córdoba
# 09/04/2020
# Graph making from the IFN
# Spanish National Forestry inventory
# Andalucia subset

# DataMp_sf from "5_0_climate_data.R" is required

#some testing graphs
Koppen<-tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + tm_fill() + tm_borders() +
  tm_shape(DataMp_sf) + tm_dots(col = "grad1", size = 1, n = 10) + tm_layout(title = "Koppen")
Emberger<-tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + tm_fill() + tm_borders() +
  tm_shape(DataMp_sf) + tm_dots(col = "grad2", size = 1, n = 10) + tm_layout(title = "Emberger")
Dm<-tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + tm_fill() + tm_borders() +
  tm_shape(DataMp_sf) + tm_dots(col = "grad3", size = 1, n = 10) + tm_layout(title = "Dm")
gradient <- tmap_arrange(Koppen, Emberger, Dm, nrow=3)
tmap_save(gradient, "Gradient.jpg")

# Making some graphics to see spatial biomass distribution data

phalep<-tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + 
  tm_fill() + tm_borders() +
  tm_shape(subset(Biom_m2_sf, Sp == "Pinus halepensis" )) + 
  tm_dots(col = "grad1", size = "bio_dens", n = 5) + 
  tm_layout(title = "P. halepensis Biomass/m2",
            legend.outside = TRUE)
qilex <- tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + 
  tm_fill() + tm_borders() +
  tm_shape(subset(Biom_m2_sf, Sp == "Quercus ilex" )) + 
  tm_dots(col = "grad1", size = "bio_dens", n = 5) + 
  tm_layout(title = "Q. ilex Biomass/m2",
            legend.outside = TRUE)
ppinea <- tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + 
  tm_fill() + tm_borders() +
  tm_shape(subset(Biom_m2_sf, Sp == "Pinus pinea" )) + 
  tm_dots(col = "grad1", size = "bio_dens", n = 5) + 
  tm_layout(title = "P. pinea Biomass/m2",
            legend.outside = TRUE)
qsuber <- tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + 
  tm_fill() + tm_borders() +
  tm_shape(subset(Biom_m2_sf, Sp == "Quercus suber" )) + 
  tm_dots(col = "grad1", size = "bio_dens", n = 5) + 
  tm_layout(title = "Q. suber Biomass/m2",
            legend.outside = TRUE)
ppinas <- tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + 
  tm_fill() + tm_borders() +
  tm_shape(subset(Biom_m2_sf, Sp == "Pinus pinaster" )) + 
  tm_dots(col = "grad1", size = "bio_dens", n = 5) + 
  tm_layout(title = "P. pinaster Biomass/m2",
            legend.outside = TRUE)
species_bio <- tmap_arrange(Koppen, qilex, phalep, qsuber, ppinea, ppinas, nrow=3, ncol = 2)
tmap_save(species_bio, "species_bio.jpg")
tmap_save(qilex, "qilex_bio.jpg")