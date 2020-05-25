
# Pablo Salazar Z
# Universidad de Córdoba
# 08/04/2020
# Proccessing TreesI3 (PCMayores) to get plant productivity
# Spanish National Forestry inventory
# Andalucia subset

# DataMp_sf from "4_0_climate_data.R" is required
# TreesI3 from "2_0_data_loading.R" is required


#  Calculate BEMs --------------

allo <- read_delim("data/Allom/Allometric.txt", ## Paloma sheet with species paramenters
                   "\t", escape_double = FALSE, trim_ws = TRUE)
allo2 <- read_excel("data/Allom/species data.xlsx", 
                    sheet = "Hoja2")## Extra parameters for more species
allo2$SEE2 <- allo2$SEE^2
allo2$CF <- exp(allo2$SEE2/2)
allo2$ea <- exp(allo2$a)
allo2$CFA <- allo2$CF*allo2$ea
allo2$SEE2r <- allo2$SEEr^2
allo2$CFr <- exp(allo2$SEE2r/2)
allo2$ear <- exp(allo2$ar)
allo2$CFAr <- allo2$CFr*allo2$ear
allo2$Ecuacion <- NULL
allo2$COD_SPP_IFN3 <- str_pad(allo2$COD_SPP_IFN3, 3, pad = "0")

allo <- rbind.fill(allo,allo2)
allo <- allo[1:102,] # the last 9 species dont have an equation yet

#Merging TreesI3 with the BEM equations to calculate tree biomass
TreesI3_allo <- merge(TreesI3, allo, by.x = "Especie", by.y = "COD_SPP_IFN3", all.x = TRUE)
TreesI3_allo$Biomass <- 0
for(i in 1:nrow(TreesI3_allo)){
  if(!is.na(TreesI3_allo$Dn[i])){
    TreesI3_allo$Biomass[i] <- TreesI3_allo$CFA[i]*(TreesI3_allo$Dn[i]/10)^TreesI3_allo$b[i]
  } else
    TreesI3_allo$Biomass[i] <- NA
}

# Recalcuate biomass by m2 because sampling method was different in each area
TreesI3_allo$bio_dens <- TreesI3_allo$Biomass/(3.1415*TreesI3_allo$area^2)

## Data transformation (grouping by Provincia, Estadillo, area and Sp)--------------
# summarise Bio_dens (as bio_m2) by Provincia and Estadillo (also show N trees, an N species)
TreesI3_allo$Sp <- as.factor(TreesI3_allo$Sp)

Biom_m2_sp <- TreesI3_allo %>%
  drop_na(bio_dens) %>%
  group_by(Provincia, Estadillo, Sp, area) %>%
  summarise(bio_dens = sum(bio_dens), N_Trees = n()) %>%
  mutate(Tree_dens = N_Trees/area) %>%
  group_by(Provincia, Estadillo, Sp) %>%
  summarise(bio_sp_m2 = sum(bio_dens), N_trees = sum(N_Trees), N_tree_m2 = sum(Tree_dens)) #Table with N trees by Provincia, estadillo and Sp 

Biom_m2 <- Biom_m2_sp %>%
  group_by(Provincia, Estadillo) %>%
  summarise(bio_plot_m2 = sum(bio_sp_m2), N_trees = sum(N_trees), N_tree_m2 = sum(N_tree_m2), N_Sp = n_distinct(Sp)) #Table with N trees by Provincia and estadillo

Biom_m2_sf <- merge(DataMp_sf[,c(3:5,14:32)], Biom_m2, by = c("Provincia","Estadillo")) #transforming to sf object
#write.csv(Biom_m2_sf, "Biomass por estadillo .csv")

# Calculate Biomass percentage per species within each plot
Biom_m2_sp_1 <- left_join(Biom_m2_sp, Biom_m2[1:3], by = c("Provincia", "Estadillo"))
Biom_m2_sp_1 <- Biom_m2_sp_1 %>%
  mutate(bio_perc = bio_sp_m2/bio_plot_m2*100,
         bio_treeSp_mean = bio_sp_m2/N_trees)
Biom_m2_sp_1$bio_plot_m2 <- NULL
Biom_m2_sp_1 <- Biom_m2_sp_1[,c(1,2,3,5,6,4,7,8)]

Biom_m2_sp_sf <- merge(DataMp_sf[,c(3:5,14:34)], Biom_m2_sp_1, by = c("Provincia","Estadillo"))
#write.csv(Biom_m2_sp_sf, "Biomass por estadillo y especie.csv")

# Transform the dataframe to show species (and its variables) in different columns-----------

Biom_m2_sp_2 <- Biom_m2_sp %>%
  pivot_wider(names_from = Sp, values_from = c(bio_sp_m2, N_trees, N_tree_m2))
Biom_m2_sp_2$Bio_plot_m2 <- rowSums(Biom_m2_sp_2[,3:68], na.rm = TRUE) # Total collumn 
xx <- apply(Biom_m2_sp_2[3:68], 2, function(x) x/Biom_m2_sp_2$Bio_plot_m2)
colnames(xx) <- paste(colnames(xx), "perc", sep = "_")
xx <- as.data.frame(xx)
Biom_m2_sp_2$N_trees_m2 <- rowSums(Biom_m2_sp_2[,135:200], na.rm = TRUE) # Total collumn 
Biom_m2_sp_2 <- bind_cols(Biom_m2_sp_2, xx) # Sp as colummns and percentage
Biom_m2_sp_2_sf <- merge(DataMp_sf[,c(3:5,14:32)], Biom_m2_sp_2, by = c("Provincia","Estadillo"))

#write.csv(Biom_m2_sp_2_sf, "Biomasa por estadillo y especie en columnas.csv")
