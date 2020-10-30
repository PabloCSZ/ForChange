
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

# loading year data per Provincia and Estadillo

year_data <- read_csv("data/dove/year_data.csv")
year_data$Provincia3 <- as.factor(year_data$Provincia3)
levels(year_data$Provincia3) <- c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla")
year_data <- year_data %>%
  mutate(Provincia_3 = Provincia3) %>%
  mutate(Estadillo_3 = str_pad(Estadillo3, 4, pad = "0")) %>%
  mutate(difyear = year32)

# Calculating Plant biomass in the IFN3
TreesI3_allo <- merge(TreesI3, allo, by.x = "Especie", by.y = "COD_SPP_IFN3", all.x = TRUE)

#Aereal biomass
TreesI3_allo <- TreesI3_allo %>%
  mutate(AB3_kgT = CFA*(Dn/10)^b) %>% # Biomass in kg per tree
  mutate(AB3_Mgha = AB3_kgT/(0.000314159265*area^2)/1000) # Biomass T3 in Mg per ha

T3_allo <- TreesI3_allo %>% ## Filtering necesarry data at the moment 
  dplyr::select(Provincia, Estadillo, area, Sp, AB3_kgT, AB3_Mgha) # at tree level

T3_allo_sp <- T3_allo %>%
  tidyr::drop_na(AB3_kgT) %>%
  group_by(Provincia, Estadillo, area, Sp) %>%
  summarise(N_Trees = n(), AB3_Mgha = sum(AB3_Mgha)) %>%
  mutate(Tree_dens = N_Trees/(0.000314159265*area^2)) %>% # Tree per ha
  group_by(Provincia, Estadillo, Sp) %>%
  summarise(Tree_dens = sum(Tree_dens),AB3_Mgha_sp = sum(AB3_Mgha)) # a #Table  by Provincia, estadillo and sp

## Data transformation (grouping by Provincia, Estadillo, area and Sp)--------------

T3_allo_p <- T3_allo %>% 
  tidyr::drop_na(AB3_kgT) %>%
  group_by(Provincia, Estadillo) %>%
  summarise(AB3_Mgha = sum(AB3_Mgha)) 
             #Table  by Provincia, estadillo 

T3_allo_p <- left_join(T3_allo_p, T3_allo_sp, by = c("Provincia", "Estadillo")) # datframe by provincia, estadillo and sp
T3_allo_p <- T3_allo_p %>%
  mutate(AB3_Mgha_perc =AB3_Mgha_sp/AB3_Mgha*100) # calculate specie importance for total biomass

T3_allo_p <- T3_allo_p %>%
  group_by(Provincia, Estadillo) %>%
  top_n(1,AB3_Mgha_perc) # filter only the most important sp / dataframe by provincia and estadillo 

T3_allo_p <- left_join(T3_allo_p, year_data[,c(5,8,9)], by = c("Provincia" = "Provincia_3", "Estadillo" = "Estadillo_3")) # merge with year 

T3_allo_sf <- left_join(T3_allo_p, DataMp_sf[,c(3,4,16,24)], by = c("Provincia", "Estadillo")) # merge with DataMp_sf to include coordinates
T3_allo_sf <- st_as_sf(T3_allo_sf)


#We shouldn't need the next two lines in the future, so delete them after try it for a second time
#T3_allo_sf <- T3_allo_sf %>%
#    filter(!Provincia %in% c("Zamora" , "Ourense", "Toledo", "Lugo"))

#write.csv(T3_allo_sf, "products/Andalucia_TotalBiomass.csv")

## From this point forward, the code is not clear and revised. Do not run  -----------


# Calculating plant biomass in the IFN2 
TreesI2_ <- TreesI2 %>%
  mutate(Especie = str_pad(Especie, 3, pad = "0"))

TreesI2_allo <- merge(TreesI2_, allo, by.x = "Especie", by.y = "COD_SPP_IFN3", all.x = TRUE)

#Aereal biomass
TreesI2_allo <- TreesI2_allo %>%
  mutate(AB2_kgT = CFA*(Dn/10)^b) %>% # Biomass in kg per tree
  mutate(AB2_Mgha = AB2_kgT/(0.000314159265*area^2)/1000) # Biomass T3 in Mg per ha

T2_allo <- TreesI2_allo %>% ## Filtering necesarry data at the moment 
  dplyr::select(Provincia, Estadillo, area, Sp, AB2_kgT, AB2_Mgha) # at tree level

T2_allo_sp <- T2_allo %>%
  tidyr::drop_na(AB2_kgT) %>%
  group_by(Provincia, Estadillo, area, Sp) %>%
  summarise(N_Trees = n(), AB2_Mgha = sum(AB2_Mgha)) %>%
  mutate(Tree_dens = N_Trees/(0.000314159265*area^2)) %>% # Tree per ha
  group_by(Provincia, Estadillo, Sp) %>%
  summarise(Tree_dens = sum(Tree_dens),AB2_Mgha_sp = sum(AB2_Mgha)) # a #Table  by Provincia, estadillo and sp

## Data transformation (grouping by Provincia, Estadillo, area and Sp)--------------

T2_allo_p <- T2_allo %>% 
  tidyr::drop_na(AB2_kgT) %>%
  group_by(Provincia, Estadillo) %>%
  summarise(AB2_Mgha = sum(AB2_Mgha)) 
#Table  by Provincia, estadillo 

T2_allo_p <- left_join(T2_allo_p, T2_allo_sp, by = c("Provincia", "Estadillo")) # datframe by provincia, estadillo and sp
T2_allo_p <- T2_allo_p %>%
  mutate(AB2_Mgha_perc = AB2_Mgha_sp/AB2_Mgha*100) # calculate specie importance for total biomass

T2_allo_p <- T2_allo_p %>%
  group_by(Provincia, Estadillo) %>%
  top_n(1,AB2_Mgha_perc) # filter only the most important sp / dataframe by provincia and estadillo 

T2_allo_p <- left_join(T2_allo_p, year_data[,c(5,8,9)], by = c("Provincia" = "Provincia_3", "Estadillo" = "Estadillo_3")) # merge with year 

T2_allo_sf <- left_join(T2_allo_p, DataMp_sf[,c(3,4,16,24)], by = c("Provincia", "Estadillo")) # merge with DataMp_sf to include coordinates
T2_allo_sf <- st_as_sf(T2_allo_sf)


#We shouldn't need the next two lines in the future, so delete them after try it for a second time
#T2_allo_sf <- T2_allo_sf %>%
#    filter(!Provincia %in% c("Zamora" , "Ourense", "Toledo", "Lugo"))

#write.csv(T2_allo_sf, "products/Andalucia_TotalBiomass.csv")

## calculation RGR between T3 and T2 -----------------
t3t2_allo <- t3t2_allo %>%
  mutate(A_RGR = (log(AB3_kgT)/log(AB2_kgT))/(difyear*1000), # RGR
         R_RGR = (log(RB3_kgT)/log(RB2_kgT))/(difyear*1000), # in Mg per year
         B_BP_Mgha = (AB3_Mgha - AB2_Mgha)/difyear) # Biomass production
         # Recalcuate biomass by m2 because sampling method was different in each area
         
         
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
