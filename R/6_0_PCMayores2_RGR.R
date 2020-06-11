
# Pablo Salazar Z
# Universidad de Córdoba
# 14/04/2020
# Proccessing TreesI3 (PCMayores) and TreesI2 (PCMayores2) to get a plant production rate
# Spanish National Forestry inventory
# Andalucia subset

# To run 
# TreesI3 from "2_0_data_loading.R" is required
# TreesI2 from "2_0_data_loading.R" is required
# allo from "5_0_PCMayores.R" is required
# DataMp_sf from 4_0_climate_data.R is required (which comes from 3_0_data_projection.R)

# cleaning data from TreesI3 before join with TreesI2--------------

# Filtering new plots, wrong subclase, and non-recorded trees from IFN2
T3_A <- TreesI3 %>%
  filter(Cla == "A" & Subclase %in% c("3C", "1")) %>%
  mutate(code = str_c(Provincia, Estadillo, OrdenIf2)) # create a single label for each tree
colnames(T3_A) <- paste(colnames(T3_A), "3", sep = "_")
# Creating a common variable between T3_A and T2_A
T2_A <- TreesI2 %>%
  mutate(code = str_c(Provincia, Estadillo, NumOrden)) %>%
  mutate(Especie = str_pad(Especie, 3, pad = "0"))
colnames(T2_A) <- paste(colnames(T2_A), "2", sep = "_")
names(T2_A)[15] <- "code_3"
# T2_A$code_3 has no duplicates, therefore it is possible to left join T2 and T3 with warnings. T2 will duplicate itself a few times in T3 but that is not an issue

# Merging T3 and T2 ----------------------
T3T2 <- left_join(T3_A, T2_A, by = "code_3", all.x = TRUE)

# Looping a new variable specie with Especie_3 NA replaced by Especie_2 data  

T3T2$specie<-T3T2$Especie_3
for(i in 1:nrow(T3T2)){
  if(is.na(T3T2$specie[i])){
    T3T2$specie[i] <- T3T2$Especie_2[i]
  } else
    T3T2$specie[i] <- T3T2$Especie_3[i]
} ## Now there is no zero in specie


## Creating an "state" variable based on OrdenIf3 and OrdenIf2 and Dove documents

#First, we need to delete that one OrdenIf2= 888 that we dont need either way.

T3T2$state <- NA
for(i in 1:nrow(T3T2)){
  if(T3T2[i,6] == "999" ){
    T3T2$state[i] <- "E" # shouldnt have been measured it in IFN3
  } else
    if(T3T2[i,6] =="888"){
      if(T3T2[i,7] == "999"){
        T3T2$state[i] <- "E" # shouldnt have been measured it in IFN2
      } else
        if(T3T2[i,7] == "000"){
          T3T2$state[i] <- "MP-nc" # Dead, still there, but not measured it
          } else
            T3T2$state[i] <- "MP" # Dead but measured it
      } else
        if(T3T2[i,6] == "000"){
          if(T3T2[i,7] == "999"){
            T3T2$state[i] <- "E" # shouldnt have been measured it in IFN2
          } else
            if(T3T2[i,7] == "000"){
              T3T2$state[i] <- "E" # shouldnt exist
            } else 
              T3T2$state[i] <- "MA" # dead and inexistent
        } else
          if(T3T2[i,7] == "999"){
            T3T2$state[i] <- "E"# shouldnt have been measured it in IFN2
          } else
            if(T3T2[i,7] == "000"){
              T3T2$state[i] <- "R"# Regenerated
            } else
              T3T2$state[i] <- "V" # alive
            }
         ## Now there is no NA in state

# Count the number of trees with Distance > 25m, Db < 75mm, or Ht >70m

# there are only three data with Dn < 75 mm, no need to filter it because they are OrdenIf3 == 999 or 888
# There are 578 data with Distancia > 25m. Paloma suggest not to filter. The mean distancia of this subset is 26.5
# There are zero data with height (Ht) > 70 m. No issues here

# add the year column from Dove data
# loading year data per Provincia and Estadillo

year_data <- read_csv("data/dove/year_data.csv")
year_data$Provincia3 <- as.factor(year_data$Provincia3)
levels(year_data$Provincia3) <- c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla")
year_data <- year_data %>%
  mutate(Provincia_3 = Provincia3) %>%
  mutate(Estadillo_3 = str_pad(Estadillo3, 4, pad = "0")) %>%
  mutate(difyear = year32)
  
T3T2 <- merge(T3T2, year_data[,c(8:10)], by = c("Provincia_3", "Estadillo_3"), all.x = TRUE)

## Merging T3T2 with allo -------
t3t2_allo <- merge(T3T2, allo, by.x = "specie", by.y = "COD_SPP_IFN3", all.x = TRUE)

## calculating Biomass for T3 and T2 ----------

# First step, exclude trees based on "state"

t3t2_allo <- t3t2_allo %>%
  filter(state == "V")
#areal biomass
t3t2_allo <- t3t2_allo %>%
  mutate(ABiomass_3 = CFA*(Dn_3/10)^b, # Biomass in kg per tree
         ABiomass_2 = CFA*(Dn_2/10)^b)

#Root biomass
t3t2_allo <- t3t2_allo %>%
  mutate(RBiomass_3 = CFAr*(Dn_3/10)^br, # Biomass in kg per tree
         RBiomass_2 = CFAr*(Dn_2/10)^br)

## calculating Biomass (Mg) per Ha for T3 and T2 ------------------
t3t2_allo <- t3t2_allo %>%
  mutate(AB3_Mgha = ABiomass_3/(0.000314159265*area_3^2)/1000, # Biomass T3 in Mg per ha
         AB2_Mgha = ABiomass_2/(0.000314159265*area_2^2)/1000, # Biomass T2
         RB3_Mgha = RBiomass_3/(0.000314159265*area_3^2)/1000, # Biomass R3
         RB2_Mgha = RBiomass_2/(0.000314159265*area_2^2)/1000) # Biomass R2

## calculation RGR between T3 and T2 -----------------
t3t2_allo <- t3t2_allo %>%
  mutate(A_RGR = (log(ABiomass_3)/log(ABiomass_2))/(difyear*1000), # RGR
         R_RGR = (log(RBiomass_3)/log(RBiomass_2))/(difyear*1000), # in Mg per year
         B_BP_Mgha = (AB3_Mgha - AB2_Mgha)/difyear,
         R_BP_MgHa = (RB3_Mgha - RB2_Mgha)/difyear)# Root Biomass 


#filter(B_RGR > 0 & B_RGR < 100) # a filter to avoid negative growth and too high values       
# This is going to be need it according to the variable you are measuring.


## adding spatial coordinates for climate data ------------
colnames(t3t2_allo)[2:3] <- c("Provincia", "Estadillo")
t3t2_allo <- t3t2_allo %>% ## Filtering unnecesarry data at the moment 
  dplyr::select(-Cla_3:-Compara_3, -Provincia_2:-ParamEsp_2, -Correspondencia:-CFAr,
                -RBiomass_3, -RBiomass_2, -RB3_Mgha, -RB2_Mgha, -R_RGR, -R_BP_MgHa)

t3t2_allo2 <- left_join(t3t2_allo, DataMp_sf[,c(3:5,11,12,14:34)], by = c("Provincia", "Estadillo"))

# print results

write.csv(t3t2_allo2, "products/Andalucia.csv")

## Summary tables with plot data ----------

Plot_a <- t3t2_allo %>%
  drop_na(ABiomass_3) %>%
  group_by(Provincia, Estadillo, Sp, area_2) %>%
  summarise_at(vars(ABiomass_3:B_BP_Mgha), sum) %>%
  mutate(N_Trees = n()) %>%
  mutate(Tree_dens = N_Trees/area_2) %>%
  group_by(Provincia, Estadillo, Sp) %>%
  summarise_at(vars(ABiomass_3:Tree_dens), sum) %>% #Table with N trees by Provincia, estadillo and Sp 
  group_by(Provincia, Estadillo) %>%
  top_n(1,Tree_dens) # Data is filtered so only Biomass and production of the main species is shown.
# Other option is to calculted the most common specie and add it to the total biomass in the plot (with more than 1 sp or not).

Plot_a <- left_join(Plot_a, DataMp_sf[,c(3:5,11,12,14:34)], by = c("Provincia", "Estadillo"))

write.csv(Plot_a, "products/Andalucia_plot.csv")
