
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

T3T2 <- T3T2 %>% filter(!is.na(OrdenIf2_3), !is.na(OrdenIf3_3))
# the If loop can not work if there is an NA inside the data. 

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
  
T3T2 <- merge(T3T2, year_data[,c(4,5,8:10)], by = c("Provincia_3", "Estadillo_3"), all.x = TRUE)

## Merging T3T2 with allo -------
t3t2_allo <- merge(T3T2, allo, by.x = "specie", by.y = "COD_SPP_IFN3", all.x = TRUE)

## calculating Biomass for  Alive trees in both T3 and T2 ----------

# First step, exclude trees based on "state"

t3t2_allo <- t3t2_allo %>%
  filter(state == "V")
#areal biomass
t3t2_allo <- t3t2_allo %>%
  mutate(AB3_kgT = CFA*(Dn_3/10)^b, # Biomass in kg per tree
         AB2_kgT = CFA*(Dn_2/10)^b)

#Root biomass
t3t2_allo <- t3t2_allo %>%
  mutate(RB3_kgT = CFAr*(Dn_3/10)^br, # Biomass in kg per tree
         RB2_kgT = CFAr*(Dn_2/10)^br)

## calculating Biomass (Mg) per Ha for T3 and T2 ------------------
t3t2_allo <- t3t2_allo %>%
  mutate(AB3_Mgha = AB3_kgT/(0.000314159265*area_2^2)/1000, # Biomass T3 in Mg per ha
         AB2_Mgha = AB2_kgT/(0.000314159265*area_2^2)/1000, # Biomass T2
         RB3_Mgha = RB3_kgT/(0.000314159265*area_2^2)/1000, # Biomass R3
         RB2_Mgha = RB2_kgT/(0.000314159265*area_2^2)/1000) # Biomass R2

# There are several ways to to calculate the previous variables. Intuitively, it make sense to calculcate the areal biomass from the IFN3 using "area_3" because it woukd provide an accurate 
# representation of the biomass per area in that moment. However, in our case is more important to make an accurate comparisson with the IFN2. Therefore, we need to use "area_2" in all equation.
# At the end, we are not really calculating the biomass per area in the IFN3. We are calculation how much biomass there is in the trees from the IFN2 that are still alive in the IFN3. 

## calculation RGR between T3 and T2 -----------------
t3t2_allo <- t3t2_allo %>%
  mutate(A_RGR = (log(AB3_kgT)-log(AB2_kgT))*1000/(difyear), # RGR
         R_RGR = (log(RB3_kgT)-log(RB2_kgT))*1000/(difyear), # in Mg per year
         B_BP_Mgha = (AB3_Mgha - AB2_Mgha)/difyear, # Biomass production
         R_BP_MgHa = (RB3_Mgha - RB2_Mgha)/difyear)# Root Biomass 

#filter(B_RGR > 0 & B_RGR < 100) # a filter to avoid negative growth and too high values       
# This is going to be need it according to the variable you are measuring.


## adding spatial coordinates for climate data ------------
colnames(t3t2_allo)[2:3] <- c("Provincia", "Estadillo")
t3t2_allo <- t3t2_allo %>% ## Filtering unnecesarry data at the moment 
  dplyr::select(-Cla_3:-Compara_3, -Provincia_2:-ParamEsp_2, -Correspondencia:-CFAr,
                -RB3_kgT, -RB2_kgT, -RB3_Mgha, -RB2_Mgha, -R_RGR, -R_BP_MgHa)

t3t2_allo2 <- left_join(t3t2_allo, DataMp_sf[,c(3:5,11,12,14:34)], by = c("Provincia", "Estadillo"))

# print results

#write.csv(t3t2_allo2, "products/Andalucia.csv")

## Summary tables with plot data ----------

Plot_a <- t3t2_allo %>%
  drop_na(AB3_kgT) %>%
  filter(Sp == "Quercus ilex") %>%
  group_by(Provincia, Estadillo, area_2) %>%
  summarise(AB3_kgT = mean(AB3_kgT), AB2_kgT = mean(AB2_kgT), AB3_Mgha = sum(AB3_Mgha), AB2_Mgha = sum(AB2_Mgha), A_RGR = mean(A_RGR),
            B_BP_Mgha = sum(B_BP_Mgha), difyear = mean(difyear), N_Trees = n()) %>%
  mutate(Tree_dens = N_Trees/area_2) %>%
  group_by(Provincia, Estadillo) %>%
  summarise(AB3_kgT = mean(AB3_kgT), AB2_kgT = mean(AB2_kgT), AB3_Mgha = sum(AB3_Mgha), AB2_Mgha = sum(AB2_Mgha), A_RGR = mean(A_RGR),
            B_BP_Mgha = sum(B_BP_Mgha), difyear = mean(difyear), Tree_dens = sum(Tree_dens)) #Table with N trees by Provincia, estadillo and Sp 
  
# Other option is to calculted the most common specie and add it to the total biomass in the plot (with more than 1 sp or not).

Plot_a <- left_join(Plot_a, DataMp_sf[,c(3:5,11,12,14:34)], by = c("Provincia", "Estadillo")) 
Plot_a <- Plot_a[!duplicated(Plot_a[,1:2]),] # For some reason that I dont get atm we have duplicates in DataMp_sf. They are harmless, but we have to be aware of them and clean them eventually. 

#write.csv(Plot_a, "products/Andalucia_plot.csv")


## Calculation Biomass for alive trees in both T3 and T2 independently ------------


## Merging T3T2 with allo -------
t3t2_allo <- merge(T3T2, allo, by.x = "specie", by.y = "COD_SPP_IFN3", all.x = TRUE)

## calculating Biomass for  Alive trees in both T3 and T2 ----------

# First step, exclude trees based on "state"

t3t2_allob <- t3t2_allo %>%
  filter(state %in% c("V", "R", "MP"))
#areal biomass
t3t2_allob <- t3t2_allob %>%
  mutate(AB3_kgT = CFA*(Dn_3/10)^b, # Biomass in kg per tree
         AB2_kgT = CFA*(Dn_2/10)^b)

#Root biomass
t3t2_allob <- t3t2_allob %>%
  mutate(RB3_kgT = CFAr*(Dn_3/10)^br, # Biomass in kg per tree
         RB2_kgT = CFAr*(Dn_2/10)^br)
## calculating Biomass (Mg) per Ha for T3 and T2 ------------------
t3t2_allob <- t3t2_allob %>%
  mutate(AB3_Mgha = AB3_kgT/(0.000314159265*area_3^2)/1000, # Biomass T3 in Mg per ha
         AB2_Mgha = AB2_kgT/(0.000314159265*area_2^2)/1000, # Biomass T2
         RB3_Mgha = RB3_kgT/(0.000314159265*area_3^2)/1000, # Biomass R3
         RB2_Mgha = RB2_kgT/(0.000314159265*area_2^2)/1000) # Biomass R2

# There are several ways to to calculate the previous variables. Intuitively, it make sense to calculcate the areal biomass from the IFN3 using "area_3" because it woukd provide an accurate 
# representation of the biomass per area in that moment. However, in our case is more important to make an accurate comparisson with the IFN2. Therefore, we need to use "area_2" in all equation.
# At the end, we are not really calculating the biomass per area in the IFN3. We are calculation how much biomass there is in the trees from the IFN2 that are still alive in the IFN3. 


## calculation RGR between T3 and T2 -----------------
t3t2_allob <- t3t2_allob %>%
  mutate(A_RGR = (log(AB3_kgT) - log(AB2_kgT))*1000/(difyear), # RGR
         R_RGR = (log(RB3_kgT) - log(RB2_kgT))*1000/(difyear), # in Mg per year
         B_BP_Mgha = (AB3_Mgha - AB2_Mgha)/difyear, # Biomass production
         R_BP_MgHa = (RB3_Mgha - RB2_Mgha)/difyear)# Root Biomass 


## Working only to get IFN3 data

T3_allo <- t3t2_allob %>% ## Filtering necesarry data at the moment 
  dplyr::select(Provincia_3, Estadillo_3, area_3, Sp, AB3_kgT, AB3_Mgha, year3, area_2, AB2_kgT, AB2_Mgha, year2, state) # at tree level

T3_allo_sp <- T3_allo %>%
  tidyr::drop_na(AB3_kgT) %>%
  group_by(Provincia_3, Estadillo_3, Sp) %>%
  summarise(AB3_Mgha_sp = sum(AB3_Mgha)) # a #Table  by Provincia, estadillo and sp

## Data transformation (grouping by Provincia, Estadillo, area and Sp)--------------

T3_allo_p <- T3_allo %>% 
  tidyr::drop_na(AB3_kgT) %>%
  group_by(Provincia_3, Estadillo_3, area_3) %>%
  summarise(N_Trees = n(), AB3_Mgha = sum(AB3_Mgha), year3 = mean(year3))  %>%
  mutate(Tree_dens = N_Trees/(0.000314159265*area_3^2)) %>% # Tree per ha
  group_by(Provincia_3, Estadillo_3) %>%
  summarise(Tree_dens = sum(Tree_dens),AB3_Mgha = sum(AB3_Mgha), year3 = mean(year3)) #Table  by Provincia, estadillo 

T3_allo_p <- left_join(T3_allo_p, T3_allo_sp, by = c("Provincia_3", "Estadillo_3")) # datframe by provincia, estadillo and sp
T3_allo_p <- T3_allo_p %>%
  mutate(AB3_Mgha_perc =AB3_Mgha_sp/AB3_Mgha*100) # calculate specie importance for total biomass

T3_allo_p <- T3_allo_p %>%
  group_by(Provincia_3, Estadillo_3) %>%
  top_n(1,AB3_Mgha_perc) # filter only the most important sp / dataframe by provincia and estadillo 


## Working only to get IFN2 data

T2_allo_sp <- T3_allo %>%
  tidyr::drop_na(AB2_kgT) %>%
  group_by(Provincia_3, Estadillo_3, Sp) %>%
  summarise(AB2_Mgha_sp = sum(AB2_Mgha)) # a #Table  by Provincia, estadillo and sp

## Data transformation (grouping by Provincia, Estadillo, area and Sp)--------------

T2_allo_p <- T3_allo %>% 
  tidyr::drop_na(AB2_kgT) %>%
  group_by(Provincia_3, Estadillo_3, area_2) %>%
  summarise(N_Trees = n(), AB2_Mgha = sum(AB2_Mgha), year2 = mean(year2))  %>%
  mutate(Tree_dens = N_Trees/(0.000314159265*area_2^2)) %>% # Tree per ha
  group_by(Provincia_3, Estadillo_3) %>%
  summarise(Tree_dens2 = sum(Tree_dens),AB2_Mgha = sum(AB2_Mgha), year2 = mean(year2)) #Table  by Provincia, estadillo 

T2_allo_p <- left_join(T2_allo_p, T2_allo_sp, by = c("Provincia_3", "Estadillo_3")) # datframe by provincia, estadillo and sp
T2_allo_p <- T2_allo_p %>%
  mutate(AB2_Mgha_perc =AB2_Mgha_sp/AB2_Mgha*100) # calculate specie importance for total biomass

T2_allo_p <- T2_allo_p %>%
  group_by(Provincia_3, Estadillo_3) %>%
  top_n(1,AB2_Mgha_perc) # filter only the most important sp / dataframe by provincia and estadillo 

T32_allo <- left_join(T3_allo_p, T2_allo_p, by = c("Provincia_3", "Estadillo_3"))
T32_allo_sf <- left_join(T32_allo, DataMp_sf[,c(3,4,16,24)], by = c("Provincia_3" = "Provincia", "Estadillo_3" = "Estadillo")) # merge with DataMp_sf to include coordinates
T32_allo_sf <- st_as_sf(T32_allo_sf)

 T32_allo_sf <- T32_allo_sf %>%
     filter(!Provincia_3 %in% c("Zamora" , "Ourense", "Toledo", "Lugo"))
#We shouldn't need the next two lines in the future, so delete them after try it for a second time
summary(T32_allo_sf)
T32_allo_sf <- cbind(T32_allo_sf, st_coordinates(T32_allo_sf))
write.csv(T32_allo_sf, "products/Andalucia_TotalBiomass.csv")
