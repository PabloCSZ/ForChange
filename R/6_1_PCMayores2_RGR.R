
# Pablo Salazar Z
# Universidad de Córdoba
# 14/04/2020
# Proccessing TreesI3 (PCMayores) and TreesI2 (PCMayores2) to get a plant production rate
# Spanish National Forestry inventory
# Andalucia subset

# To run 
# TreesI3 from "2_1_data_loading.R" is required
# TreesI2 from "2_1_data_loading.R" is required
# allo from "5_0_PCMayores.R" is required
# DataMp_sf from 4_1_climate_data.R is required (which comes from 3_0_data_projection.R)

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
# T2_A$code_3 has no duplicates, therefore it is possible to left_join T2 and T3 with warnings. T2 will duplicate itself a few times in T3 but that is not an issue

# Merging T3 and T2 ----------------------
T3T2 <- left_join(T3_A, T2_A, by = "code_3", all.x = TRUE)

# Looping a new variable specie with Especie_3 NA replaced by Especie_2 data  

T3T2$specie<-T3T2$Especie_3
for(i in 1:nrow(T3T2)){
  if(is.na(T3T2$specie[i])){
    T3T2$specie[i] <- T3T2$Especie_2[i]
  } else
    T3T2$specie[i] <- T3T2$Especie_3[i]
} ## Now there is only 26 NA in specie instead of thousands

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
        }else
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

year_data <- read_csv("data/dove/year_data2.csv")
year_data$Provincia3 <- as.factor(year_data$Provincia3)
levels(year_data$Provincia3) <- c("Almeria", "Cadiz", "Cordoba",  "Granada",
                                  "Huelva", "Jaen", "Lugo", "Malaga", "Ourense", "Sevilla", "Toledo", "Zamora")
year_data <- year_data %>%
  mutate(Provincia_3 = Provincia3) %>%
  mutate(Estadillo_3 = str_pad(Estadillo3, 4, pad = "0")) %>%
  mutate(difyear = year32)

Fechas <- year_data %>%
  group_by(Provincia_3, year2) %>%
  summarise(n())

Fechas <- year_data %>%
  group_by(Provincia_3) %>%
  summarise(year2 = mean(year2), year3 = mean(year3))%>%
  filter(!Provincia_3 %in% c("Zamora" , "Ourense", "Toledo", "Lugo"))
  
write.csv(Fechas, "products/fechas.csv")
  
T3T2 <- merge(T3T2, year_data[,c(8:10)], by = c("Provincia_3", "Estadillo_3"), all.x = TRUE)

## Merging T3T2 with allo -------
t3t2_allo <- left_join(T3T2, allo, by = c("specie" = "COD_SPP_IFN3"), all.x = TRUE)

## calculating Biomass for T3 and T2 ----------

# First step, exclude trees based on "state"

t3t2_allo <- t3t2_allo %>%
  filter(state == "V") # after this, there is no NA in "specie"
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
  mutate(A_RGR = (log(AB3_kgT)/log(AB2_kgT))/(difyear*1000), # RGR
         R_RGR = (log(RB3_kgT)/log(RB2_kgT))/(difyear*1000), # in Mg per year
         B_BP_Mgha = (AB3_Mgha - AB2_Mgha)/difyear, # Biomass production
         R_BP_MgHa = (RB3_Mgha - RB2_Mgha)/difyear)# Root Biomass 


#filter(B_RGR > 0 & B_RGR < 100) # a filter to avoid negative growth and too high values       
# This is going to be need it according to the variable you are measuring.

#write.csv(t3t2_allo, "products/plots30.csv")

## Summary tables before any future addition to the field data ----------

Plot30 <- t3t2_allo %>% ## Filtering unnecesarry data at the moment 
  dplyr::select(-Cla_3:-Compara_3, -Provincia_2:-ParamEsp_2, -Correspondencia:-CFAr,
                -RB3_kgT, -RB2_kgT, -RB3_Mgha, -RB2_Mgha, -R_RGR, -R_BP_MgHa)
  

Plot30_p <- Plot30 %>%
  drop_na(AB3_kgT) %>%
  group_by(Provincia_3, Estadillo_3, area_2) %>%
  summarise(N_Trees = n()) %>%
  mutate(Tree_dens = N_Trees/(0.000314159265*area_2^2)) %>% # Tree per ha
  group_by(Provincia_3, Estadillo_3) %>%
  summarise(N_Trees = sum(N_Trees), Tree_dens = sum(Tree_dens))

Plot30_sp <- Plot30 %>%
  tidyr::drop_na(AB3_kgT) %>%
  group_by(Provincia_3, Estadillo_3,Sp) %>%
  summarise(AB3_kgT = sum(AB3_kgT), AB2kgT = sum(AB2_kgT), AB3_Mgha = sum(AB3_Mgha), AB2_Mgha = sum(AB2_Mgha), A_RGR = mean(A_RGR),
            B_BP_Mgha = sum(B_BP_Mgha), difyear = mean(difyear)) %>% #Table with N trees by Provincia, estadillo and Sp 
  mutate(S_RGR = log(AB3_Mgha)/log(AB2_Mgha)/difyear) # another RGR for plot level Mg per ha per year? 
  
Plot30_sp <- left_join(Plot30_sp, Plot30_p, by = c("Provincia_3", "Estadillo_3"))

## merging T3_A and functional traits ------ # This part is not ready yet

T3_A2 <- leaft_join(T3_A, Muestreo_18, by = c("Provincia_3" = "Provincia", "Estadillo_3" = "Estadillo", "OrdenIf3_3" = "OrdenIf3"))
leaf_n <- leaf_n %>% 
  filter(Sp_m == "Quercus ilex") %>%
  filter(!OrdenIf3 == "Nuevo", !OrdenIf3 == "Muerto")


summary(as.factor(leaf_n$OrdenIf3))
summary(as.factor(T3_A$nArbol_3))

TreesI3 %>%
  filter(Provincia == "Lugo", Estadillo == "2850")

T3_A %>%
  filter(Provincia_3 == "Lugo", Estadillo_3 == "2894")
Muestreo_18 %>%
  filter(Provincia == "Toledo", Parcela == "1383")


## merging with IFN3-------------

t3t2_test <- right_join(t3t2_allo, leaf_n, by = c("Provincia", "Estadillo", "nArbol_3"))

#After checking T3_A, I noticed leaf_n was made using trees not included in T2_A (OrdenIF2 == 000).
# This is an example
T2_A %>%
  filter(Provincia_2 == "Toledo", Estadillo_2 == "1383")
#Which means we can not calculate RGR or Biomass productivity

## Filtering NA from the leaf data -------
t3t2_test <- t3t2_test %>%
  filter(is.na(OrdenIf3_3))


# print results



