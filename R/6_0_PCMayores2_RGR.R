
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
# cleaning data from TreesI3 before join with TreesI2--------------

# Filtering new plots, wrong subclase, and non-recorded trees from IFN2
T3_A <- TreesI3 %>%
  filter(Cla == "A" & Subclase %in% c("3C", "1") & !OrdenIf2 %in% c("999", "000","888")) %>%
  mutate(code = str_c(Provincia, Estadillo, OrdenIf2)) # create a single label for each tree

sum(duplicated(T3_A$code)) # we notice 3 replicated trees yet
T3_A[duplicated(T3_A$code),] # apparently they are recorded twice in Cordoba for some reason

T3_A <- T3_A  %>% #Filtering the 3 replicated trees and the original because we can´t tell which is which
  filter(!code %in% c("Cordoba1266002", "Cordoba1264001", "Cordoba1167009"))

colnames(T3_A) <- paste(colnames(T3_A),"3", sep = "_")

# Creating a common variable between T3_A and T2_A
T2_A <- TreesI2 %>%
  mutate(code = str_c(Provincia, Estadillo, NumOrden)) %>%
  mutate(Especie = str_pad(Especie, 3, pad = "0"))
colnames(T2_A) <- paste(colnames(T2_A), "2", sep = "_")
names(T2_A)[15] <- "code_3"

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

## Merging T3T2 with allo -------
t3t2_allo <- merge(T3T2, allo, by.x = "specie", by.y = "COD_SPP_IFN3", all.x = TRUE)

## calculating Biomass for T3 and T2 ----------
#areal biomass
t3t2_allo$Biomass_3 <- t3t2_allo$CFA*(t3t2_allo$Dn_3/10)^t3t2_allo$b # Biomass in kg per tree
t3t2_allo$Biomass_2 <- t3t2_allo$CFA*(t3t2_allo$Dn_2/10)^t3t2_allo$b

#Root biomass
t3t2_allo$RBiomass_3 <- t3t2_allo$CFAr*(t3t2_allo$Dn_3/10)^t3t2_allo$br # Biomass in kg per tree
t3t2_allo$RBiomass_2 <- t3t2_allo$CFAr*(t3t2_allo$Dn_2/10)^t3t2_allo$br

## calculating Biomass per Ha for T2 ------------------
