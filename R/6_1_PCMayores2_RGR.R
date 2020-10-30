
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
  
#write.csv(Fechas, "products/fechas.csv")
  
T3T2_ <- left_join(T3T2, year_data[,c(5,8:10)], by = c("Provincia_3", "Estadillo_3"), all.x = TRUE)


# Apparently one tree was labeled with the wrong species, so I fixed here
T3T2_[c(T3T2_$Provincia_3 == "Lugo" & T3T2_$Estadillo_3 == "2894" & T3T2_$OrdenIf3_3 =="007"), "specie"] <- "045"


## Merging T3T2 with allo -------
t3t2_allo <- left_join(T3T2_, allo, by = c("specie" = "COD_SPP_IFN3"), all.x = TRUE)

## calculating Biomass for T3 and T2 ----------

# First step, exclude trees based on "state"

Plot30 <- t3t2_allo %>%
  filter(state %in% c("R","V")) %>% # after this, there is no NA in "specie"
  filter(Sp %in% c("Quercus ilex", NA))

Plot30_p <- Plot30 %>%
  group_by(Provincia_3, Estadillo_3, area_2) %>%
  mutate(Tree_dens2 = n()/(0.000314159265*area_2^2)) %>% # Tree per ha
  group_by(Provincia_3, Estadillo_3, area_3) %>%
  mutate(Tree_dens3 = n()/(0.000314159265*area_3^2)) %>% # Tree per ha
  group_by(Provincia_3, Estadillo_3) %>%
  summarise(Tree_dens2 = sum(Tree_dens2, na.rm = TRUE), Tree_dens3 = sum(Tree_dens3, na.rm = TRUE)) # at plot level


## Functional trait data from 2018 --------------
Muestreo_18 <- read_excel("data/new_data/Muestreo_18_e.xlsx", 
                          sheet = "Arbolado", col_types = c("text", 
                                                            "numeric", "numeric", "text", "numeric", 
                                                            "text", "numeric", "text", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "text", "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "text"))

Muestreo_18$Provincia <- stringi::stri_trans_general(Muestreo_18$Provincia, "Latin-ASCII") # removing accents for the merge
Muestreo_18$Parcela <- str_sub(Muestreo_18$Parcela, start = -4)
colnames(Muestreo_18)[2:22] <- c("Estadillo", "nArbol_3", "OrdenIf3", "A3", "OrdenIf4", "A4",
                                 "Sp_m", "per_ifn", "Ht_ifn","Per", "Ht", "def", "LTy", "LMAy",
                                 "LDy", "LAy", "LTo", "LMAo", "LDo", "LAo", "WDMCo")
Muestreo_18$def <- as.numeric(Muestreo_18$def)
Muestreo_18 <- Muestreo_18 %>%
  mutate(Dn_m = Per/3.1416*10, Dn_ifn = per_ifn/3.1416*10) %>%  # Now diameter is in mm like in the IFN 
  dplyr::select(-nArbol_3,-A3, -OrdenIf4, -A4, -LTy, -LMAy, -LDy, -LAy, -PETplot, -Observaciones) %>%
  filter(Sp_m == "Quercus ilex") %>%
  mutate(OrdenIf3 = str_pad(OrdenIf3, 3, pad = "0")) %>%
  dplyr::select(-Sp_m, -per_ifn, -Ht_ifn, -Dn_ifn)


## merging T3_A and functional traits ------ 

T3_A2 <- right_join(T3_A[,c(-3:-5,-8, -10:-12, -14:-20, -23)], Muestreo_18, by = c("Provincia_3" = "Provincia", "Estadillo_3" = "Estadillo", "OrdenIf3_3" = "OrdenIf3"), all.y = TRUE)
T3_A2 <- T3_A2[-c(str_which(T3_A2$OrdenIf3_3, "[:alpha:]")),] # Deleting cases with new, miss, or dead trees (text data in OrdenIf3_3)

T3_A3 <- left_join(T3_A2, year_data[,c(4,5,8,9)], by = c("Provincia_3", "Estadillo_3"))
T3_A3 <- T3_A3 %>%
  mutate(year_m = 2018)


# Carefull with this part, we are mannually changing Lugo with data from the IFN4
T3_A3[187:192, "Distancia_3"] <- c(6.8, 9.2, 4.9, 9.8,5.6, 4.6) # Distancia_3
T3_A3[187:192, "Dn_3"] <- c(41/3.1416*10, 41/3.1416*10, 44/3.1416*10, 47/3.1416*10, 35/3.1416*10, 25/3.1416*10) # Dn_3
T3_A3[187:192, "Ht_3"] <- c(6.6, 6, 5.8, 5.6,5.1, 4.8) # Ht_3
T3_A3[187:192, "area_3"] <- c(10, 10, 5, 10, 10, 5) # area_3
T3_A3[187:192, "year3"] <- 2009 # year3

# Calculating Plot biomass, RGR, and productivity at tree level

#areal biomass
T3_A3 <- T3_A3 %>%
  drop_na(Dn_3, Dn_m) %>%
  mutate(AB3_kgT_18 = 0.10190040*(Dn_3/10)^2.477450, # Biomass in kg per tree
         ABM_kgT_18 = 0.10190040*(Dn_m/10)^2.477450)

## calculating Biomass (Mg) per Ha for T3 and Manolo sampling ------------------
T3_A3 <- T3_A3 %>%
  mutate(AB3_Mgha_18 = AB3_kgT_18/(0.000314159265*area_3^2)/1000, # Biomass T3 in Mg per ha
         ABM_Mgha_18 = ABM_kgT_18/(0.000314159265*area_3^2)/1000) # Biomass T2
         
# There are several ways to to calculate the previous variables. Intuitively, it make sense to calculcate the areal biomass from the IFN3 using "area_3" because it woukd provide an accurate 
# representation of the biomass per area in that moment. However, in our case is more important to make an accurate comparisson with the IFN2. Therefore, we need to use "area_2" in all equation.
# At the end, we are not really calculating the biomass per area in the IFN3. We are calculation how much biomass there is in the trees from the IFN2 that are still alive in the IFN3. 

## calculation RGR between T3 and T2 -----------------
T3_A3 <- T3_A3 %>%
   mutate(A_RGR_18 = (log(ABM_kgT_18) - log(AB3_kgT_18))/((year_m - year3)*1000), # RGR
         B_BP_Mgha_18 = (ABM_Mgha_18 - AB3_Mgha_18)/(year_m - year3)) # Biomass production
 
# Calculating plot data from 2018 sampling

Plot30_18 <- T3_A %>%
  filter(!is.na(OrdenIf3_3), !OrdenIf3_3 %in% c("000", "888", "777", "999")) %>%
  group_by(Provincia_3, Estadillo_3, area_3) %>%
  summarise(N_Trees = n()) %>%
  mutate(Tree_dens = N_Trees/(0.000314159265*area_3^2)) %>% # Tree per ha
  group_by(Provincia_3, Estadillo_3) %>%
  summarise(N_Trees = sum(N_Trees), Tree_dens = sum(Tree_dens)) # Tree density at plot level (all species)

Plot30_m <- T3_A3 %>%
  group_by(Provincia_3, Estadillo_3) %>%
  summarise(AB3_kgT_18 = mean(AB3_kgT_18), ABM_kgT = mean(ABM_kgT_18), AB3_Mgha_18 = sum(AB3_Mgha_18), 
            ABM_Mgha_18 = sum(ABM_Mgha_18), A_RGR_18 = mean(A_RGR_18),
            B_BP_Mgha_18 = sum(B_BP_Mgha_18), 
            Ht_3 = mean(Ht_3), def = mean(def), LTo = mean(LTo, na.rm = TRUE), LMAo = mean(LMAo, na.rm = TRUE), LDo = mean(LDo, na.rm = TRUE), 
            LAo = mean(LAo, na.rm = TRUE), WDMCo = mean(WDMCo, na.rm = TRUE), PPplot = mean(PPplot)) %>% #Table with N trees by Provincia, estadillo
  mutate(S_RGR_18 = log(ABM_Mgha_18)/log(AB3_Mgha_18)/yeardif) # another RGR for plot level Mg per ha per year? 


Plot30_m <- left_join(Plot30_m, Plot30_18, by = c("Provincia_3", "Estadillo_3"))
