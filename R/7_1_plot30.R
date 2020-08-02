
# Pablo Salazar Z
# Universidad de Córdoba
# 11/06/2020
# Using IFN data, functional traits, soil data, and climate data to model biomass production
# Spanish National Forestry inventory

# To run 
# "plot30_p" and "plot30_sp" from "6_1_PCMayores2_RGR.R" is required

# DataMp_sf from 4_1_climate_data.R is required (which comes from 3_0_data_projection.R)


## leaf nutrient data from 2018 ---------
leaf_n <- read_excel("data/new_data/leaf_n.xlsx")
leaf_n$Parcela <- str_sub(leaf_n$Parcela, start = -4)
colnames(leaf_n)[1] <- "Estadillo" # changing names for the merge
colnames(leaf_n)[4:12] <- c("P", "K", "Na", "Mg", "Ca", "Cu", "Zn", "Mn", "Fe")

# selecting the 30 plots from leaf data 2018 in the IFN

l <- leaf_n %>%
  group_by(Provincia, Estadillo) %>%
  summarize_at(vars(P:Fe), mean)

l <- left_join(l,Plot30_sp, by = c("Provincia" = "Provincia_3", "Estadillo" = "Estadillo_3"))
l <- l %>%
  filter(Sp %in% c("Quercus ilex", NA))

## adding spatial coordinates for climate data ------------

l2 <- left_join(l, DataMp_sf[,c(3:4,11,12,14:34)], by = c("Provincia", "Estadillo"))
l2 <- l2 %>%
  dplyr::select(-geometry)

## write.csv(l2,"products/plot30.csv")


## Functional trait data from 2018 --------------
Muestreo_18 <- read_excel("data/new_data/Muestreo_18.xlsx", 
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
  mutate(Dn_m = Per/3.1416*10) %>%  # Now diameter is in mm like in the IFN 
  dplyr::select(-A3, -OrdenIf4, -A4, -Per, -Ht, -LTy, -LMAy, -LDy, -LAy, -PETplot, -AI, -Observaciones) %>%
  filter(Sp_m == "Quercus ilex") %>%
  group_by(Provincia, Estadillo) %>%
  summarise_at(vars(per_ifn:PPplot), mean, na.rm = TRUE)

str(Muestreo_18)

p30 <- left_join(l2, Muestreo_18, by = c("Provincia", "Estadillo"))

## soil nutrients 2018 ---------------


datsoil <- read.table("data/new_data/soil_data.csv", stringsAsFactors = F, sep = ";", header = T, dec = ",")
datsoil <- datsoil[1:58,]
datsoil$Estadillo <- str_sub(datsoil$Parcela, 3,6) # change parcela column
datsoil2<-datsoil %>% 
  group_by(Provincia, Estadillo) %>% 
  summarise_at(vars(pH:X.Arena), mean)

#has one value without data, in special Lugo plot, must look the missing values
p30b<-merge(datsoil2, p30, by = c("Provincia", "Estadillo"), all = TRUE)


## checking basic relationships ------------

p30a <- p30b[,c(3:6, 12, 13, 17:21, 32:37, 41, 47:50, 52, 53, 64:72)]
names(p30a)
colnames(p30a)[4]<-"HR"
colnames(p30a)[6:11]<-c("Mn", "P", "MO", "clay", "silt", "sand")

#write.csv(p30a, "products/p30a.csv")

basic <- cor(p30a, use = "complete.obs")

tiff("products/basic_cor.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
corrplot(basic, method = "ellipse", insig ="blank", type = "upper", order = "FPC",
         diag=FALSE, cl.cex=1, tl.cex=0.7)
dev.off()


## Plot biomass correlations -----------------


a <- ggplot(data = p30a)+
  geom_point(aes(y = AB3_Mgha, x = AB3_kgT))+
  theme_bw() +
  labs(x = "Mean Tree Biomass (kg per Tree)", y = "Plot biomass (Mg ha-1)")

b <- ggplot(data = p30a)+
  geom_point(aes(y = AB3_Mgha, x = Tree_dens))+
  theme_bw() +
  labs(x = "Tree density (tree per ha)", y = "Plot biomass (Mg ha-1)")

d <- ggplot(data = p30a)+
  geom_point(aes(y = AB3_kgT, x = Tree_dens))+
  theme_bw() +
  labs(x = "Tree density (tree per ha)", y = "Tree biomass (kg per tree)")

## RGR correlations --------------------

a <- ggplot(data = p30a)+
  geom_point(aes(y = A_RGR, x = MAT))+
  theme_bw() +
  labs(x = "Mean annual temperature", y = "Relative growth rate")

b <- ggplot(data = p30a)+
  geom_point(aes(y = A_RGR, x = Arid_Pal))+
  theme_bw() +
  labs(x = "Palmer aridity index", y = "Relative growth rate")

c <- ggplot(data = p30a)+
  geom_point(aes(y = A_RGR, x = T_Cold_M))+
  theme_bw() +
  labs(x = "Temperature cold month", y = "Relative growth rate")

d <- ggplot(data = p30a)+
  geom_point(aes(y = A_RGR, x = per_ifn))+
  theme_bw() +
  labs(x = "Tree perimenter in the IFN", y = "Relative growth rate")

## plot productivity correlations --------------

a <- ggplot(data = p30a)+
  geom_point(aes(y = B_BP_Mgha, x = AB3_Mgha))+
  theme_bw() +
  labs(x = "Plot production", y = "Plot productivity")

b <- ggplot(data = p30a)+
  geom_point(aes(y = B_BP_Mgha, x = A_RGR))+
  theme_bw() +
  labs(x = "Plot production", y = "Plot productivity")
