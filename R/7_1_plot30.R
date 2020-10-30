
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

# loading raw data to merge the C and N analysis
leaf_n_e <- read_excel("data/new_data/leaf_n.xlsx", 
                       sheet = "Datos Brutos Digestiones")

colnames(leaf_n_e)[6] <- "tubo"
cyn <- read_excel("data/new_data/CandN_leaf.xlsx", 
                  skip = 7)
tubo <- str_extract_all(cyn$Type, "[:digit:]")
tubo <- lapply(tubo, function(x) paste(x, collapse = ""))
tubo <- t(as.data.frame(tubo))
cyn <- cbind(cyn, tubo)
cyn$tubo <- as.numeric(cyn$tubo)

leaf_n_e <- full_join(leaf_n_e, cyn, by = "tubo")
leaf_n <- left_join(leaf_n, leaf_n_e[,c(7,38,39)], by = "ID")

# renaming and reordering
leaf_n <- leaf_n[,c(1:3,15,14,4:13)]
colnames(leaf_n)[4:5] <- c("C", "N")

leaf <- leaf_n %>%
    group_by(Provincia, Estadillo) %>%
  summarise_at(vars(C:Fe), funs(qwraps2::mean_sd(., na_rm = TRUE)))

write.csv(leaf, "products/leaf.csv")

temp <- leaf_n %>%
  ungroup() %>% # If a tibble have been grouped before, it will remember forever, so is better to ungroup if you want to make new groups
  select(C:Fe)%>%
  pivot_longer(
    cols = (C:Fe),
    names_to = "variable",
    values_to = "value")  %>% # here we are melting the data for the summarise_all function
  group_by(variable) %>%
  summarise_all(list(~mean(., na.rm = TRUE), 
                     ~sd(.,na.rm = TRUE),
                     ~min(.,na.rm = TRUE),
                     ~max(.,na.rm = TRUE),
                     ~median(.,na.rm = TRUE))) %>%
  mutate(cv = sd/mean*100)

write.csv(temp, "products/summaryleaf.csv")
# selecting the 30 plots from leaf data 2018 in the IFN

l <- leaf_n 

# Leaf PCA

log.ir <- log(l[, 5:14]) ## Log Transformation of the variables included
ir.pca <- prcomp(na.omit(log.ir),
                 center = TRUE,
                 scale = TRUE)

summary(ir.pca)
print(ir.pca)
ir.zona <- l[,2]
ir.zona <- as.factor(ir.zona$Provincia) 
  
test2 <- get_pca_ind(ir.pca) ## Individual contribution to the PCA

Indi<-cbind(l$Provincia, l$Estadillo, l$nArbol_3, test2$coord[,1:2])
Indi<- as.data.frame(Indi)
colnames(Indi) <- c("Provincia", "Estadillo", "nArbol_3", "L1", "L2")
Indi$nArbol_3 <- as.numeric(Indi$nArbol_3)

# Ploting the PCA

g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
               groups = ir.zona, ellipse = TRUE, 
              circle = FALSE, repel =TRUE, varname.size = 6)
g<- g + labs(x=expression(PCA[1]~~group("(",32.7~"%",")")),
             y= expression(PCA[2]~~group("(",20.1~"%",")"))) +
  scale_color_discrete(name = "")
             
g<-g+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  ## delete background
           panel.background = element_blank(), axis.line = element_line(colour = "black"),
           axis.text=element_text(size=16),
           legend.text=element_text(size=20),
           legend.direction = 'horizontal',
           legend.position="top",
           axis.title.x=element_text(size=16),
           axis.title.y=element_text(size=16))
 
tiff("products/Appendix2.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
par(mar=c(4.1,4.4,0.3,0.3))
g
dev.off()

jpeg("Figuras/Appendix2.jpg", width = 500, height = 350)
g
dev.off()

# plotting the main variables in each axes 

test<-get_pca_var(ir.pca) ## variable contributioin to the PCA 
colnames(test$cos2)[1] <- "PCA 1 \n (32.7 %)"
colnames(test$cos2)[2] <- "PCA 2 \n (20.1 %)"
colnames(test$cos2)[3] <- "PCA 3 \n (13.2 %)"

test_cos<-as.matrix(test$cos2, ncol=10)
test_cos<-test_cos[,(1:3)]


pdf("products/leafPCA_2.pdf", width=6, height=4.5)
par(oma=c(4.1,4.4,0.3,0.3))
corrplot(test_cos, is.corr=FALSE, win.asp =0.4, cl.cex = 1, cl.align.text = "l")
dev.off()

jpeg("Figuras/leafPCA_2.jpg", width = 900, height = 1050, res = 150)
par(oma=c(0.3, 0.3,0.3,1))
corrplot(test_cos, is.corr=FALSE, win.asp =0.4, cl.cex = 1, cl.align.text = "l")
dev.off()

## Functional trait data from 2018 --------------

Muestreo_18a <- read_excel("data/new_data/Muestreo_18.xlsx", 
                           sheet = "Matorral y arbolado")
names(Muestreo_18a)

colnames(Muestreo_18a)[24:25] <- c("SD", "WDMCo")
Muestreo_18a <- Muestreo_18a %>% drop_na(WDMCo) # this variable has no replicates, so we can use it to merge SD with Muestreo_18

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
  mutate(Dn_m = Per/3.1416*10) %>%  # Now diameter is in mm like in the IFN 
  dplyr::select(-A3, -OrdenIf4, -A4, -Per, -Ht, -LTy, -LMAy, -LDy, -LAy, -PETplot, -Observaciones, -PPplot, -AI) %>%
  filter(Sp_m == "Quercus ilex") %>%
  mutate(OrdenIf3 = str_pad(OrdenIf3, 3, pad = "0")) %>%
  dplyr::select(-Sp_m, -per_ifn, -Ht_ifn) %>%
  mutate(OrdenIf3 = as.factor(OrdenIf3))


Muestreo_18 <- left_join(Muestreo_18, Muestreo_18a[,c(24:25)], by = "WDMCo")

# Merging Functional traits and leaf nutrients 


l2 <- left_join(Indi, Muestreo_18, by = c("Provincia", "Estadillo", "nArbol_3"))

# before merging with Plot30_p (for tree density)

Plot30_f <- Plot30 %>%
  select(-nArbol_3, -specie, -state)

l3 <- left_join(l2,Plot30_f, by = c("Provincia" = "Provincia_3", "Estadillo" = "Estadillo_3", "OrdenIf3" = "OrdenIf3_3"))
l3 <- left_join(l3,Plot30_p, by = c("Provincia" = "Provincia_3", "Estadillo" = "Estadillo_3"))


# Carefull with this part, we are mannually changing Lugo with data from the IFN4

l3[c(l3$Provincia == "Lugo" & l3$Estadillo == "2894" & l3$nArbol_3 =="3"), "Dn_3"] <- 35/3.1416*10 # Dn_3
l3[c(l3$Provincia == "Lugo" & l3$Estadillo == "2850" & l3$OrdenIf3 =="000"), "Dn_3"] <-c(41/3.1416*10, 41/3.1416*10, 44/3.1416*10, 47/3.1416*10)

l3[c(l3$Provincia == "Lugo" & l3$Estadillo == "2894" & l3$nArbol_3 =="3"), "area_3"] <- 5 # area_3
l3[c(l3$Provincia == "Lugo" & l3$Estadillo == "2850" & l3$OrdenIf3 =="000"), "area_3"] <-10

l3[c(l3$Provincia == "Lugo" & l3$Estadillo == "2894" & l3$nArbol_3 =="3"), "year3"] <- 2009 # area_3
l3[c(l3$Provincia == "Lugo" & l3$Estadillo == "2850" & l3$OrdenIf3 =="000"), "year3"] <-2009

l3 <- l3 %>%
  fill(CFA, b, CFAr, br)
         
#areal biomass
l3 <- l3 %>%
  mutate(AB3_kgT = CFA*(Dn_3/10)^b, # Biomass in kg per tree
         AB2_kgT = CFA*(Dn_2/10)^b,
         AB_18_kgT = 0.10190040*(Dn_m/10)^2.477450)

#Root biomass
l3 <- l3 %>%
  mutate(RB3_kgT = CFAr*(Dn_3/10)^br, # Biomass in kg per tree
         RB2_kgT = CFAr*(Dn_2/10)^br,
         RB_18_kgT = 0.5450453*(Dn_m/10)^1.7893)

## calculating Biomass (Mg) per Ha for T3 and T2 ------------------
l3 <- l3 %>%
  mutate(AB3_Mgha = AB3_kgT/(0.000314159265*area_2^2)/1000, # Biomass T3 in Mg per ha
         MB3_Mgha = AB3_kgT/(0.000314159265*area_3^2)/1000, # Biomass T3 in Mg per ha
         AB_18_Mgha = AB_18_kgT/(0.000314159265*area_3^2)/1000, # Biomass Muestreo in Mg per ha
         AB2_Mgha = AB2_kgT/(0.000314159265*area_2^2)/1000, # Biomass T2
         RB3_Mgha = RB3_kgT/(0.000314159265*area_2^2)/1000, # Biomass R3
         RB2_Mgha = RB2_kgT/(0.000314159265*area_2^2)/1000) # Biomass R2

# There are several ways to to calculate the previous variables. Intuitively, it make sense to calculcate the areal biomass from the IFN3 using "area_3" because it woukd provide an accurate 
# representation of the biomass per area in that moment. However, in our case is more important to make an accurate comparisson with the IFN2. Therefore, we need to use "area_2" in all equation.
# At the end, we are not really calculating the biomass per area in the IFN3. We are calculation how much biomass there is in the trees from the IFN2 that are still alive in the IFN3. 

## calculation RGR between T3 and T2 -----------------
l3 <- l3 %>%
  mutate(A_RGR = (log(AB3_kgT) - log(AB2_kgT))*1000/(difyear), # RGR
         M_RGR = (log(AB_18_kgT) - log(AB3_kgT))*1000/(2018-year3), # RGR
         R_RGR = (log(RB3_kgT) - log(RB2_kgT))*1000/(difyear), # in g kg-1 year-1
         M_BP_Mgha = (AB_18_Mgha - MB3_Mgha)/(2018-year3),
         B_BP_Mgha = (AB3_Mgha - AB2_Mgha)/difyear, # Biomass production
         R_BP_MgHa = (RB3_Mgha - RB2_Mgha)/difyear)# Root Biomass 

#filter(B_RGR > 0 & B_RGR < 100) # a filter to avoid negative growth and too high values       
# This is going to be need it according to the variable you are measuring.


l3 <- l3 %>% ## Filtering unnecesarry data at the moment 
  dplyr::select(-Cla_3:-Compara_3, -code_3:-ParamEsp_2, -Correspondencia:-CFAr, # at tree level
  -RB3_kgT: -RB_18_kgT, -RB3_Mgha, -RB2_Mgha, -R_RGR, -R_BP_MgHa)

## adding spatial coordinates for climate data ------------

# st_drop_geometry(DataMp_sf)
l4 <- left_join(l3, DataMp_sf[,c(3:4,11,12,14:34)], by = c("Provincia", "Estadillo"))
l4 <- st_as_sf(l4)

plot_region <- st_bbox(c(xmin = 2, xmax = -10,
                      ymin = 35.5, ymax = 44),
                    crs = st_crs(l4)) %>% 
  st_as_sfc()

Plot_map <- tm_shape(Portugal, bbox = plot_region) + tm_fill() + tm_borders() + 
  tm_shape(France, bbox = plot_region) + tm_fill() + tm_borders() + 
  tm_shape(Spain, bbox = plot_region) + tm_fill() + tm_borders() +
  tm_shape(l3) + tm_dots(col = "Arid_Dm", size = 1, n = 8, title = "Wetness") + 
  tm_layout(title = "", legend.position = c("right", "bottom"), 
            legend.bg.color = "white", legend.text.size = 1, legend.title.size = 2)
#tmap_save(Plot_map, "products/Fig2.jpg")


# st_drop_geometry(l3)
# l3 <- l3 %>%
#   dplyr::select(-geometry)

## write.csv(l2,"products/plot30.csv")


## soil nutrients 2018 ---------------

datsoil <- read.table("data/new_data/soil_data2.csv", stringsAsFactors = F, sep = ";", header = T, dec = ",")
datsoil <- datsoil[1:58,]
datsoil$Estadillo <- str_sub(datsoil$Parcela, 3,6) # change parcela column
datsoil2 <-datsoil %>% 
  select(-c(1,6,8:10,12,14,16,19,26))
names(datsoil2)
colnames(datsoil2)[4:22]<-c("CE", "FHR", "Ca", "Mg", "K", "Na", "RAS", "CIC", "Mn", "Zn", "Cu", "Fe", "P", "MO", "Arcilla",
                            "Limo", "Arena", "Ni", "C")

soil <- datsoil2 %>%
  select(-c(4,5,10,11,17,19,20)) %>%
  group_by(Provincia, Estadillo) %>%
  summarise_at(vars(pH:C),list(~qwraps2::mean_sd(., na_rm = TRUE)))

write.csv(soil, "products/soil.csv")

temp <- datsoil2 %>%
  ungroup() %>% # If a tibble have been grouped before, it will remember forever, so is better to ungroup if you want to make new groups
  select(-c(1:5,10,11,17,19,20))%>%
  pivot_longer(
    cols = (Ca:C),
    names_to = "variable",
    values_to = "value")  %>% # here we are melting the data for the summarise_all function
  group_by(variable) %>%
  summarise_all(list(~mean(., na.rm = TRUE), 
                     ~sd(.,na.rm = TRUE),
                     ~min(.,na.rm = TRUE),
                     ~max(.,na.rm = TRUE),
                     ~median(.,na.rm = TRUE))) %>%
  mutate(cv = sd/mean*100)

write.csv(temp, "products/summarysoil.csv")


datsoil2<- datsoil2 %>%
  group_by(Provincia, Estadillo) %>% 
  summarise_at(vars(pH:C), mean)

# plot(datsoil$Fe.Final..mg.kg., datsoil$P.corregido.fH)
# plot(datsoil$X.Arcilla, datsoil$P.corregido.fH)

# soil PCA

log.ir <- log(datsoil2[, c(6:9,12:16,21,22)]) ## Log Transformation of the variables included
ir.pca <- prcomp(na.omit(log.ir),
                 center = TRUE,
                 scale = TRUE)

summary(ir.pca)
print(ir.pca)
ir.zona <- datsoil2[,1]
ir.zona <- as.factor(ir.zona$Provincia)

ir.pca$x[,1] <- -ir.pca$x[,1] ## To flip the PC1
ir.pca$rotation[,1] <- -ir.pca$rotation[,1] ## Also hae to flip the eigenvectors
ir.pca$x[,2] <- -ir.pca$x[,2] ## To flip the PC2
ir.pca$rotation[,2] <- -ir.pca$rotation[,2] ## Also hae to flip the eigenvectors

test2 <- get_pca_ind(ir.pca) ## Individual contribution to the PCA

Indi2<-cbind(datsoil2$Provincia, datsoil2$Estadillo, test2$coord[,1:2], datsoil2[,c(3:5,10,11,17:20)])
colnames(Indi2)[1:4] <- c("Provincia", "Estadillo", "S1", "S2")
Indi2<-as.data.frame(Indi2)

# Ploting the PCA

g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.zona, ellipse = TRUE, 
              circle = FALSE, repel =TRUE, varname.size = 6)
g<- g + labs(x=expression(PCA[1]~~group("(",40.9~"%",")")),
             y= expression(PCA[2]~~group("(",20.7~"%",")"))) +
  scale_color_discrete(name = "")

g<-g+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  ## delete background
           panel.background = element_blank(), axis.line = element_line(colour = "black"),
           axis.text=element_text(size=16),
           legend.text=element_text(size=20),
           legend.direction = 'horizontal',
           legend.position="top",
           axis.title.x=element_text(size=16),
           axis.title.y=element_text(size=16))

 tiff("products/Appendix3.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
 par(mar=c(4.1,4.4,0.3,0.3))
 g
 dev.off()

 jpeg("Figuras/Appendix3.jpg", width = 500, height = 350)
 g
 dev.off()


# plotting the main variables in each axes 

test<-get_pca_var(ir.pca) ## variable contributioin to the PCA 
colnames(test$cos2)[1] <- "PCA 1 \n (40.9 %)"
colnames(test$cos2)[2] <- "PCA 2 \n (20.7 %)"
colnames(test$cos2)[3] <- "PCA 3 \n (11.4 %)"

test_cos<-as.matrix(test$cos2, ncol=10)
test_cos<-test_cos[,(1:3)]


pdf("products/SOilPCA_2.pdf", width=6, height=4.5)
par(oma=c(4.1,4.4,0.3,0.3))
corrplot(test_cos, is.corr=FALSE, win.asp =0.4, cl.cex = 1, cl.align.text = "l")
dev.off()

jpeg("Figuras/soilPCA_2.jpg", width = 900, height = 1050, res = 150)
par(oma=c(0.3, 0.3,0.3,1))
corrplot(test_cos, is.corr=FALSE, win.asp =0.4, cl.cex = 1, cl.align.text = "l")
dev.off()

## merging soil and leaf for exploratory purposes -----------

temp<-merge(leaf_n, datsoil2, by = c("Provincia", "Estadillo"), all = TRUE)
names(temp)
glm1 <- lme(log(P.x) ~ Arcilla,  random = ~1|Provincia/Estadillo, na.action = na.omit, data = temp)
glm1 <- lme(log(P.y) ~ Arcilla,  random = ~1|Provincia/Estadillo, na.action = na.omit, data = temp)
glm1 <- glm(log(P.y) ~ Arcilla,  na.action = na.omit, data = temp)

summary(glm1)
plot(glm1)
## Merging soil, leaf and IFN data ----------------
#has one value without data, in special Lugo plot, must look the missing values
p30b<-merge(Indi2, l4, by = c("Provincia", "Estadillo"), all = TRUE)

## checking basic relationships ------------
names(p30b)
p30a <- p30b[,-c(5:10, 12:14, 17, 18, 24,26:32, 46:49, 53:68)]
#p30a <- data.frame(lapply(p30a, function(x) as.numeric(as.character(x))))

names(p30a)
p30a <-p30a %>%
  group_by(Provincia, Estadillo) %>%
  mutate(AB3_Mgha = sum(AB3_Mgha, na.rm = TRUE),
         MB3_Mgha = sum(MB3_Mgha, na.rm = TRUE),
         AB_18_Mgha = sum(AB_18_Mgha, na.rm = TRUE),
         AB2_Mgha = sum(AB2_Mgha, na.rm = TRUE),
         M_BP_Mgha = sum(M_BP_Mgha, na.rm = TRUE),
         B_BP_Mgha = sum(B_BP_Mgha, na.rm = TRUE))
#write.csv(p30a, "products/p30a.csv")

# This is exploratory analysis, we need to deal with negative values first

basic <- rcorr(as.matrix(p30a[,c(3:29)]), type = "pearson")
p <- p.adjust(basic$P, "bonferroni")

 corrplot(basic$r, method = "ellipse", insig ="blank", type = "upper", # order = "FPC",
          diag=FALSE, cl.cex=1, tl.cex=0.7, p.mat = basic$P)
