# clear R's brain
rm(list=ls())
library(tidyverse)

#######################################################################
####### CALCULO DE VARIABLES ENTRE PIES MAYORES DE INF2 E IFN3 ########
#######################################################################

####Read data####
tree2 <- read_csv(file = "data/dove/tree2_clean.csv", col_names = TRUE) # tree data ifn2
tree3 <- read_csv(file = "data/dove/tree3_clean.csv", col_names = TRUE) # tree data ifn3
species <- read_csv("data/dove/species.csv", col_names = TRUE)
allo <- read.table(file="data/dove/Allometric.txt", header = TRUE, sep = "\t", 
                   colClasses=c("factor","factor","factor","factor","numeric","numeric",
                                "numeric","numeric","numeric","numeric","numeric","numeric",
                                "numeric","numeric","numeric","numeric","numeric","numeric",
                                "numeric","numeric","numeric"))


## filter Andalucia only

summary(tree3$Provincia3)
Atree3 <- tree3 %>%
  filter(Provincia3 %in% c("4", "11", "14", "18", "21", "23", "29", "41"))

####1. Selección parcelas permanentes: clases A1 y A3C####
Atree3 <- Atree3[Atree3$Cla3 == 'A' & Atree3$Subclase3 %in% c('1',"3C"),]

# This is where I want to chck for the OrdenIf filter

A1_IFN2 <- which(tree2$Plotcode2 %in% unique(tree3$Plotcode3)) ## not sure if this have to be done with Plotcode instead of ID_Pma2
tree2 <- tree2[A1_IFN2,]

##2. Unión por el identificador común: ID_Pma2 ####
tree23 <- merge(tree3, tree2, by = "ID_Pma2", all.x = TRUE) # This merge is not clean yet
tree23s <- tree23
glimpse(tree23s)

##3. Campos comparables y con más datos del IFN2 al IFN3####

#3.1. Especie: vivo if3 y muerto o desaparecido if2
#PRB: Yo no se por qué se quita la 999, debe ser un error de ese codigo yo tenia lo siguiente
tree23s$sppcompa<-tree23s$Especie3
tree23s$sppcompa[is.na(tree23s$sppcompa)] <- tree23s$Especie2[is.na(tree23s$sppcompa)] # Copy the species from IFN2 when IFN3 is NA
tree23s$sppcompa[tree23s$Especie3 == "999" & !is.na(tree23s$Especie3)] <- tree23s$Especie2[tree23s$Especie3 == "999" & !is.na(tree23s$Especie3)] #Copy the species from IFN2 when IFN3 is 999 and is not NA

#Observaciones especies VCA y PRB:
#revisar PRB: A lo mejor estaría bien meter un campo de errores de id de especie
kk <- tree23s[is.na(tree23s$sppcompa),] #Quedan 169 arboles sin especie que no tienen datos asociados
# but here it says 42...
summary(kk)
tree23a <- tree23s %>% 
  select(Especie2, Especie3, sppcompa)
tree23a$EspDist <- apply(tree23a[,c("Especie3","Especie2")], 1, 
                         function (x) {x[1]!= x[2]}) #Que especies cambian de codigo?
tree23a$comp <- paste0(tree23a$Especie3,tree23a$Especie2)
kk <- tree23a[!is.na(tree23a$Especie3) & tree23a$Especie3 !="999" &
                !is.na(tree23a$EspDist) & tree23a$EspDist=="TRUE",] #Selecciono registros que NO deberian tener sp diferentes
table(kk$comp)[rev(order(table(kk$comp)))] #No. de pies por cambio de especie
nrow(kk)*100/nrow(tree23a) #porcentaje de cambios en codigo

#PRB:ojo, estas usando nombres IFN3 para todo, ver los del IFN2
#2862 pasan de 44 (Quercus faginea) a 243 (Q. pubescens)
#1539 pasan de 43 (Quercus pyrenaica) a 243 (Q. pubescens)
#1164 pasan de 42 (Quercus petraea) a 243 (Q. pubescens)
#927 pasan de 37 (Juniperus communis) a 237 (J. oxycedrus)
#506 pasan de 55 (Fraxinus angustifolia) a 255 (F. excelsior)
#405 pasan de 58 (Populus nigra) a 258 (P. x canadensis)
#377 pasan de 24 (Pinus halepensis) a 25 (P. nigra)
#337 pasan de 41 (Quercus robur) a 43 (Quercus pyrenaica)
#329 pasan de 76 (Acer campestre) a 476 (Acer opalus)
#Etc
#Conclusion: muchos ids en IFN3 estaban a nivel de especie y no de genero
table(tree23a$Especie2[tree23a$Especie3 == "999"])
#Parecen especies comunes, que se suelen cortar (021, 024, 026,028, 045, 061)
nrow(tree23a[tree23a$Especie3 == "999",])*100/nrow(tree23a) #porcentaje de uso de sp de IFN2

#3.2. Distancia y rumbo: si existe if3 y sino if2
names(tree23s)

tree23s$distancia32 <- tree23s$Distancia3
tree23s$distancia32[is.na(tree23s$distancia32)] <- tree23s$Distancia2[is.na(tree23s$distancia32)]

tree23s$rumbo32 <- tree23s$Rumbo3
tree23s$rumbo32[is.na(tree23s$Rumbo3)] <- as.numeric(tree23s$Rumbo2[is.na(tree23s$rumbo32)])

##4. Calculo de demogragía del IFN2 al IFN3 y selección de errores####
names(allo)
names(tree23s)

treetot<-merge(tree23s,allo,by.x="sppcompa", by.y="COD_SPP_IFN3", all.x=TRUE)
names(treetot)
summary(as.factor(treetot$state3))

##calculamos la biomasa aÃ©rea y subterrÃ¡nea del ifn2 e ifn3
names(treetot)
treetot$R2 <- treetot$R2.x

treetot$BA3[treetot$state == "V" | treetot$state == "R"]<-
  (treetot$CFA[treetot$state == "V" | treetot$state == "R"])*
  ((treetot$dbh3[treetot$state == "V" | treetot$state == "R"])^(treetot$b[treetot$state == "V" | treetot$state == "R"]))
treetot$BR3[treetot$state == "V" | treetot$state == "R"]<-
  (treetot$CFAr[treetot$state == "V" | treetot$state == "R"])*
  ((treetot$dbh3[treetot$state == "V" | treetot$state == "R"])^(treetot$br[treetot$state == "V" | treetot$state == "R"]))
treetot$BA2[treetot$state != "R"] <-
  (treetot$CFA[treetot$state != "R"])*
  ((treetot$dbh2[treetot$state != "R"])^
     (treetot$b[treetot$state != "R"]))
treetot$BR2[treetot$state != "R"] <-
  (treetot$CFAr[treetot$state != "R"])*
  ((treetot$dbh2[treetot$state != "R"])^
     (treetot$br[treetot$state != "R"]))

#calculamos la biomasa relativa a ha
treetot$BA3_Mgha<-treetot$BA3/(0.000314159265*treetot$R3^2)/1000
treetot$BR3_Mgha<-treetot$BR3/(0.000314159265*treetot$R3^2)/1000
treetot$BA2_Mgha<-treetot$BA2/(0.000314159265*treetot$R2^2)/1000
treetot$BR2_Mgha<-treetot$BR2/(0.000314159265*treetot$R2^2)/1000

#calculamos el carbono
treetot$CA3_Mgha<-treetot$BA3_Mgha*treetot$carbono/100
treetot$CR3_Mgha<-treetot$BR3_Mgha*treetot$carbono/100
treetot$CA2_Mgha<-treetot$BA2_Mgha*treetot$carbono/100
treetot$CR2_Mgha<-treetot$BR2_Mgha*treetot$carbono/100

#Calculo el crecimiento en vivos
treetot$AB32 <- NA
treetot$AB32[treetot$state3 == "V"] <- (treetot$AB3[treetot$state3 == "V"] - treetot$AB2[treetot$state3== "V"])
treetot$AB32ha <- (treetot$AB32 * 10000) / 
  (3.14159265 * treetot$R2 ^ 2)

#biomasa aérea vivos respecto al radio del ifn2
treetot$BA32 <- NA
treetot$BA32[treetot$state3 == "V"] <- (treetot$BA3[treetot$state3 == "V"] - treetot$BA2[treetot$state3== "V"])
treetot$BA32ha <- (treetot$BA32 * 10000) / 
  (3.14159265 * treetot$R2 ^ 2)

#biomasa raiz vivos respecto al radio del ifn2
treetot$BR32 <- NA
treetot$BR32[treetot$state3 == "V"] <- (treetot$BR3[treetot$state3 == "V"] - treetot$BR2[treetot$state3== "V"])
treetot$BR32ha <- (treetot$BR32 * 10000) / 
  (3.14159265 * treetot$R2 ^ 2)

# perdidas area basal en muertos entre ifns
treetot$AB2m2ha_muertosc <- NA
treetot$AB2m2ha_muertosc[treetot$state3=="MA" | treetot$state3== "MP"] <- 
  treetot$AB2[treetot$state3=="MA" | treetot$state3== "MP"] * 10000 / 
  (3.14159265 * treetot$R2[treetot$state3=="MA" | treetot$state3== "MP"] ^ 2)
#Perdidas area basal de muertos con madera presente
treetot$AB2m2ha_presente <- NA
treetot$AB2m2ha_presente[treetot$state3== "MP" | treetot$state3=="MP-nc"] <- 
  treetot$AB2[treetot$state3== "MP" | treetot$state3=="MP-nc"] * 10000 / 
  (3.14159265 * treetot$R2[treetot$state3== "MP" | 
                             treetot$state3=="MP-nc"] ^ 2)
# perdidas area basal en pies desaparecidos
treetot$AB2m2ha_ausente <- NA
treetot$AB2m2ha_ausente[treetot$state3== "MA"] <- 
  treetot$AB2[treetot$state3== "MA"] * 10000 / 
  (3.14159265 * treetot$R2[treetot$state3== "MA"] ^ 2)

# ganancias area basal en pies nuevos
treetot$reclutamiento32 <- NA
treetot$reclutamiento32[treetot$state3 == "R"] <- treetot$AB3[treetot$state3 == "R"] * 10000 / 
  (3.14159265 * treetot$R3[treetot$state3 == "R"] ^ 2)

#marco individuos vivos en los dos ifns sin dbh2
treetot$Error32 <- NA
treetot$Error32[treetot$state3 == "V" & is.na(treetot$dbh2)] <- "V-Nodbh2"

#marco individuos vivos en los dos ifns con crecimiento negativo
treetot$Error32[treetot$state3 == "V" & treetot$AB32ha < 0] <- "V-negG"

#repongo los dbh2 faltantes en aquellos casos donde haya dbh3 para muertos
treetot[!is.na(treetot$dbh3) & is.na(treetot$dbh2) & 
          (treetot$state3=="MA" | treetot$state3== "MP" |
             treetot$state3=="MP-nc"),"dbh2"] <- 
  treetot[!is.na(treetot$dbh3) & is.na(treetot$dbh2) & 
            (treetot$state3=="MA" | treetot$state3== "MP" |
               treetot$state3=="MP-nc"),"dbh3"]

treetot$dbh2[treetot$dbh2 == 0 & (treetot$state3=="MA" | treetot$state3== "MP" |
                                    treetot$state3=="MP-nc")] <- NA

treetot$Error32[(treetot$state3=="MA" | treetot$state3== "MP" |
                   treetot$state3=="MP-nc") & is.na(treetot$dbh2)] <- "M-Nodbh2"

kk <- treetot[(treetot$state3=="MA" | treetot$state3== "MP" |
                 treetot$state3=="MP-nc"),c("ID_Pma3","IDPC3","dbh2","dbh3")]
c(length(kk$IDPC3[is.na(kk$dbh2)]),length(unique(kk$IDPC3[is.na(kk$dbh2)]))) #127 plots 
# I got 43...

#marco error no medido en el 2 porque orden 2 es = a 999
#revisar que se hace bien
treetot$Error32[(treetot$Orden22 = "999")] <- "999-NMif2"

##5. Creación archivo final arbol y parcela#####
names(treetot)
treefinal32 <- treetot[,c("ID_Pma3","ID_Pma3c","ID_Pma2","IDPC3","IDPCc3","Plotcode3", 
                          "Provincia3","Cla3","Subclase3","Orden33","Orden23",
                          "rumbo32","distancia32","state3",
                          "sppcompa","Especie2","Especie3",
                          "dbh2","dbh3","h2","h3","dens2","dens3","AB2","AB3",
                          "BA2_Mgha","BR2_Mgha","BA3_Mgha","BR3_Mgha",
                          "CA2_Mgha","CR2_Mgha","CA3_Mgha","CR3_Mgha",
                          "AB2m2ha","AB3m2ha",
                          "AB32","AB32ha","BA32","BA32ha","BR32","BR32ha",
                          "AB2m2ha_muertosc",
                          "AB2m2ha_presente","AB2m2ha_ausente","reclutamiento32",
                          "Agente3","Importancia3","Elemento3","Calidad3","Calidad2",
                          "Forma3","Forma2","Paramesp3","Paramesp2",
                          "VCCha3","VCCha2","VSCha3","VSCha2","IAVCha3","IAVCha2",
                          "VLEha3","VLEha2","Error32","Error3","Error2")]

#summary(treefinal32$distancia32)
#hay algunos errores en distancia que no he quitado
#kk<- subset(treefinal32,distancia32 > 25)
#View(kk)

write_csv(treefinal32, "4results\\20200411_tree23_biomass.csv", col_names = TRUE)

#######################################################################
####### Unión a nivel de parcela
########
#######################################################################
plot2 <- read_csv(file = "2data\\plotcode234.csv")
plot3 <- read_csv(file = "2data\\plotcampo3.csv")

head(names(plot2))
head(names(plot3))

#Union plots a nivel de parcela
pplot32 <- merge(plot3, plot2, by.x = "Plotcode3", by.y = "Plotcode")
names(pplot32)

pplot32$year32 <- pplot32$year3 - pplot32$year2
summary(pplot32$year32)
pplot32$year32[pplot32$year32 < 6] <- pplot32$Dif23.x[pplot32$year32 < 6]

plot32 <- pplot32[,c("IDPC3","Plotcode3","CXed50","CYed50","year2","year3",
                     "date2","date3","year32")]

#estaria bien meter el numero de arboles con error respecto al total: 
#esto depende del estudio, pero se podría agregar aqui por tipo de error.
#revisar VCA: da errores y muchos NAs porque hay NAs en dbh y calculos asociados
#en vez de 0!!

tplot32 <- ddply(treefinal32,.(IDPC3),summarize,
                 Plotcode3 = mean(Plotcode3),
                 ba_ha2 = sum(AB2m2ha, na.rm = TRUE),ba_ha3 = sum(AB3m2ha, na.rm = TRUE),
                 dens2 = sum(dens2, na.rm = TRUE),dens3 = sum(dens3, na.rm = TRUE),
                 VCC2 = sum(VCCha2, na.rm = TRUE),VCC3 = sum(VCCha3, na.rm = TRUE),
                 VSC2 = sum(VSCha2, na.rm = TRUE),VSC3 = sum(VSCha3, na.rm = TRUE),
                 BA_Mgha2 = sum(BA2_Mgha),BR_Mgha2 = sum(BR2_Mgha),
                 CA_Mgha2 = sum(CA2_Mgha),CR_Mgha2 = sum(CR2_Mgha),
                 BA_Mgha3 = sum(BA3_Mgha),BR_Mgha3 = sum(BR3_Mgha),
                 CA_Mgha3 = sum(CA3_Mgha),CR_Mgha3 = sum(CR3_Mgha),
                  BA32vivo = sum(BA32ha, na.rm = TRUE), BR32vivo = sum(BR32ha, na.rm = TRUE),
                  AB32vivo = sum(AB32ha, na.rm = TRUE), AB2muertosc = sum(AB2m2ha_muertosc, na.rm = TRUE),
                 AB2muertopresente = sum(AB2m2ha_presente, na.rm = TRUE), AB2muertoausente = sum(AB2m2ha_ausente, na.rm = TRUE),
                 ABreclutado32 = sum(reclutamiento32, na.rm = TRUE),
                 mdbh3 = mean(dbh3, na.rm = TRUE),mdbh2 = mean(dbh2, na.rm = TRUE),
                 sddbh3 = sd(dbh3, na.rm = TRUE),sddbh2 = sd(dbh2, na.rm = TRUE))


fplot32 <- merge(tplot32, plot32, by.x = c("Plotcode3"), by.y = c("Plotcode3"), all.x = TRUE) ### Uno la table de parcelas de campo con la total mediante plotcode para parcelas de campo y ID_PC (que es un campo unico) para la tabla total

##Calculamos la productividad ####
fplot32$CA32 <- ((fplot32$CA_Mgha3)  - (fplot32$CA_Mgha2) ) / (fplot32$year32)
fplot32$CR32 <- ((fplot32$CR_Mgha3)  - (fplot32$CR_Mgha2) ) / (fplot32$year32)                        

### Con biomasa
fplot32$BA32 <- ((fplot32$BA_Mgha3)  - (fplot32$BA_Mgha2) ) / (fplot32$year32)
fplot32$BR32 <- ((fplot32$BR_Mgha3)  - (fplot32$BR_Mgha2) ) / (fplot32$year32)                        

### Con area basal
fplot32$AB32 <- (((plotfin$ba_ha3) - (plotfin$ba_ha2))) / (plotfin$year32)

fplot32$AB32vivo <- plotfin$AB32vivo / plotfin$year32
fplot32$BA32vivo <- plotfin$BA32vivo / plotfin$year32
fplot32$BR32vivo <- plotfin$BR32vivo / plotfin$year32
names(fplot32)
  
##Antes de exportar se podrían eliminar parcelas con errores
##como no tener ecuaciones alométricas, etc. Calculando el % AB respecto al total

##escribimos el archivo final####
write_csv(fplot32, "4results\\20200411_plot23_biomass.csv", col_names = TRUE)
