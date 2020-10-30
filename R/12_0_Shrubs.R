
# Pablo Salazar Z
# Universidad de Córdoba
# 13/10/2020
# Using field data and climate data to model shrub trait variability


# loading shrub data 

db_shrubs <- read_excel("data/new_data/Muestreo_18_e.xlsx", 
                            sheet = "Matorral")
db_shrubs <- db_shrubs[,-c(6:10,12,13,15:17,20:22,25:26)]
colnames(db_shrubs) <- c("Provincia", "Estadillo", "Especie", "Abreviatura", "Individuo", "LT", "LMA", "LA", "LD",
                         "SD","SDMC", "AI")
db_shrubs <-  db_shrubs %>%
  drop_na()
# plant level analysis -------------

# functional traits PCA

log.ir <- log(db_shrubs[, 6:11]) ## Log Transformation of the variables included
ir.pca <- prcomp(na.omit(log.ir),
                 center = TRUE,
                 scale = TRUE)

summary(ir.pca)
print(ir.pca)
ir.sp <- db_shrubs[,4]
ir.sp <- as.factor(ir.sp$Abreviatura) 

ir.zona <- db_shrubs[,1]
ir.zona <- as.factor(ir.zona$Provincia) 

test2 <- get_pca_ind(ir.pca) ## Individual contribution to the PCA

Indi<-cbind(db_shrubs$Provincia,db_shrubs$Estadillo, db_shrubs$Especie, test2$coord[,1:2])
Indi<- as.data.frame(Indi)
colnames(Indi) <- c("Provincia", "Estadillo", "Especie", "FT1", "FT2")

# Ploting the PCA
th<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  ## delete background
           panel.background = element_blank(), axis.line = element_line(colour = "black"),
           axis.text=element_text(size=16),
           legend.text=element_text(size=20),
           legend.direction = 'horizontal',
           legend.position="top",
           axis.title.x=element_text(size=16),
           axis.title.y=element_text(size=16))


g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.sp, ellipse = TRUE, 
              circle = FALSE, repel =TRUE, varname.size = 6) + th

h <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.zona, ellipse = TRUE, 
              circle = FALSE, repel =TRUE, varname.size = 6) + th

h

## species level analysis -------------

# PCA at species level 

db_shrub2 <- db_shrubs %>%
  group_by(Provincia, Estadillo, Abreviatura) %>%
  summarize_at(vars(LT:SDMC), mean)


log.ir <- log(db_shrub2[, 4:9]) ## Log Transformation of the variables included
ir.pca <- prcomp(na.omit(log.ir),
                 center = TRUE,
                 scale = TRUE)

summary(ir.pca)
print(ir.pca)
ir.sp <- db_shrub2[,3]
ir.sp <- as.factor(ir.sp$Abreviatura) 

ir.zona <- db_shrub2[,1]
ir.zona <- as.factor(ir.zona$Provincia) 

ir.pca$x[,2] <- -ir.pca$x[,2] ## To flip the PC2
ir.pca$rotation[,2] <- -ir.pca$rotation[,2] ## Also hae to flip the eigenvectors

ir.pca$x[,1] <- -ir.pca$x[,1] ## To flip the PC1
ir.pca$rotation[,1] <- -ir.pca$rotation[,1] ## Also hae to flip the eigenvectors

test2 <- get_pca_ind(ir.pca) ## Individual contribution to the PCA

Indi<-cbind(db_shrubs$Provincia,db_shrubs$Estadillo, db_shrubs$Especie, test2$coord[,1:2])
Indi<- as.data.frame(Indi)
colnames(Indi) <- c("Provincia", "Estadillo", "Especie", "FT1", "FT2")

# Ploting the PCA

g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.sp, ellipse = TRUE, 
              circle = FALSE, repel =TRUE, varname.size = 6) + th

h <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.zona, ellipse = TRUE, 
              circle = FALSE, repel =TRUE, varname.size = 6) + th

h

## loading relative abundance -------

abundance <- read_excel("data/new_data/Muestreo_18_e.xlsx", 
                        sheet = "Abundancia matorral")
colnames(abundance)[2] <- "Estadillo" 

# Fixing a bunch of species with wrong names 
abundance[abundance$Especie == "Acebuche", "Especie"] <- "Olea europaea" 
abundance[abundance$Especie == "Olea", "Especie"] <- "Olea europaea" 
abundance[abundance$Especie == "Thymus", "Especie"] <- "Thymus mastichina" 
abundance[abundance$Especie == "Thimus mastichina", "Especie"] <- "Thymus mastichina" 
abundance[abundance$Especie == "Cistus salviifolius", "Especie"] <- "Cistus salvifolius" 
abundance[abundance$Especie == "Cytisus", "Especie"] <- "Cytisus spp." 
abundance[abundance$Especie == "Cytisus e.", "Especie"] <- "Cytisus spp."
abundance[abundance$Especie == "Rhamnus", "Especie"] <- "Rhamnus lycioides"
abundance[abundance$Especie == "Genista", "Especie"] <- "Genista hirsuta"
abundance[abundance$Especie == "Hedera", "Especie"] <- "Hedera helix"
abundance[abundance$Especie == "Asparagus", "Especie"] <- "Asparagus acutifolius"
abundance[abundance$Especie == "Ulex", "Especie"] <- "Ulex spp."
abundance[abundance$Especie == "S. aspera", "Especie"] <- "Smilax aspera"
abundance[abundance$Especie == "Ruscus", "Especie"] <- "Ruscus aculeatus"
abundance[abundance$Especie == "Rubus", "Especie"] <- "Rubus ulmifolius"
abundance[abundance$Especie == "Origanum", "Especie"] <- "Origanum vulgare"
abundance[abundance$Especie == "Osirys", "Especie"] <- "Osyris alba"
abundance[abundance$Especie == "Oxalis", "Especie"] <- "Oxalis spp."
abundance[abundance$Especie == "P. latifolia", "Especie"] <- "Phillyrea latifolia"
abundance[abundance$Especie == "Q. coccifera", "Especie"] <- "Quercus coccifera"
abundance[abundance$Especie == "Q. ilex", "Especie"] <- "Quercus ilex"
abundance[abundance$Especie == "Lavandula", "Especie"] <- "Lavandula stoechas"
abundance[abundance$Especie == "Jasminum", "Especie"] <- "Jazminum fruticans"
abundance[abundance$Especie == "Helichrysum", "Especie"] <- "Helichrysum  italicum"
abundance[abundance$Especie == "Halimium", "Especie"] <- "Halimium spp."
abundance[abundance$Especie == "Halimium a.", "Especie"] <- "Halimium ocymoides"
abundance[abundance$Especie == "Juniperus", "Especie"] <- "Juniperus oxycedrus"
abundance[abundance$Especie == "Juniperus u.", "Especie"] <- "Juniperus oxycedrus"
abundance[abundance$Especie == "Helichrysum  italicum", "Especie"] <- "Helichrysum italicum"
abundance[c(abundance$Especie == "Erica arborea" & abundance$Estadillo == "272894"), "Especie"] <- "Erica cinerea"

db_shrubs[db_shrubs$Especie == "Phaganlon saxatile", "Especie"] <- "Phagnalon saxatile"
db_shrubs[c(db_shrubs$Especie == "Halimium spp." & db_shrubs$Estadillo == "490525"), "Especie"] <- "Halimium ocymoides"

# mixing functional traits and abundance to calculate community weight means

shrubs <- full_join(db_shrubs, abundance, by = c("Estadillo", "Especie"))

shrubs <-  shrubs %>%
  drop_na()

# fixing categorical variables --------------

shrubs <- shrubs %>%
  mutate(Estadillo2 = str_sub(Estadillo, 3, 6),
         code = str_sub(Estadillo, 1, 2))

shrubs$code <- as.factor(shrubs$code)
summary(shrubs$code)
levels(shrubs$code) <- c("Cadiz", "Cordoba", "Lugo", "Ourense", "Sevilla", "Toledo", "Zamora")

shrubs <- shrubs[,-c(1,13)]
colnames(shrubs)[14] <- "Provincia"

# community weight means and functional diversity ------------

# making the functional traits table

shrubs3 <- db_shrubs %>%
  filter(!Especie %in% c("Fraxinus angustifolia", "Pinus pinaster")) %>%
  group_by (Especie)%>%
  summarize_at(vars(LT:SDMC), list(~mean(.,na.rm = TRUE))) %>%
  ungroup() 
shrubs3 <- as.data.frame(shrubs3)
rownames(shrubs3) <- shrubs3$Especie
shrubs3 <- shrubs3[,(2:7)] 

# making the abundance table

abundance2 <- abundance %>%
  pivot_wider(names_from = Especie, values_from = Abundancia)
abundance2 <- as.data.frame(abundance2)
rownames(abundance2) <- abundance2$Estadillo
abundance2 <- abundance2 %>% 
  select(-c(Provincia, Estadillo)) %>%
  select(sort(current_vars()))

# calculate functional diversity 

test <- dbFD(shrubs3, abundance2, w.abun=TRUE, stand.x=TRUE, 
             calc.FRic=TRUE, m="max", stand.FRic=TRUE, 
             scale.RaoQ=TRUE, calc.FGR=FALSE, clust.type="ward", calc.CWM=TRUE, 
             CWM.type="dom", calc.FDiv=TRUE, dist.bin=2, print.pco=FALSE, corr="none") 

test <- list.cbind(test)
test$Estadillo <- rownames(test)

test2 <- full_join(test, FD_matorral, by = "Estadillo")


cor.test(test2$CWM.LMA, test2$LMA)
cor.test(test2$CWM.SDMC, test2$SDMC)
cor.test(test2$FEve.y, test2$FEve.x)
cor.test(test2$FDiv.y, test2$FDiv.x)
cor.test(test2$FDis.y, test2$FDis.x)


plot(test2$CWM.LMA, test2$LMA)
plot(test2$CWM.SDMC, test2$SDMC)
plot(test2$FEve.y, test2$FEve.x)
plot(test2$FDiv.y, test2$FDiv.x)
plot(test2$FDis.y, test2$FDis.x)

# The data seems very similar to Paloma but still there are some things off. 

shrubs4<- shrubs %>%
  group_by(Provincia, Estadillo2) %>%
  summarize(LT = weighted.mean(LT,Abundancia),LMA = weighted.mean(LMA,Abundancia),LD = weighted.mean(LD,Abundancia),
            LA = weighted.mean(LA,Abundancia),SDMC = weighted.mean(SDMC,Abundancia),SD = weighted.mean(SD,Abundancia))

write.table(FD_pa_wd_mh_sm1, file = "FD_pa_wd_mh_sm1.txt", append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"))
write.table(FD_abun_wd_mh_sm1, file = "FD_abun_wd_mh_sm1.txt", append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"))


# community weight PCA

log.ir <- log(shrubs2[, 3:8]) ## Log Transformation of the variables included
ir.pca <- prcomp(na.omit(log.ir),
                 center = TRUE,
                 scale = TRUE)

summary(ir.pca)
print(ir.pca)

ir.zona <- shrubs2[,1]
ir.zona <- as.factor(shrubs2$Provincia) 

ir.pca$x[,1] <- -ir.pca$x[,1] ## To flip the PC1
ir.pca$rotation[,1] <- -ir.pca$rotation[,1] ## Also hae to flip the eigenvectors

test2 <- get_pca_ind(ir.pca) ## Individual contribution to the PCA

Indi<-cbind(db_shrubs$Provincia,db_shrubs$Estadillo, db_shrubs$Especie, test2$coord[,1:2])
Indi<- as.data.frame(Indi)
colnames(Indi) <- c("Provincia", "Estadillo", "Especie", "FT1", "FT2")

# Ploting the PCA


h <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.zona, ellipse = TRUE, 
              circle = FALSE, repel =TRUE, varname.size = 6) + th
h

# functional diversity ----------

FD_matorral <- read_csv("data/new_data/FD_matorral.csv") # Dove calculations
FD_matorral <- FD_matorral[,-c(2,3,15)]
names(FD_matorral)
colnames(FD_matorral)[1] <- c("Estadillo")
colnames(FD_matorral)[8:12] <- c("LMA", "LA", "LD", "SD", "SDMC")
FD_matorral$Estadillo <- as.character(FD_matorral$Estadillo)
