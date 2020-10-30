# Pablo Salazar Z
# Universidad de Córdoba
# 201/08/2020
# Graph making from the IFN
# Spanish National Forestry inventory
# Andalucia subset

# "p30q" from "7_1_1_plot30.R" is required

# basic correlation matrix ---------

p30a <- p30q[,c(3:13, 15:17, 22:24,30,31)]
p30a <- data.frame(lapply(p30a, function(x) as.numeric(as.character(x))))
names(p30a)
colnames(p30a)[7:19] <- c("TB3", "TB2", "FB3", "FB2", "RGR", "BP", "TD2", "TD3", "Wetness", "MAT", "MAP", "long", "lat")
basic <- rcorr(as.matrix(p30a), type = "pearson")

jpeg("Figuras/Apendix.jpg", width = 1050, height = 1050)
corrplot(basic$r, method = "ellipse", insig ="blank", type = "upper", order = "FPC",
         diag=FALSE, cl.cex=1, tl.cex=2, p.mat = basic$P)
dev.off()

 tiff("Figuras/Append.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
corrplot(basic$r, method = "ellipse", insig ="blank", type = "upper", 
         diag=FALSE, cl.cex=1, tl.cex=0.7, p.mat = basic$P)
 dev.off()
 
 db <- p30a
 
 
 # customs theme for ggplot graphs --------------
 custom_theme <- theme(panel.grid.major = element_blank(), # blank background 
                       panel.grid.minor = element_blank(), # blank background 
                       panel.background = element_blank(), # blank background
                       axis.line = element_line(colour = "black"), # black axis
                       panel.border = element_rect(colour = "black", size = 1,
                                                   linetype = "solid", fill = NA), # solid line border
                       axis.text = element_text(size = 18, colour = "black"), # axis color and size
                       axis.title = element_text(size = 20), # axis font size
                       legend.text = element_text(size = 24), # legend font size
                       legend.title = element_text(size = 24)) # legend title font size
 
# SLA model -------------------
 
 gam1 <- gam(MT1 ~ L1 + Wetness + MAP + S1 + S2, data = db, method = "REML")
 gam2 <- gam(MT1 ~ L1 + Wetness + MAP, data = db, method = "REML")
 gam3 <- gam(MT1 ~ L1 + Wetness + MAP + s(S1), data = db, method = "REML") # S1 is not notlinear
 gam4 <- gam(MT1 ~ L1 + Wetness +  MAP + s(S2), data = db, method = "REML") # S2 is notlinear
 gam5 <- gam(MT1 ~ L1 + lat + s(MAP), data = db, method = "REML") # S2 is notlinear
 
 summary(gam1)
 summary(gam2)
 summary(gam3)
 summary(gam4)
 summary(gam5)
 
 gam3 <- gam(MT2 ~ s(L2) + S2 + MAP +  Wetness + MAT, data = db, method = "REML") # L2 is not nolinear
 gam4<- gam(MT2 ~ MAP +  Wetness + MAT, data = db, method = "REML")
 gam5 <- gam(MT2 ~  s(S2) + MAP +  Wetness + MAT, data = db, method = "REML") # S2 is not nolinear
 gam6 <- gam(MT2 ~  s(S1) + S2 + MAP +  Wetness + MAT, data = db, method = "REML") # S2 is nolinear
 
summary(gam3)
summary(gam4)
summary(gam5)
summary(gam6)


gam2$aic
gam3$aic
tabl1 <- summary(gam3)
gam.check(gam3)
# summary(gam2) is a list of results and can be extract using tabl1$. Use it if you need to make new table inside R
# Otherwise, it is best to capture.output 

#capture.output(summary(gam3), file = "products/LMA_gam.txt")

 visreg2d(gam2, yvar = "L1", xvar = "Wetness", scale = "response", col = colorRampPalette(brewer.pal(9,"Greens"))(20))
 
 
 b <- ggplot(db, aes(L1, LMAo)) +
   geom_point() +
   stat_smooth(method = lm) + 
   custom_theme +
   labs(x = "L1", y = "LMA")
 
 a <- ggplot(db, aes(MAP,  LMAo)) +
   geom_point() +
   stat_smooth(method = lm) + 
   custom_theme +
   labs(x = "MAP", y = "LMA")
 
 c <- ggplot(db, aes(LDo, LMAo)) +
   geom_point() +
   stat_smooth(method = lm) + 
   custom_theme +
   labs(x = "Leaf density", y = "LMA")
 
  d<- visreg2d(gam2, xvar = "MAP", yvar = "LDo", 
              zlab = "LMA",
              ylab = "LD",
              xlab = "MAP", plot.type = "gg",
              scale = "response", col = colorRampPalette(brewer.pal(9,"Greens"))(20))
 
 d <- d + theme(axis.text = element_text(size = 20, colour = "black"), # axis color and size
                axis.title = element_text(size = 24), # axis font size
                legend.text = element_text(size = 14), # legend font size
                legend.title = element_text(size = 20),
                legend.position = "top") # legend title font size)  

 e<- ggarrange(a,b,c,d, nrow = 2,  ncol = 2,
               labels = c("A", "B", "C","D"), font.label = list(size = 24, face = "bold"))
 
 tiff("products/Figure3a.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
 e
 dev.off()
 
 jpeg("Figuras/Figure3a.jpg", width = 1050, height = 750)
 e
 dev.off()
 
 # GAM RGR model and graphs -> Figure 4 -------------
 
 gam1 <- gam(RGR ~ s(TB2) + S1, data = db, method = "REML")
 gam2 <- gam(RGR ~ s(TB2) + s(S1) + L2 + MAP , data = db, method = "REML")
 gam3 <- gam(RGR ~ s(TB2) + s(S1) + L2 + MAP + MT1 + MT2, data = db, method = "REML") # MT doesnt work
 gam4 <- gam(RGR ~ s(TB2) + s(MT1) + L2 + MAP  + MT2, data = db, method = "REML") # MT1 is notlinear
 gam5 <- gam(RGR ~ s(TB2) + s(MT2) + L2 + MAP , data = db, method = "REML") # MT2 is notlinear
 gam6 <- gam(RGR ~ s(TB2) + s(L2) + S2 + MAP , data = db, method = "REML") # L2 is linear
 gam7 <- gam(RGR ~ s(TB2) + s(S1) + L2 + MAP + MT1, data = db, method = "REML") # L2 is linear
 
 gam2 <- gam(RGR ~ s(TB2) + s(S1) + L2 + MAP , data = db, method = "REML")
 
 summary(gam1)
 summary(gam2)
 
 gam.check(gam2)
 gam2$aic
 gam3$aic
 gam4$aic
 gam5$aic
 gam1$aic
 gam6$aic
 
 capture.output(summary(gam6), file = "products/RGR_gam.txt")
 
 a <- ggplot(db, aes(S1, RGR)) +
   geom_point() +
   stat_smooth(method = "gam", formula = y ~ s(x)) + 
   custom_theme +
   labs(x = "MAP", y = "Relative growth rate")
 
 b <- ggplot(db, aes(MAT, A_RGR)) +
   geom_point() +
   stat_smooth(method = lm, formula = y ~ x) + 
   custom_theme +
   labs(x = "Mean annual Temperature", y = "Relative growth rate")
 
 c <- ggplot(db, aes(TB2, RGR)) +
   geom_point() +
   stat_smooth(method = "gam", formula = y ~ s(x)) + 
   custom_theme +
   labs(x = "Tree Biomass", y = "Relative growth rate")
 
 d <- visreg2d(gam2, xvar = "TB2", yvar = "MAP", 
               zlab = "RGR",
               ylab = "MAP",
               xlab = "TB2", plot.type = "gg",
               scale = "response", col = colorRampPalette(brewer.pal(9,"Greens"))(20))
 d <- d + theme(axis.text = element_text(size = 20, colour = "black"), # axis color and size
                axis.title = element_text(size = 24), # axis font size
                legend.text = element_text(size = 14), # legend font size
                legend.title = element_text(size = 20),
                legend.position = "top") # legend title font size) 
 
 e<- ggarrange(a,b,c,d, nrow = 2,  ncol = 2,
               labels = c("A", "B", "C","D"), font.label = list(size = 24, face = "bold"))
 
 tiff("products/Figure4b.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
 e
 dev.off()
 
 jpeg("Figuras/Figure4b.jpg", width = 1050, height = 750)
 e
 dev.off()
 
# SEM model and graph -> Figure 5 -------------

# Data transformation 
db <- p30a %>%
  mutate(TB2_t = TB2/100, B_BP_Mgha_t = B_BP_Mgha*100, A_RGR_t = A_RGR*10000000, Ht_ifn_t = Ht_ifn*100,
         L1_t = L1*100, L2_t = L2*100, LDo_t = LDo*1000, ai_chelsa_t = ai_chelsa*10, Arid_Pal_t = Arid_Pal*1000,
         S2_t = S2*100, S1_t = S1*100, Arid_Dm_t = Arid_Dm*10, MAT_t = MAT*10, TD2_t = TD2/10, ) %>%
  na.omit()
 db <- p30a %>%
   mutate(TB2_t = TB2/100, TD2_t = TD2/10, MAP_t = MAP/10) %>%
   na.omit()

sem.1<-"
# Regressions
#BP ~ FB2 + RGR + MAP
FB2 ~ TB2_t + TD2_t
#RGR ~ L1 + MT1 + TB2_t
MT1 ~ MAP_t + L1
"
sem.1.fit<-sem(sem.1, data=db, fixed.x=FALSE, conditional.x = FALSE)
vartable(sem.1.fit)
summary(sem.1.fit, rsq=T,standardized=T)
mi1<-modindices(sem.1.fit) 
print(mi1[mi1$mi>3.0,]) 

ly<-matrix(c(0,0, -0.75,0.5, 0.5,0.5, -0.5,1, 0,1, -1,1, 0.5,1.5, 0.75,1),ncol=2, byrow=TRUE)
lbls<-c("Biomass\n Production","Plot\n Biomass","RGR","Tree\n density","MAT","Tree \n Biomass","Tree \n height","PET")


tiff("products/Figure5.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
semPaths(sem.1.fit,what="std", posCol=c("blue","red"), layout = ly,
         sizeMan=10,sizeMan2=6,label.cex = 1, edge.label.cex=1, edge.label.position=0.30, fade=FALSE, nodeLabels=lbls, residuals=FALSE)
text(0.9,-1,labels="Chi-square = 18.3 \n df = 16 \n p = 0.14", cex = 1)
dev.off()

jpeg("Figuras/Figure5.jpg", width = 1050, height = 750)
semPaths(sem.1.fit,what="std", posCol=c("blue","red"), layout = ly,
         sizeMan=10,sizeMan2=6,label.cex = 1, edge.label.cex=1, edge.label.position=0.30, fade=FALSE, nodeLabels=lbls, residuals=FALSE)
text(0.9,-1,labels="Chi-square = 18.3 \n df = 16 \n p = 0.14", cex = 2)

dev.off()



sem.2<-"
# Regressions
B_BP_Mgha_t ~ AB2_Mgha + A_RGR_t + MAT_t + LMAo 
AB2_Mgha ~ AB2_kgT_t + Tree_dens_t 
A_RGR_t ~ AB2_kgT_t + Arid_Dm_t +   MAT_t + S1_t 
AB2_kgT_t ~ Tree_dens_t
LMAo ~ Arid_Dm_t + LDo_t + S1_t
Arid_Dm_t ~ MAT_t +  MAP_t 


#error covariance
#AB2_Mgha ~~ MAP_t + MAT_t
A_RGR_t ~~ LMAo  +  Tree_dens_t
Tree_dens_t ~~ MAP_t
S1_t ~~ MAP_t + MAT_t

"

sem.2.fit<-sem(sem.2, data=db, fixed.x=FALSE, conditional.x = FALSE)
#vartable(sem.2.fit)
summary(sem.2.fit, rsq=T,standardized=T)
mi3<-modindices(sem.2.fit) 
print(mi3[mi3$mi>3.0,]) 


ly<-matrix(c(0.5,0,                   -0.3,0.6,     0.2,1,        -0.3,1.8,  1.5,1,   0.75,1.8, 0.75,0.6,   -0.75,1.1,  0.2,1.8,   1.5,1.8, 0.75,2.5),ncol=2, byrow=TRUE)
lbls<-c("Forest\n Productivity","Forest\n Biomass","RGR","Tree \n Biomass",  "LMA",    "Wetness",   "MAT",  "Tree\n density", "S1",   "LD", "MAP")

#tiff("products/Figure5c.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
semPaths(sem.2.fit,what="std", posCol=c("blue","red"), layout = ly, 
         sizeMan=10,sizeMan2=6,label.cex = 1, edge.label.cex=1, edge.label.position=0.70, fade=FALSE, residuals=FALSE, exoCov = FALSE)
#text(0.9,-1,labels="Chi-square = 24 \n df = 22 \n p = 0.35", cex = 2)
#dev.off()

jpeg("Figuras/Figure6b.jpg", width = 1050, height = 750)
semPaths(sem.2.fit,what="std", posCol=c("blue","red"), layout = ly, nodeLabels=lbls,
         sizeMan=10,sizeMan2=6,label.cex = 1, edge.label.cex=1, edge.label.position=0.60, fade=FALSE, residuals=FALSE, exoCov = FALSE)
text(0.9,-1,labels="Chi-square = 27.5 \n df = 34 \n p = 0.77", cex = 2)
dev.off()




sem.3<-"
# Regressions
B_BP_Mgha_t ~ AB3_Mgha + A_RGR_t + MAT_t + LMAo 
AB3_Mgha ~ AB2_kgT_t + Tree_dens 
A_RGR_t ~ AB2_kgT_t + Arid_Dm_t +   MAT_t + S1_t
AB2_kgT_t ~ Tree_dens 
LMAo ~ Arid_Dm_t + LDo_t 
Arid_Dm_t ~ MAT_t + MAP 


#error covariance
AB3_Mgha ~~ MAP
A_RGR_t ~~ LMAo  + MAP + Tree_dens
Tree_dens ~~ MAP
AB2_kgT_t ~~ MAT_t
S1_t ~~ MAP 
"

sem.3.fit<-sem(sem.3, data=db, fixed.x=FALSE, conditional.x = FALSE)
#vartable(sem.3.fit)
summary(sem.3.fit, rsq=T,standardized=T)
mi3<-modindices(sem.3.fit) 
print(mi3[mi3$mi>3.0,]) 


sem.4<-"
# Regressions
B_BP_Mgha_t ~ AB3_Mgha + A_RGR_t + MAT_t + LMAo 
AB3_Mgha ~ AB2_kgT_t + Tree_dens 
A_RGR_t ~ AB2_kgT_t + Arid_Dm_t +   MAT_t + S1_t 
AB2_kgT_t ~ Tree_dens 
LMAo ~ Arid_Dm_t + LDo_t 
Arid_Dm_t ~ MAT_t + MAP 


#error covariance
AB3_Mgha ~~ MAP
A_RGR_t ~~ LMAo  + MAP + Tree_dens
Tree_dens ~~ MAP
AB2_kgT_t ~~ MAT_t
S1_t ~~ MAP + MAT_t
"

sem.4.fit<-sem(sem.4, data=db, fixed.x=FALSE, conditional.x = FALSE)
#vartable(sem.4.fit)
summary(sem.4.fit, rsq=T,standardized=T)
mi3<-modindices(sem.4.fit) 
print(mi3[mi3$mi>3.0,]) 


ly<-matrix(c(0.5,0,                   -0.3,0.6,       0,1,        -0.3,1.5,  1.5,1,   0.75,1.5, 0.75,0.6,   -0.75,1.1,  0.2,1.5,   1.5,1.5, 0.75,2),ncol=2, byrow=TRUE)
lbls<-c("Forest\n Productivity","Forest\n Biomass","RGR","Tree \n Biomass",  "LMA",    "Arid",   "MAT",  "Tree\n density", "S1", "LD",   "MAP")

tiff("products/Figure6.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
semPaths(sem.4.fit,what="std", posCol=c("blue","red"), layout = ly, nodeLabels=lbls,
         sizeMan=10,sizeMan2=6,label.cex = 1, edge.label.cex=1, edge.label.position=0.20, fade=FALSE, residuals=FALSE, exoCov = FALSE)
text(0.9,-1,labels="Chi-square = 31.9 \n df = 32 \n p = 0.47", cex = 2)
dev.off()

jpeg("Figuras/Figure6.jpg", width = 1050, height = 750)
semPaths(sem.4.fit,what="std", posCol=c("blue","red"), layout = ly, nodeLabels=lbls,
         sizeMan=10,sizeMan2=6,label.cex = 1, edge.label.cex=1, edge.label.position=0.20, fade=FALSE, residuals=FALSE, exoCov = FALSE)
text(-0.9,-1,labels="Chi-square = 31.9 \n df = 32 \n p = 0.47", cex = 2)
dev.off()


sem.5<-"
# Regressions
B_BP_Mgha_t ~ AB2_Mgha + A_RGR_t + MAT_t + LMAo 
AB2_Mgha ~ AB2_kgT_t + Tree_dens_t 
A_RGR_t ~ AB2_kgT_t + Arid_Dm_t 
AB2_kgT_t ~ Tree_dens_t
LMAo ~ Arid_Dm_t + LDo_t 
Arid_Dm_t ~ MAT_t + MAP_t 


#error covariance
AB2_Mgha ~~ MAP_t
A_RGR_t ~~ MAP_t + LMAo + MAT_t + Tree_dens_t 
AB2_kgT_t ~~ MAT_t
"
sem.5.fit<-sem(sem.5, data=db, fixed.x=FALSE, conditional.x = FALSE)
#vartable(sem.5.fit)
summary(sem.5.fit, rsq=T,standardized=T)
mi3<-modindices(sem.5.fit) 
print(mi3[mi3$mi>3.0,]) 


ly<-matrix(c(0,0, -0.75,1, 0,0.5, 0,1.5, 0.75,0.5, 0.75,1.5,  1.5,1.5, -0.75,1.5, 1.5,1, 1.5,0.5),ncol=2, byrow=TRUE)
lbls<-c("Biomass\n Productivity","Forest\n Biomass","RGR","Tree \n Biomass","LMA","Arid","Tree\n density",  "MAT", "LD", "MAP", "PET")

#tiff("products/Figure5c.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
semPaths(sem.5.fit,what="std", posCol=c("blue","red"), layout = ly, 
         sizeMan=10,sizeMan2=6,label.cex = 1, edge.label.cex=1, edge.label.position=0.20, fade=FALSE, residuals=FALSE, exoCov = FALSE)
#text(0.9,-1,labels="Chi-square = 24 \n df = 22 \n p = 0.35", cex = 2)
#dev.off()

jpeg("Figuras/Figure5f.jpg", width = 1050, height = 750)
semPaths(sem.4.fit,what="std", posCol=c("blue","red"), layout = ly, nodeLabels=lbls,
         sizeMan=10,sizeMan2=6,label.cex = 1, edge.label.cex=1, edge.label.position=0.20, fade=FALSE, residuals=FALSE, exoCov = FALSE)
text(-0.9,-1,labels="Chi-square = 35 \n df = 35 \n p = 0.47", cex = 2)
dev.off()

AIC(sem.4.fit)
AIC(sem.3.fit)

#some testing graphs ---------------

Koppen<-tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + tm_fill() + tm_borders() +
  tm_shape(DataMp_sf) + tm_dots(col = "grad1", size = 1, n = 10) + tm_layout(title = "Koppen")
Emberger<-tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + tm_fill() + tm_borders() +
  tm_shape(DataMp_sf) + tm_dots(col = "grad2", size = 1, n = 10) + tm_layout(title = "Emberger")
Dm<-tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + tm_fill() + tm_borders() +
  tm_shape(DataMp_sf) + tm_dots(col = "grad3", size = 1, n = 10) + tm_layout(title = "Dm")
gradient <- tmap_arrange(Koppen, Emberger, Dm, nrow=3)
tmap_save(gradient, "Gradient.jpg")

# Making some graphics to see spatial biomass distribution data ------------

phalep<-tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + 
  tm_fill() + tm_borders() +
  tm_shape(subset(Biom_m2_sf, Sp == "Pinus halepensis" )) + 
  tm_dots(col = "grad1", size = "bio_dens", n = 5) + 
  tm_layout(title = "P. halepensis Biomass/m2",
            legend.outside = TRUE)
qilex <- tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + 
  tm_fill() + tm_borders() +
  tm_shape(subset(Biom_m2_sf, Sp == "Quercus ilex" )) + 
  tm_dots(col = "grad1", size = "bio_dens", n = 5) + 
  tm_layout(title = "Q. ilex Biomass/m2",
            legend.outside = TRUE)
ppinea <- tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + 
  tm_fill() + tm_borders() +
  tm_shape(subset(Biom_m2_sf, Sp == "Pinus pinea" )) + 
  tm_dots(col = "grad1", size = "bio_dens", n = 5) + 
  tm_layout(title = "P. pinea Biomass/m2",
            legend.outside = TRUE)
qsuber <- tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + 
  tm_fill() + tm_borders() +
  tm_shape(subset(Biom_m2_sf, Sp == "Quercus suber" )) + 
  tm_dots(col = "grad1", size = "bio_dens", n = 5) + 
  tm_layout(title = "Q. suber Biomass/m2",
            legend.outside = TRUE)
ppinas <- tm_shape(subset(Spain, NAME_1 == "Andaluc?a")) + 
  tm_fill() + tm_borders() +
  tm_shape(subset(Biom_m2_sf, Sp == "Pinus pinaster" )) + 
  tm_dots(col = "grad1", size = "bio_dens", n = 5) + 
  tm_layout(title = "P. pinaster Biomass/m2",
            legend.outside = TRUE)
species_bio <- tmap_arrange(Koppen, qilex, phalep, qsuber, ppinea, ppinas, nrow=3, ncol = 2)
tmap_save(species_bio, "species_bio.jpg")
tmap_save(qilex, "qilex_bio.jpg")


a <- ggplot(db, aes(MAT, B_BP_Mgha)) +
  geom_point() +
  stat_smooth(method = lm) + 
  custom_theme +
  labs(x = "MAT", y = "B productivity")

b <- ggplot(db, aes(MAT, A_RGR)) +
  geom_point() +
  stat_smooth(method = lm) + 
  custom_theme +
  labs(x = "MAT", y = "RGR")

c <- ggplot(db, aes(A_RGR, B_BP_Mgha)) +
  geom_point() +
  stat_smooth(method = lm) + 
  custom_theme +
  labs(x = "RGR", y = "B productivity")

jpeg("Figuras/test.jpg", width = 1050, height = 750)
ggarrange(a,b,c, nrow = 2,  ncol = 2,
          font.label = list(size = 24, face = "bold"))
dev.off()


a <- ggplot(db, aes(MAT, PET)) +
  geom_point() +
  stat_smooth(method = lm) + 
  custom_theme +
  labs(x = "MAT", y = "PET")

b <- ggplot(db, aes(PET, A_RGR)) +
  geom_point() +
  stat_smooth(method = lm) + 
  custom_theme +
  labs(x = "PET", y = "RGR")

c <- ggplot(db, aes(A_RGR, MAT)) +
  geom_point() +
  stat_smooth(method = lm) + 
  custom_theme +
  labs(x = "RGR", y = "MAT")

jpeg("Figuras/test2.jpg", width = 1050, height = 750)
ggarrange(a,b,c, nrow = 2,  ncol = 2,
          font.label = list(size = 24, face = "bold"))
dev.off()

