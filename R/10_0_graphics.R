# Pablo Salazar Z
# Universidad de Córdoba
# 201/08/2020
# Graph making from the IFN
# Spanish National Forestry inventory
# Andalucia subset

# "p30a" from "7_10_plot30.R" is required

# basic correlation matrix ---------

p30c <- p30a[,c(1,2,12,13,16,19,23:25,33:35,37)]
p30c <- p30c[,c(11,10,12,13,3,4,1,2,5,6,9,8,7)]
colnames(p30c) <- c("LMA", "LT", "LD", "WDMC", "L1", "L2", "S1", "S2", "TB", "RGR", "MAP", "MAT", "Wetness")
basic <- cor(p30c, use = "complete.obs")
basic <- rcorr(as.matrix(p30c), type = "pearson")
# p <- p.adjust(basic$P, "bonferroni")

jpeg("Figuras/Apendix.jpg", width = 1050, height = 1050)
corrplot(basic$r, method = "ellipse", insig ="blank", type = "upper", 
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
 
 gam2 <- gam(LMAo ~ L1 + Arid_Dm + LDo + s(MAP), data = db, method = "REML")
 gam3 <- gam(LMAo ~ L1 + Arid_Dm + LDo + MAP, data = db, method = "REML")
 
summary(gam2)
summary(gam3)
gam2$aic
gam3$aic
tabl1 <- summary(gam3)
gam.check(gam3)
# summary(gam2) is a list of results and can be extract using tabl1$. Use it if you need to make new table inside R
# Otherwise, it is best to capture.output 

capture.output(summary(gam3), file = "products/LMA_gam.txt")

 visreg2d(gam3, yvar = "MAP", xvar = "LDo", scale = "response", col = colorRampPalette(brewer.pal(9,"Greens"))(20))
 
 
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
 

 
 # GAM Plot biomass model and graphs -> Figure 5 -------------

a <- ggplot(db, aes(AB2_kgT, AB2_Mgha)) +
  geom_point() +
  stat_smooth(method = lm) +
  custom_theme +
  labs(x = "Tree Biomass (kg per Tree)", y = "Forest biomass (Mg ha-1)")

b <- ggplot(db, aes(Tree_dens, AB2_Mgha)) +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x)) +
  custom_theme +
  labs(x = "Tree density (tree per ha)", y = "Forest biomass (Mg ha-1)")

gam <- gam(AB2_Mgha ~ AB2_kgT + s(Tree_dens), data = db)

summary(gam)
gam.check(gam)

capture.output(summary(gam), file = "products/F_biomass_gam.txt")

c<- visreg2d(gam, xvar = "Tree_dens", yvar = "AB2_kgT", 
             zlab = "Forest Biomass",
             ylab = " Tree Biomass (kg per Tree)",
             xlab = "Tree density (tree per ha)", plot.type = "gg",
             scale = "response", col = colorRampPalette(brewer.pal(9,"Greens"))(20))

c <- c + theme(axis.text = element_text(size = 20, colour = "black"), # axis color and size
               axis.title = element_text(size = 24), # axis font size
               legend.text = element_text(size = 14), # legend font size
               legend.title = element_text(size = 20),
               legend.position = "top") # legend title font size) 

e<- ggarrange(a,b, nrow = 2,  ncol = 1,
              labels = c("A", "B"), font.label = list(size = 24, face = "bold"))
e <- ggarrange(e, c, ncol = 2, labels = c("","C"), font.label = list(size = 24, face = "bold")) # Second row with box and dot plots

tiff("products/Figure5.tiff", width = 16, height = 9, units = 'in', res = 300, compression = 'lzw')
e
dev.off()

jpeg("Figuras/Figure5.jpg", width = 1050, height = 650)
e
dev.off()

# GAM RGR model and graphs -> Figure 4 -------------

gam1 <- gam(A_RGR ~  s(AB2_kgT) + MAT + MAP, data = db, method = "REML")
gam2 <- gam(A_RGR ~ s(AB2_kgT) + MAT + MAP + S1, data = db, method = "REML")
gam3 <- gam(A_RGR ~ s(AB2_kgT) + MAT + MAP + L1, data = db, method = "REML")
gam4 <- gam(A_RGR ~ s(AB2_kgT) + MAT + MAP + L1 + S1, data = db, method = "REML")
gam5 <- gam(A_RGR ~ s(AB2_kgT) + MAT + LMAo, data = db, method = "REML")
gam6 <- gam(A_RGR ~ s(AB2_kgT) + MAT + s(MAP), data = db, method = "REML")


summary(gam1)
summary(gam2)
summary(gam3)
summary(gam4)
summary(gam5)
summary(gam6)

gam.check(gam2)
gam2$aic
gam3$aic
gam4$aic
gam5$aic
gam1$aic
gam6$aic

capture.output(summary(gam6), file = "products/RGR_gam.txt")

a <- ggplot(db, aes(MAP, A_RGR)) +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x)) + 
  custom_theme +
  labs(x = "MAP", y = "Relative growth rate")

b <- ggplot(db, aes(MAT, A_RGR)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) + 
  custom_theme +
  labs(x = "Mean annual Temperature", y = "Relative growth rate")

c <- ggplot(db, aes(AB2_kgT, A_RGR)) +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x)) + 
  custom_theme +
  labs(x = "Tree Biomass", y = "Relative growth rate")

d <- visreg2d(gam6, xvar = "MAT", yvar = "MAP", 
              zlab = "RGR",
              ylab = "MAP",
              xlab = "MAT", plot.type = "gg",
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
  mutate(AB2_kgT_t = AB2_kgT/100, B_BP_Mgha_t = B_BP_Mgha*100, A_RGR_t = A_RGR*10000000, Ht_ifn_t = Ht_ifn*100,
         L1_t = L1*100, L2_t = L2*100, LDo_t = LDo*1000, ai_chelsa_t = ai_chelsa*10, Arid_Pal_t = Arid_Pal*1000,
         S2_t = S2*100, S1_t = S1*100, Arid_Dm_t = Arid_Dm*10, MAT_t = MAT*10, Tree_dens_t = Tree_dens/10, MAP_t = MAP/10) %>%
  na.omit()


sem.1<-"
# Regressions
B_BP_Mgha_t ~ AB2_Mgha + A_RGR_t + MAT
AB3_Mgha ~ AB2_kgT_t + Tree_dens
A_RGR_t ~ MAT + Ht_ifn_t + PET
Tree_dens ~ Ht_ifn_t

#error covariance
Tree_dens ~~ A_RGR_t
AB2_kgT_t ~~ Ht_ifn_t
"
sem.1.fit<-sem(sem.1, data=db, fixed.x=FALSE, conditional.x = FALSE)

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

