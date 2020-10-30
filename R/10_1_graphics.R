# Pablo Salazar Z
# Universidad de Córdoba
# 201/08/2020
# Graph making from the IFN
# Spanish National Forestry inventory
# Andalucia subset

# "p30a" from "7_1_plot30.R" is required

colnames(p30a) <- c("Provincia", "Estadillo", "S1", "S2", "Clay", "L1", "L2", "LT", "LMA", "LD", "LA", "SWDMC", 
                    "SWD", "TD2", "TD3", "TB3", "TB2", "TBM", "FB3",
                    "FB3M", "FBM", "FB2", "RGR", "RGRM", "FPM", "FP",  "Wetness", "MAT","MAP")

# Using IFN3-Muestreo sampling -----

p30c <- p30a %>%# filtering negative values
  mutate(RGRM = replace(RGRM, which(RGRM<0), NA),
         FPM = replace(FPM, which(RGRM<0), NA)) %>%
  mutate(AI = 100 - Wetness) %>%
  select(-TD2, -TBM,-TB2, -FB3, -FB2,-FBM, -RGR, -FP, -Wetness) 
  

# basic correlation matrix ---------

basic <- rcorr(as.matrix(p30c[,c(3:21)]), type = "pearson")

jpeg("Figuras/Apendix.jpg", width = 1050, height = 1050)
corrplot(basic$r, method = "ellipse", insig ="blank", type = "upper", 
         diag=FALSE, cl.cex=1, tl.cex=2, p.mat = basic$P)
dev.off()

tiff("Figuras/Append.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
corrplot(basic$r, method = "ellipse", insig ="blank", type = "upper", 
         diag=FALSE, cl.cex=1, tl.cex=0.7, p.mat = basic$P)
dev.off()

db <- p30c

db$Provincia <- as.factor(db$Provincia)
db$Estadillo <- as.factor(db$Estadillo)
db$L1 <- as.numeric(db$L1)
db$L2 <- as.numeric(db$L2)

# summary table for the paper
temp <- db %>%
  ungroup() %>% # If a tibble have been grouped before, it will remember forever, so is better to ungroup if you want to make new groups
  select(Clay,LT:AI)%>%
  pivot_longer(
    cols = (Clay:AI),
    names_to = "variable",
    values_to = "value")  %>% # here we are melting the data for the summarise_all function
  group_by(variable) %>%
  summarise_all(list(~mean(., na.rm = TRUE), 
                     ~sd(.,na.rm = TRUE),
                     ~min(.,na.rm = TRUE),
                     ~max(.,na.rm = TRUE),
                     ~median(.,na.rm = TRUE))) %>%
  mutate(cv = sd/mean*100)

write.csv(temp, "products/summarydata.csv")

db$grad <- revalue(db$Provincia, c("Lugo" = "Galicia", "Ourense" = "Galicia"))

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



# Functional traits glm model -------------------

glm1 <- lme(LMA ~ AI + S1 + S2 + Clay, random = ~1|grad/Estadillo, na.action = na.omit, data = db) 
glm2 <- lme(LD ~ AI + S1 + S2 + Clay, random = ~1|grad/Estadillo, na.action = na.omit, data = db) 
glm3 <- lme(LT ~ AI + S1 + S2 + Clay, random = ~1|grad/Estadillo, na.action = na.omit, data = db) 
glm4 <- lme(LA ~ AI + S1 + S2 + Clay, random = ~1|grad/Estadillo, na.action = na.omit, data = db) 
glm5 <- lme(SWDMC ~ AI + S1 + S2 + Clay, random = ~1|grad/Estadillo, na.action = na.omit, data = db) 
glm6 <- lme(SWD ~ AI + S1 + S2 + Clay, random = ~1|grad/Estadillo, na.action = na.omit, data = db) 
glm7 <- lme(L1 ~ AI + S1 + S2 + Clay, random = ~1|grad/Estadillo, na.action = na.omit, data = db) 
glm8 <- lme(L2 ~ AI + S1 + S2 + Clay, random = ~1|grad/Estadillo, na.action = na.omit, data = db) 


LMA<- summary(glm1)$tTable[,1]
LD <- summary(glm2)$tTable[,1]
LT <- summary(glm3)$tTable[,1]
LA <- summary(glm4)$tTable[,1]
SWDMC <- summary(glm5)$tTable[,1]
SWD <- summary(glm6)$tTable[,1]
L1 <- summary(glm7)$tTable[,1]
L2 <- summary(glm8)$tTable[,1]

table1 <- rbind(LMA, LD, LT, LA, WDMC, SD, L1, L2) # table of parameters

LMA<- summary(glm1)$tTable[,5]
LD <- summary(glm2)$tTable[,5]
LT <- summary(glm3)$tTable[,5]
LA <- summary(glm4)$tTable[,5]
WDMC <- summary(glm5)$tTable[,5]
SD <- summary(glm6)$tTable[,5]
L1 <- summary(glm7)$tTable[,5]
L2 <- summary(glm8)$tTable[,5]

table <- rbind(LMA, LD, LT, LA, WDMC, SD, L1, L2) # table of significances

table1 <- round(table1,3)
mystars <- ifelse(table < .01, "**", ifelse(table < .05, "*", ifelse(table < .1, "a", " "))) # 
Tnew <- matrix(paste(table1, mystars, sep=""), ncol=ncol(table))
rownames(Tnew) <- rownames(table1)
colnames(Tnew) <- paste(colnames(table1), "", sep="")

R2 <- c(r.squaredGLMM(glm1)[,2], r.squaredGLMM(glm2)[,2], r.squaredGLMM(glm3)[,2], r.squaredGLMM(glm4)[,2], # adding the pseudo R2 of each lm to the table
       r.squaredGLMM(glm5)[,2], r.squaredGLMM(glm6)[,2], r.squaredGLMM(glm7)[,2], r.squaredGLMM(glm8)[,2])  
R2 <- round(R2,2)
Tnew <- cbind(Tnew,R2)

write.csv(Tnew,"products/FT_glm.csv")

a <- ggplot(db, aes(AI, LMA)) +
  geom_point() +
  stat_smooth(method = "lm") + 
  custom_theme +
  labs(x = "AI index", y = "Leaf mass per area")

b <- ggplot(db, aes(AI, LT)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) + 
  custom_theme +
  labs(x = "AI index", y = "Leaf thickness")

c <- ggplot(db, aes(S1, L1)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) + 
  custom_theme +
  labs(x = "Clay (%)", y = "Leaf nutrient PC1")

d <- ggplot(db, aes(Clay, L2)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) + 
  custom_theme +
  labs(x = "Clay (%)", y = "Leaf nutrient PC2")

e<- ggarrange(a,b,c,d, nrow = 2,  ncol = 2,
              labels = c("A", "B", "C","D"), font.label = list(size = 24, face = "bold"))

tiff("products/Figure3.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
e
dev.off()

jpeg("Figuras/Figure3.jpg", width = 1050, height = 750)
e
dev.off()
## RGR models with glm ----------------

db$LogTB3 <- log(db$TB3)
glm8 <- lme(RGRM ~ LogTB3 + S1 + S2 + Clay + LMA + SWDMC + L1 + L2 + LT + AI + MAT, random = ~1|grad/Estadillo, na.action = na.omit, data = db) 
summary(glm8)



RGR <- as.matrix(summary(glm8)$tTable[,1])
RGR_p <- summary(glm8)$tTable[,5]
RGR <- round(RGR,3)
mystars <- ifelse(RGR_p < .01, "**", ifelse(RGR_p < .05, "*", ifelse(RGR_p < .1, "a", " "))) # 
Tnew <- matrix(paste(RGR, mystars, sep=""))
rownames(Tnew) <- rownames(RGR)
Tnew <- rbind(Tnew,r.squaredGLMM(glm8)[,2])
rownames(Tnew)[13] <- "R2" 
colnames(Tnew) <- "Parameters"
Tnew <- t(Tnew)
write.csv(Tnew,"products/RGR_glm.csv")

a <- ggplot(db, aes(AI, RGRM)) +
  geom_point() +
  stat_smooth() + 
  custom_theme +
  labs(x = "AI", y = "Relative growth rate")

b <- ggplot(db, aes(S2, RGRM)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) + 
  custom_theme +
  labs(x = "Soil nutrients PC2", y = "Relative growth rate")

c <- ggplot(db, aes(TB3, RGRM)) +
  geom_point() +
  stat_smooth(method = lm) + 
  custom_theme +
  scale_x_log10()+
  labs(x = "Log(Tree Biomass)", y = "Relative growth rate")


e<- ggarrange(b,c, nrow = 1,  ncol = 2,
              labels = c("A", "B"), font.label = list(size = 24, face = "bold"))

tiff("products/Figure4.tiff", width = 12, height = 4.5, units = 'in', res = 300, compression = 'lzw')
e
dev.off()

jpeg("Figuras/Figure4.jpg", width = 1050, height = 350)
e
dev.off()


# piecewise model ------------

db <- na.omit(db)
# is not necessary to re-dimension the variables to make homogeneous variances

sem.1 <- psem(
  lme(FPM ~ FB3M + RGRM, random = ~1|grad, na.action = na.omit, data = db),
  lme(FB3M ~ TB3 + TD3,  random = ~1|grad, na.action = na.omit, data = db),
  lme(RGRM ~ TB3 + AI + LMA + S2 + L1, random = ~1|grad, na.action = na.omit, data = db),
  lme(TB3 ~ TD3, random = ~1|grad, na.action = na.omit, data = db),
  lme(LMA ~ AI, random = ~1|grad, na.action = na.omit, data = db))
  
summary(sem.1)
plot(sem.1, layout = "tree")


sem.2 <- psem(
  lme(FPM ~ FB3M + RGRM + LMA, random = ~1|grad, na.action = na.omit, data = db),
  lme(FB3M ~ TB3 ,  random = ~1|grad, na.action = na.omit, data = db),
  lme(RGRM ~ TB3 + S2, random = ~1|grad, na.action = na.omit, data = db),
  #lme(TB3 ~ TD3, random = ~1|grad, na.action = na.omit, data = db),
  lme(LMA ~ AI + L1, random = ~1|grad, na.action = na.omit, data = db))

summary(sem.2)
plot(sem.2, layout = "tree")

sem.3 <- psem(
  lme(FPM ~ FB3M + RGRM + TD3, random = ~1|grad, na.action = na.omit, data = db),
  lme(FB3M ~ TB3 + LMA,  random = ~1|grad, na.action = na.omit, data = db),
  lme(RGRM ~ TB3 + S2 , random = ~1|grad, na.action = na.omit, data = db),
  #lme(TB3 ~ TD3, random = ~1|grad, na.action = na.omit, data = db),
  lme(LMA ~ AI + L1, random = ~1|grad, na.action = na.omit, data = db))

summary(sem.3)
plot(sem.3, layout = "tree")


sem.4 <- psem(
  lme(FPM ~ FB3M + RGRM, random = ~1|grad, na.action = na.omit, data = db),
  lme(FB3M ~ TB3 ,  random = ~1|grad, na.action = na.omit, data = db),
  lme(RGRM ~ TB3 + S2, random = ~1|grad, na.action = na.omit, data = db),
  lme(TB3 ~ L1, random = ~1|grad, na.action = na.omit, data = db),
  lme(LMA ~ AI, random = ~1|grad, na.action = na.omit, data = db),
  lme(L1 ~ LMA, random = ~1|grad, na.action = na.omit, data = db))

summary(sem.4)
plot(sem.4, layout = "tree")

sem.5 <- psem(
  lme(FPM ~ FB3M + RGRM, random = ~1|grad, na.action = na.omit, data = db),
  lme(FB3M ~ TB3 ,  random = ~1|grad, na.action = na.omit, data = db),
  lme(RGRM ~ TB3 + S2 + S1 + L2, random = ~1|grad, na.action = na.omit, data = db),
  lme(TB3 ~ L1, random = ~1|grad, na.action = na.omit, data = db),
  lme(LMA ~ AI, random = ~1|grad, na.action = na.omit, data = db),
  lme(L1 ~ LMA, random = ~1|grad, na.action = na.omit, data = db))

summary(sem.5)
plot(sem.5, layout = "tree")


sem.6 <- psem(
  lme(FPM ~ FB3M + RGRM, random = ~1|grad, na.action = na.omit, data = db),
  lme(FB3M ~ TB3 + LMA,  random = ~1|grad, na.action = na.omit, data = db),
  lme(RGRM ~ TB3 + S2 + S1 , random = ~1|grad, na.action = na.omit, data = db),
  lme(LMA ~ AI + L1, random = ~1|grad, na.action = na.omit, data = db),
  S1 %~~% FPM,
  S2 %~~% FPM)

summary(sem.6)
plot(sem.6, layout = "tree")

a<- c(summary(sem.1)$IC$AIC, summary(sem.2)$IC$AIC, summary(sem.3)$IC$AIC, summary(sem.4)$IC$AIC, summary(sem.5)$IC$AIC, summary(sem.6)$IC$AIC)


tiff("products/Figure5.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
plot(sem.6, layout = "tree")
#text(0.9,-1,labels="Fishers's C = 31.08 \n df = 30 \n p = 0.41", cex = 1)
dev.off()


tiff("products/Figure5b.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
plot(sem.1, layout = "tree")
#text(0.9,-1,labels="Fishers's C = 31.08 \n df = 30 \n p = 0.41", cex = 1)
dev.off()


# Using IFN3-2 sampling -----

p30c <- p30a %>%# filtering negative values
  mutate(RGR = replace(RGR, which(RGR<0), NA),
         FP = replace(FP, which(RGR<0), NA)) %>%
  select(-TD3, -TBM,- TB3, -FB3, -FB3M, -FBM, -RGRM, -FPM)

# basic correlation matrix ---------

basic <- rcorr(as.matrix(p30c[,c(3:21)]), type = "pearson")

jpeg("Figuras/Apendix.jpg", width = 1050, height = 1050)
corrplot(basic$r, method = "ellipse", insig ="blank", type = "upper", 
         diag=FALSE, cl.cex=1, tl.cex=2, p.mat = basic$P)
dev.off()

tiff("Figuras/Append.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
corrplot(basic$r, method = "ellipse", insig ="blank", type = "upper", 
         diag=FALSE, cl.cex=1, tl.cex=0.7, p.mat = basic$P)
dev.off()

db <- p30c

