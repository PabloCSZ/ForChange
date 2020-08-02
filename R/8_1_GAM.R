
# Pablo Salazar Z
# Universidad de Córdoba
# 11/06/2020
# Using IFN data, functional traits, soil data, and climate data to model biomass production
# Spanish National Forestry inventory

# To run 
# "p30a" from "7_1_plot30.R" is required


## GAM model for Plot biomass

 # Plot biomass explained by tree biomass 

gam1 <- gam(AB3_Mgha ~ AB3_kgT, data = p30a) # This is the good one

gam2 <- gam(AB3_Mgha ~ s(AB3_kgT, bs = "ts"), data = p30a)
coef(gam2) ## The GAM only makes 1 coef becuase the relationship is in fact linear

summary(gam1)
summary(gam2)
plot(gam2, residuals = TRUE, pch = 1)
a
gam.check(gam2)
AIC(gam1)
AIC(gam2)

anova(gam1, gam2, test= "Chisq")

 # Plot biomass explained by tree density 

gam2 <- gam(AB3_Mgha ~ Tree_dens, data = p30a)
gam3 <- gam(AB3_Mgha ~ s(Tree_dens), data = p30a, method = "REML") # none of them are good but this is better
coef(gam3) ## The GAM makes several coefs because the relationship is not linear

summary(gam2)
summary(gam3)
plot.gam(gam3, residuals = TRUE, pch = 1)
b
gam.check(gam3, pages = 1)

## Plot biomass explained by tree biomass and tree density 

Pgam1 <- gam(AB3_Mgha ~ AB3_kgT + Tree_dens, data = p30a)
Pgam2 <- gam(AB3_Mgha ~ AB3_kgT + s(Tree_dens), data = p30a)

summary(Pgam1)
summary(Pgam2)
gam.check(Pgam2)

AIC(Pgam1)
AIC(Pgam2)
summary(Pgam1)$r.sq
summary(Pgam2)$r.sq

anova(Pgam1, Pgam2, test= "Chisq")

ggplot(p30a, aes(Tree_dens, AB3_Mgha)) +
  geom_point() +
  stat_smooth(method = lm) + theme_bw()  +
  geom_text(x = 400, y = 60, label = "R2 = -0.01")

ggplot(p30a, aes(Tree_dens, AB3_Mgha)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) + theme_bw()  +
  geom_text(x = 400, y = 60, label = "R2 = 0.07")

ggplot(p30a, aes(AB3_kgT, AB3_Mgha)) +
  geom_point() +
  stat_smooth(method = lm) + theme_bw() + 
  geom_text(x = 2500, y = 75, label = "R2 = 0.62")



vis.gam(Pgam2, type = "response", plot.type = "persp", phi = 30, theta = 30, n.grid = 500)
visreg2d(Pgam2, xvar = "Tree_dens", yvar = "AB3_kgT", scale = "response", col = colorRampPalette(brewer.pal(9,"Greens"))(20))


## RGR explained by functional traits, Aridity, and tree size


gam1 <- gam(A_RGR ~ PET + MAT + Ht_ifn, data = p30a, method = "REML")
summary(gam1)
gam.check(gam1)

gam.check(gam1)

ggplot(p30a, aes(PET, A_RGR)) +
  geom_point() +
  stat_smooth(method = lm) + theme_bw()  +
  geom_text(x = 400, y = 60, label = "R2 = -0.01")

ggplot(p30a, aes(MAT, A_RGR)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) + theme_bw()  +
  geom_text(x = 400, y = 60, label = "R2 = 0.07")

ggplot(p30a, aes(per_ifn, A_RGR)) +
  geom_point() +
  stat_smooth(method = lm) + theme_bw() + 
  geom_text(x = 2500, y = 75, label = "R2 = 0.62")

ggplot(p30a, aes(clay, A_RGR)) +
  geom_point() +
  stat_smooth(method = lm) + theme_bw() 
  

visreg2d(gam1, xvar = "PET", yvar = "Ht_ifn", scale = "response", col = colorRampPalette(brewer.pal(9,"Greens"))(20))



## alternative GAMs but none of them work

gam2 <- gam(A_RGR ~ PET + MAT + per_ifn + LMAo, data = p30a, method = "REML")
summary(gam2)


gam2 <- gam(A_RGR ~ PET + MAT + per_ifn + s(clay), data = p30a, method = "REML")
summary(gam2)


gam2 <- gam(A_RGR ~ PET + MAT + per_ifn + s(clay) + s(pH), data = p30a, method = "REML")
summary(gam2)

## B_BP_Mgha explained by AB3_Mgha and A_RGR


gam1 <- gam(B_BP_Mgha ~ s(AB3_Mgha) + s(A_RGR), data = p30a, method = "REML")
summary(gam1)
gam.check(gam1)

visreg2d(gam1, xvar = "AB3_Mgha", yvar = "A_RGR", scale = "response", col = colorRampPalette(brewer.pal(9,"Greens"))(20))

