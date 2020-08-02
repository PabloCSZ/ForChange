
# Pablo Salazar Z
# Universidad de Córdoba
#15/07/2020
# Using IFN data, functional traits, soil data, and climate data to model biomass production
# Spanish National Forestry inventory
# Structural equational model 

# To run 
# "p30a" from "7_1_plot30.R" is required
# Only sem1. works so far

# Data transformation 
p30c <- p30a %>%
  mutate(AB3_kgT_t = AB3_kgT/10, B_BP_Mgha_t = B_BP_Mgha*100, A_RGR_t = A_RGR*10000000, Ht_ifn_t = Ht_ifn*100) %>%
  na.omit()

## All SEMs will be saved even if it is just a mess up idea :)

sem.1<-"
# Regressions
B_BP_Mgha_t ~ AB3_Mgha + A_RGR_t + MAT
AB3_Mgha ~ AB3_kgT_t + Tree_dens
A_RGR_t ~ MAT + Ht_ifn_t + PET
Tree_dens ~ Ht_ifn_t

#error covariance
Tree_dens ~~ A_RGR_t
AB3_kgT_t ~~ Ht_ifn_t
"
sem.1.fit<-sem(sem.1,data=p30c, fixed.x=FALSE, conditional.x = FALSE)
# with std.ov = TRUE all observed variables are standardized before entering the analysis
# conditional.x = FALSE he exogenous ‘x’ variables are modeled jointly with the other variables,
# and the model-implied statistics refect both sets of variables
summary(sem.1.fit, rsq=T,standardized=T)
mi1<-modindices(sem.1.fit) 
print(mi1[mi1$mi>3.0,]) 

ly<-matrix(c(0,0, -0.75,0.5, 0.5,0.5, -0.5,1, 0,1, -1,1, 0.5,1.5, 0.75,1),ncol=2, byrow=TRUE)
lbls<-c("Biomass\n Production","Plot\n Biomass","RGR","Tree\n density","MAT","Tree \n Biomass","Tree \n height","PET")

tiff("products/FigSem_1.tiff", width=6, height=4.5, res= 300, units="in", compression ="lzw")
par(mar=c(4.1,4.4,0.3,0.3))
semPaths(sem.1.fit,what="std", posCol=c("blue","red"), layout = ly,
         sizeMan=10,sizeMan2=6,label.cex = 1, edge.label.cex=1, edge.label.position=0.30, fade=FALSE, nodeLabels=lbls, residuals=FALSE)
text(0.9,-1,labels="Chi-square = 18.3 \n df = 16 \n p = 0.31", cex = 1)
dev.off()

sem.2<-"
# Regressions
B_BP_Mgha_t ~ AB3_Mgha + A_RGR_t 
AB3_Mgha ~ AB3_kgT_t + Tree_dens
A_RGR_t ~ MAT + per_ifn + PET + LMAo 
LMAo ~ PET + MAT
#error covariance

B_BP_Mgha_t ~~ AB3_kgT_t
AB3_kgT_t ~~ per_ifn
Tree_dens ~~ A_RGR_t
"
sem.2.fit<-sem(sem.2, data=p30c, fixed.x=FALSE, conditional.x = FALSE)
# with std.ov = TRUE all observed variables are standardized before entering the analysis
# conditional.x = FALSE he exogenous ‘x’ variables are modeled jointly with the other variables,
# and the model-implied statistics refect both sets of variables
summary(sem.2.fit, rsq=T,standardized=T)
mi2<-modindices(sem.2.fit) 
print(mi2[mi2$mi>3.0,]) 



sem.3<-"
# Regressions
B_BP_Mgha_t ~ AB3_kgT_t + Tree_dens +  MAT + per_ifn + PET
"
sem.3.fit <- sem(sem.3, data=p30c, fixed.x=FALSE)
varTable(sem.1.fit) ## We notice here everything doesnt looks ok

summary(sem.3.fit, rsq=T,standardized=T) ## and here we can see nothing
mi3<-modindices(sem.3.fit) 
print(mi3[mi3$mi>3.0,]) 


sem.4<-"
# Regressions
B_BP_Mgha_t ~ AB3_Mgha + A_RGR + AB3_kgT + MAT
AB3_Mgha ~ AB3_kgT_t + Tree_dens
A_RGR ~ MAT + per_ifn + PET
"

sem.4.fit <- sem(sem.4, data=p30c, fixed.x=FALSE)
varTable(sem.4.fit) ## We notice here everything doesnt looks ok

sem.5<-"
# Regressions
B_BP_Mgha_t ~ AB3_Mgha + A_RGR
AB3_Mgha ~ A_RGR
"

sem.5.fit <- sem(sem.5, data=p30c, fixed.x=FALSE)
varTable(sem.5.fit) ## We notice here everything doesnt looks ok
inspectSampleCov(sem.5,data=p30c)
summary(p30c)
