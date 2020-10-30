
# Pablo Salazar Z
# Universidad de Córdoba
# 08/04/2020
# Packages for "For Change" Data analysis

## Libraries -----------

### for data loading 
library(readr) 
library(readxl)
library(data.table)
library(xlsx)
library(rJava)
library(devtools)

## for data processing
library(stringr)
library(tidyr)
library(plyr)
library(dplyr)
library(qwraps2) # to get mean and sd in the same cell
library(rlist) # to cbind several elements of a list

## To load and process spatial data 
library(sf)
library(sp)
library(raster)
library(rgdal)
library(lwgeom)
library(envirem)
library(leaflet)
# To create graphical display of spatial data
library(mapview)
library(tmap)

library(ggplot2)
library(ggthemes)
library(ggpubr)
library(visreg)
library(grid)
library(RColorBrewer)
#install_github("vqv/ggbiplot")
library(ggbiplot)

# For data analysis

library(nlme)
library(MuMIn) # to calculate pseudo R2 in glm
#library(rsq) # we could use this one to calculate R2 in glm, but doesnt work on nested analysis
library(factoextra) # for get_pca_ind
library(corrplot)
library(Hmisc)
# library(mgcv)
# library(lavaan) ## To run the sem function
# library(semPlot) ## To do the SEM plot
# library(semTools)
#install_github("jslefche/piecewiseSEM")
library(piecewiseSEM) # for Piecewise SEM
library(FD) # to calculate functional diversity and CWM
