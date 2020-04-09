
# Pablo Salazar Z
# Universidad de Córdoba
# 08/04/2020
# Spanish National forest inventory
# Donwloaded from "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_bbdd_descargas.htm.aspx" in .amd format
# converted to excel file in Excel 32 bits
# Only Andalucia province are included

## Function to load multiple sheets ----------------
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

## Data loading ------------

L <- list.files(path= "data/ifn_data", pattern= ".xlsx", full.names = TRUE)

Cdb <- lapply(L, function(x) read_excel_allsheets(x))

names(Cdb) <- c("Almeria", "Cadiz", "Cordoba",  "Granada",
                "Huelva", "Jaen", "Malaga", "Sevilla")

## splitting and joining the data in tables ---------------

## Joining all the PCDatosMap into DataMp (not the last version file)-------
DataMp <- lapply(Cdb, function(x) rbind(x[1]))
DataMp <- do.call("rbind", do.call("rbind",DataMp))

## Joining all the PCEspMapa into SpMp ----------------
SpMp <- lapply(Cdb, function(x) rbind(x[2]))
SpMp <- do.call("rbind", SpMp) ## from large to not-so-large list
a<-lapply(SpMp, names) ## All the variables doesnt have the same name
a <- rbindlist(lapply(a, function(x) data.table(t(x))),fill=TRUE)
a <- as.data.frame(t(a)) ## So I need this to check the names in the list
SpMp <- lapply(SpMp, function(x) {names(x)[names(x)=='Ocupaci?n'] <- 'Ocupa'; x}) ## replace wrong names
names(SpMp) <- c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla")
SpMp <- dplyr::bind_rows(SpMp, .id = "Provincia")

## Joining all the PCEspParc into SpPlot ------------
SpPlot <- lapply(Cdb, function(x) rbind(x[3]))
SpPlot <- do.call("rbind", SpPlot)
names(SpPlot) <- c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla")
SpPlot <- do.call("rbind", SpPlot)
SpPlot <- cbind(rownames(SpPlot),SpPlot)
colnames(SpPlot)[1] <- "Provincia"
q <- str_extract(SpPlot$Provincia, "[:alpha:]+") # Provincia has number for some reason so I am taking them out
SpPlot$Provincia <- q

## Joining all the PCMatorral into Shrubs ------------
Shrubs <- lapply(Cdb, function(x) rbind(x[4]))
Shrubs <- do.call("rbind", Shrubs)
names(Shrubs) <- c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla")
Shrubs <- dplyr::bind_rows(Shrubs, .id = "Provincia")

## Joining all the PCMayores into TreesI3 ---------------
TreesI3 <- lapply(Cdb, function(x) rbind(x[5]))
TreesI3 <- do.call("rbind",TreesI3)
a <- lapply(TreesI3, names) ## All the variables doesnt have the same name
a <- rbindlist(lapply(a, function(x) data.table(t(x))),fill=TRUE)
a <- as.data.frame(t(a)) ## So I need this to check the names of all the list
TreesI3 <- lapply(TreesI3, function(x) {names(x)[names(x)=='Distanci'] <- 'Distancia'; x}) ## replace wrong names
TreesI3 <- lapply(TreesI3, function(x) {x <- x[!names(x)=='DRed']; x}) ## replace wrong names
names(TreesI3) <- c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla")
TreesI3 <- dplyr::bind_rows(TreesI3, .id = "Provincia")

# Joining all the PCMayores2 into TreesI2 ------------------
TreesI2 <- lapply(Cdb, function(x) rbind(x[6]))
TreesI2 <- do.call("rbind", TreesI2)
TreesI2 <- lapply(TreesI2, function(x) {names(x)[names(x)=='Distanci'] <- 'Distancia'; x}) ## replace wrong names
names(TreesI2) <- c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla")
TreesI2 <- dplyr::bind_rows(TreesI2, .id = "Provincia")

# Joining all the PCParcelas into Plots --------------------
Plots <- lapply(Cdb, function(x) rbind(x[8]))
Plots <- do.call("rbind", Plots)
a <- lapply(Plots, names) ## All the variables doesnt have the same name
a <- rbindlist(lapply(a, function(x) data.table(t(x))),fill=TRUE)
a <- as.data.frame(t(a)) ## So I need this to check the names of all the list
# Time variables have different formats between providences so I am taking them down
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='FechaPh']; x}) ## Delete variables
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='HoraPh']; x}) ## Delete variables
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='FechaIni']; x}) ## Delete variables
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='HoraIni']; x}) ## Delete variables
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='HoraFin']; x}) ## Delete variables
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='FechaFin']; x}) ## Delete variables
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='MejVue2']; x}) ## Delete variables
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='Orienta2']; x}) ## Delete variables
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='MaxPend2']; x}) ## Delete variables
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='Tiempo']; x}) ## Delete variables
Plots <- lapply(Plots, function(x) {x <- x[!names(x)=='Pasada2']; x}) ## Delete variables
names(Plots) <- c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla")
Plots <- dplyr::bind_rows(Plots, .id = "Provincia2")
Plots <- Plots[,-2]
colnames(Plots)[1] <- "Provincia"

# Joining all the PCRegenera into Reg -----------------
Reg <- lapply(Cdb, function(x) rbind(x[9]))
Reg <- do.call("rbind", Reg)
names(Reg) <- c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla")
Reg <- do.call("rbind", Reg)
Reg <- cbind(rownames(Reg),Reg)
colnames(Reg)[1] <- "Provincia"
q <- str_extract(Reg$Provincia, "[:alpha:]+")
Reg$Provincia <- q

# Correcting DataMp coordinates (create DataMp2) -------------------
# Coordinates in Plots are more precise than coordinates in DataMp.
# Replace DataMp for Plots coordinates unless Plots is NA.
# Both sets of coordinates are kept and more issues will be fixed
# during the transformation to spatial data

colnames(Plots)[5] <- "CoorXp"
colnames(Plots)[6] <- "CoorYp"
Plots2 <- Plots %>%
  filter(CoorXp > 15)
Plots2 <- Plots2[!duplicated(Plots2[,1:2]),]

DataMp[,1] <- as.factor(DataMp[,1])
levels(DataMp$Provincia) <- c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla")
DataMp2 <- merge(DataMp, Plots2[,c(1,2,5,6)], by = c("Provincia","Estadillo"), all.x = TRUE)
DataMp2$CoorXp <- ifelse(is.na(DataMp2$CoorXp), paste(DataMp2$CoorX), paste(DataMp2$CoorXp))
DataMp2$CoorYp <- ifelse(is.na(DataMp2$CoorYp), paste(DataMp2$CoorY), paste(DataMp2$CoorYp))
