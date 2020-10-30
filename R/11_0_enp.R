# Pablo Salazar 
# Natural protected parks 
# plot selection 
# 08/10/2020

# required files 
# T32_allo_sf from 6_o_PCMayores2_RGR


# Filtering DataMp_sf based on species and tree density

qs <- T32_allo_sf %>%
  filter(Sp.x %in% c("Quercus faginea", "Quercus ilex", "Quercus suber", "Olea europaea"), Tree_dens < 1500)

pal <- colorFactor(c("yellow", "red", "black", "green"), domain = c("Quercus faginea", "Quercus ilex", "Quercus suber", "Olea europaea"))

#Loading natural protected areas (Espacios naturales protegidos)


enp <- "http://www.juntadeandalucia.es/medioambiente/mapwms/REDIAM_Espacios_Naturales_Protegidos?request=GetCapabilities&service=WMS"

leaflet(qs) %>% 
  setView(lng = -6.287638, lat = 36.703039, zoom = 8) %>% 
  addCircleMarkers(data = qs, color = ~pal(Sp.x), label = ~as.factor(Sp.x), radius = 2) %>%
  addLegend("bottomright", pal = pal, values = ~as.factor(Sp.x),
            title = "Especie dominante",
            opacity = 1) %>%
  addTiles() %>%
  addWMSTiles(
    enp,
    layers = "eennpp",
    options = WMSTileOptions(format = "image/png", transparent = TRUE) 
  )


Ps <- T32_allo_sf %>%
  filter(Sp.x %in% c("Pinus halepensis","Pinus pinaster", "Pinus pinea"), Tree_dens < 1500)

pal <- colorFactor(c("red", "black", "green"), domain = c("Pinus halepensis","Pinus pinaster", "Pinus pinea"))

leaflet(Ps) %>% 
  setView(lng = -6.287638, lat = 36.703039, zoom = 8) %>% 
  addCircleMarkers(data = Ps, color = ~pal(Sp.x), label = ~as.factor(Sp.x), radius = 2) %>%
  addLegend("bottomright", pal = pal, values = ~as.factor(Sp.x),
            title = "Especie dominante",
            opacity = 1) %>%
  addTiles() %>%
  addWMSTiles(
    enp,
    layers = "eennpp",
    options = WMSTileOptions(format = "image/png", transparent = TRUE) 
  )

Phal <- T32_allo_sf %>%
  filter(Sp.x == "Pinus halepensis", Tree_dens < 1500)  %>%
  mutate(grad_BM = cut(AB3_Mgha, breaks = c(-Inf, 30, 60, 90, 120, Inf), labels = c("2", "4", "6", "8", "10")))

pal <- colorNumeric(palette = "Reds", domain = Phal$Arid_Dm)

leaflet(Phal) %>% 
  setView(lng = -6.287638, lat = 36.703039, zoom = 8) %>% 
  addCircleMarkers(data = Phal, color = ~pal(Arid_Dm), radius = ~grad_BM, fillOpacity = 1) %>%
  addTiles() %>%
  addWMSTiles(
    enp,
    layers = "eennpp",
    options = WMSTileOptions(format = "image/png", transparent = TRUE) 
  )


Ppina <- T32_allo_sf %>%
  filter(Sp.x == "Pinus pinaster", Tree_dens < 1500)  %>%
  mutate(grad_BM = cut(AB3_Mgha, breaks = c(-Inf, 30, 60, 90, 120, Inf), labels = c("2", "4", "6", "8", "10")))

pal <- colorNumeric(palette = "Reds", domain = Ppina$Arid_Dm)

leaflet(Ppina) %>% 
  setView(lng = -6.287638, lat = 36.703039, zoom = 8) %>% 
  addCircleMarkers(data = Ppina, color = ~pal(Arid_Dm), radius = ~grad_BM, fillOpacity = 1) %>%
  addTiles() %>%
  addWMSTiles(
    enp,
    layers = "eennpp",
    options = WMSTileOptions(format = "image/png", transparent = TRUE) 
  )

qfag <- T32_allo_sf %>%
  filter(Sp.x == "Quercus faginea", Tree_dens < 1500)  %>%
  mutate(grad_BM = cut(AB3_Mgha, breaks = c(-Inf, 30, 60, 90, 120, Inf), labels = c("2", "4", "6", "8", "10")))

pal <- colorNumeric(palette = "Reds", domain = qfag$Arid_Dm)

leaflet(qfag) %>% 
  setView(lng = -6.287638, lat = 36.703039, zoom = 8) %>% 
  addCircleMarkers(data = qfag, color = ~pal(Arid_Dm), radius = ~grad_BM, fillOpacity = 1) %>%
  addTiles() %>%
  addWMSTiles(
    enp,
    layers = "eennpp",
    options = WMSTileOptions(format = "image/png", transparent = TRUE) 
  )


qsub <- T32_allo_sf %>%
  filter(Sp.x == "Quercus suber", Tree_dens < 1500)  %>%
  mutate(grad_BM = cut(AB3_Mgha, breaks = c(-Inf, 30, 60, 90, 120, Inf), labels = c("2", "4", "6", "8", "10")))

pal <- colorNumeric(palette = "Reds", domain = qsub$Arid_Dm)

leaflet(qsub) %>% 
  setView(lng = -6.287638, lat = 36.703039, zoom = 8) %>% 
  addCircleMarkers(data = qsub, color = ~pal(Arid_Dm), radius = ~grad_BM, fillOpacity = 1) %>%
  addTiles() %>%
  addWMSTiles(
    enp,
    layers = "eennpp",
    options = WMSTileOptions(format = "image/png", transparent = TRUE) 
  )


qile <- T32_allo_sf %>%
  filter(Sp.x == "Quercus ilex", Tree_dens < 1500)  %>%
  mutate(grad_BM = cut(AB3_Mgha, breaks = c(-Inf, 30, 60, 90, 120, Inf), labels = c("2", "4", "6", "8", "10")))

pal <- colorNumeric(palette = "Reds", domain = qile$Arid_Dm)

leaflet(qile) %>% 
  setView(lng = -6.287638, lat = 36.703039, zoom = 8) %>% 
  addCircleMarkers(data = qile, color = ~pal(Arid_Dm), radius = ~grad_BM, fillOpacity = 1) %>%
  addTiles() %>%
  addWMSTiles(
    enp,
    layers = "eennpp",
    options = WMSTileOptions(format = "image/png", transparent = TRUE) 
  )


oeur <- T32_allo_sf %>%
  filter(Sp.x == "Olea europaea", Tree_dens < 1500)  %>%
  mutate(grad_BM = cut(AB3_Mgha, breaks = c(-Inf, 30, 60, 90, 120, Inf), labels = c("2", "4", "6", "8", "10")))

pal <- colorNumeric(palette = "Reds", domain = oeur$Arid_Dm)

leaflet(oeur) %>% 
  setView(lng = -6.287638, lat = 36.703039, zoom = 8) %>% 
  addCircleMarkers(data = oeur, color = ~pal(Arid_Dm), radius = ~grad_BM, fillOpacity = 1) %>%
  addTiles() %>%
  addWMSTiles(
    enp,
    layers = "eennpp",
    options = WMSTileOptions(format = "image/png", transparent = TRUE) 
  )
