library(readr)
library(dplyr)
library(leaflet)
library(leaflet.extras)

data <- read_csv("./IRVE_clean.csv")


carte_chaleur_pdc <- leaflet(data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(
    lng = ~consolidated_longitude, 
    lat = ~consolidated_latitude,
    intensity = ~nbre_pdc, 
    gradient = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"), # Les couleurs de la heatmap

    radius = 25,
    blur = 30,
    max = 50,
    minOpacity = 0.5
  ) %>%
  
  # Ajout de la légende interactive en bas à droite
  addLegend(
    position = "bottomright",
    
    # On reprend 3 couleurs clés 
    colors = c("#FFFFB2", "#FD8D3C", "#BD0026"),
    
    labels = c("Faible", 
               "Moyenne", 
               "Forte"),
    
    title = "Densité de Points<br>de Charge (PDC)", 
    opacity = 0.9
  )

carte_chaleur_pdc
