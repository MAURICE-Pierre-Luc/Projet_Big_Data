library(readr)
library(dplyr)
library(leaflet)
library(leaflet.extras)

data_propre <- read_csv(file="./IRVE_clean.csv")

carte_cluster <- leaflet(data_propre) %>%
  addTiles() %>%
  
  # Ajout des marqueurs avec l'option  pour le clustering
  addMarkers(
    lng = ~consolidated_longitude, 
    lat = ~consolidated_latitude,
    clusterOptions = markerClusterOptions(), 
    popup = ~paste("Puissance :", puissance_nominale, "kW") 
  )

# Afficher la carte
carte_cluster
