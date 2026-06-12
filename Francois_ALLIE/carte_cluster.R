library(readr)
library(dplyr)
library(leaflet)
library(leaflet.extras)

data <- read_csv("./IRVE_clean.csv")

carte_cluster <- leaflet(data) %>%  # L'opérateur "%>%" de R provient de la librairie dplyr et permet simplement de passer le résultat à sa gauche comme argument de la fonction à sa droite
  addTiles() %>%                    # Ce qui permet de gagner des lignes 
  
  # Ajout des marqueurs avec l'option  pour le clustering
  addMarkers(
    lng = ~consolidated_longitude, 
    lat = ~consolidated_latitude,
    clusterOptions = markerClusterOptions(), 
    popup = ~paste("Puissance :", puissance_nominale, "kW") 
  )

# Afficher la carte
carte_cluster
