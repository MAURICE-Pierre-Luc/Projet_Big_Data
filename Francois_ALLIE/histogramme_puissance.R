# library(readr)
# library(ggplot2)

data <- read_csv("./IRVE_clean.csv")

# Histogramme de la répartition des puissances nominales

# Pour nettoyer les puissances nominales, on ne garde que celles qui sont supérieures à 0 et inférieures à 500 kW

# On nettoie juste la colonne puissance_nominale 
# data_filtree <- subset(data, puissance_nominale > 0 & puissance_nominale <= 500)



ggplot(data, aes(x = puissance_nominale)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  
  # Le pas en abscisse 
  scale_x_continuous(breaks = seq(0, 400, by =50)) +
  
  # Le pas en ordonnée 
  scale_y_continuous(breaks = seq(0, 8300, by = 1000)) 

  

    

