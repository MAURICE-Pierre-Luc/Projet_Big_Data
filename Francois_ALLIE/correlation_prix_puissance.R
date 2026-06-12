library(dplyr)
library(readr)
library(ggplot2)

data <- read_csv("./IRVE_clean.csv")


# Calcul du coefficient de corrélation de Pearson

correlation_prix_puissance <- cor(data$puissance_nominale, data$tarification_eur_kWh , use = "complete.obs")

print(correlation)

# Nuage de points pour vérifier la corrélation 

ggplot(data, aes(x = puissance_nominale, y = prix_num)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  
  # Ajout d'une ligne de tendance (Régression linéaire simple)
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  
  theme_minimal() +
  labs(title = "Relation entre la Puissance Nominale et la tarification",
       x = "Puissance Nominale (kW)",
       y = "Prix de la recharge (€/kWh)")
