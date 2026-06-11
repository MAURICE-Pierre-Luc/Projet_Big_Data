library(dplyr)
library(readr)
library(ggplot2)

data <- read_csv("./IRVE_clean.csv")

donnees_visu <- data %>%
  mutate(
    # On enlève exactement le texte "€/kWh" (remplacé par du vide "")
    prix_num = gsub("€/kwh", "", tarification, ignore.case = TRUE),
    
    # L'argument ignore.case = TRUE permet d'effacer le texte qu'il soit en majuscule ou en minuscules
    # Car dans le CSV on a "kwh" et parfois "kWh"
    
    # On enlève les espaces vides restants et on convertit en numérique
    prix_num = as.numeric(trimws(prix_num))
  )

# Calcul du coefficient de corrélation de Pearson

correlation_prix_puissance <- cor(donnees_visu$puissance_nominale, donnees_visu$prix_num, use = "complete.obs")

print(correlation)

# Nuage de points pour vérifier la corrélation 

ggplot(donnees_visu, aes(x = puissance_nominale, y = prix_num)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  
  # Ajout d'une ligne de tendance (Régression linéaire simple)
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  
  theme_minimal() +
  labs(title = "Relation entre la Puissance Nominale et la tarification",
       x = "Puissance Nominale (kW)",
       y = "Prix de la recharge (€/kWh)")
