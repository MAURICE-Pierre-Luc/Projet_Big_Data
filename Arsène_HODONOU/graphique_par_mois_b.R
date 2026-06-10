#AUTEUR : ARSENE HODONOU



# 1. Chargement des packages indispensables
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)

stations_annee <- read_delim(
  "C:/Users/ETUDIANT/Downloads/IRVE_clean.csv",
  delim = ",", 
  show_col_types = FALSE
)

# 3. Préparation du résumé PAR MOIS
stations_mois <- stations_annee %>% 
  mutate(date_propre = as.Date(date_mise_en_service)) %>% 
  # month(..., label = TRUE, abbr = FALSE) extrait "janvier", "février", etc.
  mutate(mois = month(date_propre, label = TRUE, abbr = FALSE)) %>% 
  filter(!is.na(mois)) %>% 
  count(mois)

# 4. Le graphique par mois
ggplot(stations_mois, aes(x = mois, y = n)) +
  geom_col(fill = "steelblue", color = "white") +
  labs(
    title = "Saisonnalité des installations de points de recharge",
    subtitle = "Répartition globale des mises en service par mois",
    x = "Mois de l'année",
    y = "Nombre de points de recharge"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )

