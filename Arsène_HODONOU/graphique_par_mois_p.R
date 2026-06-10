#AUTEUR : ARSENE HODONOU



# 1. Chargement des packages indispensables
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)

# 2. importation 
stations_annee <- read_delim(
  "C:/Users/ETUDIANT/Downloads/IRVE_clean.csv",
  delim = ",", 
  show_col_types = FALSE
)

# 3. résumé PAR MOIS
stations_mois <- stations_annee %>% 
  mutate(date_propre = as.Date(date_mise_en_service)) %>% 
  mutate(mois = month(date_propre, label = TRUE, abbr = FALSE)) %>% 
  filter(!is.na(mois)) %>% 
  count(mois)

# 4. Le graphique EN POINTS et LIGNES (comme le premier)
ggplot(stations_mois, aes(x = mois, y = n, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 2.5) +
  labs(
    title = "Saisonnalité des installations de points de recharge",
    subtitle = "Évolution des mises en service au fil des mois (cumul global)",
    x = "Mois de l'année",
    y = "Nombre de points de recharge"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )
