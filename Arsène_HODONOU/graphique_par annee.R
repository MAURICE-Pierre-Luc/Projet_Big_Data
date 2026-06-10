#AUTEUR : ARSENE HODONOU

# 1. Chargement des packages indispensables
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)

# 2. Importation
stations_annee <- read_delim(
  "C:/Users/ETUDIANT/Downloads/IRVE_clean.csv" ,
  delim = ",", 
  show_col_types = FALSE
)

# 3. résumé
stations_resume <- stations_annee %>% 
  mutate(date_propre = as.Date(date_mise_en_service)) %>% 
  mutate(annee = year(date_propre)) %>% 
  filter(!is.na(annee)) %>% 
  count(annee)

# 4. graphique
ggplot(stations_resume, aes(x = annee, y = n)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 2.5) +
  xlim(2018, 2026) +
  labs(
    title = "Évolution du nombre de stations mises en service par année",
    x = "Année",
    y = "Nombre de stations"
  ) +
  theme_minimal()
