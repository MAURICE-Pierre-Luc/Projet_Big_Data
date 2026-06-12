library(ggplot2)
library(readr)

data <- read_csv("./IRVE_clean.csv")

# On regarde si le nombre de points de charge est corrélé au type d'implantation

# ggplot de R fonctionne par "couches", on ajoute d'abord les axes (1re couche), puis le boxplot (2e couche) avec "+" etc...
ggplot(data, aes(x = implantation_station, y = nbre_pdc)) +
  # Création du boxplot
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.color = "red", outlier.alpha = 0.5) +
  
  coord_cartesian(ylim = c(0, 50)) + 
  
  theme_minimal() +
  
  # Inclinaison des étiquettes de l'axe X à 45 degrés pour qu'elles ne se chevauchent pas
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  
  labs(title = "Nombre de points de charge selon le type d'implantation",
       x = "Type d'implantation",
       y = "Nombre de points de charge (PDC)")

# Analyse de la variance (ANOVA)

modele_anova <- aov(nbre_pdc ~ implantation_station, data)
    summary(modele_anova)