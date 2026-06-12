library(dplyr)
library(readr)

data <- read_csv("./IRVE_clean.csv")

donnees_visu <- data %>%
  mutate(implantation_station = case_when(
    # On modifie le nom des colonnes pour qu'ils soient plus court et plus lisible pour le mosaicplot 
    implantation_station == "Parking privé à usage public" ~ "P.Privé(Public)",
    implantation_station == "Parking privé réservé à la clientèle" ~ "P.Privé(Client)",
    implantation_station == "Parking public" ~ "P.Public",
    
    # On garde le nom "Voirie" car il est assez court
    TRUE ~ implantation_station 
    ),

    # Création de la variable qualitative de tarification 
    # Car le test du chi-deux s'effectue avec deux variables qualitative, or le prix est au départ une variable quantitative
    tarification_qualitative = case_when(
      tarification_eur_kWh == 0 ~ "Gratuit",
      tarification_eur_kWh > 0 ~ "Payant"
    )

  )


# Le test du Chi-2 devient possible
tableau_croise <- table(donnees_visu$implantation_station, donnees_visu$tarification_qualitative)
chisq.test(tableau_croise)

# Le mosaicplot 
mosaicplot(tableau_croise, main = "Implantation vs Gratuité", shade = TRUE, las = 1, cex.axis = 0.9)

