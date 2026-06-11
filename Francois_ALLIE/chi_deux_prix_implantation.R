library(dplyr)
library(readr)

data <- read_csv("./IRVE_clean.csv")


donnees_visu <- data %>%
  mutate(implantation_station = case_when(
    implantation_station == "Parking privé à usage public" ~ "P.Privé(Public)",
    implantation_station == "Parking privé réservé à la clientèle" ~ "P.Privé(Client)",
    implantation_station == "Parking public" ~ "P.Public",
    
    # On garde les noms courts existants tels quels (ex: "Voirie")
    TRUE ~ implantation_station 
    ),

    # On enlève exactement le texte "€/kWh" (remplacé par du vide "")
    prix_num = gsub("€/kwh", "", tarification, ignore.case = TRUE),
    
    # L'argument ignore.case = TRUE permet d'effacer le texte qu'il soit en majuscule ou en minuscules
    # Car dans le CSV on a parfois "kwh" et parfois "kWh" 
    
    # On enlève les espaces vides restants et on convertit en numérique
    prix_num = as.numeric(trimws(prix_num))
  )

# On crée une nouvelle variable qualitative à partir de la variable quantitative du prix
# Car le test du chi-deux s'effectue avec deux variables qualitative, or le prix est au départ une variable quantitative

data_prix_qualitatif <- donnees_visu %>%
  mutate(tarification = case_when(
    prix_num == 0 ~ "Gratuit",
    prix_num > 0 ~ "Payant"
  ))

# Le test du Chi-2 devient possible
tableau_croise <- table(data_prix_qualitatif$implantation_station, data_prix_qualitatif$tarification)
chisq.test(tableau_croise)

# Le mosaicplot 
mosaicplot(tableau_croise, main = "Implantation vs Gratuité", shade = TRUE, las = 1, cex.axis = 0.9)

