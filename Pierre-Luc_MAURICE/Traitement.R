install.packages(c("tidyverse", "janitor", "lubridate", "naniar", "outliers", "stringi"))

source("./Pierre-Luc_MAURICE/F_Traitement.R")

.vsc.attach() #Pour pouvoir voir les variables sous vs code


df <- read.csv(file = "./Pierre-Luc_MAURICE/IRVE.csv", encoding="UTF-8")


# Exemple d'utilisation
colonnes_avec_vides(df)

#On regarde le % de remplissage de chaque colone
#taux <- (1 - colMeans(is.na(df))) * 100
#df_taux <- data.frame(
#  colonne = names(taux),
#  remplissage = paste0(round(taux, 1), " %"),
#  valeur = taux
#)
#df_taux[order(df_taux$valeur), c("colonne", "remplissage")]


#Nombre de lignes avant traitement
nlignes_avant <- nrow(df)

#On trie par date pour ne garder que les lignes changés le plsu récement si on a des doublons sur des id
df_dedup <- df[order(df$id_pdc_itinerance, df$date_maj, decreasing = TRUE), ]

#On supprime les lignes qui ont des doublons d'id_pdc qui est unique pour chaque chargeur
df_dedup <- df_dedup[!duplicated(df_dedup$id_pdc_itinerance), ]

#On supprime les lignes où l'id_pdc vaut non concerné
df_dedup <- df_dedup[df_dedup$id_pdc_itinerance != "Non concerné", ]


#On supprime les lignes où il nous manque des valeurs dans les colonnes où ces valeurs sont obligatoires
df_dedup <- supprimer_lignes_vides(df_dedup, "contact_operateur")
df_dedup <- supprimer_lignes_vides(df_dedup, "nom_enseigne")
df_dedup <- supprimer_lignes_vides(df_dedup, "id_station_itinerance")
df_dedup <- supprimer_lignes_vides(df_dedup, "nom_station")
df_dedup <- supprimer_lignes_vides(df_dedup, "implantation_station")
df_dedup <- supprimer_lignes_vides(df_dedup, "adresse_station")
df_dedup <- supprimer_lignes_vides(df_dedup, "coordonneesXY")
df_dedup <- supprimer_lignes_vides(df_dedup, "nbre_pdc")
df_dedup <- supprimer_lignes_vides(df_dedup, "id_pdc_itinerance")
df_dedup <- supprimer_lignes_vides(df_dedup, "puissance_nominale")
df_dedup <- supprimer_lignes_valeur(df_dedup, "puissance_nominale", 0)
df_dedup <- supprimer_lignes_vides(df_dedup, "prise_type_ef")
df_dedup <- supprimer_lignes_vides(df_dedup, "prise_type_2")
df_dedup <- supprimer_lignes_vides(df_dedup, "prise_type_combo_ccs")
df_dedup <- supprimer_lignes_vides(df_dedup, "prise_type_chademo")
df_dedup <- supprimer_lignes_vides(df_dedup, "prise_type_autre")
df_dedup <- supprimer_lignes_vides(df_dedup, "paiement_acte")
df_dedup <- supprimer_lignes_vides(df_dedup, "condition_acces")
df_dedup <- supprimer_lignes_vides(df_dedup, "reservation")
df_dedup <- supprimer_lignes_vides(df_dedup, "horaires")
df_dedup <- supprimer_lignes_vides(df_dedup, "accessibilite_pmr")
df_dedup <- supprimer_lignes_vides(df_dedup, "restriction_gabarit")
df_dedup <- supprimer_lignes_vides(df_dedup, "station_deux_roues")
df_dedup <- supprimer_lignes_vides(df_dedup, "date_maj")



#On normalise la colonne gratuit, comme on doit faire un comparaison avec ses éléments
df_dedup <- normaliser_booleen(df_dedup,"gratuit")

#Si on a pas de prix et que dans la colonne gratuit on a TRUE, alors on met le prix à 0€/kwh
df_dedup$tarification <- ifelse(
  (is.na(df_dedup$tarification) | trimws(df_dedup$tarification) == "") & 
  df_dedup$gratuit == TRUE & !is.na(df_dedup$gratuit),
  "0€/kWh",
  df_dedup$tarification
)

df_dedup <- supprimer_lignes_vides(df_dedup, "tarification")
df_dedup$tarification <- sapply(df_dedup$tarification, normalize_tarif)
df_dedup <- df_dedup[!is.na(df_dedup$tarification), ]


#Vérifie pour combien de lignes le id_station_itinerance et id_pdc_itinerance sont identique, inutile actuellement
#sum(df_dedup$id_pdc_itinerance == df_dedup$id_station_itinerance, na.rm = TRUE)

# On fait un traitement sur la colone horaire pour normaliser la syntaxe
df_dedup$horaires <- sapply(df_dedup$horaires, normaliser_horaires)

#On normalise la syntaxe des colonnes ayant True et False en valeurs
df_dedup <- normaliser_booleen(df_dedup,"prise_type_ef")
df_dedup <- normaliser_booleen(df_dedup,"prise_type_2")
df_dedup <- normaliser_booleen(df_dedup,"prise_type_combo_ccs")
df_dedup <- normaliser_booleen(df_dedup,"prise_type_chademo")
df_dedup <- normaliser_booleen(df_dedup,"prise_type_autre")
df_dedup <- normaliser_booleen(df_dedup,"paiement_acte")
df_dedup <- normaliser_booleen(df_dedup,"paiement_cb")
df_dedup <- normaliser_booleen(df_dedup,"paiement_autre")
df_dedup <- normaliser_booleen(df_dedup,"reservation")
df_dedup <- normaliser_booleen(df_dedup,"station_deux_roues")
df_dedup <- normaliser_booleen(df_dedup,"consolidated_is_lon_lat_correct")
df_dedup <- normaliser_booleen(df_dedup,"consolidated_is_code_insee_verified")
df_dedup <- normaliser_booleen(df_dedup,"consolidated_is_code_insee_modified")
df_dedup <- normaliser_booleen(df_dedup,"cable_t2_attache")

#On noramlise la sytaxe de la colone gabarit
df_dedup$restriction_gabarit <- sapply(df_dedup$restriction_gabarit, normaliser_gabarit)

df_dedup <- to_uppercase(df_dedup, "nom_operateur")
df_dedup <- to_uppercase(df_dedup, "nom_amenageur")
df_dedup <- to_uppercase(df_dedup, "nom_enseigne")
df_dedup <- to_lowercase(df_dedup, "nom_station")
df_dedup <- to_lowercase(df_dedup, "addresse_station")


for (j in seq_along(df_dedup)) {

  col <- as.character(df_dedup[[j]])

  col[col == "" | trimws(col) == ""] <- NA

  col[is.na(col)] <- "inconnu"

  df_dedup[[j]] <- col
}


nlignes_apres <- nrow(df_dedup)


cat("Lignes supprimées :", nlignes_avant - nlignes_apres, "\n")
cat("Lignes restantes :", nlignes_apres, "\n")

#Export en csv, on encode bien en UTF-8
write.csv(df_dedup, "IRVE_clean.csv", row.names=FALSE, fileEncoding = "UTF-8")
