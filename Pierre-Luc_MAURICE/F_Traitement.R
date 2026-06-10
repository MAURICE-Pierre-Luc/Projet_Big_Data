library(tidyverse)
library(janitor)
library(lubridate)
library(naniar)
library(stringi)

normaliser_booleen <- function(df, colonne) {
    valeur <- tolower(trimws(df[[colonne]]))
    df[[colonne]] <- ifelse(valeur %in% c("true", "vrai", "oui", "1", "o"), TRUE,
                    ifelse(valeur %in% c("false", "faux", "non", "0", "n"), FALSE,
                    NA))
    return(df)
}

normaliser_gabarit <- function(h) {
    h_lower <- tolower(trimws(h))
    
    # Pas de restriction
    pas_de_restriction <- c(
        "aucune", "aucun", "aucune restriction", "aucune restriction connue",
        "pas de restriction", "pas de restriciton", "pas de restriction de hauteur",
        "pas de hauteur maximale", "non concerné", "non", "false", "ras", "xx",
        "99999", "restriction de gabarit non précisée", "restrictions gabarit inconnues",
        "restriction gabarit inconnue", "aucune restriciton", "aucune restriction",
        "néant", "neant", "aucune (hors poids lourd)", "pas de restrictions",
        "pas de restrictions.", "acune", "ps de restriction", "no restriction", "sans restriction",
        "sans", "pas de restrictions"
    )
    
    # Inconnu
    inconnu <- c(
        "inconnu", "inconnue", "non renseigné", "non précisé",
        "restriction de gabarit non précisée", "restriction de gabarit inconnue",
        "hauteur maximale (parking souterrain)", "non communiqué", "/", "0", "oui",
        "Restriction de gabarit non pr\u008ecis\u008ee", "nc", "non spécifié", "no information",""
    )
    
    # Véhicules légers uniquement
    vl <- c("vl uniquement", "vl uniquewent", "que du poids léger", "place vl", "parking réservé vl", "hors pl",
        "vl et vul"
    )
    
    if (h_lower %in% pas_de_restriction) return("aucune")
    if (h_lower %in% inconnu) return("inconnu")
    if (h_lower %in% vl) return("vehicules legers uniquement")
    
    # Valeurs numériques aberrantes (grands nombres comme 100011, 50042629...)
    if (grepl("^\\d{5,}$", h_lower)) return("inconnu")
    
    # Extraire un nombre de hauteur
    h_clean <- gsub(",", ".", h_lower)
    h_clean <- gsub("mètres|metres|mètre|metre", "m", h_clean)
    h_clean <- gsub("m(\\d)", "m \\1", h_clean)
    
    match <- regmatches(h_clean, regexpr("\\d+\\.?\\d*", h_clean))
    
    if (length(match) > 0) {
        valeur <- as.numeric(match)
        if (!is.na(valeur) && valeur >= 1 && valeur <= 10) {
        return(paste0("hauteur ", valeur, "m"))
        }
    }
    
    return(h)
}



normaliser_horaires <- function(h) {
    # 1. Supprimer les espaces en début et fin
    h <- trimws(h)
    
    # 2. Remplacer les tirets longs par des tirets courts
    h <- gsub("–", "-", h)
    
    # 3. Tout en minuscules
    h <- tolower(h)
    
    # 4. Traduire les jours français
    h <- gsub("lundi|lun|lu", "mo", h)
    h <- gsub("mardi|mar", "tu", h)
    h <- gsub("mercredi|mer|me", "we", h)
    h <- gsub("jeudi|jeu", "th", h)
    h <- gsub("vendredi|ven|ve", "fr", h)
    h <- gsub("samedi|sam|sa", "sa", h)
    h <- gsub("dimanche|dim|di", "su", h)
    
    # 5. Uniformiser les abréviations anglaises
    h <- gsub("monday", "mo", h)
    h <- gsub("tuesday|tue", "tu", h)
    h <- gsub("wednesday", "we", h)
    h <- gsub("thursday", "th", h)
    h <- gsub("friday|fri", "fr", h)
    h <- gsub("saturday|sat", "sa", h)
    h <- gsub("sunday|sun", "su", h)
    
    # 6. Supprimer les ":" parasites après les jours
    h <- gsub("(mo|tu|we|th|fr|sa|su):", "\\1", h)
    
    # 7. Remplacer les espaces entre deux jours par un tiret
    h <- gsub("(mo|tu|we|th|fr|sa|su) (mo|tu|we|th|fr|sa|su)", "\\1-\\2", h)
    
    # 8. Remplacer les doubles espaces par un espace simple
    h <- gsub("  +", " ", h)
    
    # 9. Supprimer les espaces autour des virgules et tirets
    h <- gsub("\\s*,\\s*", ",", h)
    h <- gsub("\\s*-\\s*", "-", h)
    
    # 10. Convertir 24/7 et les variantes 7 jours 00:00-23:59 en format standard
    jours <- c("mo", "tu", "we", "th", "fr", "sa", "su")
    tous_les_jours <- all(sapply(jours, function(j) grepl(j, h)))
    if (grepl("^24/7$", h) || (tous_les_jours && grepl("00:00-24:00", h))) {
        h <- "mo-su 00:00-23:59"
    }
    
    return(h)
}

supprimer_lignes_vides <- function(df, colonne) {

    nlignes_avant <- nrow(df)

    df <- df[!is.na(df[[colonne]]) & trimws(df[[colonne]]) != "", ]  
        
    nlignes_apres <- nrow(df)
    
    #For debug purposes only
    #cat("Lignes supprimées :", nlignes_avant - nlignes_apres, "\n")
    #cat("Lignes restantes :", nlignes_apres, "\n")

    return(df)
}

supprimer_lignes_valeur <- function(df, colonne, valeur) {

    nlignes_avant <- nrow(df)

    df <- df[is.na(df[[colonne]]) | trimws(df[[colonne]]) != valeur,]  
    nlignes_apres <- nrow(df)

    #For debug purposes only
    #cat("Lignes supprimées :", nlignes_avant - nlignes_apres, "\n")
    #cat("Lignes restantes :", nlignes_apres, "\n")

    return(df)
}

nb_val_unique <- function(df, colonne, list){
    cat("Valeurs uniques",colonne,":", length(unique(df[[colonne]])), "\n")
    if(list){
        sort((unique(df[[colonne]]))) |> head(10)
    }
}

convert_cts <- function(x, unit) {
  pattern <- paste0("(\\d+\\.?\\d*)\\s*cts\\s*/\\s*", unit)
  while (grepl(pattern, x, ignore.case = TRUE, perl = TRUE)) {
    m <- regmatches(x, regexpr(pattern, x, ignore.case = TRUE, perl = TRUE))
    if (length(m) == 0) break
    val <- as.numeric(sub(pattern, "\\1", m, ignore.case = TRUE, perl = TRUE))
    repl <- paste0(round(val / 100, 5), "€/", unit)
    x <- sub(pattern, repl, x, ignore.case = TRUE, perl = TRUE)
  }
  x
}

normalize_tarif <- function(x) {

  # Trim + nettoyage espaces/tabs
  x <- trimws(x)                          # enlève espaces début/fin
  x <- gsub("[\t]+", " ", x)              # remplace tabulations par espaces
  x <- gsub(" {2,}", " ", x)              # réduit multiples espaces à un seul

  # Lowercase
  x <- tolower(x)                         # uniformise en minuscules

  # NA stricts - valeurs totalement inutilisables
  na_exact <- c(
    "-", "/", "false", "true", "fixe", "payant", "au kwh", "nc",
    "non communiqué", "non concerné", "inconnu",
    "0.50? 2.5 connection", "050 kwh",
    "voir acceuil bat 1", "voir facturation interne", "voir tarif gireve",
    "par défaut :  prix de départ 0.0€, "
  )

  # Patterns considérés comme NA
  na_patterns <- c(
    "^https?://",                         # URLs
    "^par d",                             # texte tronqué
    "^les tarifs de recharge peuvent varier",
    "^tarif li",
    "^tarification au kwh plus frais",
    "^6ct from",
    "kwhva",
    "0000000000"
  )

  # Suppression directe si match exact
  if (x %in% na_exact) return(NA_character_)

  # Suppression si match NA
  for (p in na_patterns) if (grepl(p, x, perl = TRUE)) return(NA_character_)

  # Cas gratuit
  gratuit_exact <- c("0€/kwh", "gratuit", "0")  # valeurs exactes gratuites

  gratuit_patterns <- c(
    "^énergie gratuite$",
    "^0 pour utilisateur",
    "^la recharge est gratuite",
    "^gratuit pour"
  )

  # Conversion directe en format standard gratuit
  if (x %in% gratuit_exact) return("0€/kwh")

  # Détection gratuit
  for (p in gratuit_patterns) if (grepl(p, x, perl = TRUE)) return("0€/kwh")

  # Corrections d'encodage (€ mal encodé)
  x <- gsub("â‚¬", "€", x, fixed = TRUE)  # bug UTF-8 -> €
  x <- gsub("ū",   "€", x, fixed = TRUE)  # autre encodage cassé
  x <- gsub("\\?", "€", x)                # "?" suspect -> €

  # Normalisation virgules décimales
  x <- gsub("(\\d),(\\d)", "\\1.\\2", x, perl = TRUE)  # 0,42 -> 0.42
  x <- gsub("(\\d),(\\d)", "\\1.\\2", x, perl = TRUE)  # double passe sécurité

  # Nettoyage formats collés €/kWh
  x <- gsub("(\\d)€kwh(ttc|ht)", "\\1€/kwh", x, perl = TRUE)  # 0.42€kwhht
  x <- gsub("(\\d)€kwh\\b", "\\1€/kwh", x, perl = TRUE)        # 0€kwh
  x <- gsub("(\\d\\.\\d+)kwh", "\\1€/kwh", x, perl = TRUE)     # 0.42kwh

  # Normalisation unités énergie
  x <- gsub("\\bk\\s*w\\s*h\\b", "kwh", x, perl = TRUE)  # k w h -> kwh
  x <- gsub("/\\s*kwh\\b", "/kwh", x, perl = TRUE)       # nettoyage espaces
  x <- gsub("\\bkwh\\b", "kwh", x, perl = TRUE)          # uniformisation
  x <- gsub("/kw\\b", "/kwh", x, perl = TRUE)            # kw -> kwh
  x <- gsub("kw/h", "kwh", x, perl = TRUE)                # kw/h -> kwh

  # Formats texte -> format standard €/kWh
  x <- gsub("(\\d+\\.?\\d*)\\s*€\\s*le\\s*kwh", "\\1€/kwh", x, perl = TRUE)
  x <- gsub("(\\d+\\.?\\d*)\\s*€\\s*par\\s*kwh", "\\1€/kwh", x, perl = TRUE)

  # Conversion HT -> TTC (x1.055)
  ht_pattern <- "(\\d+\\.?\\d*)€/kwh\\s*ht"

  # Boucle pour gérer plusieurs occurrences HT
  while (grepl(ht_pattern, x, perl = TRUE)) {

    m   <- regmatches(x, regexpr(ht_pattern, x, perl = TRUE))  # extrait match
    val <- as.numeric(sub(ht_pattern, "\\1", m, perl = TRUE))  # valeur numérique

    repl <- paste0(round(val * 1.055, 4), "€/kwh")  # conversion HT -> TTC

    x <- sub(ht_pattern, repl, x, perl = TRUE)  # remplacement
  }

  # Suppression mentions ht / ttc restantes
  x <- gsub("\\s*ttc\\b", "", x, perl = TRUE)
  x <- gsub("\\s*ht\\b",  "", x, perl = TRUE)

  # Conversion centimes -> euros (fonction externe)
  x <- convert_cts(x, "kwh")  # ex: 42 cts/kWh -> 0.42€/kWh
  x <- convert_cts(x, "min")  # idem pour minutes

  # Extraction de tous les prix €/kWh
  prix_pattern <- "(\\d+\\.?\\d*)€/kwh"

  # Récupération des matches
  matches <- regmatches(x, gregexpr(prix_pattern, x, perl = TRUE))[[1]]

  # Si aucun prix -> NA
  if (length(matches) == 0) return(NA_character_)

  # Extraction valeurs numériques
  #cat(matches)
  valeurs <- as.numeric(sub(prix_pattern, "\\1", matches, perl = TRUE))

  # Filtrage des outliers (prix incohérents)
  valeurs <- valeurs[valeurs <= 5]

  # Si tout filtré -> NA
  if (length(valeurs) == 0) return(NA_character_)

  # Moyenne des prix restants
  moyenne <- round(mean(valeurs), 4)

  # Cas particulier gratuit
  if (moyenne == 0) return("0€/kwh")

  # Retour final formaté
  return(paste0(moyenne, "€/kWh"))
}

to_uppercase <- function(df, column) {
  
  # Récupération de la colonne
  col <- as.character(df[[column]])
  
  # Remplacer NA et chaînes vides (ou espaces) par "INCONNU"
  col[is.na(col) | trimws(col) == ""] <- "INCONNU"
  
  # Passage en majuscules
  df[[column]] <- toupper(col)
  
  return(df)
}


to_lowercase <- function(df, column) {
  
  # Récupération de la colonne
  col <- as.character(df[[column]])
  
  # Remplacer NA et chaînes vides (ou espaces) par "INCONNU"
  col[is.na(col) | trimws(col) == ""] <- "INCONNU"
  
  # Passage en majuscules
  df[[column]] <- tolower(col)
  
  return(df)
}

colonnes_avec_vides <- function(df) {
  
  nb_colonnes_vides <- 0
  
  for (col in names(df)) {
    
    if (any(is.na(df[[col]])) || any(trimws(df[[col]]) == "")) {
      nb_colonnes_vides <- nb_colonnes_vides + 1
    }
  }
  
  return(nb_colonnes_vides)
}
