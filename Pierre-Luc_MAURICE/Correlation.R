
source("./Pierre-Luc_MAURICE/F_Correlation.R")

.vsc.attach() #Pour pouvoir voir les variables sous vs code

df_clean <- read.csv(file = "IRVE_clean.csv", encoding="UTF-8")

df_clean <- mutate(df_clean,tarification = as.numeric(gsub("€/kWh|€/kwh", "", tarification)))

#for(name in names(df_clean)){
#    nb_val_unique(df_clean, name,FALSE)
#}


# Calcul des pourcentages
pct_operateurs <- df_clean %>%
  count(nom_operateur) %>%
  mutate(pct = n / sum(n) * 100)

# Préparation des variables (tout en facteur)
df_matrix <- select( #select() sert à choisir seulement certaines colonnes dans un data frame et on ignore les autres
  mutate( #mutate() sert à modifier les colonnes d'un data frame
    df_clean,

    # as.factor renvoie un tableau de contingence 
    nom_operateur = ifelse( #Ici on ne garde que les operateurs qi représnetent plus de 1% des parts du marché, sinon il sera trop difficaile de calculer le qi2
      nom_operateur %in% (pct_operateurs %>% filter(pct >= 1) %>% pull(nom_operateur)),
      nom_operateur,
      "AUTRES"
    ) %>% as.factor(),
    prise_type_ef        = as.factor(prise_type_ef),
    prise_type_2         = as.factor(prise_type_2),
    prise_type_combo_ccs = as.factor(prise_type_combo_ccs),
    prise_type_chademo   = as.factor(prise_type_chademo),
    prise_type_autre     = as.factor(prise_type_autre),
    reservation          = as.factor(reservation),
    station_deux_roues   = as.factor(station_deux_roues),
    gratuit              = as.factor(gratuit),
    paiement_acte        = as.factor(paiement_acte),
    paiement_cb          = as.factor(paiement_cb),
    paiement_autre       = as.factor(paiement_autre),
    implantation_station = as.factor(implantation_station),
    condition_acces      = as.factor(condition_acces),
    accessibilite_pmr    = as.factor(accessibilite_pmr),
    restriction_gabarit  = as.factor(restriction_gabarit),
    raccordement         = as.factor(raccordement),

    tarification       = cut(tarification, breaks = 5, include.lowest = TRUE),
    puissance_nominale = cut(puissance_nominale, breaks = 5, include.lowest = TRUE),
    nbre_pdc = cut(nbre_pdc, breaks = c(0, 2, 5, 10, 44), labels = c("1-2", "3-5", "6-10", "11+"), include.lowest = TRUE)
  ),

  #L'ensemble des colonnes qu'on veut garder
  nom_operateur, nbre_pdc, puissance_nominale, tarification,
  prise_type_ef, prise_type_2, prise_type_combo_ccs, prise_type_chademo, prise_type_autre,
  gratuit, paiement_acte, paiement_cb, paiement_autre,
  implantation_station, condition_acces, accessibilite_pmr,
  restriction_gabarit, station_deux_roues, raccordement,
  reservation)

# Calcul de la matrice de chi²
vars <- names(df_matrix)
n <- length(vars)

chi2_pvalue <- matrix(1,    nrow = n, ncol = n, dimnames = list(vars, vars)) #On crée une matrice vide pour les p-value
chi2_stat   <- matrix(0,    nrow = n, ncol = n, dimnames = list(vars, vars)) #On crée une matrice vide pour les scores

for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      test <- chisq.test(table(df_matrix[[i]], df_matrix[[j]]), simulate.p.value = TRUE)
      chi2_pvalue[i, j] <- test$p.value
      chi2_stat[i, j]   <- test$statistic
    }
  }
}

matrix_graph(chi2_stat, "matrice_chi2_score_color", TRUE, TRUE, "color")
matrix_graph(chi2_stat, "matrice_chi2_score_number", FALSE, TRUE, "number")

matrix_graph(chi2_pvalue, "matrice_chi2_pvalue_color", TRUE, TRUE, "color")
matrix_graph(chi2_pvalue, "matrice_chi2_pvalue_number", FALSE, TRUE, "number")


cramerV_manual <- function(tbl) {
  test <- chisq.test(tbl, simulate.p.value = TRUE)
  n    <- sum(tbl)
  k    <- min(nrow(tbl) - 1, ncol(tbl) - 1)
  v    <- sqrt(test$statistic / (n * k))
  return(as.numeric(v))
}

vars <- names(df_matrix)
n    <- length(vars)

cramer_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(vars, vars))
diag(cramer_matrix) <- 1

for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      tbl  <- table(df_matrix[[i]], df_matrix[[j]])
      v    <- cramerV_manual(tbl)
      cramer_matrix[i, j] <- round(v, 3)
      cramer_matrix[j, i] <- round(v, 3)
    }
  }
}

matrix_graph(cramer_matrix, "matrice_cramer_number", TRUE, TRUE, "number")
matrix_graph(cramer_matrix, "matrice_cramer_color", TRUE, TRUE, "color")

paires_fortes <- which(cramer_matrix > 0.3, arr.ind = TRUE)

data.frame(
  var1   = rownames(cramer_matrix)[paires_fortes[, 1]],
  var2   = colnames(cramer_matrix)[paires_fortes[, 2]],
  cramer = cramer_matrix[paires_fortes]
) %>%
  filter(var1 < var2) %>%  #On garde seulement le triangle supérieur pour éviter les doublons, avec tuple inversé
  arrange(desc(cramer))


# Code type pour représenter une paire en mosaicplot, içi 64 paires c'est trop et probablement sans interret de toutes les représneter
# Remplacer var1 et var2 par les variables souhaitées

# mosaicplot(table(df_clean$var1, df_clean$var2),
#            main  = "Titre du graphique",
#            xlab  = "Variable 1",
#            ylab  = "Variable 2",
#            color = TRUE)



#Regression logistique sur la tarification

# On sépare les prix en 3 catégories
df_clean <- df_clean %>%
  mutate(tarification_groupe = case_when(
    tarification == 0    ~ "bas",
    tarification <= 0.4  ~ "modere",
    tarification > 0.4   ~ "eleve",
    TRUE                 ~ NA_character_
  ) %>% factor(levels = c("bas", "modere", "eleve")))

# On transforme les données pour pouvoir les exploiter
df_reg <- df_clean %>%
  filter(!is.na(tarification_groupe)) %>%
  mutate(
    nom_operateur = ifelse( # On ne garde que les operateurs qui composent 1% ou plus du marche pour éviter d'avoir trop de valeurs
      nom_operateur %in% (pct_operateurs %>% filter(pct >= 1) %>% pull(nom_operateur)),
      nom_operateur,
      "AUTRES"
    ) %>% as.factor(),
    paiement_autre      = as.factor(ifelse(is.na(paiement_autre), "inconnu", as.character(paiement_autre))), #On remplace les NA par inconnu
    accessibilite_pmr   = as.factor(accessibilite_pmr),
    restriction_gabarit = as.factor(restriction_gabarit),
    reservation         = as.factor(reservation),
    paiement_acte       = as.factor(paiement_acte)
  )

# Séparation train/test (80/20)
set.seed(42) #On fixe l'aspect aléatoire pour pouvoir reproduire les résultats

idx_train <- sample(nrow(df_reg), 0.8 * nrow(df_reg)) #On prend 80% des donnés de la colonne pour faire l'entrainement

#On sépare les donnés dans leurs data frame respectifs
df_train  <- df_reg[idx_train, ]
df_test   <- df_reg[-idx_train, ]

# Régression logistique multinomiale : prédit la classe de tarification (bas/modere/eleve)
# en fonction des variables explicatives sélectionnées via la matrice de Cramér.
# multinom() estime 2 équations (standard vs gratuit) et (élevé vs gratuit),
# "gratuit" étant la classe de référence.
# maxit = 500 : nombre maximum d'itérations pour la convergence du modèle.
modele_logit <- multinom(tarification_groupe ~ nom_operateur + paiement_autre +
                           accessibilite_pmr + restriction_gabarit +
                           reservation + paiement_acte,
                         data = df_train,
                         maxit = 500)

# On fait une prédiction par le modèle sur les donnés de test, les 20 autres % qu'on a aps pris pour l'entrainement
predictions_test <- predict(modele_logit, df_test)

# On calcule le taux de bonne classification
cat("Taux de bonne classification :",
    round(mean(predictions_test[idx_valides] == df_test$tarification_groupe[idx_valides]) * 100, 2), "%\n")


  png(paste0("./Pierre-Luc_MAURICE/matrice_regression_tarification.png"), width = 1200, height = 1000)
# La matrice de confusion des classifications sur les donnés de test
as.data.frame(table(Prédit = predictions_test, Réel = df_test$tarification_groupe)) %>%
  group_by(Réel) %>%
  mutate(pct = Freq / sum(Freq) * 100) %>%
  ggplot(aes(x = Réel, y = Prédit, fill = pct)) +
  geom_tile() +
  geom_text(aes(label = paste0(Freq, "\n(", round(pct, 1), "%)")),
            size = 4, color = "white", fontface = "bold") +
  scale_fill_viridis_c() +
  labs(title = "Matrice de confusion - données test (20%)") +
  theme_minimal()
  dev.off()

#Regression sur la puissance

# On transforme les donnes pour pouvoir les exploiter
df_reg_puissance <- df_clean %>%
  filter(!is.na(puissance_nominale)) %>%
  mutate(
    prise_type_combo_ccs = as.factor(prise_type_combo_ccs),
    prise_type_2         = as.factor(prise_type_2),
    paiement_acte        = as.factor(paiement_acte),
    reservation          = as.factor(reservation),
    condition_acces      = as.factor(condition_acces),
    paiement_autre       = as.factor(ifelse(is.na(paiement_autre), "inconnu", as.character(paiement_autre)))
  )

# Séparation train/test (80/20)
set.seed(42) # On fixe le 'aspect aleatoire pour pouvoir reproduire les résultats

#On gardde seulement 80% des donnés pour entrainer le modèle
idx_train <- sample(nrow(df_reg_puissance), 0.8 * nrow(df_reg_puissance))

#On sépare les données dans leurs data frame respectifs
df_train_p <- df_reg_puissance[idx_train, ]
df_test_p  <- df_reg_puissance[-idx_train, ]

# La fonction nm cheche à minimiser la somme des carrés  des erreurs entre les valeurs  réelles et prédites
modele_lm <- lm(puissance_nominale ~ prise_type_combo_ccs + prise_type_2 +
                  paiement_acte + reservation + condition_acces + paiement_autre,
                data = df_train_p)

summary(modele_lm)

# On fait une prédiction par le modèle sur les donnés de test, les 20 autres % qu'on a aps pris pour l'entrainement
predictions_lm <- predict(modele_lm, df_test_p)


# Performance du modèle
rmse <- sqrt(mean((predictions_lm - df_test_p$puissance_nominale)^2, na.rm = TRUE))
r2   <- cor(predictions_lm, df_test_p$puissance_nominale, use = "complete.obs")^2
cat("RMSE :", round(rmse, 2), "kW\n") # Erreure moyenne du modèle, il se trompe en moyenne de X kW
cat("R²   :", round(r2, 4), "\n") # De combien on réduit l'erreur totale par rapport à un modèle qui donnerais uniquement la moyenne

png(paste0("./Pierre-Luc_MAURICE/graphe_regression_puissance_residu_vs_valeurs_predites.png"), width = 1200, height = 1000)
# Résidus vs valeurs prédites
data.frame(
  predit  = fitted(modele_lm),
  residus = residuals(modele_lm)
) %>%
  ggplot(aes(x = predit, y = residus)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Résidus vs valeurs prédites",
       x = "Valeurs prédites (kW)", y = "Résidus (kW)") +
  theme_minimal()
  
dev.off()


png(paste0("./Pierre-Luc_MAURICE/graphe_regression_puissance_reel_vs_valeurs_predites.png"), width = 1200, height = 1000)
# Réel vs prédit
data.frame(
  reel   = df_test_p$puissance_nominale,
  predit = predict(modele_lm, df_test_p)
) %>%
  ggplot(aes(x = reel, y = predit)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Puissance réelle vs prédite (test)",
       x = "Puissance réelle (kW)", y = "Puissance prédite (kW)") +
  theme_minimal()

dev.off()
