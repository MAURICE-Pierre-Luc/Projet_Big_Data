

source("./Pierre-Luc_MAURICE/F_Visualisation.R")

.vsc.attach() #Pour pouvoir voir les variables sous vs code

df <- read.csv(file = "IRVE_clean.csv", encoding="UTF-8")


df_clean <- read.csv(file = "IRVE_clean.csv", encoding="UTF-8")


val <- nb_val_unique(df_clean, "nom_operateur", FALSE)
val <- nb_val_unique(df_clean, "tarification", FALSE)
val <- nb_val_unique(df_clean, "puissance_nominale", FALSE)

df_clean <- df_clean %>%
  mutate(tarification = as.numeric(gsub("€/kWh|€/kwh", "", tarification)))

# Calcul des pourcentages
pct_operateurs <- df_clean %>%
  count(nom_operateur) %>%
  mutate(pct = n / sum(n) * 100)

# Préparation pour le graphique
df_plot <- pct_operateurs %>%
  mutate(nom_operateur = ifelse(pct < 1, "AUTRES", nom_operateur)) %>%
  group_by(nom_operateur) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(
    pct = n / sum(n) * 100,
    label = paste0(nom_operateur, " (", round(pct, 1), "%)")
  ) %>%
  arrange(desc(pct)) %>%
  mutate(label = factor(label, levels = label))

png(paste0("./Pierre-Luc_MAURICE/parts_marche.png"),
        width = 1200,
        height = 1000)

ggplot(df_plot, aes(x = "", y = n, fill = label)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    #scale_fill_brewer(palette = "Set3") +
    scale_fill_viridis_d(direction = -1) +
    labs(title = "Répartition des opérateurs", fill = "") +
    theme(
        legend.position = "bottom",
        legend.text = element_text(size = 9)
    ) +
    guides(fill = guide_legend(nrow =3 , byrow = FALSE))

dev.off()

val <- nb_val_unique(df_clean, "tarification", FALSE)
hist(df_clean$tarification, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 3))

val <- nb_val_unique(df_clean, "puissance_nominale", FALSE)
hist(df_clean$puissance_nominale, breaks = c(0, 20, 40, 60, 80, 100, 120, 160, 180, 200, 220, 240, 260, 300, 320, 340, 360, 380, 400))

val <- nb_val_unique(df_clean, "nbre_pdc", FALSE)
hist(df_clean$nbre_pdc, breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45))




