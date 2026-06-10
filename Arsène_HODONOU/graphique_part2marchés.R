#ARSENE HODONOU


# Le graphique de Part de Marché (Format Classique Vertical)
ggplot(parts_operateur, aes(x = reorder(nom_operateur, -pourcentage), y = pourcentage, fill = nom_operateur)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  # Ajout des étiquettes de pourcentage au-dessus des barres (vjust = -0.5)
  geom_text(aes(label = paste0(round(pourcentage, 1), "%")), 
            vjust = -0.5, size = 3.5, fontface = "bold") + 
  labs(
    title = "Parts de marché des opérateurs de bornes de recharge",
    subtitle = "Répartition en pourcentage du parc total installé",
    x = "Opérateurs",
    y = "Part de marché (%)"
  ) +
  scale_fill_viridis_d(option = "plasma") + 
  theme_minimal() +
  theme(
    # Rotation des noms des opérateurs pour une lecture parfaite en bas
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

