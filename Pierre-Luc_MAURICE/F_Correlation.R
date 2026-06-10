library(tidyverse)
library(janitor)
library(lubridate)
library(naniar)
library(stringi)
library(ggplot2)
library(dplyr)
library(corrplot)
library(nnet)

nb_val_unique <- function(df, colonne, show_list = FALSE) {

  values <- df[[colonne]]
  tab <- table(values)

  cat("Colonne :", colonne, "\n")
  cat("Nombre de valeurs uniques :", length(tab), "\n\n")

  cat("Occurrences :\n")
  print(tab)

  if (show_list) {
    cat("\nValeurs uniques (triées) :\n")
    print(sort(unique(values)))
  }

  return(list(
    n_unique = length(tab),
    occurrences = tab,
    uniques = sort(unique(values))
  ))
}

# Renvoie un dictionnaire de variances
variance_df <- function(df) {

    vars <- new.env()

    for (colname in names(df)) {
        vars[[colname]] <- var(df[[colname]], na.rm = TRUE)
    }

    return(vars)
}


matrix_graph <- function(matrix, name, use_color = TRUE, export = FALSE, method) {

  if (export) {
    png(paste0("./Pierre-Luc_MAURICE/",name,".png"),
        width = 1200,
        height = 1000)
  }

  if (use_color) {

    corrplot(matrix,
             is.corr   = FALSE,
             method    = method,
             type      = "full",
             tl.cex    = 0.7,
             tl.col    = "black",
             col       = colorRampPalette(c("#3B8BD4", "#40c496", "#E85D24"))(200),
             title     = name,
             mar       = c(0, 0, 2, 0))

  } else {

    corrplot(matrix,
             is.corr    = FALSE,
             method     = method,
             type       = "full",
             tl.cex     = 0.7,
             tl.col     = "black",
             number.cex = 0.5,
             col        = "black",
             title      = name,
             mar        = c(0, 0, 2, 0))
  }

  if (export) {
    dev.off()
  }
}

