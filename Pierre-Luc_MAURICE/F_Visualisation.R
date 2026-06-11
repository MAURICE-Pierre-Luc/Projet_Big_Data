library(tidyverse)
library(janitor)
library(lubridate)
library(naniar)
library(stringi)
library(ggplot2)
library(dplyr)


nb_val_unique <- function(df, colonne, list){
    cat("Valeurs uniques",colonne,":", length(unique(df[[colonne]])), "\n")
    if(list){
        sort((unique(df[[colonne]]))) |> head(10)
    }
    return(sort((unique(df[[colonne]]))))
}


to_uppercase <- function(df, column) {
  df[[column]] <- toupper(as.character(df[[column]]))
  return(df)
}



