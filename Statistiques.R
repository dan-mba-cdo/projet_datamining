# ---------------------------------------------------------------------------------------
#      Fichier : Statistiques.R
# D?veloppeurs : Thomas FONTAINE, Dan GOLDMAN, Juliette LEMAINS, Nicolas ROBIN
#         Date : 22 janvier 2019.
#  Description : Ce script inclut :
#                 - 
#
# ---------------------------------------------------------------------------------------


library(dplyr)
library(data.table)
library(readxl)
library(assertthat)
library(xlsx)
library(questionr)

source("Parametres_NR.R")


#Statistiques descriptives


column_date <- c("DATEDEBUTADHESION","DATEREADHESION","DATEFINADHESION","DATENAISSANCE",
                 "TIC_DATE")
column_id <- c("IDCLIENT","CODEINSEE","CODEARTICLE","IDTICKET","IDARTICLE")
  
convert_date <- function(table){
  table_bis<-table
  for (column in names(table)){
    if (column %in% column_date){
      table_bis[[column]] <- as.Date(table_bis[[column]],"%d/%m/%Y")
    }
  }
  return(table_bis)
}

clients<-convert_date(clients)
entete<-convert_date(entetes)

statistiques <- function(table) {
  cat("----------------------- Table :",deparse(substitute(table)),"-----------",'\n\n')
  for (column in names(table)){
    cat("------------",column,": -----------",'\n\n')
    if (column %in% column_date){
      cat("Type :", "Date",'\n')
      cat("Minimum :",min(format(table[[column]]),na.rm = TRUE))
      cat(" Maximum :", max(format(table[[column]]),na.rm = TRUE),'\n')
      cat("Taux de valeurs manquantes :", 
          (sum(is.na(table[[column]]))/length(table[[column]])*100),"%",'\n')
      cat('\n')
    }else if (typeof(table[[column]])=="character" & !(column %in% column_id)){
      cat("Type :", typeof(table[[column]]),'\n')
      cat("Valeurs unique :",length(unique(table[[column]])),'\n')
      if(nrow(freq(table[[column]]))<10){
        cat("Frequence de modalites :",'\n')
        print(freq(table[[column]])[,1:2])
      }
      cat("Taux de valeurs manquantes :", 
          (sum(is.na(table[[column]]))/length(table[[column]])*100),"%",'\n')
      cat('\n')
    }else if (!(column %in% column_id)){
      cat("Type :", typeof(table[[column]]),'\n')
      cat("Valeurs unique :",length(unique(table[[column]])),'\n')
      cat("Minimum :",min(format(table[[column]]),na.rm = TRUE))
      cat(" Minimum 1:",sort(unique(table[[column]],na.rm = TRUE))[2],'\n')
      cat("Maximum :", max(format(table[[column]]),na.rm = TRUE))
      cat(" Maximum 1:", sort(unique(table[[column]],na.rm = TRUE),decreasing = TRUE)[2],'\n')
      cat("Moyenne :",round(mean(table[[column]],na.rm = TRUE),1),'\n')
      cat("Median :",median(table[[column]],na.rm = TRUE),'\n')
      cat("Ecart type :",round(sd(table[[column]],na.rm = TRUE),1),'\n')
      cat("Coefficient de variation :",round(sd(table[[column]],na.rm = TRUE)/
                                               mean(table[[column]],na.rm = TRUE),1),'\n')
      cat("Taux de valeurs manquantes :", 
          (sum(is.na(table[[column]]))/length(table[[column]])*100),"%",'\n')
      cat('\n')
    }else{
      cat("Variable representant un identifiant",'\n\n')
    }
  }
}

# Data Frames qui indique la structure d'une table : pour chaque colonne, on indique si :
# 1) si la valeur est un identifiant -> "I"
# 2) si la valeur est discrète : on appliquera le tri à plat des modalités -> "M"
# 3) si la valeur est continue : on appliquera les statistiques de base -> "C"
# 4) si la valeur est une date -> "D"
table_articles_column_nature <- data.frame("CODEARTICLE"="I", "CODEUNIVERS"="M", "CODEFAMILLE"="M", "CODESOUSFAMILLE"="M")
table_clients_column_nature <- data.frame("IDCLIENT"="I", "CIVILITE"="M", "DATENAISSANCE"="D", "MAGASIN"="M", "DATEDEBUTADHESION"="D", "DATEREADHESION"="D", "DATEFINADHESION"="D", "VIP"="M", "CODEINSEE"="M", "PAYS"="M")
table_entetes_column_nature <- data.frame("IDTICKET"="I", "TIC_DATE"="D", "MAG_CODE"="M", "IDCLIENT"="I", "TIC_TOTALTTC"="C")
table_lignes_column_nature <- data.frame("IDTICKET"="I", "NUMLIGNETICKET"="M", "IDARTICLE"="I", "QUANTITE"="C", "MONTANTREMISE"="C", "TOTAL"="C", "MARGESORTIE"="C")
table_magasins_column_nature <- data.frame("CODESOCIETE"="I", "VILLE"="M", "LIBELLEDEPARTEMENT"="M", "LIBELLEREGIONCOMMERCIALE"="M")


# Fonction qui retourne la dataframe structure correspondant à la table
get_column_nature_df <- function(table_name) {
  
  if (table_name == "articles") {
    valeur_retournee <- table_articles_column_nature
  } else if (table_name == "clients") {
    valeur_retournee <- table_clients_column_nature
  }  else if (table_name == "entetes") {
    valeur_retournee <- table_entetes_column_nature
  }  else if (table_name == "lignes") {
    valeur_retournee <- table_lignes_column_nature
  }  else if (table_name == "magasins") {
    valeur_retournee <- table_magasins_column_nature
  } else {
    cat("ERREUR: le nom de la table n'a pas été trouvé.")
    valeur_retournee <- NULL
  }
  
  return(valeur_retournee)
}

get_column_nature <- function(column, table_name) {
  
  # Exemple : on récupère table_articles_column_structure
  column_nature_df <- get_column_nature_df(table_name)
  
  # Exemple : On récupère l'index de "CODEARTICLE"
  index_colonne_a_trouver <- which(column == colnames(column_nature_df))
  variable_nature <- column_nature_df[1, index_colonne_a_trouver]
  
  return (variable_nature)
}

est_ID <- function(column, table_name) {
  
  variable_nature <- get_column_nature(column, table_name)
  valeur_retournee <- (variable_nature == "I")
  
  return(valeur_retournee)
}

est_Discrete <- function(column, table_name) {
  
  variable_nature <- get_column_nature(column, table_name)
  valeur_retournee <- (variable_nature == "M")
  
  return(valeur_retournee)
}

est_Continue <- function(column, table_name) {
  
  variable_nature <- get_column_nature(column, table_name)
  valeur_retournee <- (variable_nature == "C")
  
  return(valeur_retournee)
}

est_Date <- function(column, table_name) {
  
  variable_nature <- get_column_nature(column, table_name)
  valeur_retournee <- (variable_nature == "D")
  
  return(valeur_retournee)
}

get_column_nature_text <- function(column, table_name) {
  
  variable_nature <- get_column_nature(column, table_name)
  valeur_retournee <- switch(as.character(variable_nature), "I"="Identifiant", "M"="Discrete", "C"="Continue", "D"="Date")
  
  return(valeur_retournee)
}


resume_structure <- function(table) {
  
  # On récupère le nom de la table.
  table_name <- deparse(substitute(table))
  
  # La liste des colonnes de la table.
  # Exemple : colonnes de la table "articles"
  column_names <- names(table)
  
  # Exemple de colonne : "CODEARTICLE"
  for (column in column_names) {
    
    column_name <- toupper(column)
    column_type <- typeof(table[[column]])
    column_nature <- get_column_nature_text(column, table_name)
    column_valeurs_uniques <- length(unique(table[[column]]))
    taux_valeurs_manquantes <- paste0(round(sum(is.na(table[[column]]))/length(table[[column]])*100,PRECISION),"%")
      
    # Si c'est la 1ère fois qu'on rencontre la colonne...
    if (column == column_names[[1]]) {
      
      # ... on construit un tableau avec 4 colonnes
      kable_output <- data.frame("Colonne"=column_name, 
                                 "Type"=column_type, 
                                 "Nature de la variable"=column_nature, 
                                 "Valeurs uniques"=column_valeurs_uniques,
                                 "TVM"=taux_valeurs_manquantes)
      
    }else{
      
      new_row <- data.frame("Colonne"=column_name, 
                            "Type"=column_type, 
                            "Nature de la variable"=column_nature, 
                            "Valeurs uniques"=column_valeurs_uniques,
                            "TVM"=taux_valeurs_manquantes)
      
      kable_output <- rbind(kable_output,new_row)
    }
  }
  
  kable(kable_output, align='lllrr') %>%
    kable_styling(bootstrap_options = c("striped", "bordered", "hover", "condensed", "responsive", full_width=F, position="left", row_label_position="r")) %>% 
    row_spec(0, bold = T, color = "black", background = "#DDEBF7") %>% 
    footnote(general = "TVM = Taux des Valeurs Manquantes")
}

statistiques_de_base <- function(table) {
  
  kable_output <- NULL
  
  # On récupère le nom de la table.
  table_name <- deparse(substitute(table))
  
  # La liste des colonnes de la table.
  # Exemple : colonnes de la table "articles"
  column_names <- names(table)
  
  # Exemple de colonne : "CODEARTICLE"
  for (column in column_names) {
    
    if (est_Continue(column, table_name) == TRUE) {
      
      column_name <- toupper(column)
      min <- as.character(min(format(table[[column]]),na.rm = TRUE))
      min1 <- as.character(sort(unique(table[[column]],na.rm = TRUE))[2])
      max <- as.character(max(format(table[[column]]),na.rm = TRUE))
      max1 <- as.character(sort(unique(table[[column]],na.rm = TRUE),decreasing = TRUE)[2])
      mean <- as.character(round(mean(table[[column]],na.rm = TRUE),PRECISION))
      median <- as.character(round(median(table[[column]],na.rm = TRUE), PRECISION))
      std <- as.character(round(sd(table[[column]],na.rm = TRUE),PRECISION))
      cv <- as.character(round(sd(table[[column]],na.rm = TRUE) / mean(table[[column]],na.rm = TRUE),PRECISION))
      
      # Si c'est la 1ère fois qu'on rencontre la colonne...
      if (column == column_names[[1]]) {
        
        # ... on construit un tableau avec 9 colonnes
        kable_output <- data.frame("Colonne"=column_name, 
                                   "Min"=min, 
                                   "Min1"=min1, 
                                   "Max"=max,
                                   "Max1"=max1,
                                   "Mean"=mean,
                                   "Median"=median,
                                   "Ecart Type"=std,
                                   "CV"=cv)
        
      }else{
        
        # ... on ajoute une ligne
        new_row <- data.frame("Colonne"=column_name, 
                              "Min"=min, 
                              "Min1"=min1, 
                              "Max"=max,
                              "Max1"=max1,
                              "Mean"=mean,
                              "Median"=median,
                              "Ecart Type"=std,
                              "CV"=cv)
        
        kable_output <- rbind(kable_output,new_row)
      } #endif
    } #endif
    
    if (est_Date(column, table_name) == TRUE) {
      
      column_name <- toupper(column)
      min <- as.character(min(format(table[[column]]),na.rm = TRUE))
      min1 <- "NA"
      max <- as.character(max(format(table[[column]]),na.rm = TRUE))
      max1 <- "NA"
      mean <- "NA"
      median <- "NA"
      std <- "NA"
      cv <- "NA"
      
      # Si c'est la 1ère fois qu'on rencontre la colonne...
      if (column == column_names[[1]]) {
        
        # ... on construit un tableau avec 9 colonnes
        kable_output <- data.frame("Colonne"=column_name, 
                                   "Min"=min, 
                                   "Min1"=min1, 
                                   "Max"=max,
                                   "Max1"=max1,
                                   "Mean"=mean,
                                   "Median"=median,
                                   "Ecart Type"=std,
                                   "CV"=cv)
        
      }else{
        
        # ... on ajoute une ligne
        new_row <- data.frame("Colonne"=column_name, 
                              "Min"=min, 
                              "Min1"=min1, 
                              "Max"=max,
                              "Max1"=max1,
                              "Mean"=mean,
                              "Median"=median,
                              "Ecart Type"=std,
                              "CV"=cv)
        
        kable_output <- rbind(kable_output,new_row)
      } #endif
    } #endif
    
  } #endfor
  
  if (is.null(kable_output)) {
    
    texte_a_afficher <- paste0("Cette section est vide : la table <", table_name, "> ne renferme aucune colonne étiquetées avec des variables continues.")
    pander(cat(texte_a_afficher))
    
  }else{
    
    kable(kable_output, align='lrrrrrrrr') %>%
      kable_styling(bootstrap_options = c("striped", "bordered", "hover", "condensed", "responsive", full_width=F, position="left", row_label_position="r")) %>% 
      row_spec(0, bold = T, color = "black", background = "#DDEBF7")
  }
}

# Fonctin permettant d'afficher les modalités d'une table.
tris_a_plat <- function(table) {
  
  # Index qui va être utilisé pour le monitoring de l'affichage en gras des modalités.
  index_highlight_monitoring <- 0
  rows_to_highlight <- c()
  
  # On récupère le nom de la table.
  nom_de_la_table <- deparse(substitute(table))
  
  # La liste des colonnes de la table.
  # Exemple : colonnes de la table "articles"
  column_names <- names(table)
  
  # On affiche la ligne de titre : "Modalités" - "Effectifs" - "Pourcentage"
  kable_output <- data.frame("Modalités"=" ", "Effectifs"=0, "Pourcentage"=0)
  index_highlight_monitoring <- index_highlight_monitoring + 1

  # On parcours chaque colonne
  # Exemple : "CODEARTICLE"
  for (column in column_names) {

    # On récupère le nombre de modalites de la colonne courante.
    nb_modalites <- length(unique(table[[column]]))

    # On n'affiche que les modalités des variables dicrètes.
    # NOTE : on ne les affiche tous que si le nombre de modalités est inférieur au nombre de modalités maximum autorisées.
    if ((est_Discrete(column, nom_de_la_table) == TRUE) && (nb_modalites <= NB_MODALITES_MAX)) {

      new_row <- data.frame("Modalités"=column, "Effectifs"="", "Pourcentage"="")
      kable_output <- rbind(kable_output,new_row)
      index_highlight_monitoring <- index_highlight_monitoring + 1
      rows_to_highlight <- c(rows_to_highlight, index_highlight_monitoring)
      
      
      freq_table <- freq(table[[column]])[,1:2]
      names(freq_table)[1]<-"Effectifs"
      names(freq_table)[2]<-"Pourcentage"
      row_name_freq_table <- row.names(freq_table)
      
      for (index in 1:nb_modalites) {
        new_row <- data.frame("Modalités"=row_name_freq_table[index], "Effectifs"=freq_table[index,1], "Pourcentage"=freq_table[index,2])
        kable_output <- rbind(kable_output,new_row)
        index_highlight_monitoring <- index_highlight_monitoring + 1
      }
      
      new_row <- data.frame("Modalités"=" ", "Effectifs"="", "Pourcentage"="")
      kable_output <- rbind(kable_output,new_row)
      index_highlight_monitoring <- index_highlight_monitoring + 1

    } #endif

  } #endfor
  
  kable_output[1,1] <- " "
  kable_output[1,2] <- ""
  kable_output[1,3] <- ""

  if (index_highlight_monitoring <= 2) {
    
    texte_a_afficher <- paste0("Cette section est vide : la table <", nom_de_la_table, "> ne renferme aucune colonne étiquetées avec des variables discrètes.")
    pander(cat(texte_a_afficher))
    
  }else{
    
    kable(kable_output, align='lrr') %>%
      #kable_styling(bootstrap_options = c("striped", "bordered", "hover", "condensed", "responsive", full_width=F, position="left", row_label_position="r")) %>% 
      kable_styling(bootstrap_options = c("striped", "bordered", "hover", "condensed")) %>% 
      row_spec(rows_to_highlight, bold = T, color = "black", background = "#DDEBF7")
      
  }
}


