# ---------------------------------------------------------------------------------------------------
#      Fichier : Statistiques.R
# Développeurs : Thomas FONTAINE, Dan GOLDMAN, Juliette LEMAINS, Nicolas ROBIN
#         Date : 22 janvier 2019.
#  Description : Ce fichier renferme l'ensemble des fonctions statistiques nécessaires au calcul 
#                des statistiques descriptives de base :
#                 - resume_structure(table)
#                 - statistiques_de_base(table)
#                 - tris_a_plat(table)
# ---------------------------------------------------------------------------------------------------

# Import du fichier des paramètres globaux
source("Parametres.R")


# On identifie les variables qui sont des dates ou des identifiants
# Ces deux variables vont être utilisées par la fonction statistique(table)
column_date <- c("DATEDEBUTADHESION","DATEREADHESION","DATEFINADHESION","DATENAISSANCE",
                 "TIC_DATE")
column_id <- c("IDCLIENT","CODEINSEE","CODEARTICLE","IDTICKET","IDARTICLE")


# ---------------------------------------------------------------------------------------------------
#       Fonction : convert_date(table)
# Développeur(s) : Juliette LEMAINS
#           Date : 28 décembre 2018.
#    Description : Cette fonction convertit les dates au format dd/mm/YYYY en date R.
#         Entrée : une table dataframe
#         Sortie : une table dataframe dont les dates ont été converties au format dd/mm/YY.
# ---------------------------------------------------------------------------------------------------
convert_date <- function(table){
  table_bis<-table
  for (column in names(table)){
    if (column %in% column_date){
      table_bis[[column]] <- as.Date(table_bis[[column]],"%d/%m/%Y")
    }
  }
  return(table_bis)
}

# ---------------------------------------------------------------------------------------------------
#       Fonction : convert_date2(table)
# Développeur(s) : Nicolas ROBIN
#           Date : 30 janvier 2018.
#    Description : Cette fonction convertit les dates au format YYYY/mm/dd.
#         Entrée : une table dataframe
#         Sortie : une table dataframe dont les dates ont été converties au format dd/mm/YY.
# ---------------------------------------------------------------------------------------------------
convert_date2 <- function(table){
  table_bis<-table
  for (column in names(table)){
    if (column %in% column_date){
      table_bis[[column]] <- as.Date(table_bis[[column]],"%Y-%m-%d")
    }
  }
  return(table_bis)
}

# Converion du format date des dates contenues dans les tables clients et entetes
clients<-convert_date(clients)
entetes<-convert_date2(entetes)

# ---------------------------------------------------------------------------------------------------
#       Fonction : statistique(table)
# Développeur(s) : Juliette LEMAINS
#           Date : 28 décembre 2018.
#    Description : Cette fonction permet d'afficher une sortie console (texte) des statistiques 
#                  descriptives de base.
#         Entrée : une table dataframe
#         Sortie : sortie console (texte) des staistiques descriptives de base.
# ---------------------------------------------------------------------------------------------------
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


# ---------------------------------------------------------------------------------------------------
#       Fonction : get_column_nature_df(table_name)
# Développeur(s) : Nicolas ROBIN
#           Date : 23 janvier 2018.
#    Description : Cette fonction retourne la table dataframe qui définit la nature des valeurs des
#                   contenant dans chaque colonne de la table.
#                   Cette table est définie dans le fichier des paramètres globaux.
#         Entrée : le nom de la table au format chaîne de caractères.
#         Sortie : la data frame recherchée et qui correspond à la table renseignée en entrée.
# ---------------------------------------------------------------------------------------------------
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
  } else if (table_name == "datamining_client") {
    valeur_retournee <- table_datamining_client_column_nature
  } else {
    cat("ERREUR: le nom de la table n'a pas été trouvé.")
    valeur_retournee <- NULL
  }
  
  return(valeur_retournee)
}

# ---------------------------------------------------------------------------------------------------
#       Fonction : get_column_nature(table_name)
# Développeur(s) : Nicolas ROBIN
#           Date : 23 janvier 2018.
#    Description : Cette fonction retourne le type de variable définit pour chacune des variables
#                   des tables définies dans le fichier de paramètres..
#         Entrée : le nom de la table au format chaîne de caractères.
#         Sortie : "I", "N" ou "C" selon que la variable est un :
#                   - Identifiant,
#                   - Une valeur nominale
#                   - Une valeur continue
# ---------------------------------------------------------------------------------------------------
get_column_nature <- function(column, table_name) {
  
  # Exemple : on récupère table_articles_column_structure
  column_nature_df <- get_column_nature_df(table_name)
  
  # Exemple : On récupère l'index de "CODEARTICLE"
  index_colonne_a_trouver <- which(column == colnames(column_nature_df))
  variable_nature <- column_nature_df[1, index_colonne_a_trouver]
  
  return (variable_nature)
}

# ---------------------------------------------------------------------------------------------------
#       Fonction : est_ID(column, table_name)
# Développeur(s) : Nicolas ROBIN
#           Date : 23 janvier 2018.
#    Description : Cette fonction indique si la colonne de la table renferme des identifiants.
#        Entrées : la colonne et le nom de la table de la colonne au format chaîne de caractères.
#         Sortie : Valeur booléenne
# ---------------------------------------------------------------------------------------------------
est_ID <- function(column, table_name) {
  
  variable_nature <- get_column_nature(column, table_name)
  valeur_retournee <- (variable_nature == "I")
  
  return(valeur_retournee)
}

# ---------------------------------------------------------------------------------------------------
#       Fonction : est_Nominale(column, table_name)
# Développeur(s) : Nicolas ROBIN
#           Date : 23 janvier 2018.
#    Description : Cette fonction indique si la colonne de la table renferme des valeurs nominales.
#        Entrées : la colonne et le nom de la table de la colonne au format chaîne de caractères.
#         Sortie : Valeur booléenne
# ---------------------------------------------------------------------------------------------------
est_Nominale <- function(column, table_name) {
  
  variable_nature <- get_column_nature(column, table_name)
  valeur_retournee <- (variable_nature == "N")
  
  return(valeur_retournee)
}

# ---------------------------------------------------------------------------------------------------
#       Fonction : est_Continue(column, table_name)
# Développeur(s) : Nicolas ROBIN
#           Date : 23 janvier 2018.
#    Description : Cette fonction indique si la colonne de la table renferme des valeurs continues.
#        Entrées : la colonne et le nom de la table de la colonne au format chaîne de caractères.
#         Sortie : Valeur booléenne
# ---------------------------------------------------------------------------------------------------
est_Continue <- function(column, table_name) {
  
  variable_nature <- get_column_nature(column, table_name)
  valeur_retournee <- (variable_nature == "C")
  
  return(valeur_retournee)
}

# ---------------------------------------------------------------------------------------------------
#       Fonction : get_column_nature_text(column, table_name)
# Développeur(s) : Nicolas ROBIN
#           Date : 23 janvier 2018.
#    Description : Cette fonction renvoie une valeur texte correspondant à son indice.
#                  Cette fonction sera notamment utilisée dans la fonction resume_structure(table).
#        Entrées : la colonne et le nom de la table de la colonne au format chaîne de caractères.
#         Sortie : Une valeur texte.
# ---------------------------------------------------------------------------------------------------
get_column_nature_text <- function(column, table_name) {
  
  variable_nature <- get_column_nature(column, table_name)
  valeur_retournee <- switch(as.character(variable_nature), "I"="Identifiant", "N"="Nominale", "C"="Continue")
  
  return(valeur_retournee)
}


# ---------------------------------------------------------------------------------------------------
#       Fonction : resume_structure(table)
# Développeur(s) : Nicolas ROBIN
#           Date : 24 janvier 2018.
#    Description : Cette fonction renvoie une table kable composé des valeurs suivantes :
#                  - Nom des colonnes de la table
#                  - Type de l'ensemble des colonnes de la table
#                  - Nature de l'ensemble des colonnes de la table
#                  - Valeurs uniques (effectif réel) de l'ensemble des colonnes de la table
#                  - Taux des valeurs manquantes de l'ensemble des colonnes de la table
#        Entrées : la colonne et le nom de la table de la colonne au format chaîne de caractères.
#         Sortie : NA.
# ---------------------------------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------------------------------
#       Fonction : statistiques_de_base(table)
# Développeur(s) : Nicolas ROBIN
#           Date : 25 janvier 2018.
#    Description : Cette fonction renvoie une table kable composé des valeurs suivantes :
#                  - Nom des colonnes de la table
#                  - min et min1 de l'ensemble des colonnes de la table
#                  - max et max1 de l'ensemble des colonnes de la table
#                  - mean, median de l'ensemble des colonnes de la table
#                  - std et cv de l'ensemble des colonnes de la table
#         Entrée : une table au format dataframe.
#         Sortie : NA.
# ---------------------------------------------------------------------------------------------------
statistiques_de_base <- function(table, ...) {
  
  kable_output <- NULL
  
  # On récupère le nom de la table.
  table_name <- deparse(substitute(table))
  
  # La liste des colonnes de la table.
  # Exemple : colonnes de la table "articles"
  column_names <- names(table)
  
  # On récupère la liste des options d'affichage des statistiques
  parametres <- list(...)
  
  # Exemple de colonne : "CODEARTICLE"
  for (column in column_names) {
    
    if (est_Continue(column, table_name) == TRUE) {
      
      # Initialisation des colonnes à la valeur "".
      column_name <- min <- min1 <- max <- max1 <- mean <- median <- std <- cv <- ""
      
      if (length(parametres) != 0) {
        noms <- names(parametres[[1]])
        valeurs <- parametres[[1]]
        nb_parametres <- length(parametres[[1]])
        
        for (index in 1:nb_parametres ) {
          if ((noms[index] == "column_name") && (valeurs[index]==TRUE)) {column_name <- toupper(column)}
          if ((noms[index] == "min") && (valeurs[index]==TRUE)) {min <- as.character(min(format(table[[column]]),na.rm = TRUE))}
          if ((noms[index] == "min1") && (valeurs[index]==TRUE)) {min1 <- as.character(sort(unique(table[[column]],na.rm = TRUE))[2])}
          if ((noms[index] == "max") && (valeurs[index]==TRUE)) {max <- as.character(max(format(table[[column]]),na.rm = TRUE))}
          if ((noms[index] == "max1") && (valeurs[index]==TRUE)) {max1 <- as.character(sort(unique(table[[column]],na.rm = TRUE),decreasing = TRUE)[2])}
          if ((noms[index] == "mean") && (valeurs[index]==TRUE)) {mean <- as.character(round(mean(table[[column]],na.rm = TRUE),PRECISION))}
          if ((noms[index] == "median") && (valeurs[index]==TRUE)) {median <- as.character(round(median(table[[column]],na.rm = TRUE), PRECISION))}
          if ((noms[index] == "std") && (valeurs[index]==TRUE)) {std <- as.character(round(sd(table[[column]],na.rm = TRUE),PRECISION))}
          if ((noms[index] == "cv") && (valeurs[index]==TRUE)) {cv <- as.character(round(sd(table[[column]],na.rm = TRUE) / mean(table[[column]],na.rm = TRUE),PRECISION))}
        }
        
        # Si c'est la 1ère fois qu'on rencontre la colonne...
        if (column == column_names[[1]]) {
          
          # ... on construit un tableau avec 9 colonnes
          kable_output <- data.frame("Colonne"=column_name, "Min"=min, "Min1"=min1, "Max"=max, "Max1"=max1, "Mean"=mean,
                                     "Median"=median,"Ecart Type"=std,"CV"=cv)
          
        }else{
          
          # ... on ajoute une ligne dans le tableau
          new_row <- data.frame("Colonne"=column_name, "Min"=min, "Min1"=min1, "Max"=max, "Max1"=max1, "Mean"=mean,
                                "Median"=median,"Ecart Type"=std, "CV"=cv)
          
          kable_output <- rbind(kable_output,new_row)
        } #endif
        
      }else{
        cat("La liste des éléments statistiques à choisir pour le calcul des statistiques descriptives est vide.")
      } #endif
      
    } #endif
      
    if ((est_Nominale(column, table_name) == TRUE) && (is.date(table[[column]]))) {
      
      # Initialisation des colonnes à la valeur "" ou "NA".
      min1 <- max1 <- mean <- median <- std <- cv <- "NA"
      column_name <- min <- max <- ""
      
      if (length(parametres) != 0) {
        noms <- names(parametres[[1]])
        valeurs <- parametres[[1]]
        nb_parametres <- length(parametres[[1]])
        
        for (index in 1:nb_parametres ) {
          if ((noms[index] == "column_name") && (valeurs[index]==TRUE)) {column_name <- toupper(column)}
          if ((noms[index] == "min") && (valeurs[index]==TRUE)) {min <- as.character(min(format(table[[column]]),na.rm = TRUE))}
          if ((noms[index] == "max") && (valeurs[index]==TRUE)) {max <- as.character(max(format(table[[column]]),na.rm = TRUE))}
        }
        
        # Si c'est la 1ère fois qu'on rencontre la colonne...
        if (column == column_names[[1]]) {
          
          # ... on construit un tableau avec 9 colonnes
          kable_output <- data.frame("Colonne"=column_name, "Min"=min, "Min1"=min1, "Max"=max,"Max1"=max1,"Mean"=mean,
                                     "Median"=median, "Ecart Type"=std, "CV"=cv)
          
        }else{
          
          # ... on ajoute une ligne
          new_row <- data.frame("Colonne"=column_name, "Min"=min, "Min1"=min1, "Max"=max, "Max1"=max1, "Mean"=mean,
                                "Median"=median, "Ecart Type"=std, "CV"=cv)
          
          kable_output <- rbind(kable_output,new_row)
        }#endif
      } #endif
    } #endif
  } #endfor
  
  if (is.null(kable_output)) {
    
    texte_a_afficher <- paste0("Cette section est vide. \nLa table <", table_name, "> ne renferme aucune colonne étiquetée avec des variables continues.")
    pander(cat(texte_a_afficher))
    
  }else{
    
    kable(kable_output, align='lrrrrrrrr') %>%
      kable_styling(bootstrap_options = c("striped", "bordered", "hover", "condensed", "responsive", full_width=F, position="left", row_label_position="r")) %>% 
      row_spec(0, bold = T, color = "black", background = "#DDEBF7")
  }
}

# ---------------------------------------------------------------------------------------------------
#       Fonction : tris_a_plat(table)
# Développeur(s) : Nicolas ROBIN
#           Date : 26 janvier 2018.
#    Description : Cette fonction renvoie une table "kable" indiquant les modalités, et effectifs et 
#                  pourcentages de chacune colonnes renfermant des valeurs nominales.
#         Entrée : une table au format dataframe.
#         Sortie : NA.
# ---------------------------------------------------------------------------------------------------

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
    if ((est_Nominale(column, nom_de_la_table) == TRUE) && (nb_modalites <= NB_MODALITES_MAX)) {

      # Création d'une nouvelle table dans laquelle on affiche Modalités, Effectifs et Pourcentage.
      new_row <- data.frame("Modalités"=column, "Effectifs"="", "Pourcentage"="")
      kable_output <- rbind(kable_output,new_row)
      
      # On taggue les lignes de titre pour mettre le fond de la cellule du titre en bleu.
      index_highlight_monitoring <- index_highlight_monitoring + 1
      rows_to_highlight <- c(rows_to_highlight, index_highlight_monitoring)
      
      # On récupère les effectives et pourcentages de toutes les modalites que renferme la colonne.
      freq_table <- freq(table[[column]])[,1:2]
      names(freq_table)[1]<-"Effectifs"
      names(freq_table)[2]<-"Pourcentage"
      row_name_freq_table <- row.names(freq_table)
      
      # On ajoute autant de lignes dans le tableau kable qu'il y a de modalités.
      for (index in 1:nb_modalites) {
        new_row <- data.frame("Modalités"=row_name_freq_table[index], "Effectifs"=freq_table[index,1], "Pourcentage"=freq_table[index,2])
        kable_output <- rbind(kable_output,new_row)
        index_highlight_monitoring <- index_highlight_monitoring + 1
      }
      
      # On rajoute une ligne vide pour améliorer la visibilité du tableau si on doit afficher les modalités pour plusieurs colonnes.
      new_row <- data.frame("Modalités"=" ", "Effectifs"="", "Pourcentage"="")
      kable_output <- rbind(kable_output,new_row)
      index_highlight_monitoring <- index_highlight_monitoring + 1

    } #endif

  } #endfor
  
  kable_output[1,1] <- " "
  kable_output[1,2] <- " "
  kable_output[1,3] <- " "

  if (index_highlight_monitoring <= 2) {
    
    # Si on est ici, cela veut dire qu'il n'y a aucune modalité à afficher.
    texte_a_afficher <- paste0("Aucunes modalités affichées dans la table <", nom_de_la_table, ">. Il se peut que certaines colonnes n'aient pas été affichées du fait que le nombre de leurs modalités est supérieur à ", NB_MODALITES_MAX, " .")
    pander(cat(texte_a_afficher))
    
  }else{
    
    # On finalise l'affichage du tableau avec la fonction kable(...).
    kable(kable_output, align='lrr') %>%
      kable_styling(bootstrap_options = c("striped", "bordered", "hover", "condensed")) %>% 
      row_spec(rows_to_highlight, bold = T, color = "black", background = "#DDEBF7")
  }
}

