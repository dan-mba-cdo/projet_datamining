# ---------------------------------------------------------------------------------------
#      Fichier : Parametres.R
# Développeurs : Thomas FONTAINE, Dan GOLDMAN, Juliette LEMAINS, Nicolas ROBIN
#         Date : 28 décembre 2018.
#  Description : Ce script inclut :
#                 - Le chargement de l'ensemble des libraries nécessaires au projet.
#                 - La déclaration du répertoires de travail et des fichiers
#                 - Le chargement des fichiers d'entrée.
#
# ---------------------------------------------------------------------------------------

# Ligne de test pour la compatibilité des caractères accentués dans un environnement différent : àéèëêEÉÊiïÎôöùüœπß$£€...

# La variable booléene "utilise_des_extraits_de_fichier" permet de sélectionner des extraits de fichiers
# plutôt que les fichiers d'entrée volumineux du projet. Cela permet de travailler sur des machines moins véloces.
# Mettre la valeur à TRUE lorsque l'on désire travailler avec des fichiers plus petits.
# Ne pas oublier de mettre cette valeur à FALSE lorsque le projet est finalisé.
utilise_des_extraits_de_fichier <- FALSE

# Boolean qui modifie le comportement de l'application si on l'exécute depuis R-Markdown ou depuis le fichier "Projet.R".
execution_avec_RMarkdown <- TRUE

# Declaration de la variable pour l'annee en cours 
ANNEE_EN_COURS <- 2018

# Définition du nombre maximal pour l'affichage des modalités.
NB_MODALITES_MAX = 20

# Definition de la precision quand un arrondi est effectué.
PRECISION = 3

# Chemins de dossier des fichiers de donnée respectifs dans le cas où on n'utilise pas la boite de dialogue.
chemin_dossier_donnees_Thomas <- "C:/Users/timti/Documents/R/Rproject/DATA/"
chemin_dossier_donnees_Dan <- "C:/Users/dgoldman/Desktop/MBA - BIG DATA CDO/DataMining/projet transverse R/Projet R a rendre/DATA/"
chemin_dossier_donnees_Juliette <- "C:/Users/Juliette/Rproject/DATA_UTF-8/"
chemin_dossier_donnees_Nicolas <- "/Users/nrobin/Documents/GitHub/projet_datamining/"

# SELECTION DU CHEMIN DE DOSSIER DES FICHIERS DE DONNEES PAR DEFAUT.
# Pour sélectionner le dossier des fichiers de donnée avec la boite de dialogue :
# - Soit on affecte une chaine de caratère vide dans la variable "chemin_dossier_donnees_courant"
#   Cela signifie qu'on lance l'exécution du programme à partir du fichier "Projet.R"
# - Soit on renseigne la variable qui contient la chaîne de caractères avec le chemin correspondant 
#   à son environnement de travail.
#   Dans ce cas cela signifie qu'on lance l'exécution du programme à partir du fichier "Projet.Rmd"
if (execution_avec_RMarkdown == TRUE) {
  chemin_dossier_donnees_par_default <- chemin_dossier_donnees_Nicolas
}else{
  # Le dossier sera sélectionné par l'utilisateur lors de l'exécution du programme lors
  # de l'appel de la fonction cheminDossierFichiersDonnees()
  chemin_dossier_donnees_par_default <- ""
}

# Fonction "chargementDesLibraries".
# Cette fonction charge l'ensemble des libraries utiles au projet.
# Elles sont automatiquement sélectionnées et installées dans le cas où elles sont manquantes.
chargementDesLibraries <- function() {
  
  libraries_utilies <- c('assertthat', 'data.table', 'dplyr', 'DT', 'dygraphs','flextable', 'formattable', 'ggplot2', 'grid', 'gridExtra', 'knitr', 'kableExtra', 'pander', 'plotly', 'questionr', 'rAmCharts', 'stringr', 'svDialogs', 'tidyverse', 'tinytex','bit64')
  
  for (package in libraries_utilies) {
    if (!require(package, character.only=T, quietly=T)) {
      install.packages(package, repos = "https://cran.rstudio.com")
      library(package, character.only=T)
    }
  } 
}

# On charge l'ensemble des libraries utiles au projet.
chargementDesLibraries()

# Fonction "cheminDossierFichiersDonnees".
# Cette fonction renvoie le chemin du dossier où se trouve les fichiers de données d'entrée.
# Si aucun dossier n'est sélectionné, la fonction retourne "NA".
cheminDossierFichiersDonnees <- function(chemin_dossier_par_defaut = "") {
  
  valeur_a_retourner <- ""
  
  # Declaration du chemin du dossier dans lequel se trouve les fichiers de donnees.
  if (chemin_dossier_par_defaut == "") {
    
    chemin <- dlg_dir(default = getwd(), title = "Selection du dossier des fichiers d'entree")$res
    
    if (identical(chemin, character(0))) {
      
      valeur_a_retourner <- chemin_dossier_par_defaut
      
    }else{
      
      # Ajout d'un "/" pour le dossier sélectionné par l'utilisateur.
      valeur_a_retourner <- paste0(chemin, "/")
    }
    
  }else{
    valeur_a_retourner <- chemin_dossier_par_defaut
  }
  
  # Si la valeur de "chemin_dossier_donnees" n'a pas été renseignée, on affiche un message d'erreur.
  if (valeur_a_retourner == "") {
    dlg_message("Le dossier des fichiers d'entree n'a pas ete choisi. Les tables ne peuvent pas être chargees.", "ok", gui = .GUI)
    valeur_a_retourner = NA
  }
  
  return(valeur_a_retourner)
}

# Affectation de la variable globale "chemin_dossier_donnees".
# Cette valeur indique le chemin du dossier dans lequel se trouvent tous les fichiers de donnée.
chemin_dossier_donnees_selectionne <- cheminDossierFichiersDonnees(chemin_dossier_donnees_par_default)

# La liste des fichiers de donnees necessaires au projet.
nom_fichier_INSEE <- "correspondance-code-insee-code-postal.csv"
nom_fichier_articles <- "REF_ARTICLE.CSV"
nom_fichier_magasins <- "REF_MAGASIN.CSV"

if (utilise_des_extraits_de_fichier == TRUE) {
  nom_fichier_clients <- "SUBCLIENT.CSV"
}else{
  nom_fichier_clients <- "CLIENT.CSV"
}

if (utilise_des_extraits_de_fichier == TRUE) {
  nom_fichier_entetes <- "SUBENTETE.CSV"
}else{
  nom_fichier_entetes <- "ENTETES_TICKET_V4.CSV"
}

if (utilise_des_extraits_de_fichier == TRUE) {
  nom_fichier_lignes <- "SUBLIGNES.CSV"
}else{
  nom_fichier_lignes <- "LIGNES_TICKET_V4.CSV"
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableArticles"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableArticles <- function(chemin_dossier) {
  
  tableArticles <- NULL
  
  if (!is.na(chemin_dossier)) {
    
    # Information pour la console
    cat("Chargement de la table Articles...","\n")

    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier, nom_fichier_articles)

    # Chargement du fichier articles
    tableArticles <- fread(chemin_fichier, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    
    # Cette ligne de code retire la ligne COUPON.
    # Faut-il la retirer ou pas ? N'aura-t-on pas besoin de cette information plus tard ?
    # tableArticles <- tableArticles[-c(1),]
  }
  
  returnValue(tableArticles)
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableMagasins"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableMagasins <- function(chemin_dossier) {
  
  tableMagasins <- NULL
  
  if (!is.na(chemin_dossier)) {
    
    # Information pour la console
    cat("Chargement de la table Magasins...","\n")
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier, nom_fichier_magasins)
    
    # Chargement du fichier magasins
    tableMagasins <- fread(chemin_fichier, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    
  }

  returnValue(tableMagasins)
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableClients"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableClients <- function(chemin_dossier) {
  
  tableClients <- NULL
  
  if (!is.na(chemin_dossier)) {
    
    # Information pour la console
    cat("Chargement de la table Clients","\n")
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier, nom_fichier_clients)
    
    # Chargement du fichier clients
    tableClients <- fread(chemin_fichier, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    tableClients[tableClients == ""] <- NA
    
  }
  
  returnValue(tableClients)
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableEntetes"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableEntetes <- function(chemin_dossier) {
  
  tableEntetes <- NULL
  
  if (!is.na(chemin_dossier)) {
  
    # Information pour la console
    cat("Chargement de la table Entetes","\n")
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier, nom_fichier_entetes)
    
    # Chargement du fichier entêtes
    tableEntetes <- fread(chemin_fichier, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    
  }
  
  returnValue(tableEntetes)
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableLignes"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableLignes <- function(chemin_dossier) {
  
  tableLignes <- NULL
  
  if (!is.na(chemin_dossier)) {
    
    # Information pour la console
    cat("Chargement de la table Lignes...","\n")
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier, nom_fichier_lignes)
    
    # Chargement du fichier lignes
    tableLignes <- fread(chemin_fichier, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    
  }
  
  returnValue(tableLignes)
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableInsee"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableInsee <- function(chemin_dossier) {
  
  tableInsee <- NULL
  
  if (!is.na(chemin_dossier)) {
    
    # Information pour la console
    cat("Chargement de la table Insee...","\n")
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier, nom_fichier_INSEE)
    
    # Chargement du fichier code insee code postal.
    tableInsee <- fread(chemin_fichier, sep=";", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    
  }
  
  returnValue(tableInsee)
}

# Chargement des 5 tables à utiliser en entrée, plus la table Insee.
# Les données peuvent maintenant être traitées pour chacun dees exercices du projet.
articles <- chargementTableArticles(chemin_dossier_donnees_selectionne)
magasins <- chargementTableMagasins(chemin_dossier_donnees_selectionne)
clients <- chargementTableClients(chemin_dossier_donnees_selectionne)
entetes <- chargementTableEntetes(chemin_dossier_donnees_selectionne)
lignes <- chargementTableLignes(chemin_dossier_donnees_selectionne)
insee <- chargementTableInsee(chemin_dossier_donnees_selectionne)

# Table réduite.
table_finale<-data.table(nb_client=770163, nb_entete_ticket=6713822)

# Data Frames qui indiquent la structure de chacune des tables à analyser.
# On taggue chaque colonne, avec l'étiquette :
# 1) "I" --> si la valeur est un identifiant.
# 2) "N" --> si la valeur est nominale.
# 3) "C" --> si la valeur est continue.
table_articles_column_nature <- data.frame("CODEARTICLE"="I", "CODEUNIVERS"="N", "CODEFAMILLE"="N", "CODESOUSFAMILLE"="N")
table_clients_column_nature <- data.frame("IDCLIENT"="I", "CIVILITE"="N", "DATENAISSANCE"="N", "MAGASIN"="N", "DATEDEBUTADHESION"="N", "DATEREADHESION"="N", "DATEFINADHESION"="N", "VIP"="N", "CODEINSEE"="N", "PAYS"="N")
table_entetes_column_nature <- data.frame("IDTICKET"="I", "TIC_DATE"="N", "MAG_CODE"="N", "IDCLIENT"="I", "TIC_TOTALTTC"="C")
table_lignes_column_nature <- data.frame("IDTICKET"="I", "NUMLIGNETICKET"="N", "IDARTICLE"="I", "QUANTITE"="C", "MONTANTREMISE"="C", "TOTAL"="C", "MARGESORTIE"="C")
table_magasins_column_nature <- data.frame("CODESOCIETE"="I", "VILLE"="N", "LIBELLEDEPARTEMENT"="N", "LIBELLEREGIONCOMMERCIALE"="N")

# C'est ici que l'on choisit d'afficher telle ou telle statistique pour chacune des tables sur lesquelles on travaille.
options_affichage_stats_articles <- list(column_name=TRUE, min=TRUE, min1=TRUE, max=TRUE, max1=TRUE, mean=TRUE, median=TRUE, std=TRUE, cv=TRUE)
options_affichage_stats_clients <- list(column_name=TRUE, min=TRUE, min1=TRUE, max=TRUE, max1=TRUE, mean=TRUE, median=TRUE, std=TRUE, cv=TRUE)
options_affichage_stats_entetes <- list(column_name=TRUE, min=TRUE, min1=TRUE, max=TRUE, max1=TRUE, mean=TRUE, median=TRUE, std=TRUE, cv=TRUE)
options_affichage_stats_lignes <- list(column_name=TRUE, min=TRUE, min1=TRUE, max=TRUE, max1=TRUE, mean=TRUE, median=TRUE, std=TRUE, cv=TRUE)
options_affichage_stats_magasins <- list(column_name=TRUE, min=TRUE, min1=TRUE, max=TRUE, max1=TRUE, mean=TRUE, median=TRUE, std=TRUE, cv=TRUE)
