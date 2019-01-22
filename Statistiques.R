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

source("Parametres.R")


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
        cat("Fr?quence de modalit?s :",'\n')
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
      cat("Variable repr?sentant un identifiant",'\n\n')
    }
  }
  
}











