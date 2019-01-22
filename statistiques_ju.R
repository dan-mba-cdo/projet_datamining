library(dplyr)
library(data.table)
library(readxl)
library(assertthat)
library(xlsx)
library(questionr)

#Import des donnees
articles <- fread("D:/Documents/Juh/MBA/MBA/Data Projet/REF_ARTICLE.CSV",nrows=500)
#Ajout de cette article qui apparait dans la table tiquet mais qui 
#n'apparait pas dans la table articles
newarticle <- data.frame(CODEARTICLE='395460',CODEUNIVERS='unknown',
                         CODEFAMILLE='unknown', CODESOUSFAMILLE='unknown')
articles<- rbind(articles,newarticle)

magasin <- fread("D:/Documents/Juh/MBA/MBA/Data Projet/REF_MAGASIN.CSV")

clients <- fread("D:/Documents/Juh/MBA/MBA/Data Projet/CLIENT.CSV")
clients[clients == ""] <- NA

entete <- fread("D:/Documents/Juh/MBA/MBA/Data Projet/ENTETES_TICKET_V4.CSV",nrows=500,dec=",")

lignes <- fread("D:/Documents/Juh/MBA/MBA/Data Projet/LIGNES_TICKET_V4.CSV",nrows=500,dec=",")

#Statistiques descriptives

#CLIENTS
freq(clients$CIVILITE)


summarize(clients$DATENAISSANCE)




