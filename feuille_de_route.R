setwd("C:/Users/dgoldman/Desktop/MBA - BIG DATA CDO/DataMining/projet transverse R/projet regression logistique")

getwd()

library(dplyr)
library(data.table)
library(readxl)
library(assertthat)
library(xlsx)
library(stringr)
library(rAmCharts)
library(formattable)
library(RSQLite)
library(proto)
library(gsubfn)
library(sqldf)
library(ggplot2)
library(scales)
memory.limit(size = 25000)

#### ETAPE 0 #####
### PREPARATION DES DONNEES ET DU PERIMETRE ######
#1 - supprimer les doublons de la table client
#analyse des doublons
Doublon_ligne_ticket <- sqldf("SELECT IDTICKET,NUMLIGNETICKET,IDARTICLE,QUANTITE, MONTANTREMISE, TOTAL, MARGESORTIE
                              ,COUNT(*) AS DOUBLON
                              
                              FROM lignes
                              
                              GROUP BY
                              IDTICKET,NUMLIGNETICKET,IDARTICLE,QUANTITE, MONTANTREMISE, TOTAL, MARGESORTIE having count(*) > 1 ")


#dedoublonnage
lignes_wrk<-lignes[,.(nb=.N), by=c("IDTICKET","NUMLIGNETICKET","IDARTICLE","QUANTITE", "MONTANTREMISE", "TOTAL", "MARGESORTIE")]
lignes_wrk<-lignes_wrk%>% select(-nb)

#exclure les lignes tickets dont la remise est negative
lignes_wrk<-lignes_wrk[MONTANTREMISE>=0.000,]

#jointure des clients avec les entetes tickets
client_entete_wrk<-merge(clients,entetes, by="IDCLIENT", all=FALSE)

#suppression des clients qui ont achet? dans un autre magasin pour lequel ils ne sont pas adherant
setDT(client_entete_wrk)
client_entete_wrk<-client_entete_wrk[MAGASIN==MAG_CODE,]


##montant remise superieur ? 0
#regroupement par IDTTICKET
remise_oui<-lignes_wrk[MONTANTREMISE>0.000,.(
  TOTAL_O=sum(TOTAL),
  REMISE_O=sum(MONTANTREMISE),
  MARGE_O=sum(MARGESORTIE),
  QTE_O=sum(QUANTITE),
  NB_ARTICLE_O=.N
),
by=.(IDTICKET)]

##montant remise egal ? 0
#regroupement par IDTICKET
remise_non<-lignes_wrk[MONTANTREMISE==0.000,.(
  TOTAL_N=sum(TOTAL),
  REMISE_N=sum(MONTANTREMISE),
  MARGE_N=sum(MARGESORTIE),
  QTE_N=sum(QUANTITE),
  NB_ARTICLE_N=.N
),
by=.(IDTICKET)]



#regrouprement des clients avec remise
client_remise<-merge(client_entete_wrk,remise_oui, by="IDTICKET", all.x=TRUE)
client_remise<-merge(client_remise,remise_non, by="IDTICKET", all.x=TRUE)
#garder uniquement les clients qui ont eu ? la fois des remises et pas de remises
client_remise<-client_remise[is.na(TOTAL_O)==FALSE & is.na(TOTAL_N)==FALSE,]


#agregation des indicateurs rergoup? par client
perimetre_client_remise<-client_remise[, .(
  TOTAL_CA_TTC=sum(TIC_TOTALTTC),
  TOTAL_O=sum(TOTAL_O),
  REMISE_O=sum(REMISE_O),
  MARGE_O=sum(MARGE_O),
  QTE_O=sum(QTE_O),
  NB_ARTICLE_O=sum(NB_ARTICLE_O),
  TOTAL_N=sum(TOTAL_N),
  REMISE_N=sum(REMISE_N),
  MARGE_N=sum(MARGE_N),
  QTE_N=sum(QTE_N),
  NB_ARTICLE_N=sum(NB_ARTICLE_N)),
  by=.(IDCLIENT)]


perimetre_client_remise_not_OUT <- subset(perimetre_client_remise,(TOTAL_CA_TTC>quantile(perimetre_client_remise$TOTAL_CA_TTC,c(0.01)))&
                              (TOTAL_CA_TTC<quantile(perimetre_client_remise$TOTAL_CA_TTC,c(0.99))))


#ajout des flag permettant de comparer les indicateurs avec remises et sans remises
perimetre_client_remise_not_OUT$COMP_MARGE <- ifelse(perimetre_client_remise_not_OUT$MARGE_N<perimetre_client_remise_not_OUT$MARGE_O,1,0)
perimetre_client_remise_not_OUT$COMP_TOTAL <- ifelse(perimetre_client_remise_not_OUT$TOTAL_N<perimetre_client_remise_not_OUT$TOTAL_O,1,0)
perimetre_client_remise_not_OUT$COMP_QTE <- ifelse(perimetre_client_remise_not_OUT$QTE_N<perimetre_client_remise_not_OUT$QTE_O,1,0)
perimetre_client_remise_not_OUT$COMP_NB_ARTICLE <- ifelse(perimetre_client_remise_not_OUT$NB_ARTICLE_N<perimetre_client_remise_not_OUT$NB_ARTICLE_O,1,0)


###### ETAPE 1 #########
######RECUPERER LA TABLE perimetre_client_remise_not_OUT #########

prop.table(table(perimetre_client_remise_not_OUT$COMP_MARGE))
prop.table(table(perimetre_client_remise_not_OUT$COMP_TOTAL))
prop.table(table(perimetre_client_remise_not_OUT$COMP_QTE))

#ne recupere que l'IDCLIENT et les flag de la table perimetre_client_remise
perimetre_client_remise2<-perimetre_client_remise%>%select(IDCLIENT,COMP_MARGE,COMP_TOTAL,COMP_QTE,COMP_NB_ACHAT)


#initialisation de la table datamining client qui sera notre table pour la regression logistique
datamining_client<-perimetre_client_remise_not_OUT%>%select(IDCLIENT)
setDT(datamining_client)


###### ETAPE 2 ######
###### CREER LES VARIABLES EXPLICATIVES ########

### VARIABLE civilité #########
#normalisation de la civilite.
clients_r<-clients%>%mutate(CIVILITE_r=recode(`CIVILITE`,
                                                    "MADAME" = "femme",
                                                    "Mme" = "femme",
                                                    "madame" = "femme",
                                                    "monsieur" = "homme",
                                                    "Mr" = "homme",
                                                    "MONSIEUR" = "homme"))

datamining_client<-merge(datamining_client,clients_r, by="IDCLIENT", all.x=TRUE)

datamining_client<-datamining_client%>%select(IDCLIENT,CIVILITE_r)


### VARIABLE tranche d'age #########
# Suppression des lignes clients sans date de naissance.
clients_r<- subset(clients_r, !is.na(DATENAISSANCE))
# creation d'une colonne age.
clients_r$age <- 2018 - as.numeric(format(as.Date(clients_r$DATENAISSANCE, tryFormats = c("%d/%m/%Y")),"%Y"))
# Suppression des clients agees de plus de 98ans.
clients_r<- subset(clients_r, age < 99 ) 
# Suppression des clients agees de moins de 18ans.
clients_r<- subset(clients_r, age > 17 ) 
# la population etudier sera donc les personnes qui ont renseiger leurs age et qui on actuellement entre 
# 18 et 98 ans (inclus).
# Creation des groupe client suivant leurs ages. 8 groupes de 18 � 98 ans.
clients_r$age_group <- cut(clients_r$age,seq(18,98,10), include.lowest= TRUE, right = FALSE)

datamining_client<-merge(datamining_client,clients_r, by="IDCLIENT", all.x=TRUE)

datamining_client<-datamining_client%>%select(IDCLIENT,CIVILITE_r.x, age, age_group)

### variable categorie de client ####
#VIP : client etant VIP (VIP = 1)
VIP <- clients[clients$VIP==1,]
setDT(VIP)
VIP<-VIP[,CAT_CLIENT:='VIP']
VIP<-VIP%>%select(IDCLIENT,CAT_CLIENT)

#NEW_N2 : client ayant adhere au cours de l'annee N-2 (date debut adhesion)
NEW_N2 <-clients[(clients$VIP!=1)&
                        (format(as.Date(clients$DATEDEBUTADHESION, tryFormats = c("%d/%m/%Y")),"%Y")==2016),]
setDT(NEW_N2)
NEW_N2<-NEW_N2[,CAT_CLIENT:='NEW_N2']
NEW_N2<-NEW_N2%>%select(IDCLIENT,CAT_CLIENT)

#NEW_N1 : client ayant adhere au cours de l'annee N-1 (date debut adhesion)
NEW_N1 <-clients[(clients$VIP!=1)&
                   (format(as.Date(clients$DATEDEBUTADHESION, tryFormats = c("%d/%m/%Y")),"%Y")==2017),]
setDT(NEW_N1)
NEW_N1<-NEW_N1[,CAT_CLIENT:='NEW_N1']
NEW_N1<-NEW_N1%>%select(IDCLIENT,CAT_CLIENT)

#ADHERANT : client toujours en cours d'adhesion (date de fin d'adhesion > 2018/01/01)
ADHERANT <-clients[(clients$VIP!=1)&
                          format(as.Date(clients$DATEDEBUTADHESION, tryFormats = c("%d/%m/%Y")),"%Y")<2016&
                          (format(as.Date(clients$DATEFINADHESION, tryFormats = c("%d/%m/%Y")),"%Y-%m-%d"))>as.Date("2018-01-01"),]
setDT(ADHERANT)
ADHERANT<-ADHERANT[,CAT_CLIENT:='ADHERANT']
ADHERANT<-ADHERANT%>%select(IDCLIENT,CAT_CLIENT)


#CHURNER : client ayant churner (date de fin d'adhesion < 2018/01/01)
CHURNER <-clients[((format(as.Date(clients$DATEFINADHESION, tryFormats = c("%d/%m/%Y")),"%Y-%m-%d"))<as.Date("2018-01-01"))&
                         (clients$VIP!=1)&
                         format(as.Date(clients$DATEDEBUTADHESION, tryFormats = c("%d/%m/%Y")),"%Y")<2016,]
setDT(CHURNER)
CHURNER<-CHURNER[,CAT_CLIENT:='CHURNER']
CHURNER<-CHURNER%>%select(IDCLIENT,CAT_CLIENT)

CAT_CLIENT <- rbind(VIP,NEW_N2,NEW_N1,ADHERANT,CHURNER)


datamining_client<-merge(datamining_client,CAT_CLIENT, by="IDCLIENT", all.x=TRUE)



### variable distance client magasin ####
toRadians <- function (degree)
{
  return (degree * pi / 180)
}
distanceGeo <- function(lat1, lon1, lat2, lon2) 
{
  rayonTerre <- 6371e3
  phi1 <- toRadians(lat1)
  phi2 <- toRadians(lat2)
  deltaPhi <- toRadians(lat2-lat1)
  deltaLamba <- toRadians(lon2-lon1)
  a <- (sin(deltaPhi/2)^2) + cos(phi1) * cos(phi2) * (sin(deltaLamba/2)^2)
  unMoinsA <- 1 - a
  c <- (2 * atan2(sqrt(a),sqrt(unMoinsA)))
  
  distanceCalculée <- rayonTerre * c / 1000
  
  return(distanceCalculée)
}

distance_Client_Magasin <- function(table_insee, table_magasins, table_clients) {
  
  #------------------------------------------------------------------------------------
  # ETAPE 2a - CONSTRUCTION ET PREPARATION DE LA "TABLE_DE_TRAVAIL"
  #------------------------------------------------------------------------------------
  # Cette table est une version aménagée et simplifiée de la table de correspondance de l'INSEE.
  
  # On récupère uniquement les 4 colonnes qui nous intéressent :
  # - CODEINSEE, Code Postal, Commune et geo_point_2d.
  # TABLE_DE_TRAVAIL <- table_insee[, list('Code INSEE', 'Code Postal', Commune, geo_point_2d)]
  TABLE_DE_TRAVAIL <- table_insee[, c(1,2,3, 10)]
  
  # Pour pouvoir faire les jointures avec les tables magasinss et clients :
  # 2a1 - on renomme la colonne "Code INSEE" en "CODEINSEE".
  setnames(TABLE_DE_TRAVAIL, old=c("Code INSEE"), new=c("CODEINSEE"))
  # 2a2 - on renomme la colonne "Code Postal" en "NUMDEPT".
  setnames(TABLE_DE_TRAVAIL, old=c("Code Postal"), new=c("NUMDEPT"))
  # 2a3 - on renomme la colonne "Commune" en "VILLE".
  setnames(TABLE_DE_TRAVAIL, old=c("Commune"), new=c("VILLE"))
  # 2a4 - On "splite" les informations contenues dans geo_point_2d et on les insère dans les 2 colonnes "LATITUDE" et "LONGITUDE".
  TABLE_DE_TRAVAIL[, c("LATITUDE", "LONGITUDE") := tstrsplit(geo_point_2d, ", ", fixed=TRUE)]
  TABLE_DE_TRAVAIL[,geo_point_2d:=NULL] # on élimine la colonne "geo_point_2d" devenue inutile.
  # On tronque le numéro de code postal pour obtenir le numéro de département qui nous permmettra
  # plus tard de discriminer plusieurs communes françaises avec le même nom.
  TABLE_DE_TRAVAIL[,NUMDEPT:=str_trunc(NUMDEPT, 2, "right", "")]
  TABLE_DE_TRAVAIL[,NUMDEPT :=as.integer(NUMDEPT)] 
  
  #------------------------------------------------------------------------------------
  # ETAPE 2b - PHASE DE TRAITEMENT DES DONNEES - RETABLISSEMENT DE LA COHERENCE
  #------------------------------------------------------------------------------------
  # Cette phase permet de rétablir la cohérence entre les tables 
  # TABLE_DE_TRAVAIL, magasins et clients.
  
  # On retire tous les "-" entre les mots des villes afin de ramener
  # de la cohérence dans le champ VILLE entre la table de travail et celui du fichier magasins.
  TABLE_DE_TRAVAIL[,VILLE:=str_replace_all(VILLE, pattern = "-", replacement = " ")]
  
  # On retire tous les "-" entre les mots des villes afin de ramener
  # de la cohérence avec les villes du fichier magasins.
  magasins[, VILLE:=str_replace_all(VILLE, pattern = "-", replacement = " ")]
  
  # On remplace tous les "ST" par "SAINT", toujours pour amener de la cohérence
  # avec les villes du fichier CODE INSEE et le fichier magasins.
  magasins[, VILLE:=str_replace(VILLE, pattern = "^[S-T,s-t][S-T,s-t][ ]", replacement = "SAINT ")]
  
  # On retire le mot "CEDEX" trouvé dans certains noms de villes.
  magasins[, VILLE:=str_replace(VILLE, pattern = " CEDEX", replacement = " ")]
  magasins[, VILLE:=str_replace(VILLE, pattern = "^\\s+|\\s+$", replacement = "")]
  
  # Dans l'objectif de pouvoir faire une jointure entre la TABLE_DE_TRAVAIL et la TABLE magasins
  #setnames(magasins, old=c("CODESOCIETE"), new=c("MAGASINS"))
  #setnames(magasins, old=c("LIBELLEDEPARTEMENT"), new=c("NUMDEPT"))
  
  # Dans l'objectif de pouvoir faire une jointure entre la TABLE_DE_TRAVAIL et la TABLE magasins
  #setnames(clients, old=c("MAGASIN"), new=c("MAGASINS"))
  
  #------------------------------------------------------------------------------------
  # ETAPE 3 - JOINTURE ENTRE LES TABLES "MAGASINS" et "TABLE DE TRAVAIL"
  #------------------------------------------------------------------------------------
  
  # Création d'une table avec jointure entre la table MAGASINS et la TABLE DE TRAVAIL
  jointureGeoMagasins <- table_magasins %>% 
    left_join(TABLE_DE_TRAVAIL, by = c("VILLE", "NUMDEPT")) %>% 
    select(MAGASINS, NUMDEPT, VILLE, LATITUDE, LONGITUDE)
  
  #------------------------------------------------------------------------------------
  # ETAPE 3a - PREPARATION DE LA TABLE jointureGeoMagasins
  #------------------------------------------------------------------------------------
  
  #Transformation de la table en DATA TABLE
  setDT(jointureGeoMagasins)
  
  # Renommage des colonnes
  setnames(jointureGeoMagasins, old=c("LATITUDE"), new=c("LATITUDEMAG"))
  setnames(jointureGeoMagasins, old=c("LONGITUDE"), new=c("LONGITUDEMAG"))
  
  # La ville "LES MILLES" dans La table MAGASINS n'est pas répertoriée dans le
  # Tableau INSEE. Nous avons rajouté l'information "manuellement", 
  # puisque c'était la seule ville qui n'était pas répertoriée.
  jointureGeoMagasins[VILLE=="LES MILLES", LATITUDEMAG := "43.5042670000"]
  jointureGeoMagasins[VILLE=="LES MILLES", LONGITUDEMAG := "5.3916980000"]
  
  #------------------------------------------------------------------------------------
  # ETAPE 4 - JOINTURE ENTRE LES TABLES "CLIENTS" et "TABLE DE TRAVAIL"
  #------------------------------------------------------------------------------------
  
  # Création d'une table avec jointure entre la table CLIENTS et la TABLE DE TRAVAIL
  positionGeoClients <- table_clients %>% 
    left_join(TABLE_DE_TRAVAIL, by = c("CODEINSEE")) %>% 
    select(IDCLIENT, CODEINSEE, VILLE, MAGASINS, LATITUDE, LONGITUDE)
  
  #Transformation de la table en DATA TABLE
  setDT(positionGeoClients)
  
  # Renommage des colonnes
  setnames(positionGeoClients, old=c("LATITUDE"), new=c("LATITUDECLIENT"))
  setnames(positionGeoClients, old=c("LONGITUDE"), new=c("LONGITUDECLIENT"))
  
  #------------------------------------------------------------------------------------
  # ETAPE 5 - JOINTURE ENTRE LES TABLES "jointureGeoMagasins" et "jointureGeoClients"
  #------------------------------------------------------------------------------------
  
  # Creation d'une table avec jointure entre la table jointureGeoMagasins" et la "jointureGeoClients"
  # Cette table enfin finalisée va permettre de calculer la distance client <-> magasin pour chacun
  # des clients de la table clients.
  jointureGeoMagasinsClients <- positionGeoClients %>% 
    left_join(jointureGeoMagasins, by = c("MAGASINS")) %>% 
    select(IDCLIENT, LATITUDECLIENT, LONGITUDECLIENT, MAGASINS, LATITUDEMAG, LONGITUDEMAG)
  
  #Transformation de la table en DATA TABLE
  setDT(jointureGeoMagasinsClients)
  
  # Les paramètres de latitude et de longitude étaient pour l'instant disponibles en tant
  # que chaînes de caractères. Il convient donc de convertir chacun d'eux en valeur décimale
  # avant de lancer le calcul de la distance entre la ville du client et celle du magasin.
  jointureGeoMagasinsClients[,LATITUDECLIENT :=as.double(LATITUDECLIENT)] 
  jointureGeoMagasinsClients[,LONGITUDECLIENT :=as.double(LONGITUDECLIENT)] 
  jointureGeoMagasinsClients[,LATITUDEMAG :=as.double(LATITUDEMAG)] 
  jointureGeoMagasinsClients[,LONGITUDEMAG :=as.double(LONGITUDEMAG)] 
  
  #--------------------------------------------------------------------------------------------
  # ETAPE 6 - AFFECTATION DE LA DISTANCE AVEC LA FONCTION distanceGeo(lat1, long1, lat2, long2)
  #--------------------------------------------------------------------------------------------
  
  # C'est ici que l'on calcule la distance entre les 2 localités.
  jointureGeoMagasinsClients[,DISTANCE_CLIENT_magasins
                             :=distanceGeo(LATITUDECLIENT, LONGITUDECLIENT, LATITUDEMAG, LONGITUDEMAG)]
  
  #------------------------------------------------------------------------------------
  # Ensuite j'aurais aimé utiliser la fonction retourneValeurBorne(distance) qui renvoie une valeur bornée
  # sous forme de chaînes de caractères, mais la fonction ne fonctionne pas correctement lorsqu'on l'utilise
  # avec la library data.table.
  # jointureGeoMagasinsClients[,BORNE_DISTANCE:=retourneValeurBorne(DISTANCE_CLIENT_magasins)]
  #------------------------------------------------------------------------------------
  # A la place j'ai utilisé cette stratégie.
  # Cela fonctionne, mais c'est beaucoup moins élégant !
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins > 50), BORNE_DISTANCE:="plus de 50km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins <= 50), BORNE_DISTANCE:="20 à 50km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 20), BORNE_DISTANCE:="10 à 20km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 10), BORNE_DISTANCE:="5 à 10km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 5), BORNE_DISTANCE:="0 à 5km"]
  
  jointureGeoMagasinsClients<-jointureGeoMagasinsClients%>%select(IDCLIENT,BORNE_DISTANCE)
}
distance_cli_mag<-distance_Client_Magasin(insee, magasins, clients)

datamining_client<-merge(datamining_client,distance_cli_mag, by="IDCLIENT", all.x=TRUE)



#RFM client

#recence -- date de dernier achat par rapport au 01/01/2018
setDT(client_entete_wrk)
client_entete_wrk[,TIC_DATE2:= as.Date(client_entete_wrk$TIC_DATE)]
recence<-client_entete_wrk[,.(last_achat=max(TIC_DATE2)), by=IDCLIENT]
N<-as.Date("2018-01-01")
recence<-recence[,recence:=as.numeric(difftime(N,last_achat, units = "days"))]

#Montant
montant<-client_entete_wrk[,.(MTT_TOTAL=sum(TIC_TOTALTTC), PANIER_MOY=mean(TIC_TOTALTTC), NB_ACHAT=.N), by=IDCLIENT]

#frequence
#moyenne en nb de jours d'acte d'achat entre le 1er achat et le dernier achat
frequence<-client_entete_wrk[,.(max_achat=max(TIC_DATE2), min_achat=min(TIC_DATE2), NB_ACHAT=.N), by=IDCLIENT]
frequence<-frequence[,delai:=as.numeric(difftime(max_achat,min_achat, units = "days"))]


#que fait on des clients qui ont fait plusieurs achats dans la même journée ?
#on considere qu'ils n'ont fait qu'un achat
frequence2<-frequence[delai==0, NB_ACHAT:=1]
frequence2<-frequence2[delai==0, delai:=1]
frequence2<-frequence2[,freq:=ceiling(delai/NB_ACHAT)]

rfm<-merge(recence,montant, by="IDCLIENT", all=FALSE)
rfm<-merge(rfm,frequence2, by="IDCLIENT", all=FALSE)

colnames(rfm)
rfm2<-rfm%>%select(IDCLIENT,recence,delai, freq, MTT_TOTAL,PANIER_MOY,NB_ACHAT.x)
rfm2<-rename(rfm2, NB_ACHAT = NB_ACHAT.x)

clients[is.na(CODEINSEE)==TRUE, .(n=.N)]

analyse_rfm<-summary(rfm2)

write.csv(rfm2, file = "C:/Users/dgoldman/Desktop/rfm.csv")
