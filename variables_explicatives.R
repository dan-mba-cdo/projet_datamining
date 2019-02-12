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

### VARIABLE FLAG REHADHESION #########
clients_r$En_re_adhesion <- ifelse(is.na(clients_r$DATEREADHESION),0,1)

### VARIABLE DATE FIN VALIDITE ADHESION#########
clients_r<- clients_r%>%mutate(annee_finadhesion=
                                                as.numeric(format(as.Date(clients_r$DATEFINADHESION, tryFormats = c("%d/%m/%Y")),"%Y")))
clients_r$En_fin_adhesion <- ifelse(clients_r$annee_finadhesion==2100,0,1)

datamining_client<-merge(datamining_client,clients_r, by="IDCLIENT", all.x=TRUE)


### VARIABLE tranche d'age #########
# Suppression des lignes clients sans date de naissance.
clients_r2<- subset(clients_r, !is.na(DATENAISSANCE))
setDT(clients_r2)
# creation d'une colonne age.
clients_r2$age <- 2018 - as.numeric(format(as.Date(clients_r2$DATENAISSANCE, tryFormats = c("%d/%m/%Y")),"%Y"))
# Suppression des clients agees de plus de 98ans.
clients_r2<- subset(clients_r2, age < 99 ) 
# Suppression des clients agees de moins de 18ans.
clients_r2<- subset(clients_r2, age > 17 ) 
# la population etudier sera donc les personnes qui ont renseiger leurs age et qui on actuellement entre 
# 18 et 98 ans (inclus).
# Creation des groupe client suivant leurs ages. 8 groupes de 18 � 98 ans.
#clients_r$age_group <- cut(clients_r$age,seq(18,98,10), include.lowest= TRUE, right = FALSE)
clients_r2[(age > 17 & age<=25), age_group:="1 - moins de 25 ans"]
clients_r2[(age > 25 & age<=40), age_group:="2 - de 26 a 40 ans"]
clients_r2[(age > 40 & age<=65), age_group:="3 - de 41 a 65 ans"]
clients_r2[(age > 65 & age<=98), age_group:="4 - plus de 65 ans"]

datamining_client<-merge(datamining_client,clients_r2, by="IDCLIENT", all.x=TRUE)


### variable categorie de client ####
#VIP : client etant VIP (VIP = 1)
VIP <- clients[clients$VIP==1,]
setDT(VIP)
VIP<-VIP[,CAT_CLIENT:='1 - VIP']
VIP<-VIP%>%select(IDCLIENT,CAT_CLIENT)

#NEW_N2 : client ayant adhere au cours de l'annee N-2 (date debut adhesion)
NEW_N2 <-clients[(clients$VIP!=1)&
                   (format(as.Date(clients$DATEDEBUTADHESION, tryFormats = c("%d/%m/%Y")),"%Y")==2016),]
setDT(NEW_N2)
NEW_N2<-NEW_N2[,CAT_CLIENT:='2 - NEW_N2']
NEW_N2<-NEW_N2%>%select(IDCLIENT,CAT_CLIENT)

#NEW_N1 : client ayant adhere au cours de l'annee N-1 (date debut adhesion)
NEW_N1 <-clients[(clients$VIP!=1)&
                   (format(as.Date(clients$DATEDEBUTADHESION, tryFormats = c("%d/%m/%Y")),"%Y")==2017),]
setDT(NEW_N1)
NEW_N1<-NEW_N1[,CAT_CLIENT:='3 - NEW_N1']
NEW_N1<-NEW_N1%>%select(IDCLIENT,CAT_CLIENT)

#ADHERANT : client toujours en cours d'adhesion (date de fin d'adhesion > 2018/01/01)
ADHERANT <-clients[(clients$VIP!=1)&
                     format(as.Date(clients$DATEDEBUTADHESION, tryFormats = c("%d/%m/%Y")),"%Y")<2016&
                     (format(as.Date(clients$DATEFINADHESION, tryFormats = c("%d/%m/%Y")),"%Y-%m-%d"))>as.Date("2018-01-01"),]
setDT(ADHERANT)
ADHERANT<-ADHERANT[,CAT_CLIENT:='4 - ADHERANT']
ADHERANT<-ADHERANT%>%select(IDCLIENT,CAT_CLIENT)


#CHURNER : client ayant churner (date de fin d'adhesion < 2018/01/01)
CHURNER <-clients[((format(as.Date(clients$DATEFINADHESION, tryFormats = c("%d/%m/%Y")),"%Y-%m-%d"))<as.Date("2018-01-01"))&
                    (clients$VIP!=1)&
                    format(as.Date(clients$DATEDEBUTADHESION, tryFormats = c("%d/%m/%Y")),"%Y")<2016,]
setDT(CHURNER)
CHURNER<-CHURNER[,CAT_CLIENT:='5 - CHURNER']
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
  
  distanceCalculee <- rayonTerre * c / 1000
  
  return(distanceCalculee)
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
  setnames(magasins, old=c("CODESOCIETE"), new=c("MAGASINS"))
  setnames(magasins, old=c("LIBELLEDEPARTEMENT"), new=c("NUMDEPT"))
  
  # Dans l'objectif de pouvoir faire une jointure entre la TABLE_DE_TRAVAIL et la TABLE magasins
  setnames(clients, old=c("MAGASIN"), new=c("MAGASINS"))
  
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
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins > 50), BORNE_DISTANCE:="5 - plus de 50km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins <= 50), BORNE_DISTANCE:="4 - de 20 a 50km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 20), BORNE_DISTANCE:="3 - de 10 a 20km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 10), BORNE_DISTANCE:="2 - de 5 a 10km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 5), BORNE_DISTANCE:="1- moins de 5km"]
  
  jointureGeoMagasinsClients<-jointureGeoMagasinsClients%>%select(IDCLIENT,BORNE_DISTANCE)
}

distance_cli_mag<-distance_Client_Magasin(insee, magasins, clients)

datamining_client<-merge(datamining_client,distance_cli_mag, by="IDCLIENT", all.x=TRUE)


#RFM client
entetes_wrk_rfm<-entetes
setDT(entetes_wrk_rfm)
entetes_wrk_rfm[,TIC_DATE2:= as.Date(entetes_wrk_rfm$TIC_DATE)]

analysis_date <- lubridate::as_date('2018-01-01', tz = 'UTC')

RFM_Cmd_Clients <- rfm_table_order(data=entetes_wrk_rfm,customer_id = IDCLIENT, order_date =TIC_DATE2, revenue = TIC_TOTALTTC, analysis_date =as_date("2018-01-01", tz="UTC"),
                                   recency_bins = 9, frequency_bins = 9, monetary_bins = 9)

RFM_Clients<-RFM_Cmd_Clients$rfm
setDT(RFM_Clients)

datamining_client<-merge(datamining_client,RFM_Clients, by.x="IDCLIENT", by.y="customer_id", all.x=TRUE)

#top univers par client en nb achat et en marge
lignes_wrk_art<-merge(lignes_wrk, articles, by.x="IDARTICLE", by.y="CODEARTICLE", all=FALSE)
lignes_wrk_art_ent<-merge(lignes_wrk_art, entetes, by="IDTICKET", all=FALSE)
setDT(lignes_wrk_art_ent)
top_univers_client<-lignes_wrk_art_ent[,.(
  TOTAL=sum(TOTAL),
  MARGE=sum(MARGESORTIE),
  NB_ACTE_ACHAT=.N),
  by=.(IDCLIENT, CODEUNIVERS)]
#trie de la table top_univers_famille par marge
top_univers_client_trie_marge<-top_univers_client[order(IDCLIENT,CODEUNIVERS, MARGE, decreasing = TRUE),]
#créé un rank de la marge par code univers
top_univers_client_trie_marge<- within(top_univers_client_trie_marge, rank <- ave(MARGE,IDCLIENT,
                                                                       FUN=function(x)rev(order(x))))
#afficher le resultat du rank par code univers et code famille
top_univers_client_final_marge<-subset(top_univers_client_trie_marge, rank<2)
datamining_client<-merge(datamining_client,top_univers_client_final_marge, by="IDCLIENT", all.x=TRUE)
setnames(datamining_client, old=c("CODEUNIVERS"), new=c("top_univers_marge"))


#trie de la table top_univers_famille par CA
top_univers_client_trie_ca<-top_univers_client[order(IDCLIENT,CODEUNIVERS, TOTAL, decreasing = TRUE),]
#créé un rank de la marge par code univers
top_univers_client_trie_ca<- within(top_univers_client_trie_ca, rank <- ave(TOTAL,IDCLIENT,
                                                                                  FUN=function(x)rev(order(x))))
#afficher le resultat du rank par code univers et code famille
top_univers_client_final_ca<-subset(top_univers_client_trie_ca, rank<2)
datamining_client<-merge(datamining_client,top_univers_client_final_ca, by="IDCLIENT", all.x=TRUE)
setnames(datamining_client, old=c("CODEUNIVERS"), new=c("top_univers_ca"))

setnames(datamining_client, old=c("COMP_MARGE"), new=c("Margeur"))
setnames(datamining_client, old=c("CIVILITE_r.x"), new=c("CIVILITE"))
setnames(datamining_client, old=c("age_group"), new=c("Groupe_age"))
setnames(datamining_client, old=c("En_re_adhesion.x"), new=c("En_re_adhesion"))
setnames(datamining_client, old=c("En_fin_adhesion.x"), new=c("En_fin_adhesion"))
setnames(datamining_client, old=c("VIP.x"), new=c("VIP"))

datamining_client<-datamining_client%>%select(
IDCLIENT,Margeur,CIVILITE,age,Groupe_age,CAT_CLIENT,VIP,BORNE_DISTANCE,En_re_adhesion,En_fin_adhesion, TOTAL_CA_TTC,rfm_score,top_univers_marge,top_univers_ca)

#write.csv(datamining_client, file = "Margeur.csv")

#impt <- missForest(data.frame(datamining_client%>%select(-c(BORNE_DISTANCE,Groupe_age))))

#memory.limit(size=25000)
