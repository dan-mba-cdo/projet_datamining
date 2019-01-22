#REGLE DE DECISION
# ON ELIMINE LES DOUBLONS DANS LA TABLE LIGNE TICKET
#Identification des doublons
lignes<-lignes[,.(nb=.N), by=c("IDTICKET","NUMLIGNETICKET","IDARTICLE","QUANTITE", "MONTANTREMISE", "TOTAL", "MARGESORTIE")]
#Dedoublonnage
lignes<-lignes%>% select(-nb)
#nombre de doublon identifie et elimine : 130 doublons identifie

#On recupere uniquement les informations reliées entre elles : 
#A Savoir uniquement les clients qui ont un entete ticket, lui meme rattaché à une ligne ticket, et dont l'article est reference
#Condition à respecter --  "inner join" entre les tables
#jointure interne entre la table client et la table magasin
#845 876 clients
PERIMETRE<-merge(clients,magasins,by.x="MAGASIN", by.y="CODESOCIETE",all=FALSE)

#jointure interne entre la table client-magasin et entete
PERIMETRE<-merge(PERIMETRE, entetes, by="IDCLIENT", all=FALSE)
#nombre de client : 770 163.
#75 713 client de la base n'ont pas effectue d'achat
#afficher le nombre de client
PERIMETRE[,.(nb_clients=length(unique(IDCLIENT)))]


#jointure interne entre la table client-magasin-entete et ligne ticket
PERIMETRE<-merge(PERIMETRE,lignes,by="IDTICKET", all=FALSE)
#afficher nombre de client :770 163
PERIMETRE[,.(nb_clients=length(unique(IDCLIENT)))]
#afficher nombre d'entete ticket : 6 713 822
#351 entetes tickets n'ont pas de lignes ticket
PERIMETRE[,.(nb_clients=length(unique(IDTICKET)))]



#Analyse table article avec la table ligne ticket : jointure externe
article_ligne<-merge(lignes,articles,by.x="IDARTICLE", by.y="CODEARTICLE",all.x=TRUE)
#Analyse pour savoir si tous les articles de la table lignes existe bien dans la table article
article_ligne[is.na(CODEUNIVERS)==TRUE,.(nb=.N), by=IDARTICLE]
#on constate que l'IDARTICLE "395460" n'existe pas dans la table article et qu'il y a 31 lignes ticket lié à ce dernier, il faut donc le rajouter

#creation du nouvel article
newarticle <- data.frame(CODEARTICLE='395460',CODEUNIVERS='unknown',CODEFAMILLE='unknown', CODESOUSFAMILLE='unknown')
articles<- rbind(articles,newarticle)
setDT(articles)

#jointure interne entre la table client-magasin-entete-ligne et article (avec l'ajout du nouvel article)
PERIMETRE<-merge(PERIMETRE,articles,by.x="IDARTICLE", by.y="CODEARTICLE", all=FALSE)
#afficher le nombre de client :770 163
PERIMETRE[,.(nb_clients=length(unique(IDCLIENT)))]
#afficher nombre d'entete ticket : 6 713 822
PERIMETRE[,.(nb_clients=length(unique(IDTICKET)))]

#RESUME INDICATEUR
nb_clients<-PERIMETRE[,.(nb_clients=length(unique(IDCLIENT)))]
nb_acte_achat<-PERIMETRE[,.(nb_acte_achat=length(unique(IDTICKET)))]
montant_total<-PERIMETRE[,.(montant_total=sum(TOTAL))]
marge_total<-PERIMETRE[,.(marge_total=sum(MARGESORTIE))]
quantite_total<-PERIMETRE[,.(quantite_total=sum(QUANTITE))]


kpi<-cbind(cbind(cbind(cbind(nb_clients,nb_acte_achat),montant_total),marge_total),quantite_total)
write.csv(kpi, file = "C:/Users/dgoldman/Desktop/MBA - BIG DATA CDO/DataMining/projet datamining/projet_datamining/kpi.csv")
