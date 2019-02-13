
# Dédoublonnage
lignes_wrk<-lignes[,.(nb=.N), by=c("IDTICKET","NUMLIGNETICKET","IDARTICLE","QUANTITE", "MONTANTREMISE", "TOTAL", "MARGESORTIE")]
lignes_wrk<-lignes_wrk%>% select(-nb)

#exclure les lignes tickets dont la remise est negative
lignes_wrk<-lignes_wrk[MONTANTREMISE>=0.000,] 

#exclure les lignes dont l'article est coupon
lignes_wrk<-lignes_wrk[IDARTICLE!='COUPON',] 

#jointure des clients avec les entetes tickets
client_entete_wrk<-merge(clients,entetes, by="IDCLIENT", all=FALSE)

##montant remise superieur > 0
#regroupement par IDTTICKET
remise_oui<-lignes_wrk[MONTANTREMISE>0.000,.(
  TOTAL_O=sum(TOTAL),
  REMISE_O=sum(MONTANTREMISE),
  MARGE_O=sum(MARGESORTIE),
  QTE_O=sum(QUANTITE),
  NB_ACTE_ACHAT_O=.N
),
by=.(IDTICKET)]

##montant remise egal à 0
#regroupement par IDTICKET
remise_non<-lignes_wrk[MONTANTREMISE==0.000,.(
  TOTAL_N=sum(TOTAL),
  REMISE_N=sum(MONTANTREMISE),
  MARGE_N=sum(MARGESORTIE),
  QTE_N=sum(QUANTITE),
  NB_ACTE_ACHAT_N=.N
),
by=.(IDTICKET)]


#regrouprement des clients selon remise ou non
client_remise<-merge(client_entete_wrk,remise_oui, by="IDTICKET", all.x=TRUE)
client_remise<-merge(client_remise,remise_non, by="IDTICKET", all.x=TRUE)
#garder uniquement les clients qui ont eu a la fois des remises et pas de remises
client_remise<-client_remise[is.na(TOTAL_O)==FALSE & is.na(TOTAL_N)==FALSE,]


#agregation des indicateurs rergoupe par client
perimetre_client_remise<-client_remise[, .(
  TOTAL_CA_TTC=sum(TIC_TOTALTTC),
  TOTAL_O=sum(TOTAL_O),
  REMISE_O=sum(REMISE_O),
  MARGE_O=sum(MARGE_O),
  QTE_O=sum(QTE_O),
  NB_ACTE_ACHAT_O=sum(NB_ACTE_ACHAT_O),
  TOTAL_N=sum(TOTAL_N),
  REMISE_N=sum(REMISE_N),
  MARGE_N=sum(MARGE_N),
  QTE_N=sum(QTE_N),
  NB_ACTE_ACHAT_N=sum(NB_ACTE_ACHAT_N)),
  by=.(IDCLIENT)]

#exclusion des valeur 1% et 99% en dehors du seuil centile
perimetre_client_remise_not_OUT <- subset(perimetre_client_remise,(TOTAL_CA_TTC>quantile(perimetre_client_remise$TOTAL_CA_TTC,c(0.01)))&
                                            (TOTAL_CA_TTC<quantile(perimetre_client_remise$TOTAL_CA_TTC,c(0.99))))


#ajout des flag permettant de comparer les indicateurs avec remises et sans remises
perimetre_client_remise_not_OUT$COMP_MARGE <- ifelse(perimetre_client_remise_not_OUT$MARGE_N<perimetre_client_remise_not_OUT$MARGE_O,1,0)
perimetre_client_remise_not_OUT$COMP_TOTAL <- ifelse(perimetre_client_remise_not_OUT$TOTAL_N<perimetre_client_remise_not_OUT$TOTAL_O,1,0)
perimetre_client_remise_not_OUT$COMP_QTE <- ifelse(perimetre_client_remise_not_OUT$QTE_N<perimetre_client_remise_not_OUT$QTE_O,1,0)
perimetre_client_remise_not_OUT$COMP_NB_ACTE_ACHAT <- ifelse(perimetre_client_remise_not_OUT$NB_ACTE_ACHAT_N<perimetre_client_remise_not_OUT$NB_ACTE_ACHAT_O,1,0)


#initialisation de la table datamining client qui sera notre table pour la regression logistique
datamining_client<-perimetre_client_remise_not_OUT%>%select(IDCLIENT, COMP_MARGE, COMP_TOTAL,COMP_NB_ACTE_ACHAT, TOTAL_CA_TTC)

setDT(datamining_client)

