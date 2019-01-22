# Inclure le chargement du fichier paramètres qui contient :
# - Le chargement des biliothèques nécessaires à l'éxecutuon des scripts R de ce fichier.
source("Parametres.R")
source("DistanceGeo.r")

#library(dplyr)
#library(data.table)
#library(readxl)
#library(assertthat)
#library(xlsx)

getwd()

wb<-createWorkbook(type="xlsx")
# Define some cell styles
#++++++++++++++++++++
# Title and sub title styles
TITLE_STYLE <- CellStyle(wb)+ Font(wb,heightInPoints=16,color="blue", isBold=TRUE, underline=1)
SUB_TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=TRUE, isBold=FALSE)
TITLE_TABLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=FALSE, isBold=TRUE,color="chocolate1")

# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE)

xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  xlsx::setCellStyle(sheetTitle[[1,1]], titleStyle)
}


articles <- read.csv("DATA_UTF-8/REF_ARTICLE.CSV",sep="|",
                     header=TRUE,skipNul = T,stringsAsFactors = FALSE)
articles <- articles[-c(1),]
magasin <- read.csv("DATA/REF_MAGASIN.CSV",sep="|",
                    header=TRUE,skipNul = T,stringsAsFactors = FALSE)

clients <- read.csv("DATA_UTF-8/SUBCLIENT.CSV",sep="|",
                    header=TRUE,skipNul = T,stringsAsFactors = FALSE)
clients[clients == ""] <- NA
entete <- read.csv("DATA_UTF-8/SUBENTETE.CSV",sep="|",
                    header=TRUE,skipNul = T,stringsAsFactors = FALSE)
lignes <- read.csv("DATA_UTF-8/SUBLIGNES.CSV",sep="|",
                    header=TRUE,skipNul = T)

sheet1 <- xlsx::createSheet(wb, sheetName = "Statistiques")

#ARTICLES

# Add title
xlsx.addTitle(sheet1, rowIndex=1, title="Statistiques sur ARTICLES",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=2,title="Tableau descriptif",titleStyle = SUB_TITLE_STYLE)

stat <- data.frame(matrix(nrow = 2,ncol=length(colnames(articles))))
colnames(stat)<- colnames(articles)

for (i in colnames(articles)){
  stat[[i]]<- c(length(unique(articles[[i]])),
                sum(is.na(articles[[i]]))/length(articles[[i]])*100)
}
row.names(stat) <- c("Unique values","Taux de valeurs manquantes")

# Creation de la feuille Statistiques"
addDataFrame(stat, sheet1, startRow=4, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)



#MAGASIN
# Add title
xlsx.addTitle(sheet1, rowIndex=10, title="Statistiques sur MAGASIN",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=11,title="Tableau descriptif",titleStyle = SUB_TITLE_STYLE)

stat <- data.frame(matrix(nrow = 2,ncol=length(colnames(magasin))))
colnames(stat)<- colnames(magasin)

for (i in colnames(magasin)){
  stat[[i]]<- c(length(unique(magasin[[i]])),
                sum(is.na(magasin[[i]]))/length(magasin[[i]])*100)
}
row.names(stat) <- c("Unique values","Taux de valeurs manquantes")

# Creation de la feuille Statistiques"
addDataFrame(stat, sheet1, startRow=15, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
xlsx::setColumnWidth(sheet1, colIndex=c(1:ncol(stat)), colWidth=20)


#CLIENTS
# Add title
xlsx.addTitle(sheet1, rowIndex=20, title="Statistiques sur CLIENTS",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=21,title="Tableau descriptif",titleStyle = SUB_TITLE_STYLE)

colcat <- colnames(clients)[!colnames(clients) %in% c("DATEDEBUTADHESION",
                                                      "DATEREADHESION",
                                                      "DATEFINADHESION",
                                                      "DATENAISSANCE")]
coldate <- c("DATEDEBUTADHESION","DATEREADHESION","DATEFINADHESION",
             "DATENAISSANCE")

statcat <- data.frame(matrix(nrow = 2,ncol=length(colcat)))
colnames(statcat)<- colcat

for (i in colcat){
  statcat[[i]]<- c(length(unique(clients[[i]])),
                sum(is.na(clients[[i]]))/length(clients[[i]])*100)
}

statdateMM <- data.frame(matrix(nrow = 2,ncol=length(coldate)))
colnames(statdateMM)<- coldate

for (i in coldate){
  statdateMM[[i]]<- c(min(clients[[i]],na.rm = TRUE),
                    max(clients[[i]],na.rm = TRUE))
  #vec<-c(vec,(sum(is.na(clients[[i]]))/length(clients[[i]])*100))
}

statdateNA <- data.frame(matrix(nrow = 1,ncol=length(coldate)))
colnames(statdateNA)<- coldate

for (i in coldate){
  statdateNA[[i]]<-(sum(is.na(clients[[i]]))/length(clients[[i]])*100)
}


row.names(statcat) <- c("Unique values","Taux de valeurs manquantes")
row.names(statdateMM) <- c("Minimum","Maximum")
row.names(statdateNA) <- c("Taux de valeurs manquantes")


# Creation de la feuille Statistiques"
addDataFrame(statcat, sheet1, startRow=25, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
addDataFrame(statdateMM, sheet1, startRow=30, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
addDataFrame(statdateNA, sheet1, startRow=33, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)

xlsx::setColumnWidth(sheet1, colIndex=c(1:15), colWidth=20)


#ENTETE

# Add title
xlsx.addTitle(sheet1, rowIndex=37, title="Statistiques sur SUBENTETE",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=38,title="Tableau descriptif",titleStyle = SUB_TITLE_STYLE)

colcat_entete <- colnames(entete)[!colnames(entete) %in% c("TIC_DATE","TIC_TOTALTTC")]
coldate_entete <- c("TIC_DATE")
colquali_entete <- c("TIC_TOTALTTC")

statcat_entete <- data.frame(matrix(nrow = 2,ncol=length(colcat_entete)))
colnames(statcat_entete)<- colcat_entete

for (i in colcat_entete){
  statcat_entete[[i]]<- c(length(unique(entete[[i]])),
                   sum(is.na(entete[[i]]))/length(entete[[i]])*100)
}

statdate_entete <- data.frame(matrix(nrow = 2,ncol=length(coldate_entete)))
colnames(statdate_entete)<- coldate_entete


for (i in coldate_entete){
  statdate_entete[[i]]<- c(
    min(entete[[i]],na.rm = TRUE),
    max(entete[[i]],na.rm = TRUE))
}


statdate_entete_NA <- data.frame(matrix(nrow = 1,ncol=length(coldate_entete)))
colnames(statdate_entete_NA)<- coldate_entete

for (i in coldate_entete){
  statdate_entete_NA[[i]]<-(sum(is.na(entete[[i]]))/length(entete[[i]])*100)
}

statquanti_entete <- data.frame(matrix(nrow = 10,ncol=length(colquali_entete)))
colnames(statquanti_entete)<- colquali_entete

for (i in colquali_entete){
  entete_num <- as.numeric(sub(",", ".", entete[[i]]))
  statquanti_entete[[i]]<- c(
    length(entete_num),
    min(entete_num,na.rm = TRUE),
    sort(unique(entete_num,na.rm = TRUE))[2],
    max(entete_num,na.rm = TRUE),
    sort(unique(entete_num,na.rm = TRUE),decreasing = TRUE)[2],
    round(mean(entete_num,na.rm = TRUE),1),
    median(entete_num,na.rm = TRUE),
    round(sd(entete_num,na.rm = TRUE),1),
    round(sd(entete_num,na.rm = TRUE)/mean(entete_num,na.rm = TRUE),1),
    sum(is.na(entete_num))/length(entete_num)*100
  
  )
  
}

row.names(statcat_entete) <- c("Unique values","Taux de valeurs manquantes")
row.names(statdate_entete) <- c("Minimum","Maximum")
row.names(statdate_entete_NA) <- c("Taux de valeurs manquantes")
row.names(statquanti_entete) <- c("Effectif","Minimum","Min1","Maximum","Max1","Moyenne","Mediane","Ecart type","coefficient de variation","Taux de valeurs manquantes")

# Creation de la feuille Statistiques"
addDataFrame(statcat_entete, sheet1, startRow=42, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
addDataFrame(statdate_entete, sheet1, startRow=47, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
addDataFrame(statdate_entete_NA, sheet1, startRow=50, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
addDataFrame(statquanti_entete, sheet1, startRow=53, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)

xlsx::setColumnWidth(sheet1, colIndex=c(1:15), colWidth=20)


#Ligne

# Add title
xlsx.addTitle(sheet1, rowIndex=66, title="Statistiques sur Sub Ligne",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=67,title="Tableau descriptif",titleStyle = SUB_TITLE_STYLE)


colcat_ligne <- colnames(lignes)[!colnames(lignes) %in% c("QUANTITE","MONTANTREMISE","TOTAL","MARGESORTIE")]
colquali_ligne <- c("QUANTITE","MONTANTREMISE","TOTAL","MARGESORTIE")


statcat_ligne <- data.frame(matrix(nrow = 2,ncol=length(colnames(colcat_ligne))))
colnames(statcat_ligne)<- colnames(colcat_ligne)


for (i in colcat_ligne){
  statcat_ligne[[i]]<- c(length(unique(lignes[[i]])),
                sum(is.na(lignes[[i]]))/length(lignes[[i]])*100)
}


statquali_ligne <- data.frame(matrix(nrow = 10,ncol=length(colnames(colquali_ligne))))
colnames(statquali_ligne)<- colnames(colquali_ligne)

for (i in colquali_ligne){
  ligne_num <- as.numeric(sub(",", ".", levels(lignes[[i]])[lignes[[i]]]))
  statquali_ligne[[i]]<- c(
    length(ligne_num),
    min(ligne_num,na.rm = TRUE),
    sort(unique(ligne_num,na.rm = TRUE))[2],
    max(ligne_num,na.rm = TRUE),
    sort(unique(ligne_num,na.rm = TRUE),decreasing = TRUE)[2],
    round(mean(ligne_num,na.rm = TRUE),1),
    median(ligne_num,na.rm = TRUE),
    round(sd(ligne_num,na.rm = TRUE),1),
    round(sd(ligne_num,na.rm = TRUE)/mean(ligne_num,na.rm = TRUE),1),
    sum(is.na(ligne_num))/length(ligne_num)*100
    
  )
  
}


row.names(statcat_ligne) <- c("Unique values","Taux de valeurs manquantes")
row.names(statquali_ligne) <- c("Effectif","Minimum","Min1","Maximum","Max1","Moyenne","Mediane","Ecart type","coefficient de variation","Taux de valeurs manquantes")


# Creation de la feuille Statistiques"
addDataFrame(statcat_ligne, sheet1, startRow=71, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
addDataFrame(statquali_ligne, sheet1, startRow=76, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)


xlsx::saveWorkbook(wb, "temp.xlsx")

#---------------------------------------------------------------------

correspondance_INSEE_CP <- fread("DATA/correspondance-code-insee-code-postal.csv", sep = ";", 
                 header = TRUE, stringsAsFactors = FALSE)


bobo <- correspondance_INSEE_CP[`Code INSEE` == 32460, list(`Code INSEE`, geo_point_2d)]

bobosplit <- correspondance_INSEE_CP[, list(geo_point_2d)]

library(dplyr)
library(tidyr)
#bobodf <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
bobosplitDT <- as.data.frame(bobosplit)
jjjjjj <- bobosplitDT$geo_point_2d[1]
kkkkkk <- unlist(strsplit(bobosplitDT$geo_point_2d[1], ", "))

#twoColums <- bobosplitDT %>% separate(bobosplitDT$geo_point_2d, c("latitude", "longitude"), sep=',')
twoColums <- bobosplitDT %>% separate(bobosplitDT$geo_point_2d[1], c("latitude", "longitude"), sep=',')
#twoColums <- unlist(strsplit(bobosplitDT$geo_point_2d, ","))
#myVector <- correspondance_INSEE_CP[,factor(geo_point_2d)] 

df <- data.frame(x = c("10.00, 12.11", "11.11, 12.22", "11.33, 13.33", "14.44, 14.42"))
df %>% separate(x, c("A", "B"), sep=',')

#[entete_num!=0]


