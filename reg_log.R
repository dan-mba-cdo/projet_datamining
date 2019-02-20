install.packages("ElemStatLearn")
install.packages("questionr")
install.packages("ggplot2")
install.packages("gplots")
install.packages("broom")
install.packages("stats")
install.packages("carData")
install.packages("effects")
install.packages("ROCR")
install.packages("pROC")
install.packages("rpart")
library(tidyr)
library(dplyr)
library(ElemStatLearn)
library(questionr)
library(ggplot2)
library(broom)
library(stats)
library(effects)
library(ROCR)
library(pROC)
library(rpart)

d <- fread("C:/Users/dgoldman/Desktop/MBA - BIG DATA CDO/DataMining/projet datamining/projet_datamining/projet_datamining/Margeur.csv", sep=",", header = TRUE, dec = ".", stringsAsFactors = TRUE)

d<-d%>%select(-V1)

colnames(d)

#Population de référence
#d$Groupe_age <- relevel(d$Groupe_age,"1 - moins de 36 ans")
#d$CAT_CLIENT <- relevel(d$CAT_CLIENT,"anc. 1 an  ou moins")
#d$BORNE_DISTANCE <- relevel(d$BORNE_DISTANCE,"Non domicilié")

#unique(d$BORNE_DISTANCE)

#Création échantillons apprentissage et test
i <- sample(2, nrow(d), replace=T, prob=c(0.7,0.3))

training_set <- d[i==1,]# apprentissage = 70%
training_set <- training_set[,c("Margeur",
                                "CIVILITE",
                                "Groupe_age",
                                "CAT_CLIENT",
                                "BORNE_DISTANCE",
                                "En_re_adhesion",
                                "TOTAL_CA_TTC",
                                "rfm_score",
                                "top_univers_marge",
                                "top_univers_ca")]

test_set <- d[i==2,] # test = 30%
test_set <- test_set[,c("Margeur",
                        "CIVILITE",
                        "Groupe_age",
                        "CAT_CLIENT",
                        "BORNE_DISTANCE",
                        "En_re_adhesion",
                        "TOTAL_CA_TTC",
                        "rfm_score",
                        "top_univers_marge",
                        "top_univers_ca")]

#Regression logistique
classifier <- glm(
  formula = Margeur ~ CIVILITE+Groupe_age+CAT_CLIENT+BORNE_DISTANCE+En_re_adhesion+TOTAL_CA_TTC+rfm_score+top_univers_marge+top_univers_ca,
  data = training_set, 
  family=binomial(link="logit")
)

coefficients <- as.data.frame(coef(classifier))

odds_ratios <- odds.ratio(classifier)

#Prediction des resultats de l'echantillon test
predict_test_set <- predict(classifier,
                            type = 'response',
                            newdata = test_set)

predict_test_set
#affectation du seuil
s <- 0.62
y_pred <- ifelse(predict_test_set > s, 1, 0)



#Matrice de confusion
Y <- test_set[, 1] #variable à expliquer de l'échantillon de test
mc<-table(predict_test_set>0.62,test_set$Margeur)

mc <- table(Y, y_pred)


accurate_value <- (mc[1,1] + mc[2,2]) / nrow(test_set) #taux de prédictions justes du modèle
taux_erreur <- (mc[1,2] + mc[2,1]) / nrow(test_set) #taux d'erreur
precision <- mc[2,2] / (mc[1,2] + mc[2,2])
sensibilite <- mc[2,2] / (mc[2,1] + mc[2,2])
specificite <- mc[1,1] / (mc[1,1] + mc[1,2])
tfp <- mc[1,2] / (mc[1,1]+mc[1,2])

X11()
#Courbe ROC
PROC <- plot.roc(as.matrix(Y),predict_test_set,main="", percent=TRUE, auc=TRUE,
                 ci=TRUE)
SE <- ci.se(PROC,specificities=seq(0, 100, 5))
plot(SE, type="shape", col="light blue")

#Aire sous la courbe
auc_reg_logistique <- auc(Y,predict_test_set)