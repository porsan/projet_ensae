
##########################################
#          Librairies - options          #
##########################################


# Visualisation
library(ggplot2)
library(GGally) # Pour des visualisations de corrélations (ggcorr) sous ggplot (+ ggpairs())
library(ggthemes) # Thèmes additionnels ggplot (dont economist)
library(ggrepel) # Pour placer les points de ggplot de façon optimal

library(plotly) # Graphiques avec intéractions
library(maps) # Cartographie avec ggplot
library(shiny) # Shiny...
library(caret)
library(gbm)
#install.packages("mapproj")
library(mapproj)

#library(mapdata)

# Traitement de données
library(reshape2)
library(stringr)
library(dplyr)

## specifiques textmining
library(data.table)
library(stringi)
library(stringr)
library(tm)
# install_github("lchiffon/wordcloud2")
library(wordcloud2)
library(wordcloud)
library('SnowballC')
library(memoise)
#library(RWeka)
library(tidyr)
library(caret)
library(MLmetrics)
library(e1071)
library(randomForest)
library(xgboost)

############################################
#             Lecture données              #
############################################


#setwd("H:/Formation CEPE/Projet")

#final <- read.csv2("final.csv", encoding = "latin1") # Table finale utilisée pour la modélisation
final <- read.csv2("final.csv") # Table finale utilisée pour la modélisation
data_cartographie <- read.csv2("data_cartographie.csv") # Table spécifique pour les données cartographiées

#setwd("F:/projet_ensae/data")

# Chargement des donnees presse
performances_model_presse <- read.csv("performances_model_presse.csv")
performances_model_presse <- performances_model_presse[-1]
final_results_presse <- read.csv("final_results_presse.csv")
final_results_presse <- final_results_presse[-1]

# Creation des graphes de performances presse
# graphe de performance RMSE presse
graph_RMSE_presse <- ggplot(performances_model_presse) + 
  geom_bar(aes(x = reorder(model,RMSE), y = RMSE, fill = RMSE), stat="identity", width = 0.85) +
  labs(title = "Performances des différents modèles sur note presse", subtitle = "RMSE sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "white",colour = "black",size = 1),
        axis.text.x = element_text(size = 12, face = "bold", angle = 45, lineheight = 1, vjust = 0.7),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = RMSE, label = round(RMSE, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)
# graphe de performance Rsquared presse
graph_Rsquared_presse <- ggplot(performances_model_presse) + 
  geom_bar(aes(x = reorder(model,Rsquared), y = Rsquared, fill = Rsquared), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note presse", subtitle = "Rsquared sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "white",colour = "black",size = 1),
        axis.text.x = element_text(size = 12, face = "bold", angle = 45, lineheight = 1, vjust = 0.7),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = Rsquared, label = round(Rsquared, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)
# graphe de performance MAE presse
graph_MAE_presse <- ggplot(performances_model_presse) + 
  geom_bar(aes(x = reorder(model,MAE), y = MAE, fill = MAE), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note presse", subtitle = "MAE sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "white",colour = "black",size = 1),
        axis.text.x = element_text(size = 12, face = "bold", angle = 45, lineheight = 1, vjust = 0.7),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = MAE, label = round(MAE, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)

# Creation du graphe des distributions presse
data <- reshape2::melt(final_results_presse) 
graph_distrib_presse <- ggplot(data) + geom_histogram(aes(x=value, fill = variable), bins = 30) + 
  facet_wrap(~variable, ncol = 2) +
  labs(title ="Distribution des notes presse selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

# Creation du graphe des nuages presse
data2 <- cbind(data, y_reel = rep(final_results_presse["valeur_reelle"], 8))
data2 <- data2[data2$variable != "valeur_reelle",]
data2 <- as.data.frame(data2)
graph_nuage_presse <- ggplot(data2) + geom_abline(aes(intercept=0, slope=1), color = "light grey") + geom_jitter(aes(x=value, y = y_reel.valeur_reelle,color = variable), shape = 1) + 
  facet_wrap(~variable, ncol = 3) +
  labs(title ="Distribution des notes presse selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

# Chargement des donnees spectateurs
performances_model_spectateurs <- read.csv("performances_model_spectateurs.csv")
performances_model_spectateurs <- performances_model_spectateurs[-1]
final_results_spectateurs <- read.csv("final_results_spectateurs.csv")
final_results_spectateurs <- final_results_spectateurs[-1]

# Creation des graphes de performances spectateurs
# graphe de performance RMSE spectateurs
graph_RMSE_spectateurs <- ggplot(performances_model_spectateurs) + 
  geom_bar(aes(x = reorder(model,RMSE), y = RMSE, fill = RMSE), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note spectateurs", subtitle = "RMSE sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "white",colour = "black",size = 1),
        axis.text.x = element_text(size = 12, face = "bold", angle = 45, lineheight = 1, vjust = 0.7),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = RMSE, label = round(RMSE, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)
# graphe de performance Rsquared spectateurs
graph_Rsquared_spectateurs <- ggplot(performances_model_spectateurs) + 
  geom_bar(aes(x = reorder(model,Rsquared), y = Rsquared, fill = Rsquared), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note spectateurs", subtitle = "Rsquared sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "white",colour = "black",size = 1),
        axis.text.x = element_text(size = 12, face = "bold", angle = 45, lineheight = 1, vjust = 0.7),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = Rsquared, label = round(Rsquared, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)
# graphe de performance MAE spectateurs
graph_MAE_spectateurs <- ggplot(performances_model_spectateurs) + 
  geom_bar(aes(x = reorder(model,MAE), y = MAE, fill = MAE), stat="identity", width = 0.85 ) +
  labs(title = "Performances des différents modèles sur note spectateurs", subtitle = "MAE sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.7) + 
  scale_fill_gradient(low="green", high="red") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "white",colour = "black",size = 1),
        axis.text.x = element_text(size = 12, face = "bold", angle = 45, lineheight = 1, vjust = 0.7),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = MAE, label = round(MAE, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)

# graphe de performance MAE / MSE spectateurs
graph_MAE_MSE_spectateurs <- ggplot(performances_model_spectateurs) + 
  geom_point(aes(x = RMSE, y = MAE, group = model, color = model), size = 4) +
  labs(title = "Performances des différents modèles sur note spectateurs", subtitle = "MAE et MSE sur échantillon de validation") + 
  theme_minimal()  + 
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "light grey",colour = "black",size = 1),
        axis.text.x = element_text(size = 8, face = "bold", lineheight = 1),
        legend.position = "null") +
  geom_text_repel( aes(x = RMSE, y = MAE, label = model, vjust = 3), fontface ="bold",  size = 4)

# graphe de performance MAE / MSE presse
graph_MAE_MSE_presse <- ggplot(performances_model_presse) + 
  geom_point(aes(x = RMSE, y = MAE, group = model, color = model), size = 4) +
  labs(title = "Performances des différents modèles sur note presse", subtitle = "MAE et MSE sur échantillon de validation") + 
  theme_minimal()  + 
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        plot.background = element_rect(fill = "light grey",colour = "black",size = 1),
        axis.text.x = element_text(size = 8, face = "bold", lineheight = 1),
        legend.position = "null") +
  geom_text_repel( aes(x = RMSE, y = MAE, label = model, vjust = 3), fontface ="bold",  size = 4)

# Creation du graphe des distributions spectateurs
data <- reshape2::melt(final_results_spectateurs) 
graph_distrib_spectateurs <- ggplot(data) + geom_histogram(aes(x=value, fill = variable), bins = 30) + 
  facet_wrap(~variable, ncol = 2) +
  labs(title ="Distribution des notes spectateurs selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

# Creation du graphe des nuages spectateurs
data2 <- cbind(data, y_reel = rep(final_results_spectateurs["valeur_reelle"], 8))
data2 <- data2[data2$variable != "valeur_reelle",]
data2 <- as.data.frame(data2)
graph_nuage_spectateurs <- ggplot(data2) + geom_abline(aes(intercept=0, slope=1), color = "light grey") + geom_jitter(aes(x=value, y = y_reel.valeur_reelle,color = variable), shape = 1) + 
  facet_wrap(~variable, ncol = 3) +
  labs(title ="Distribution des notes spectateurs selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

#textes <- read.csv(paste0(data_dir,"textes.csv"))

RMSE_def <- "Root-mean-square error (RMSE) ou root-mean-square deviation (RMSD). C'est la racine carrée de l’erreur quadratique moyenne d’un estimateur. Elle est plus souvent appelée « erreur quadratique »,(« moyenne » étant sous-entendu) ; elle est parfois appelée aussi « risque quadratique »." #, a(href="https://en.wikipedia.org/wiki/Root-mean-square_deviation","lien wiki"))
#RMSE_def <- 'a(href="https://en.wikipedia.org/wiki/Root-mean-square_deviation","lien wiki")'
Rsquared_def <- "La somme des carrés des résidus (SCR ou Sum of Squared Errors). Comme on mesure des carrés, on majore l’importance des grosses erreurs. https://en.wikipedia.org/wiki/Coefficient_of_determination" #a(href="", "link"),
MAE_def <- "L’erreur absolue moyenne (MAE pour Mean Absolute Error) : moyenne arithmétique des valeurs absolues des écarts. https://en.wikipedia.org/wiki/Mean_absolute_error" #a(href="", "link"),
RMSE_MAE_def <- "Coordonnees MSE par MAE des modeles."
Resultats_txt <- "Présentation des résultats des modèles appliquées sur les données pour définir la note moyenne presse ou spectateurs."

# Creation des graphes variables

graph_variable <- function(n = 20, macouleur = "light pink", titre, monmodele) {
  
  tmp <- as.data.frame(monmodele$importance[1:n,])
  colnames(tmp) <- "importance"
  tmp$variable <- row.names(monmodele$importance)[1:n]
  legraph <- ggplot(tmp,aes(y = tmp$importance, x = reorder(tmp$variable,tmp$importance))) + 
    geom_bar(color = "black", fill = macouleur, stat = "identity") +
    geom_text(aes(label = round(tmp$importance,2), vjust=0.5), position = position_stack(vjust=0.5),  fontface ="bold", size = 3, color = "black") +
    coord_flip() + theme_economist()  +
    labs(x ="", y = "", title = titre, subtitle = "Variables par ordre d'importance") +
    theme(axis.text.x = element_text(size = 12),legend.position="null",
          axis.text.y = element_text(size = 12),
          plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
          plot.subtitle = element_text(size = 14),
          plot.caption = element_text(face = "italic"))
}

load("rf_presse")
load("rpart_presse")
load("gradient_presse")
load("regression_presse")
load("regbackward_presse")
load("regforward_presse")
load("glmnet_presse")
load("svm_presse")
load("svmlineaire_presse")
load("xgboost_presse")

graph_rf_presse <- graph_variable(20,"light blue",titre = "Random Forest",monmodele =  varImp(rf_presse))
graph_rpart_presse <- graph_variable(20,"light blue",titre = "Arbre CART",monmodele =  varImp(rpart_presse))
graph_gradient_presse <- graph_variable(20,"light blue",titre = "Gradient Boosting",monmodele =  varImp(gradient_presse))
graph_regression_presse <- graph_variable(20,"light blue",titre = "Regression Lineaire",monmodele =  varImp(regression_presse))
graph_regbackward_presse <- graph_variable(20,"light blue",titre = "Regression Lineaire Backward",monmodele =  varImp(regbackward_presse))
graph_regforward_presse <- graph_variable(20,"light blue",titre = "Regression Lineaire Forward",monmodele =  varImp(regforward_presse))
graph_glmnet_presse <- graph_variable(20,"light blue",titre = "Regression Ridge Lasso",monmodele =  varImp(glmnet_presse))
graph_svm_presse <- graph_variable(20,"light blue",titre = "SVM",monmodele =  varImp(svm_presse))
graph_svmlineaire_presse <- graph_variable(20,"light blue",titre = "SVM Lineaire",monmodele =  varImp(svmlineaire_presse))
graph_xgboost_presse <- graph_variable(20,"light blue",titre = "eXtreme Gradient Boosting",monmodele =  varImp(xgboost_presse))

load("rf_spectateurs")
load("rpart_spectateurs")
load("gradient_spectateurs")
load("regression_spectateurs")
load("regbackward_spectateurs")
load("regforward_spectateurs")
load("glmnet_spectateurs")
load("svm_spectateurs")
load("svmlineaire_spectateurs")
load("xgboost_spectateurs")

graph_rf_spectateurs <- graph_variable(20,titre = "Random Forest",monmodele =  varImp(rf_spectateurs))
graph_rpart_spectateurs <- graph_variable(20,titre = "Arbre CART",monmodele =  varImp(rpart_spectateurs))
graph_gradient_spectateurs <- graph_variable(20,titre = "Gradient Boosting",monmodele =  varImp(gradient_spectateurs))
graph_regression_spectateurs <- graph_variable(20,titre = "Regression Lineaire",monmodele =  varImp(regression_spectateurs))
graph_regbackward_spectateurs <- graph_variable(20,titre = "Regression Lineaire Backward",monmodele =  varImp(regbackward_spectateurs))
graph_regforward_spectateurs <- graph_variable(20,titre = "Regression Lineaire Forward",monmodele =  varImp(regforward_spectateurs))
graph_glmnet_spectateurs <- graph_variable(20,titre = "Regression Ridge Lasso",monmodele =  varImp(glmnet_spectateurs))
graph_svm_spectateurs <- graph_variable(20,titre = "SVM",monmodele =  varImp(svm_spectateurs))
graph_svmlineaire_spectateurs <- graph_variable(20,titre = "SVM Lineaire",monmodele =  varImp(svmlineaire_spectateurs))
graph_xgboost_spectateurs <- graph_variable(20,titre = "eXtreme Gradient Boosting",monmodele =  varImp(xgboost_spectateurs))


#xtrain<-as.data.frame(fread("data/xtrain_980var.csv"))
xtest<-as.data.frame(fread("xtest_980var.csv"))
#ytrain<-as.factor(unname(unlist(as.data.frame(fread("data/ytrain_980var.csv")))))
ytest<-as.factor(unname(unlist(as.data.frame(fread("ytest_980var.csv")))))

# setwd("/Users/remimichel/R/working_space/total")

RF<- readRDS("model_RF_980var_100trees.rds")
XGB<- readRDS("model_XGB_980var_100trees.rds")
LDA<- readRDS("model_LDA_980var.rds")

pred_RF <- predict(object=RF$finalModel, xtest,type='class')
# selon la version chargée de xgboost, nécessité ou pas de transformer les mots en numeric
# xtest<- as.data.frame(lapply(xtest, as.numeric))
pred_XGB <- predict(object=XGB, xtest, type="raw")
pred_LDA <- predict(object=LDA$finalModel, xtest,type='class')$class

pred_supervise<- data.frame( OBS= ytest, RF =pred_RF, XGB = pred_XGB, LDA = pred_LDA )


# sur le train
# mesures_RF <- cbind(data.frame("modele" = "Random Forest"),RF$results[,c("Accuracy","Kappa","Mean_F1", "Mean_Sensitivity", "Mean_Specificity")])
# mesures_XGB <- cbind(data.frame("modele" = "eXtreme Gradient Boosting"),XGB$results[,c("Accuracy","Kappa","Mean_F1", "Mean_Sensitivity", "Mean_Specificity")])
# mesures_LDA <- cbind(data.frame("modele" = "Linear Discriminant Analysis"),LDA$results[,c("Accuracy","Kappa","Mean_F1", "Mean_Sensitivity", "Mean_Specificity")])

confmat_RF <- confusionMatrix(pred_RF, ytest)    
confmat_XGB <- confusionMatrix(pred_XGB, ytest)
confmat_LDA <- confusionMatrix(pred_LDA, ytest)

mesures_RF <- cbind(data.frame("modele" = "Random Forest"),t(confmat_RF$overall[c("Accuracy","Kappa")]) )
mesures_XGB <- cbind(data.frame("modele" = "eXtreme Gradient Boosting"),t(confmat_XGB$overall[c("Accuracy","Kappa")]) )
mesures_LDA <- cbind(data.frame("modele" = "Linear Discriminant Analysis"),t(confmat_LDA$overall[c("Accuracy","Kappa")]) )

mesures_supervise<- rbind( mesures_RF, mesures_XGB,mesures_LDA )

ggplot_mesure_tm<- function(df,mesure_select) {
  Modele<- df[,"modele"]
  Mesure<- df[,mesure_select]
  
  ggplot(df) + 
    geom_bar(aes(x = Modele, y = Mesure, fill = Mesure), stat="identity", width = 0.85 ) +
    labs(title = "Performances globales sur données de validation") + 
    theme_minimal()  +  ylim(0, 0.7) + 
    scale_fill_gradient(low="green", high="red") +
    theme(plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(face = "italic"), 
          #plot.background = element_rect(fill = "light grey",colour = "black",size = 1),
          axis.text.x = element_text(size = 10, face = "bold", angle = 0, lineheight = 1),
          axis.text.y = element_blank(),
          legend.position = "null") + 
    geom_text(aes(x = modele, y = Mesure, label = round(Mesure, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)
}


cm.plot <- function(table_cm){
  tablecm <- round(t(t(table_cm) / colSums(as.matrix(table_cm))*100)) # cr?e les pourcentages
  tablemelt <- melt(tablecm)
  ggplot(tablemelt, aes(Reference, Prediction)) +
    geom_point(aes(size = value, color=value), alpha=0.8, show.legend=FALSE) +
    geom_text(aes(label = value), color="white") +
    scale_size(range = c(5,25)) +
    scale_y_discrete(limits = rev(levels(tablemelt$Prediction)))+
    theme_bw() + 
    theme(plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
          axis.text.x = element_text(size = 10, face = "bold", angle = 40, lineheight = 1 , hjust = 1),
          axis.text.y = element_text(size = 10, face = "bold", angle = 0, lineheight = 1) ) +
    labs(title = "Matrice de confusion sur données de validation")
}


dtm_final<- readRDS("dtmnonsupervise_980var.rds")
var_a_expliquer<- as.data.frame(fread("var_a_expliquer.csv"))

df_model<- cbind(var_a_expliquer,as.data.frame(as.matrix(dtm_final)))
names(df_model)<-c("genre_film",names(as.data.frame(as.matrix(dtm_final))))

df_model_nonsupervise<- df_model[-which(rowSums(as.matrix(dtm_final))==0),]


var_a_expliquer<- as.factor(unname(unlist(var_a_expliquer)))

yobs_lda<- as.data.frame(fread("yobs_lda_980var.csv"))
ynonsupervise_lda<-as.data.frame(fread("ynonsupervise_lda_980var.csv"))

pred_nonsupervise<- data.frame( yobs_lda, ynonsupervise_lda)
names(pred_nonsupervise)<- c("OBS","LDA")
pred_nonsupervise$OBS<- as.factor(pred_nonsupervise$OBS)
pred_nonsupervise$LDA<- as.factor(pred_nonsupervise$LDA)


#regarder comment les genres se répartissent dans chaque cluster

distgenres_nonsupervise<- function(choix_cluster) { 
  #Representation de la categorie dans le cluster choisi
  prop_choix<-table(pred_nonsupervise[pred_nonsupervise[,"LDA"]==choix_cluster,"OBS"])/sum(table(pred_nonsupervise[pred_nonsupervise[,"LDA"]==choix_cluster,"OBS"]))
  #Represenation de la categorie dans le total
  prop_total<-table(pred_nonsupervise[,"OBS"])/sum(table(pred_nonsupervise[,"OBS"]))
  #Regarder quelle categorie est surrepresentee dans le total
  surrepresentation_genre<- as.data.frame(sort(round(prop_choix/prop_total,2), decreasing = T))
  names(surrepresentation_genre)<- c("Genre","Sur_representation")
  surrepresentation_genre$Genre <- factor(surrepresentation_genre$Genre, levels = surrepresentation_genre$Genre)
  return(surrepresentation_genre)
}


#et quels sont les genres surreprésentés qui apparaissent beaucoup de fois (= genres difficiles à distinguer)

df_test<- data.frame()
for (i in 1:18) {
  df_test<- rbind(df_test,distgenres_nonsupervise(i))
} 
df_test$Nb_Surrepresente<- ifelse(df_test$Sur_representation>1,1,0)
table(df_test$Nb_Surrepresente,df_test$Genre)
# les genres qui ont le plus de sur-représentation > 1 dans les clusters :
# comedie_dramatique, drame, comedie, epouvante_horreur
# à l'inverse ceux qui en ont le moins (= ceux dont le genre est très spécifique) :
plot(df_test$Genre[df_test$Nb_Surrepresente==1])

df_test_ggplot<- as.data.frame(table(df_test$Genre[df_test$Nb_Surrepresente==1],df_test$Nb_Surrepresente[df_test$Nb_Surrepresente==1]))
df_test_ggplot<- df_test_ggplot[,-2]
names(df_test_ggplot)<- c("Genre","Nb_Surrepresente")

genres_generiques<- ggplot(df_test_ggplot, aes(x=Genre, y=Nb_Surrepresente/18, fill=Nb_Surrepresente/18)) +
  #coord_flip() +
  scale_fill_gradient("Ciblés >...> Génériques", low="#663399", high="#CCCCFF") +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = levels(var_a_expliquer)) +
  theme(legend.position="bottom",
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 10, face = "bold", angle = 40, lineheight = 1, hjust=1),
        # axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        #axis.text.y=element_text(size = 10, face = "bold", angle = 0, lineheight = 1),
        axis.ticks.y=element_blank() ) +
  labs(title="Apparitions des genres sur-représentés dans les clusters")


#extraire les topword pour chaque cluster

dtm_final_nonsupervise_m <- as.matrix(df_model[,-1])
df_mots_nonsupervise<-data.frame()
clusters <- levels(pred_nonsupervise$LDA)
for (i in 1:length(clusters)){
  m <- dtm_final_nonsupervise_m[pred_nonsupervise[,"LDA"]==clusters[i],]
  wordsFreq <- sort(colSums(m),decreasing = TRUE)
  tmp_wordsFreqcat <- colSums(m)
  tmp_sumcat <- sum(tmp_wordsFreqcat)
  tmp_wordsFreqtot <- colSums(dtm_final_nonsupervise_m)
  tmp_sumtot <-sum(tmp_wordsFreqtot)
  surrepresentation<- (tmp_wordsFreqcat/tmp_sumcat) / (tmp_wordsFreqtot/tmp_sumtot)
  df_mots_nonsupervise<-rbind(df_mots_nonsupervise,data.frame(Cluster_LDA=clusters[i], Mot = names(surrepresentation), 
                                                              Comptage = as.vector(tmp_wordsFreqcat),
                                                              Proportion_cluster = as.vector(tmp_wordsFreqcat/tmp_sumcat),
                                                              Proportion_total = as.vector(tmp_wordsFreqtot/tmp_sumtot),
                                                              Sur_representation = as.vector(surrepresentation)))
}

surrepresentation_mot<- df_mots_nonsupervise[order(df_mots_nonsupervise[,"Cluster_LDA"], -df_mots_nonsupervise[,"Comptage"]), c("Cluster_LDA","Mot","Comptage","Sur_representation")]
names(surrepresentation_mot)<- c("Cluster LDA","Top words (racinisés)","Nb occurences","Sur-représentation")

topwords_nonsupervise<- function(choix_cluster) {
  surrepresentation_mot<- surrepresentation_mot[surrepresentation_mot[,"Cluster LDA"] == choix_cluster, -1]
  
  df <- data.frame(Mots=surrepresentation_mot$'Top words (racinisés)'[1:20], 
                   Sur_representation=surrepresentation_mot$'Sur-représentation'[1:20], 
                   Comptage= surrepresentation_mot$'Nb occurences'[1:20])
  df <- df[order(-df[,"Comptage"]), c("Mots","Sur_representation")]
  df$Mots <- factor(df$Mots, levels = df$Mots)
  return(df)
}




histo<- ggplot(df_model_nonsupervise, aes(x=genre_film)) +
  geom_bar(fill="lightskyblue3") +
  labs(title = "Genres à expliquer", x = "", y = "") +
  theme(axis.text.x = element_text(size = 10, face = "bold", angle = 40, lineheight = 1, hjust=1))


liste_genres<-levels(var_a_expliquer)
names(liste_genres) <- c("Action","Animation","Aventure","Biopic","Comédie","Comédie dramatique","Comédie musicale",
                         "Documentaire","Drame","Epouvante-horreur","Fantastique","Guerre","Historique",
                         "Policier","Romance","Science-fiction","Thriller","Western")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(genre) {
  if (!(genre %in% liste_genres))
    stop("Genre inconnu")
  
  select_genre<- sapply(var_a_expliquer, function(x) {x==genre})
  dtm_genre = dtm_final[select_genre,]
  m = as.matrix(t(dtm_genre))
  sort(rowSums(m), decreasing = TRUE)
})




liste_mesures_tm <- c("Accuracy","Kappa")
liste_modeles_tm <- c("Random Forest","eXtreme Gradient Boosting","Linear Discriminant Analysis")
liste_cluster_tm <- clusters


##########################################
#                 Interface              #
##########################################


ui <- fluidPage(
  
  navbarPage("Projet Allociné",
             tabPanel("exploration",
                      fluidPage(
                        fluidRow(
                          column(4,
                                 radioButtons("stat", "Statistique à suivre", c("Nombre de films" = 1, "Notes presse moyenne" = 2, "Note spectateurs moyenne" = 3))),
                          column(5, 
                                 sliderInput("periode", label = "Période de temps", min = 1990, max = 2018, value = c(1990, 2018)))),
                        fluidRow(column(width = 12),
                                 tabsetPanel(
                                   tabPanel("Par année",plotOutput("graphe_annee",height="600px")),
                                   tabPanel("Par genre", plotOutput("graphe_genre",height="600px")),
                                   tabPanel("Par année et par genre", plotOutput("graphe_genre_annee",height="600px")),
                                   tabPanel("Par nationalité", plotOutput("graphe_nationalites",height="600px")))
                        )
                      )
                      
                      
                      
             ),
             tabPanel("Les résultats",
                      fluidPage(
                        fluidRow(column(width = 3),
                                 textOutput("text_resultat"),
                                 column(width = 12),
                                 radioButtons("choix_press_spect", "Notes : ",
                                              c("presse","spectateur"), "presse", inline = TRUE)),
                        fluidRow(column(width = 12),
                                 tabsetPanel(tabPanel("Mesures",
                                                      selectInput("select_mesure", "mesure choisie :", c("RMSE", "MAE", "RMSE & MAE"),
                                                                  "RMSE", multiple = FALSE),
                                                      textOutput("description"),
                                                      plotOutput("mesure",height="600px")),
                                             tabPanel("Distributions", plotOutput("distribution",height="600px")),
                                             tabPanel("Nuages", plotOutput("nuage",height="600px")),
                                             tabPanel("Variables",
                                                      selectInput("select_modele", "modele choisi :", c("Arbre CART","Gradient Boosting","Random Forest","Regression Lineaire","Regression Lineaire Backward","Regression Lineaire Forward","Regression Ridge Lasso","SVM","SVM Lineaire","eXtreme G Boost"),
                                                                  "RMSE", multiple = FALSE),
                                                      plotOutput("variable",height="600px"))
                                                    ))
                      )
                      
             ),
             tabPanel("Textmining",
                      fluidPage(
                        fluidRow(column(width = 3),
                                 textOutput("textmining")),
                        fluidRow(column(width = 12),
                                 tabsetPanel(
                                   tabPanel("Descriptif", 
                                            sidebarLayout(
                                              # Sidebar with a slider and selection inputs
                                              sidebarPanel(
                                                width = 3,
                                                selectInput("select_genre_tm", "Genre choisi :",
                                                            choices = liste_genres),
                                                hr(),
                                                sliderInput("freq",
                                                            "Fréquence minimum:",
                                                            min = 1,  max = 100, value = 25),
                                                sliderInput("max",
                                                            "Nombre maximum:",
                                                            min = 1,  max = 300,  value = 100)
                                              ),
                                              
                                              # Show Word Cloud
                                              mainPanel(
                                                fluidRow(  plotOutput("nuage_tm") )
                                                # fluidRow( plotOutput("nuage_tm") )
                                              )
                                            ),
                                            # fluidRow( plotOutput("histo_plot") )
                                            fluidRow( splitLayout(cellWidths = c("20%", "80%"), verbatimTextOutput("summary"), plotOutput("histo_plot")))
                                            
                                   ),
                                   tabPanel("Performances modèles supervisés",
                                            selectInput("select_mesure_tm", "Mesure choisie :", liste_mesures_tm,
                                                        "Accuracy", multiple = FALSE),
                                            #textOutput("description_tm"),
                                            plotOutput("mesure_tm"),
                                            selectInput("select_modele_tm", "Modèle choisi :", liste_modeles_tm,
                                                        "Random Forest", multiple = FALSE),
                                            plotOutput("modele_tm")),
                                   tabPanel("Aperçu non supervisé", 
                                            selectInput("select_cluster_tm", "Cluster LDA choisi :", liste_cluster_tm,
                                                        "1", multiple = FALSE),
                                            # splitLayout(cellWidths = c("50%", "50%"), verbatimTextOutput("nonsupervise_genres"), verbatimTextOutput("nonsupervise_topwords")) )
                                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("nonsupervise_genres"), plotOutput("nonsupervise_topwords")),
                                            splitLayout(cellWidths = c("60%", "40%"), plotOutput("nonsupervise_generiques") ) ) 
                                 )
                        )
                      )                      
             )
  )
)

##########################################
#                   Serveur              #
##########################################

server <- function(input, output) {
  
  # Couleur du graphique en fonction du paramètre
  couleur_graph <- reactive({
    couleur_graph<-  switch(input$stat,
                            "1" = c("royalblue", "Blues"),
                            "2" = c("blueviolet","Purples"),
                            "3" = c("aquamarine4", "Greens"))
  })
  
  
  
  # Stats du nombre de films et notes moyennes en fonction du type de film
  stats_genre <- reactive({
    data <- cbind(annee_sortie = final$annee_sortie, id_film = final$id_film, final[,str_detect(names(final), "genre")]) %>% 
      melt(id =c("id_film", "annee_sortie")) %>% 
      left_join(final[,c("id_film","note_presse_moyenne", "note_spectateurs_moyenne")], by ="id_film") %>% 
      filter(annee_sortie >= input$periode[1], annee_sortie <= input$periode[2]) 
    
    data$variable <- str_replace(data$variable, "genre_", "")
    
    stats_genre <- group_by(data, variable) %>% filter(value==1) %>%  
      summarise(nb_films = n(), note_presse = round(mean(note_presse_moyenne, na.rm = TRUE), 2),
                note_spectateurs = round(mean(note_spectateurs_moyenne, na.rm = TRUE), 2))
    
  })
  
  # Stats du nombre de films et notes moyennes en fonction des années
  stats_annee <- reactive({
    stats_annee <- filter(final, annee_sortie >= input$periode[1], annee_sortie <= input$periode[2]) %>%
      group_by(annee_sortie) %>% summarise(nb_films = n(),
                                           note_presse = round(mean(note_presse_moyenne, na.rm = TRUE), 1),
                                           note_spectateurs = round(mean(note_spectateurs_moyenne, na.rm = TRUE), 1))
    
  })
  
  # Stats du nombbre de films et notes moyennes en fonction du type de film et des années
  stats_genre_annee <- reactive({
    data <- cbind(annee_sortie = final$annee_sortie, id_film = final$id_film, final[,str_detect(names(final), "genre")]) %>% 
      melt(id =c("id_film", "annee_sortie")) %>% 
      left_join(final[,c("id_film","note_presse_moyenne", "note_spectateurs_moyenne")], by ="id_film") %>% 
      filter(annee_sortie >= input$periode[1], annee_sortie <= input$periode[2]) 
    
    data$variable <- str_replace(data$variable, "genre_", "")
    
    stats_genre_annee <- group_by(data, variable, annee_sortie) %>% filter(value==1) %>%  
      summarise(nb_films = n(), note_presse = round(mean(note_presse_moyenne, na.rm = TRUE), 1),
                note_spectateurs = round(mean(note_spectateurs_moyenne, na.rm = TRUE), 1))
  })
  
  # Stats du nombre de films et notes moyenes par nationalites
  # Nb films par nationalité
  stats_nationalites <- reactive({
    stats_nationalites <- data_cartographie %>% group_by(region) %>% filter(value==1) %>% 
      filter(annee_sortie >= input$periode[1], annee_sortie <= input$periode[2]) %>% 
      summarise(nb_films = n(),
                note_presse = mean(note_presse_moyenne, na.rm = TRUE),
                note_spectateurs = mean(note_spectateurs_moyenne, na.rm = TRUE)) %>% 
      arrange(desc(nb_films)) %>% 
      head(30)  
    
  })
  
  
  
  output$graphe_annee <- renderPlot({
    
    # Graphiques du nombre de films et notes moyennes par année
    graphe_annee <- function(varname, titregraph) {
      
      graph <- ggplot(stats_annee(),aes(annee_sortie, get(varname), fill = nb_films)) + 
        geom_bar(color = "white", fill = couleur_graph()[1], stat = "identity") +
        geom_text(aes(label = get(varname), vjust = 0.5), position = position_stack(vjust=0.5), color = "white", fontface ="bold", size = 4) +
        theme_economist()  + 
        scale_x_continuous(breaks=seq(input$periode[1],input$periode[2],2)) +
        labs(x ="", y = "", title = titregraph, subtitle = "... par année", caption = paste0("Période : ", input$periode[1], "-", input$periode[2])) +
        theme(axis.text.x = element_text(size = 12),legend.position="null",
              plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
              plot.subtitle = element_text(size = 14),
              plot.caption = element_text(face = "italic"))
      
      if (varname != "nb_films") {     graph <- graph + scale_y_continuous(limits=c(0, 5)) }
      graph
      
    }
    
    switch(input$stat,
           "1" = graphe_annee("nb_films", "Nombre de films"),
           "2" = graphe_annee("note_presse", "Notes moyenne presse"),
           "3" = graphe_annee("note_spectateurs", "Notes moyenne spectateurs")
    )
    
    
  })
  
  
  output$graphe_genre <- renderPlot({
    
    # Graphiques du nombre de films et notes moyennes par genre
    graphe_genre <- function(varname, titregraph) {
      
      
      
      stats_genre <- stats_genre()
      
      if (varname != "nb_films") { 
        stats_genre[,get("varname")] <- ifelse(pull(stats_genre,get("nb_films"))< 30, NA, pull(stats_genre,get("varname")))}
      
      stats_genre <- arrange(stats_genre, get(varname))
      
      stats_genre$variable <- factor(stats_genre$variable, stats_genre$variable)
      
      
      graph <- ggplot(stats_genre,aes(variable, get(varname))) + 
        geom_bar(color = "white", fill = couleur_graph()[1], stat = "identity") +
        geom_text(aes(label = get(varname), vjust = 0.5), position = position_stack(vjust=0.5), fontface ="bold", size = 4, color = "white") +
        coord_flip() + theme_economist()  +
        labs(x ="", y = "", title = titregraph, subtitle = "... par genre", caption = paste0("Période : ", input$periode[1], "-", input$periode[2])) +
        theme(axis.text.x = element_text(size = 12),legend.position="null",
              axis.text.y = element_text(size = 12),
              plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
              plot.subtitle = element_text(size = 14),
              plot.caption = element_text(face = "italic"))
      
      if (varname != "nb_films") {     graph <- graph + scale_y_continuous(limits=c(0, 5)) }
      graph
      
    }
    
    switch(input$stat,
           "1" = graphe_genre("nb_films", "Nombre de films"),
           "2" = graphe_genre("note_presse", "Notes moyenne presse"),
           "3" = graphe_genre("note_spectateurs", "Notes moyenne spectateurs")
    )
    
    
  }) 
  
  output$graphe_genre_annee <- renderPlot({
    
    
    
    # Grahiques du nombre de films / notes moyennes par genre et année
    graphe_genre_annee <- function(varname, titregraph) {
      
      stats_genre_annee <- stats_genre_annee()
      
      if (varname != "nb_films") { 
        stats_genre_annee[,get("varname")] <- ifelse(pull(stats_genre_annee,get("nb_films"))< 10, NA, pull(stats_genre_annee,get("varname")))}
      
      
      ggplot(stats_genre_annee) +
        geom_tile(aes(annee_sortie, variable, fill = get(varname))) +
        #scale_fill_gradient(low = "skyblue", high = "royalblue4") +
        geom_text(aes(annee_sortie,variable,label = get(varname)), size = 4, color = "white") +
        theme_minimal()  + 
        scale_fill_distiller(palette = couleur_graph()[2], trans = "reverse") +
        scale_x_continuous(breaks=seq(input$periode[1],input$periode[2],2)) +
        labs(x ="", y = "", title = titregraph, subtitle = "... par genre et par année", caption = paste0("Période : ", input$periode[1], "-", input$periode[2])) +
        theme(axis.text.x = element_text(size = 12),legend.position="null",
              axis.text.y = element_text(size = 12),
              plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
              plot.subtitle = element_text(size = 14),
              plot.caption = element_text(face = "italic"))
    }
    
    switch(input$stat,
           "1" = graphe_genre_annee("nb_films", "Nombre de films"),
           "2" = graphe_genre_annee("note_presse", "Notes moyenne presse"),
           "3" = graphe_genre_annee("note_spectateurs", "Notes moyenne spectateurs")
    )
  })  
  
  output$graphe_nationalites <- renderPlot({
    
    # Cartographie du nombre de films / notes moyennes par nationalités
    graphe_nationalites <- function(varname, titregraph) {
      
      map <- map_data("world",wrap=c(-170,175))
      map <- merge(map, stats_nationalites(), by = "region", all.x = TRUE)
      map <- subset(map, region!="Antarctica")
      table(map$region)
      map <- map[order(map$order), ]
      
      graph <- ggplot(map, aes(long, lat, group = group, fill = get(varname))) + 
        geom_polygon(color = "white", size = 0.1) + theme_void()+coord_map() +
        #geom_point(aes(long, lat, size = nb_films), col = "red") +
        scale_fill_distiller(palette = couleur_graph()[2], trans = "reverse" ) +
        labs(title = titregraph, subtitle = "... par année", fill = titregraph, caption = paste0("Période : ", input$periode[1], "-", input$periode[2]))  +
        theme(plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
              plot.subtitle = element_text(size = 14),
              plot.caption = element_text(face = "italic"))
      graph
    }
    
    switch(input$stat,
           "1" = graphe_nationalites("nb_films", "Nombre de films"),
           "2" = graphe_nationalites("note_presse", "Notes moyenne presse"),
           "3" = graphe_nationalites("note_spectateurs", "Notes moyenne spectateurs")
    )
  })  
  
  output$summary <- renderPrint({
    if (input$type_graphe == 1) {  print(stats_annee()) }
    else if (input$type_graphe == 3) {  print(stats_nationalites()) }
    else if (input$type_graphe == 2) {  print(stats_genre()) }
    else if (input$type_graphe == 4) {  print(stats_genre_annee()) }    
  })
  
  output$text_resultat <- renderText({ Resultats_txt })
  output$description <- renderText({
    switch(input$select_mesure,
           "RMSE" = RMSE_def,
           "RMSE & MAE" = RMSE_MAE_def,
           "MAE" = MAE_def,
           "NA")
  })
  
  output$mesure <- renderPlot({
    if(input$choix_press_spect == "presse") {
      switch(input$select_mesure,
             "RMSE" = graph_RMSE_presse,
             "RMSE & MAE" = graph_MAE_MSE_presse,
             "MAE" = graph_MAE_presse,
             "NA")
    } else {
      switch(input$select_mesure,
             "RMSE" = graph_RMSE_spectateurs,
             "RMSE & MAE" = graph_MAE_MSE_spectateurs,
             "MAE" = graph_MAE_spectateurs,
             "NA")
    }
    })

    output$variable <- renderPlot({
      if(input$choix_press_spect == "presse") {
        switch(input$select_modele,
               #"Random Forest" = graph_rf_presse,
               "Arbre CART" = graph_rpart_presse,
               "Gradient Boosting" = graph_gradient_presse,
               "Random Forest" = graph_rf_presse,
               "Regression Lineaire" = graph_regression_presse,
               "Regression Lineaire Backward" = graph_regbackward_presse,
               "Regression Lineaire Forward" = graph_regforward_presse,
               "Regression Ridge Lasso" = graph_glmnet_presse,
               "SVM" = graph_svm_presse,
               "SVM Lineaire" = graph_svmlineaire_presse,
               "eXtreme G Boost" = graph_xgboost_presse,
               "NA")
      } else {
        switch(input$select_modele,
               #"Random Forest" = graph_rf_spectateurs,
               "Arbre CART" = graph_rpart_spectateurs,
               "Gradient Boosting" = graph_gradient_spectateurs,
               "Random Forest" = graph_rf_spectateurs, 
               "Regression Lineaire" = graph_regression_spectateurs,
               "Regression Lineaire Backward" = graph_regbackward_spectateurs,
               "Regression Lineaire Forward" = graph_regforward_spectateurs,
               "Regression Ridge Lasso" = graph_glmnet_spectateurs,
               "SVM" = graph_svm_spectateurs,
               "SVM Lineaire" = graph_svmlineaire_spectateurs,
               "eXtreme G Boost" = graph_xgboost_spectateurs,
               "NA")
      }
  })
    
  output$distribution <- renderPlot({
    if(input$choix_press_spect == "presse") { graph_distrib_presse } else { graph_distrib_spectateurs}
  })
  
  output$nuage <- renderPlot({
    if(input$choix_press_spect == "presse") { graph_nuage_presse } else { graph_nuage_spectateurs}
  })
  
  ##textmining
  
  genre_tm_reactive <- reactive({
    input$select_genre_tm
    isolate({
      withProgress({
        setProgress(message = "Compilation...")
        getTermMatrix(input$select_genre_tm)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$nuage_tm <- renderPlot({
    v <- genre_tm_reactive()
    wordcloud_rep(names(v), v, scale=c(2.5,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  output$summary <- renderPrint({
    v <- as.data.frame(genre_tm_reactive())
    names(v)<- "Occurences d'un Mot explicatif"
    summary(v)
  })
  
  output$histo_plot <- renderPlot({
    histo
  })
  
  
  mesure_tm_reactive <- reactive({
    input$select_mesure_tm  
  })
  
  # output$description_tm <- renderText({
  #   desc_tm_reactive()
  # })
  
  
  
  output$mesure_tm <- renderPlot({
    ggplot_mesure_tm(mesures_supervise,mesure_tm_reactive())
  })
  
  modele_tm_reactive <- eventReactive(input$select_modele_tm, {
    switch(input$select_modele_tm,
           "Random Forest" = confmat_RF,
           "eXtreme Gradient Boosting" = confmat_XGB,
           "Linear Discriminant Analysis" = confmat_LDA)
  }, ignoreNULL = FALSE)
  
  
  output$modele_tm <- renderPlot({
    cm.plot(modele_tm_reactive()$table)
  })
  
  
  
  
  nonsupervise_cluster_reactive <- reactive({
    input$select_cluster_tm  
  })
  
  
  output$nonsupervise_genres <- renderPlot({
    df <- distgenres_nonsupervise(nonsupervise_cluster_reactive())
    ggplot(df, aes(x=Genre, y=Sur_representation, fill=Sur_representation )) +
      coord_flip() +
      scale_fill_gradient("Sur-représentation si > 1", low="#333300", high="#99FF33") +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits = rev(levels(df$Genre))) +
      theme(legend.position="bottom",
            axis.title.x=element_blank(),
            # axis.text.x=element_blank(),
            # axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_text(size = 10, face = "bold", angle = 0, lineheight = 1),
            axis.ticks.y=element_blank() ) +
      labs(title="Répartition des genres")
  })
  
  output$nonsupervise_topwords <- renderPlot({
    df <- topwords_nonsupervise(nonsupervise_cluster_reactive())
    ggplot(df, aes(x=Mots, y=Sur_representation, fill=Sur_representation )) +
      coord_flip() +
      scale_fill_gradient("Sur-représentation si > 1") +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits = rev(levels(df$Mots))) + 
      theme(legend.position="bottom",
            axis.title.x=element_blank(),
            # axis.text.x=element_blank(),
            # axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_text(size = 10, face = "bold", angle = 0, lineheight = 1),
            axis.ticks.y=element_blank() ) +
      labs(title="Top 20 des mots les plus occurents")
  })
  
  output$nonsupervise_generiques <- renderPlot({
    genres_generiques
  })
  
}

##########################################
#             Lancement appli            #
##########################################

shinyApp(ui = ui, server = server)
#runApp(display.mode="showcase")
