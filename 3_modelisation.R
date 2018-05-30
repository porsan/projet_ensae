##########################################
#          Librairies - options          #
##########################################

#install.packages("caret")
#install.packages("gbm")
#install.packages("leaps")
#install.packages("glmnet")
#install.packages("kernlab")

# Visualisation
library(ggplot2)
library(plotly) # Graphiques avec intéractions
library(rpart.plot) # Tracer des arbres de décisions 

# Traitement de donn?es
library(reshape2)
library(stringr)
library(dplyr)

# Modélisation
library(caret) # Ensemble de modèles machine learning 
library(randomForest) # Random Forest
library(rpart) # Arbres de décision 
library(gbm) # Gradient boosting
library(leaps) # Régression avec sélection stepward / forward
library(glmnet) # Régression pénalisée (Lasso - Ridge - ElasticNet)
library(e1071) # SVM (autre alternative : kernlab)
library(extraTrees) # Extreme radio forest

# Autres
library(parallel) # Pas utilisé chez moi car seulement 2 coeurs :)


############################################
#             Lecture données              #
############################################




rep_data <- "E:/Formation CEPE/Projet/"

principal <- read.csv2(paste0(rep_data,"table_principal2.csv"))
notes <- read.csv2(paste0(rep_data,"table_notes.csv"))
personnes <- read.csv2(paste0(rep_data,"variables_personnes.csv"))
casting <- read.csv2(paste0(rep_data,"table_casting2.csv"))

# table note : on ne garde que l'ID et les notes moyennes
str(notes)
notes <- select(notes, c(id_film.x, note_spectateurs_moyenne, note_presse_moyenne))
notes <- rename(notes,id_film=id_film.x)


# table personnes : supprimer les variables avec trop de NA, et imputer les autres NA avec la moyenne
str(personnes)
sapply(personnes, function(x) sum(is.na(x))) # Nombre de NA pour chacune des variables


# Suppression des réalisateurs 2 et acteurs > 4

var_to_drop <- c(names(personnes)[str_detect(names(personnes), "realisateur2")],
                 names(personnes)[str_detect(names(personnes), "5")],
                 names(personnes)[str_detect(names(personnes), "6")],
                 names(personnes)[str_detect(names(personnes), "7")],
                 names(personnes)[str_detect(names(personnes), "8")])
personnes <- select(personnes, -one_of(var_to_drop))


# Remplacer les NA par la moyenne sur les age / ans / nb_films

remplace_NA_moyenne <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}

remplace_NA_0 <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

var_NA_to_moyenne <- c(names(personnes)[str_detect(names(personnes), "age")],
                       names(personnes)[str_detect(names(personnes), "an_carriere")],
                       names(personnes)[str_detect(names(personnes), "films")])

var_NA_to_0 <- c(names(personnes)[str_detect(names(personnes), "prix")],
                 names(personnes)[str_detect(names(personnes), "nomination")])

personnes[,var_NA_to_moyenne] <- data.frame(sapply(personnes[,var_NA_to_moyenne], remplace_NA_moyenne))
personnes[,var_NA_to_0] <- data.frame(sapply(personnes[,var_NA_to_0], remplace_NA_0))

str(personnes[,var_NA_to_moyenne])
str(data.frame(sapply(personnes[,var_NA_to_moyenne], remplace_NA_moyenne)))


final <- left_join(principal, notes[!is.na(notes$id_film),], "id_film")
final <- left_join(final, personnes, "id_film")
final <- left_join(final, casting[!is.na(casting$id_film),], "id_film")

# Vérif : nombre de valeurs renseignées dans les variables à prédire
sum(!is.na(final$note_presse_moyenne))
sum(!is.na(final$note_spectateurs_moyenne))


sum(!is.na(final[final$annee_sortie >= 1990,"note_presse_moyenne"]))


############################################
#             Nettoyage final              #
############################################


summary(final)
str(final, list.len= 500)
head(final)
dim(final)



# Variables à convertir en factor
list_var_fact <- c(names(final) %>% str_subset("dist_"),
                   names(final) %>% str_subset("genre_"),
                   names(final) %>% str_subset("nationalite_"),
                   names(final) %>% str_subset("scenariste_"),
                   names(final) %>% str_subset("acteur_"),
                   names(final) %>% str_subset("realisateur_"))

# Passage en factors des variables quali 
final[,list_var_fact] <- data.frame(sapply(select(final, list_var_fact), factor))

# Dernières variables à rajouter 
#final$pct45_spectateurs<- (final$notes_spectateurs_4 + final$notes_spectateurs_5)/100

# Suppression de l'année 2018
final <- filter(final, annee_sortie != 2018)

# Suppression des années > 1990, car les notes critiques sont trop fortes (biais de la variable année)
final <- filter(final, annee_sortie >= 1990)


# Suppression des dernières lignes à NA
final <- filter(final, !is.na(jour_semaine_sortie)) %>% filter(!is.na(saison_sortie)) %>% filter(!is.na(acteur1_age_sortie))
final <- filter(final, !is.na(jour_semaine_sortie)) %>% filter(!is.na(saison_sortie)) %>% filter(!is.na(acteur_Isabelle.Huppert))


# Dernières Variables à supprimer juste pour la modélisation
final <- select(final, -one_of(c("X", "X.x", "X.y", "synopsis", "nb_prix", "nb_nominations", "date_sortie")))  # On garde "annee_sortie" pour l'instant....


# Transformation des boolean en factor (nécessaire pour gradient boosting)
final$reprise <- factor(final$reprise)
final$multilingues <- factor(final$multilingues)

# Jeux de données presse et spectaters
final_spectateurs <- filter(final, !is.na(note_spectateurs_moyenne)) %>% select(-one_of(c("note_presse_moyenne")))
final_presse <- filter(final, !is.na(note_presse_moyenne))  %>% select(-one_of(c("note_spectateurs_moyenne")))

dim(final_spectateurs) ##  15 758 lignes
dim(final_presse) ## 11 138 lignes

# Vérif que plus aucun NA ne traine
sapply(final, function(x) {sum(is.na(x))})

str(final, list = 500)


############################################
#     Découpage test / validation          #
############################################



# On fixe la graine
set.seed(19)

###### Presse ###### 

nb_films_presse <- nrow(final_presse)

id_train_presse <- sample.int(nb_films_presse, round(nb_films_presse *0.75, 0))

train_presse <- final_presse[id_train_presse,] %>% select(-one_of(c("titre", "url", "id_film")))
x_train_presse <-  select(train_presse, -one_of(c("note_presse_moyenne"))) 
y_train_presse <- pull(train_presse,note_presse_moyenne) 

validation_presse <- final_presse[-id_train_presse,] %>% select(-one_of(c("titre", "url", "id_film")))
x_validation_presse <- select(validation_presse, -one_of(c("note_presse_moyenne")))
y_validation_presse <-  pull(validation_presse,note_presse_moyenne)

# Vérif cohérence
dim(train_presse)
dim(x_train_presse)
length(y_train_presse)

dim(validation_presse)
dim(x_validation_presse)
length(y_validation_presse)

str(x_train_presse)


###### Spectateurs  ###### 

nb_films_spectateurs <- nrow(final_spectateurs)

id_train_spectateurs <- sample.int(nb_films_spectateurs, round(nb_films_spectateurs *0.75, 0))

train_spectateurs <- final_spectateurs[id_train_presse,] %>% select(-one_of(c("titre", "url", "id_film")))
x_train_spectateurs <- select(train_spectateurs, -one_of(c("note_spectateurs_moyenne")))
y_train_spectateurs <- pull(train_spectateurs,note_spectateurs_moyenne)

validation_spectateurs <- final_spectateurs[-id_train_spectateurs,] %>% select(-one_of(c("titre", "url", "id_film")))
x_validation_spectateurs <- select(validation_spectateurs, -one_of(c("note_spectateurs_moyenne")))
y_validation_spectateurs <- pull(validation_spectateurs,note_spectateurs_moyenne)

# Vérif cohérence
dim(x_train_spectateurs)
length(y_train_spectateurs)
dim(x_validation_spectateurs)
length(y_validation_spectateurs)

dim(train_spectateurs)
dim(validation_spectateurs)





############################################
#          Train Random Forest             #
############################################

?randomForest


###### Presse ###### 

# Train du modèle avec paramètres de base (500 arbres, nb variables / 3 en mtry, 5 obs par feuille finale)
rf_presse <- randomForest(x_train_presse, y_train_presse, ntree = 500)

# Résumé modèle
summary(rf_presse)

# Ajustement du mtry
#error <- numeric(5)
#for (i in 1:4) {
#  print(paste0("Tour ",i))
#  rf_presse <- randomForest(x_train_presse, y_train_presse, mtry = 26 + i) # On teste entre 27 et 30
#  error[i] <- rf_presse$mse[500]
#  }
#error

# Importance des variables
importance(rf_presse)[order(importance(rf_presse)),]

# vecteurs des résultats
rf_presse_predict  <- rf_presse$predict

# MSE train
rf_presse_MSE_train  <- mean((rf_presse_predict - y_train_presse)^2)




###### Spectateurs ###### 

#rf_spectateurs <- randomForest(x_train_spectateurs, y_train_spectateurs)

importance(rf_spectateurs)[order(importance(rf_spectateurs)),]

summary(rf_spectateurs)


importance(rf_presse)[order(importance(rf_presse)),]


## Sauvegarde des mod?els obtenus
setwd("H:/Formation CEPE/Projet")
save(rf_presse, file = "rf_presse")
save(rf_spectateurs, file = "rf_spectateurs")


############################################
#             Train Arbre CART             #
############################################


?rpart

###### Presse ###### 

# Modélisation sur échantillon train
rpart_presse <- rpart(note_presse_moyenne ~ ., data = train_presse, control = rpart.control(cp = 0.0012) )

# Résultats modélisation
summary(rpart_presse)

# Visualisation de l'arbre
prp(rpart_presse)

# Visualisation des résultats de la validation croisée en fonction du paramètre cp choisi
plotcp(rpart_presse)

# Détermination du seuil optimal
rpart_presse$cptable[which.min(rpart_presse$cptable[,4]),1]

# Importance des variables
rpart_presse$variable.importance


# vecteurs des résultats
rpart_presse_predict  <- rpart_presse$predict

# MSE sur échantillon train
rpart_presse_MSE_train  <- mean((predict(rpart_presse)- y_train_presse)^2)


#############################################
#         Train Gradient boosting           #
#############################################

?gbm


# Modélisation sur échantillon train, avec validation croisée 10n pour l'optimisation du nombre d'itérations
gradient_presse <- gbm(note_presse_moyenne ~ ., train_presse, distribution = "gaussian", n.trees = 500, interaction.depth = 4, shrinkage = 0.1, cv.folds = 10)

# Shrinkage ralentit la convergence de l'algorithme donc nécessite plus d'itérations si il est petit 

# même chose sans validation croisée mais en hold out : forcément beaucoup plus rapide (~dix fois)
gradient_presse <- gbm(note_presse_moyenne ~ ., train_presse, distribution = "gaussian", n.trees = 500, interaction.depth = 4, shrinkage = 0.1, train.fraction = 0.75)

# Importance des variables
summary(gradient_presse)

# Trouver le meilleur nombre d'itérations
nbarbres <- gbm.perf(gradient_presse, method = "test")


# vecteurs des résultats
gradient_presse_predict  <- predict(gradient_presse, n.trees = nbarbres )

# MSE suréchantillon train
gradient_presse_MSE_train  <- mean((gradient_presse_predict - train_presse$note_presse_moyenne)^2)




#############################################
#           Régression linéaire             #
#############################################




?lm
# On sait d'office que c'est pas pertinent car on veut en sortie une note entre 0 et 5 ; hors la r?gression va donner en résultats des valeurs plus extr?mes


# SPECIAL : pour la régression linéaire on doit enlever ces deux variables qui ont que des 0...
train_presse <- select(train_presse,-one_of(c("scenariste__Michel.Audiard", "acteur_Jean.Carmet")))




# Train régression linéaire avec toutes les variables
regression_presse <- lm(note_presse_moyenne~., data = train_presse)

# Coefficients de la régression
sort(coefficients(regression_presse), decreasing = TRUE)

# Analyse des résidus
residus <- predict(regression_presse)-train_presse$note_presse_moyenne  #Idem que  :residuals(regression_presse)
plot(residus)

# Calcul manuel du R2
regression_presse_R2_train <- 1 - sum((regression_presse_predict - y_train_presse)^2) / sum((y_train_presse - mean(y_train_presse))^2)

# Résidus studentisés
plot(rstudent(regression_presse))

# vecteurs des résultats
regression_presse_predict  <- predict(regression_presse)

# MSE sur échantillon train
regression_presse_MSE_train  <- mean((regression_presse_predict - train_presse$note_presse_moyenne)^2)



#############################################
#      Régression linéaire regsubset        #
#############################################

?regsubsets


# Utilisation de regsubsets pour sélectionner le nombre optimal de variables
regbackward_presse <- regsubsets(note_presse_moyenne~., data = train_presse, method ="backward", nvmax = 130, really.big = TRUE)

# Détermination du nombre de variable optimal au regard du BIC : 41 variables
nbvar_opti <- which.min(summary(regbackward_presse)$bic)

# Coefficients du "modèle optimal" avec 41 variables
final_coeff <- coef(regbackward_presse, nbvar_opti)


# Fonction pour prédire les valeurs avec regsubsets (trouvé sur Internet...)
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

# Vecteur des prédictions avec 41 variables
regbackward_presse_predict <- predict.regsubsets(regbackward_presse, train_presse, 41)

# MSE sur échantillon train
regbackward_presse_MSE_train  <- mean((regbackward_presse_predict - train_presse$note_presse_moyenne)^2)




#############################################
#             Régression Lasso              #
#############################################

?glmnet
?cv.glmnet

# Transformation des données pour avoir des données au format matriciel
x_train_presse_lasso = model.matrix(note_presse_moyenne~.,data=train_presse) 
y_train_presse_lasso=y_train_presse
x_validation_presse_lasso = model.matrix(note_presse_moyenne~.,data=validation_presse)

# NOTE : centrer réduire les données, indispensable / pour la régression bridge / lasso, mais déjà fait par GLM (standardize = TRUE)

# Train du modèle avec cross validation pour le paramètre lambda et alpha à 1 (lasso)
lasso_presse  <- cv.glmnet(x_train_presse_lasso, y_train_presse_lasso, alpha = 1) #cv.glmnet pour cross validation du lambda

# Lambda élevé (proche de 1) --> Tous les coefficients sont à 0 à part l'intercept
# Lambda à zéro --> Comme en régression linéaire simple, aucun coefficient à 0

# Evolution du MSE en fonction du lambda choisi (chemin régularisation)
plot(lasso_presse)

# Déterminer le lambda optimal, entre 0 et 1 (0.001, 0.001, 0.01....)
lasso_lambda_opti <- lasso_presse$lambda.min 
summary(lasso_presse)

# Petit test pour vérifier que quand on diminue le lambda, le nombre de variables sélectionnées augmente (jusqu'à tout garder à zéro)
#for(l in c(0.9, 0.1, 0.01, 0.001, 0.0001, 0) ) {
#  lasso_presse  <- glmnet(x_train_presse_lasso, y_train_presse_lasso, lambda = l, alpha = 1) #cv.glmnet pour cross validation du lambda
#  print(sum(coefficients(lasso_presse) != 0))
#}

# Train du modèle avec lambda optimal (pas sûr que ce soit vraiment nécessaire de le refaire...)
lasso_presse  <- glmnet(x_train_presse_lasso, y_train_presse_lasso, lambda = lasso_lambda_opti, alpha = 1) #cv.glmnet pour cross validation du lambda

# Coefficients du modèle optimal
coefficients(lasso_presse)

# Nb variables retenues modèle optimal
sum(coefficients(lasso_presse) != 0)

# Vecteur des prédictions 
lasso_presse_predict <- predict(lasso_presse, x_train_presse_lasso) # Rajouter "lambda = lasso_lambda_opti" si on est en mode "validation croisée"

# MSE sur échantillon train
lasso_presse_MSE_train  <- mean((lasso_presse_predict - train_presse$note_presse_moyenne)^2)



#############################################
#             Régression Ridge              #
#############################################


# On reprend les mêmes données qu'avec le lasso : même package et mêmes principes de préparation


# Train du modèle avec cross validation pour le paramètre lambda et alpha à 0 (ridge)
ridge_presse  <- cv.glmnet(x_train_presse_lasso, y_train_presse_lasso, alpha = 0) #cv.glmnet pour cross validation du lambda

# Courbe du RSE en fonction du lambda
plot(ridge_presse)

# Lambda optima
ridge_lambda_opti <- ridge_presse$lambda.min 

# Train du modèle avec lambda optimal 
ridge_presse  <- glmnet(x_train_presse_lasso, y_train_presse_lasso, lambda = ridge_lambda_opti, alpha = 0) 

# Coefficients du modèle optimal
coefficients(ridge_presse)

# Nb variables retenues modèle optimal toujours égal au total des variables !


# Vecteur des prédictions 
ridge_presse_predict <- predict(ridge_presse, x_train_presse_lasso) # Rajouter "lambda = lasso_lambda_opti" si on est en mode "validation croisée"

# MSE sur échantillon train
ridge_presse_MSE_train  <- mean((ridge_presse_predict - train_presse$note_presse_moyenne)^2)





############################################
#           SVM pour régression           #
############################################

?svm

# Train du modèle avec paramètres de base (kernel radial)
svm_presse <- svm(note_presse_moyenne ~ ., data = train_presse)
summary(svm_presse)

# Optimisation des paramètres cost et epsilon (en gardant le kernel radial = non linéaire) (on pourrait aussi optimiser le gamma ? )  
svm_presse_tune <- tune.svm(note_presse_moyenne ~ ., data = train_presse, cost = 10^(-3:2), epsilon = seq(0,0.1,0.01))
  # Epislon : maximul allowed error (défault : 0.1)
  # cost : penality cost (défaut : 1)


# Visualisation des performances de la svm en fonction des paramètres
plot(svm_presse_tune)

# Sélection du model optimal dans svm presse
svm_presse <- svm_presse_tune$best.model
summary(svm_presse)


# vecteurs des résultats
svm_presse_predict  <- predict(svm_presse)

# MSE train
svm_presse_MSE_train  <- mean((svm_presse_predict - y_train_presse)^2) #0.32



################################################
#         Validation / comparaison             #
################################################


# Vecteurs de résultats sur l'échantillon de validation pour chaque modèle optimisé 
results_rf_presse <- predict(rf_presse, newdata = x_validation_presse )
results_rpart_presse <- predict(rpart_presse, newdata = x_validation_presse )
results_gradient_presse <- predict(gradient_presse, newdata = x_validation_presse, n.tree = 238 )
results_regression_presse <- predict(regression_presse, newdata = x_validation_presse)
results_regbackward_presse <- predict.regsubsets(regbackward_presse, validation_presse, 41)
results_lasso_presse <- predict(lasso_presse, x_validation_presse_lasso)
results_ridge_presse <- predict(ridge_presse, x_validation_presse_lasso)
results_svm_presse <- predict(svm_presse, x_validation_presse)

# Erreurs par modéle sur l'échantillon de validation
rf_presse_MSE_valid <- mean((results_rf_presse - y_validation_presse)^2) # 0.337
rpart_presse_MSE_valid <- mean((results_rpart_presse - y_validation_presse)^2) #0,378
gradient_presse_MSE_valid <- mean((results_gradient_presse - y_validation_presse)^2) #0,325
regression_presse_MSE_valid <- mean((results_regression_presse - y_validation_presse)^2) #0,343
regbackward_presse_MSE_valid <- mean((results_regbackward_presse - y_validation_presse)^2) #0,346
lasso_presse_MSE_valid <- mean((results_lasso_presse - y_validation_presse)^2) #0,339
ridge_presse_MSE_valid <- mean((results_ridge_presse - y_validation_presse)^2) #0,343
svm_presse_MSE_valid <- mean((results_svm_presse - y_validation_presse)^2) #0,335

# R2 par modèle sur échantillon de validation
rf_presse_R2_valid <- 1 - sum((y_validation_presse - results_rf_presse)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)
rpart_presse_R2_valid <- 1 - sum((y_validation_presse - results_rpart_presse)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)
gradient_presse_R2_valid <- 1 - sum((y_validation_presse - results_gradient_presse)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)
regression_presse_R2_valid <- 1 - sum((y_validation_presse - results_regression_presse)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)
regbackward_presse_R2_valid <- 1 - sum((y_validation_presse - results_regbackward_presse)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)
lasso_presse_R2_valid <- 1 - sum((y_validation_presse - results_lasso_presse)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)
ridge_presse_R2_valid <- 1 - sum((y_validation_presse - results_ridge_presse)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)
svm_presse_R2_valid <- 1 - sum((y_validation_presse - results_svm_presse)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)



# Dataframe des MSE train / validation pour chaque modèle
performances_models <- data.frame(model = c("Random Forest", "arbre CART", "Gradient Boosting", "regression","regression backward", "régression Lasso", "régresion ridge", "SVM"),
                                  mse_valid = round(c(rf_presse_MSE_valid, rpart_presse_MSE_valid, gradient_presse_MSE_valid, regression_presse_MSE_valid, regbackward_presse_MSE_valid, lasso_presse_MSE_valid, ridge_presse_MSE_valid, SVM_presse_MSE_valid), 3),
                                  mse_train = round(c(rf_presse_MSE_train, rpart_presse_MSE_train, gradient_presse_MSE_train, regression_presse_MSE_train, regbackward_presse_MSE_train, lasso_presse_MSE_train, ridge_presse_MSE_train, SVM_presse_MSE_train), 3)
)


# Visualisation du taux d'erreur pour chaque modèle
ggplot(performances_models) + 
  geom_point(aes(x = mse_train, y = mse_valid), shape= 1, size = 10, color = "red") + 
  geom_text(aes(label = model, x = mse_train, y = mse_valid, vjust = 2)) + 
  #xlim(0, 0.5) + ylim(0, 0.5) + 
  theme_grey()

ggplot(performances_models) + 
  geom_bar(stat =  "identity", aes(x = model, y = mse_valid), fill = "blue") +
  theme_minimal() +
  labs(title = "Performances des différents modèles", subtitle = "MSE sur échantillon de validation") +
  ylim(0, 0.5)



# Dataframe compilant les résultats des différents modèles sur l'échantillon de validation

final_results <- cbind(final_presse[-id_train_presse,c("titre","note_presse_moyenne")], 
                       data.frame( y_rf = results_rf_presse,
                                   y_rpart = results_rpart_presse,
                                   y_gradient = results_gradient_presse,
                                   y_regression = results_regression_presse,
                                   y_regbackward = results_regbackward_presse, 
                                   y_lasso = results_lasso_presse ))
head(final_results)

# Ajout d'un taux d'erreur défini arbitrairement comme : erreur si écart de plus 0,5 avec la valeur d'origine
final_results$erreurs_rf <- abs(final_results$y_rf - final_results$note_presse_moyenne) > 0.5
final_results$erreurs_rpart <- abs(final_results$y_rpart - final_results$note_presse_moyenne) > 0.5
final_results$erreurs_gradient <- abs(final_results$y_gradient - final_results$note_presse_moyenne) > 0.5
final_results$erreurs_regression <- abs(final_results$y_regression - final_results$note_presse_moyenne) > 0.5
final_results$erreurs_regbackward <- abs(final_results$y_regbackward - final_results$note_presse_moyenne) > 0.5

sum(final_results$erreurs_rf) / length(final_results$erreurs_rf)
sum(final_results$erreurs_rpart) / length(final_results$erreurs_rf)
sum(final_results$erreurs_gradient) / length(final_results$erreurs_rf)
sum(final_results$erreurs_regression) / length(final_results$erreurs_rf)
sum(final_results$erreurs_regbackward) / length(final_results$erreurs_rf)


# Distributions de la variable cible selon les différents modèles

final_presse[-id_train_presse,c("titre","y_validation_presse")]

data <- melt(final_results) 


ggplot(data) + geom_histogram(aes(x=value), fill = "red") + 
  facet_wrap(~variable, ncol = 2) +
  labs(title ="Distribution des notes presse selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_void()




# QUESTIONS A POSER
# Variables binaires : les mettre en boolean ou factor ? (ou num?rique)
# Quand on fait du gbm par exemple avec train = 0,75, ca veut dire qu'on fait 2 fois de la validation crois?e : sur les param?tres puis tout ? la fin en comparant les modèles ? 
# Un arbre donnera toujours les mêmes résultats en le relancant, ok ? Mais les gradient ou forets par contre sont tjs différents ? 
# Quelle diff?rence, pour l'opti des param?tres, entre faire du hold out ou du cv 10 folds ? 10 folds est bcp plus lent... vraiment mieux ? 
# Utile d'utiliser les packages originaux plutôt que Caret ? J'ai l'impression qu'on est un peu plus flexible avec les packages originaux, mais caret tellement plus pratique dès qu'on teste plusieurs modèles...

# Le R2, c'est en gros la même chose que le MSE, non ? 
# A quoi sert la distance de cook ?  Importance d'un point par rapport aux autres ? Utile uniquement en r?gression lin?aire ?
# IUdem pour l'AIC ? Ca sert pour les autres modèles que la r?gression ? 


############################################
#               Hors sujet...              #
############################################

list_films <- final[,c("titre", "note_spectateurs_moyenne", "note_presse_moyenne",names(final)[str_detect(names(final), "genre")] )]

head(list_films)
str(list_films)

# Meilleures films par catégorie

list_films %>% filter(genre_Romance == 1) %>% select(titre, note_presse_moyenne, note_spectateurs_moyenne) %>% 
  mutate(note = (note_presse_moyenne + note_spectateurs_moyenne) / 2) %>% arrange(desc(note)) %>% head(10)

str(train_presse, list = 500)
table(train_presse$realisateur_Georges.Lautner)
table(validation_presse$realisateur_Georges.Lautner)
table(final$realisateur_Georges.Lautner)
View(final[final$realisateur_Georges.Lautner==1,])



############################################
#            Même chose avec CARET         #
############################################


# Créer l'index de partition train et la partition validation
#id_train_presse <- createDataPartition(y = final_presse$note_moyenne_presse, p = 0.75, list = FALSE)

?caret
# trControl : type de validation croisée utilisée
# Tunegrid : paramètres à otpimiser via la validation croisée

# On remplace note presse moyenne par un indicateur en 1 ou 0 indiquant un bon (>=4) ou mauvais film
#train_presse <- mutate(final_presse[id_train_presse,], y_train_presse = (note_moyenne_presse >= 4)) %>% 
#   select(-one_of(c("titre", "url", "id_film", "note_moyenne_presse")))
#validation_presse <- mutate(final_presse[-id_train_presse,], y_train_presse = (note_moyenne_presse >= 4)) %>% 
#  select(-one_of(c("titre", "url", "id_film", "note_moyenne_presse")))

# Train des modèles

grid <- expand.grid(x = ?))
rf_presse <- train(y_train_presse ~ ., data = train_presse, method = "rf", tuneGrid = grid, trControl = trainControl(method = "cv", number = 10))
# Paramètres : mtry

grid <- expand.grid(cp = c(0.1, 0.001, 0.001))
rpart_presse <- train(y_train_presse ~ ., data = train_presse, method = "rpart", tuneGrid = grid, trControl = trainControl(method = "cv", number = 10))

grid <- expand.grid(x = ?))
gradient_presse <- train(y_train_presse ~ ., data = train_presse, method = "gbm", tuneGrid = grid, trControl = trainControl(method = "cv", number = 10))
# Paramètres : n.trees, interaction.depth, shrinkage, n.minobsinnode

regression_presse <- train(y_train_presse ~ ., data = train_presse, method = "lm", tuneGrid = grid, trControl = trainControl(method = "cv", number = 10))
regbackward_presse <- train(y_train_presse ~ ., data = train_presse, method = "leapBackward", tuneGrid = grid, trControl = trainControl(method = "cv", number = 10))
# Paramètres : nvmax

regression_presse <- train(y_train_presse ~ ., data = train_presse, method = "glmnet", tuneGrid = grid, trControl = trainControl(method = "cv", number = 10))
# Paramètres : alpha, lambda (régularisation)

# PRédictions sur échantillon de validation
results_rpart_presse <- predict(rpart_presse, newdata = x_validation_presse )

# Importance des variables retenues
varImp(gradient_presse)

# Mesures de performance
postResample(pred = results_rpart_presse, obs = )

