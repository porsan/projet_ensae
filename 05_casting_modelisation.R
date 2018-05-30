##########################################
#          Librairies - options          #
##########################################

#install.packages("caret")
#install.packages("gbm")
#install.packages("leaps")
#install.packages("glmnet")

# Visualisation
library(ggplot2)
# library(plotly)
# library(rpart.plot)
# 
# # Traitement de donnees
# library(reshape2)
# library(stringr)
# library(dplyr)
# 
# # Modelisation
library(randomForest)
library(caret)
# library(rpart)
library(gbm)
library(leaps)
library(glmnet)
library(xgboost)
library(extraTrees)
# 
# # Autres
library(parallel)
library(doParallel)

# library(doMC)

########## Pour travail en paralel ##########

detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

 # registerDoMC(cores = 3)

# On arrete le cluster
stopCluster(cluster)
registerDoSEQ()

# Apprentissage et tests des algorithmes
seeds <- list(1:4,1:4,1:4,1:4,1:4,5)
objControl <- trainControl(method = 'cv', number = 5, returnResamp ='none', allowParallel = TRUE, seeds = seeds)

########## Lecture donnees ##########

source("02_casting_functions.R")

# Repertoire de sauvergarde des donnees
data_dir <- "data/"

# Multiplicateur : nombre de replication des donnees pour augmenter le nombre de lignes
multiplicateur <- 10

# Creation de la table de donnees
final <- construit_table_final(data_dir)

summary(final)
str(final, list.len= 300)
head(final)
dim(final)

# Verif : nombre de valeurs renseignees dans les variables a predire
sum(!is.na(final$note_presse_moyenne))
sum(!is.na(final$note_spectateurs_moyenne))

###################################################
# Travail sur les films sortis a partir de 1990   #
###################################################

# Voir la fonction traite_table_final() du fichier 02_casting_functions.R


############################################
#               Presse                     #
############################################

# Creation de la table de donnees
final <- construit_table_final(data_dir)

# Jeux de donnees presse
final_presse_tmp <- final_presse <- filter(final, !is.na(note_presse_moyenne))  %>% select(-one_of(c("note_spectateurs_moyenne")))

# Suppression de la table final
rm(final)

dim(final_presse) ## 11 138 lignes, 10 654 lignes avant 1990

# Verif que plus aucun NA ne traine
sapply(final_presse, function(x) {sum(is.na(x))})

# Multiplication des pains (ou des donnees)
for (i in 1:multiplicateur) { final_presse <- rbind(final_presse,final_presse_tmp) }
rm(final_presse_tmp,i)

##### Decoupage test / validation #####

# On fixe la graine
set.seed(19)

nb_films_presse <- nrow(final_presse)
id_train_presse <- sample(1:nb_films_presse, nb_films_presse*(4/5))
#length(id_train_presse)/nb_films_presse

train_presse <- final_presse[id_train_presse,] %>% select(-one_of(c("titre", "url", "id_film")))
x_train_presse <-  select(train_presse, -one_of(c("note_presse_moyenne"))) 
y_train_presse <- pull(train_presse,note_presse_moyenne) 

validation_presse <- final_presse[-id_train_presse,] %>% select(-one_of(c("titre", "url", "id_film")))
x_validation_presse <- select(validation_presse, -one_of(c("note_presse_moyenne")))
y_validation_presse <-  pull(validation_presse,note_presse_moyenne)

# Verif coherence
# dim(train_presse), dim(x_train_presse), length(y_train_presse)
# dim(validation_presse), dim(x_validation_presse)
# nrow(final_presse) - (length(y_validation_presse) + length(y_train_presse))

############################################
#           spectateurs                    #           
############################################

# Creation de la table de donnees
final <- construit_table_final(data_dir)

# Jeux de donnees spectateurs
final_spectateurs_tmp <- final_spectateurs <- filter(final, !is.na(note_spectateurs_moyenne)) %>% select(-one_of(c("note_spectateurs_moyenne")))

# Suppression de la table final
rm(final)

dim(final_spectateurs) ##  15 758 lignes, 13 315 lignes avant 1990

# Verif que plus aucun NA ne traine
sapply(final_spectateurs, function(x) {sum(is.na(x))})

# Multiplication des pains (ou des donnees)
for (i in 1:10) { final_spectateurs <- rbind(final_spectateurs,final_spectateurs_tmp) }
rm(final_spectateurs_tmp)

##### Decoupage test / validation #####

# On fixe la graine
set.seed(19)

nb_films_spectateurs <- nrow(final_spectateurs)

id_train_spectateurs <- sample.int(nb_films_spectateurs, round(nb_films_spectateurs *0.75, 0))

x_train_spectateurs <- final_spectateurs[id_train_spectateurs,] %>% select(-one_of(c("note_spectateurs_moyenne", "titre", "url", "id_film")))
y_train_spectateurs <-final_spectateurs[id_train_spectateurs, "note_spectateurs_moyenne"]

x_validation_spectateurs <- final_spectateurs[-id_train_spectateurs,] %>% select(-one_of(c("note_spectateurs_moyenne", "titre", "url", "id_film")))
y_validation_spectateurs <-final_spectateurs[-id_train_spectateurs, "note_spectateurs_moyenne"]

# Verif coherence
dim(x_train_spectateurs)
length(y_train_spectateurs)
dim(x_validation_spectateurs)
length(y_validation_spectateurs)

############################################
#          Train Random Forest             #
############################################

?randomForest


###### Presse ###### 

rf_presse <- randomForest(x_train_presse, y_train_presse, importance = TRUE)

# Resume modele
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

# vecteurs des r?sultats
rf_presse_predict  <- rf_presse$predict

# MSE train
rf_presse_MSE_train  <- mean((rf_presse_predict - y_train_presse)^2)

# Version CARET
gridsearch <- expand.grid(mtry = seq(30,200,50))
rf_presse <- train(x_train_presse, y_train_presse, method = "rf", tuneGrid = gridsearch, trControl = objControl)

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

# Mod?lisation sur ?chantillon train
rpart_presse <- rpart(note_presse_moyenne ~ ., data = train_presse, control = rpart.control(cp = 0.0012) )

# R?sultats mod?lisation
summary(rpart_presse)

# Visualisation de l'arbre
prp(rpart_presse)

# Visualisation des r?sultats de la validation crois?e en fonction du param?tre cp choisi
plotcp(rpart_presse)

# D?termination du seuil optimal
rpart_presse$cptable[which.min(rpart_presse$cptable[,4]),1]

# Importance des variables
rpart_presse$variable.importance


# vecteurs des r?sultats
rpart_presse_predict  <- rpart_presse$predict

# MSE sur ?chantillon train
rpart_presse_MSE_train  <- mean((predict(rpart_presse)- y_train_presse)^2)


#############################################
#         Train Gradient boosting           #
#############################################

?gbm

# Modelisation sur echantillon train, avec validation croisee 10n pour l'optimisation du nombre d'iterations
gradient_presse <- gbm(note_presse_moyenne ~ ., train_presse, distribution = "gaussian", n.trees = 500, interaction.depth = 4, shrinkage = 0.1, cv.folds = 10, n.cores = 3)

# Trouver le meilleur nombre d'iterations
gbm.perf(gradient_presse, method = "cv")

# Shrinkage ralentit la convergence de l'algorithme donc necessite plus d'iterations si il est petit 

# Meme chose sans validation croisee mais en hold out : forcement beaucoup plus rapide (~dix fois)
gradient_presse <- gbm(note_presse_moyenne ~ ., train_presse, distribution = "gaussian", n.trees = 3000, interaction.depth = 4, shrinkage = 0.01, train.fraction = 0.75, n.cores = 3)

# Importance des variables
summary(gradient_presse)

# Trouver le meilleur nombre d'iterations
gbm.perf(gradient_presse, method = "test")

# vecteurs des resultats
gradient_presse_predict  <- predict(gradient_presse, n.trees = 3000)

# MSE sur echantillon train
gradient_presse_MSE_train2  <- mean((gradient_presse_predict - train_presse$note_presse_moyenne)^2)

# On arrete le cluster
stopCluster(cluster)
registerDoSEQ()

rm(gradient_presse)

#############################################
#           Regression lineaire             #
#############################################

?lm
# On sait d'office que c'est pas pertinent car on veut en sortie une note entre 0 et 5 ; hors la regression va donner en resultats des valeurs plus extremes

# Train regression lineaire avec toutes les variables
regression_presse <- lm(note_presse_moyenne~., data = train_presse)

# Coefficients de la regression
sort(coefficients(regression_presse), decreasing = TRUE)

# Analyse des residus
# residus <- predict(regression_presse) - train_presse$note_presse_moyenne  #Idem que  :residuals(regression_presse)
plot(regression_presse$residuals)
qplot(regression_presse$residuals)

# Residus studentises
plot(rstudent(regression_presse))

# Vecteurs des resultats
regression_presse_predict  <- predict(regression_presse)

# MSE sur echantillon train
regression_presse_MSE_train  <- mean((regression_presse_predict - train_presse$note_presse_moyenne)^2)

rm(regression_presse)

#### Version CARET ####
reg_lin_caret <- train(x_train_presse, y_train_presse, method = "lm", trControl = objControl)
summary(reg_lin_caret)
predictors(reg_lin_caret)
predits <- predict(reg_lin_caret, x_validation_presse)

# MSE sur echantillon val : 0.3305199
reg_lin_caret_MSE  <- mean((predits - y_validation_presse)^2)
reg_lin_caret_MSE
rm(reg_lin_caret, predits)

#############################################
#      Regression lineaire regsubset        #
#############################################

?regsubsets

# Utilisation de regsubsets pour selectionner le nombre optimal de variables
regbackward_presse <- regsubsets(note_presse_moyenne~., data = train_presse, method ="backward", nvmax = 130, really.big = TRUE)

# Determination du nombre de variable optimal au regard du BIC : 77 variables
which.min(summary(regbackward_presse)$bic)

# Coefficients du "modele optimal" avec 77 variables
final_coeff <- coef(regbackward_presse, 77)


# Fonction pour predire les valeurs avec regsubsets (trouvee sur Internet...)
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

# Vecteur des predictions avec 77 variables
regbackward_presse_predict <- predict.regsubsets(regbackward_presse, validation_presse, 77)

# MSE sur echantillon train : 0.3310892
regbackward_presse_MSE_train  <- mean((regbackward_presse_predict - validation_presse$note_presse_moyenne)^2)

#### Version CARET ####
reg_lin_back_caret <- train(x_train_presse, y_train_presse, method = "leapBackward", tuneGrid = expand.grid(nvmax = 150), trControl = objControl)

reg_lin_back_caret <- train(x_train_presse, y_train_presse, method = "leapBackward")

summary(reg_lin_back_caret)
predictors(reg_lin_back_caret)
predits <- predict(reg_lin_caret, x_validation_presse)

# MSE sur echantillon val :
reg_lin_back_caret_MSE  <- mean((predits - y_validation_presse)^2)
reg_lin_back_caret_MSE
rm(reg_lin_back_caret, predits)

#############################################
#             Regression Lasso              #
#############################################

?glmnet

# Transformation des donnees pour avoir des donnees au format matriciel
x_train_presse_lasso = model.matrix(note_presse_moyenne~.,data=train_presse) 
y_train_presse_lasso=y_train_presse
x_validation_presse_lasso = model.matrix(note_presse_moyenne~.,data=validation_presse)

# Train du modele avec cross validation pour le parametre lambda et alpha a 1 (lasso)
lasso_presse  <- cv.glmnet(x_train_presse_lasso, y_train_presse_lasso, alpha = 1)

lasso_presse$lambda.min

summary(lasso_presse)
plot(lasso_presse)
predict(lasso_presse, newx = x_train_presse_lasso)

# Vecteur des predictions 
lasso_presse_predict <- predict(lasso_presse, x_validation_presse_lasso, lambda = lasso_presse$lambda.min)

# MSE sur echantillon train : 0.3323244
lasso_presse_MSE  <- mean((lasso_presse_predict - validation_presse$note_presse_moyenne)^2)

# Version CARET
reg_lasso_caret <- train(x_train_presse, y_train_presse, method = "leapBackward", trControl = objControl)
summary(regression_lin_back_caret)
predictors(regression_lin_back_caret)
predict.train(regression_lin_back_caret)
regression_lin_caret_predict <- predict(regression_lin_caret, x_validation_presse)

# MSE sur echantillon val
regression_lin_caret_MSE  <- mean((regression_lin_caret_predict - y_validation_presse)^2)

#############################################
#             Regression Ridge              #
#############################################

?glmnet

# Transformation des donnees pour avoir des donnees au format matriciel
x_train_presse_lasso = model.matrix(note_presse_moyenne~.,data=train_presse) 
y_train_presse_lasso = y_train_presse
x_validation_presse_lasso = model.matrix(note_presse_moyenne~.,data=validation_presse)

# Train du modele avec cross validation pour le parametre lambda et alpha a 0 (ridge)
ridge_presse  <- cv.glmnet(x_train_presse_lasso, y_train_presse_lasso, alpha = 0)

ridge_presse$lambda.min

summary(ridge_presse)
plot(ridge_presse)
predict(ridge_presse, newx = x_train_presse_lasso)

# Vecteur des predictions 
ridge_presse_predict <- predict(ridge_presse, x_validation_presse_lasso, lambda = ridge_presse$lambda.min)

# MSE sur echantillon train
ridge_presse_MSE  <- mean((ridge_presse_predict - validation_presse$note_presse_moyenne)^2)



#############################################
#           Regression Elasticnet           #
#############################################

?glmnet

# Transformation des donnees pour avoir des donnees au format matriciel
x_train_presse_lasso = model.matrix(note_presse_moyenne~.,data=train_presse) 
y_train_presse_lasso = y_train_presse
x_validation_presse_lasso = model.matrix(note_presse_moyenne~.,data=validation_presse)

var_iter <- matrix(NA,nrow = length(seq(0.05,0.95,0.05)), ncol = 3)
var_iter[,1]  <- seq(0.05,0.95,0.05)

for(i in 1:nrow(var_iter)) {
# Train du modele avec cross validation pour le parametre lambda et alpha a 0 (elasticnet)
elastic_presse  <- cv.glmnet(x_train_presse_lasso, y_train_presse_lasso, alpha = var_iter[i,1])
elastic_presse_predict <- predict(elastic_presse, x_validation_presse_lasso, lambda = elastic_presse$lambda.min)
print(paste0("alpha : ", var_iter[i,1], " - lambda min : ", var_iter[i,2] <- elastic_presse$lambda.min, " - MSE : ", var_iter[i,3] <- mean((elastic_presse_predict - validation_presse$note_presse_moyenne)^2)))
}

ggplot(as.data.frame(var_iter),aes(x = var_iter[,1], y = var_iter[,2])) + geom_jitter()

alpha_min <- var_iter[which.min(var_iter[,3]),1]
elastic_presse  <- cv.glmnet(x_train_presse_lasso, y_train_presse_lasso, alpha = alpha_min)

summary(elastic_presse)
plot(elastic_presse)
predict(elastic_presse, newx = x_train_presse_lasso)

elastic_presse$lambda.min

# Vecteur des predictions 
elastic_presse_predict <- predict(elastic_presse, x_validation_presse_lasso, lambda = 0.0003960467)

# MSE sur echantillon train
elastic_presse_MSE  <- mean((elastic_presse_predict - validation_presse$note_presse_moyenne)^2)

# Version CARET


################################################
#         Validation / comparaison             #
################################################


# Vecteurs de r?sultats sur l'?chantillon de validation pour chaque mod?le optimis? 
results_rf_presse <- predict(rf_presse, newdata = x_validation_presse )
results_rpart_presse <- predict(rpart_presse, newdata = x_validation_presse )
results_gradient_presse <- predict(gradient_presse, newdata = x_validation_presse, n.tree = 391 )
results_regression_presse <- predict(regression_presse, newdata = x_validation_presse)
results_regbackward_presse <- predict.regsubsets(regbackward_presse, validation_presse, 41)
results_lasso_presse <- predict(lasso_presse, x_validation_presse_lasso, lambda = 0.0019)


# Erreurs par mod?le sur l'?chantillon de validation
rf_presse_MSE_valid <- mean((results_rf_presse - y_validation_presse)^2) # 0.337
rpart_presse_MSE_valid<- mean((results_rpart_presse - y_validation_presse)^2) #0,3865
gradient_presse_MSE_valid <- mean((results_gradient_presse - y_validation_presse)^2) #0,325
regression_presse_MSE_valid <- mean((results_regression_presse - y_validation_presse)^2) #0,351
regbackward_presse_MSE_valid <- mean((results_regbackward_presse - y_validation_presse)^2) #0,356
lasso_presse_MSE_valid <- mean((results_lasso_presse - y_validation_presse)^2) #0,356



# Dataframe des MSE train / validation pour chaque mod?le
performances_models <- data.frame(model = c("Random Forest", "arbre CART", "Gradient Boosting", "regression","regression backward", "r?gression Lasso"),
                                  mse_valid = round(c(rf_presse_MSE_valid, rpart_presse_MSE_valid, gradient_presse_MSE_valid, regression_presse_MSE_valid, regbackward_presse_MSE_valid, lasso_presse_MSE_valid), 3),
                                  mse_vtrain = round(c(rf_presse_MSE_train, rpart_presse_MSE_train, gradient_presse_MSE_train, regression_presse_MSE_train, regbackward_presse_MSE_train, lasso_presse_MSE_train), 3)
)


# Visualisation du taux d'erreur pour chaque mod?le
ggplot(performances_models) + 
  geom_point(aes(x = mse_train, y = mse_valid), shape= 1, size = 10, color = "red") + 
  geom_text(aes(label = model, x = mse_train, y = mse_valid, vjust = 2)) + 
  #xlim(0, 0.5) + ylim(0, 0.5) + 
  theme_grey()

ggplot(performances_models) + 
  geom_bar(stat =  "identity", aes(x = model, y = mse_valid), fill = "blue") +
  theme_minimal() +
  labs(title = "Performances des diff?rents mod?les", subtitle = "MSE sur ?chantillon de validation") +
  ylim(0, 0.5)



# Dataframe compilant les r?sultats des diff?rents mod?les sur l'?chantillon de validation

final_results <- cbind(final_presse[-id_train_presse,c("titre","note_presse_moyenne")], 
                       data.frame( y_rf = results_rf_presse,
                                   y_rpart = results_rpart_presse,
                                   y_gradient = results_gradient_presse,
                                   y_regression = results_regression_presse,
                                   y_regbackward = results_regbackward_presse, 
                                   y_lasso = results_lasso_presse ))
head(final_results)

# Ajout d'un taux d'erreur d?fini arbitrairement comme : erreur si ?cart de plus 0,5 avec la valeur d'origine
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


# Distributions de la variable cible selon les diff?rents mod?les

final_presse[-id_train_presse,c("titre","y_validation_presse")]

data <- melt(final_results) 


ggplot(data) + geom_histogram(aes(x=value), fill = "red") + 
  facet_wrap(~variable, ncol = 2) +
  labs(title ="Distribution des notes presse selon le mod?le choisi", subtitle = "Echantillon de validation") +
  theme_void()




# QUESTIONS A POSER
# Variables bin?aires : les mettre en boolean ou factor ? (ou num?rique)
# Quand on fait du gbm par exemple avec train = 0,75, ca veut dire qu'on fait 2 fois de la validation crois?e : sur les param?tres puis tout ? la fin en comparant les mod?les ? 
# Un arbre donnera toujours les m?mes r?sultats en le relancant, ok ? Mais les gradient ou forets par contre sont tjs diff?rents ? 
# Quelle diff?rence, pour l'opti des param?tres, entre faire du hold out ou du cv 10 folds ? 10 folds est bcp plus lent... vraiment mieux ? 

# Le R2, c'est en gros la m?me chose que le MSE, non ? 
# A quoi sert la distance de cook ?  Importance d'un point par rapport aux autres ? Utile uniquement en r?gression lin?aire ?
# IUdem pour l'AIC ? Ca sert pour les autres mod?les que la r?gression ? 


############################################
#               Hors sujet...              #
############################################

list_films <- final[,c("titre", "note_spectateurs_moyenne", "note_presse_moyenne",names(final)[str_detect(names(final), "genre")] )]

head(list_films)
str(list_films)

# Meilleures films par cat?gorie

list_films %>% filter(genre_Romance == 1) %>% select(titre, note_presse_moyenne, note_spectateurs_moyenne) %>% 
  mutate(note = (note_presse_moyenne + note_spectateurs_moyenne) / 2) %>% arrange(desc(note)) %>% head(10)

