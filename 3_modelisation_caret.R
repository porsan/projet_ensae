
############################################
#           Chargement modèles             #
############################################



setwd("/Data2/R programming/Projets/projet_ensae")

######### Lecture donnees ##########
source("02_casting_functions.R")

# Repertoire de sauvergarde des donnees
data_dir <- "data/"

########## Pour travail en paralel ##########

detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

# registerDoMC(cores = 3)

# On arrete le cluster
stopCluster(cluster)
registerDoSEQ()

# Creation de la table de donnees
final <- construit_table_final(data_dir)

# Jeux de donnees presse
final_presse_tmp <- final_presse <- filter(final, !is.na(note_presse_moyenne))  %>% select(-one_of(c("note_spectateurs_moyenne")))

# Suppression de la table final
rm(final)

dim(final_presse) ## 11 138 lignes, 10 654 lignes avant 1990

# Verif que plus aucun NA ne traine
# sapply(final_presse, function(x) {sum(is.na(x))})

# Multiplication des pains (ou des donnees)
# for (i in 1:multiplicateur) { final_presse <- rbind(final_presse,final_presse_tmp) }
# rm(final_presse_tmp,i)

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
load("extraTrees_presse")



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
load("extraTrees_spectateurs")


############################################
#              CARET presse                #
############################################


# Créer l'index de partition train et la partition validation
#id_train_presse <- createDataPartition(y = final_presse$note_moyenne_presse, p = 0.75, list = FALSE)

# trControl : type de validation croisée utilisée
# Tunegrid : paramètres à otpimiser via la validation croisée



control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)



# Train des modèles pour note moyenne presse avec optimisation des paramètres

grid <- expand.grid(mtry = c(sqrt(ncol(train_presse))))  # Paramètres : mtry (nb variables dans chaque échantillon)
rf_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "rf", trControl = (trainControl(method = "oob", allowParallel = TRUE)), ntree = 500)
# rf_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "rf", trControl = (trainControl(method = "oob")), ntree = 500)
rf_presse
ggplot(rf_presse)


grid <- expand.grid(cp = c(0.0005, 0.001, 0.005, 0.01, 0.05, 0.1)) # Paramètres : cp, élagage de l'arbre
rpart_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "rpart",  tuneGrid = grid, trControl = control)
rpart_presse
ggplot(rpart_presse)

grid <- expand.grid(n.trees= c(100, 150, 200, 250, 275, 300, 350, 400), interaction.depth = c(3, 4, 5, 6), shrinkage = 0.1 , n.minobsinnode = 5) # Paramètres : n.trees, interaction.depth, shrinkage, n.minobsinnode
gradient_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "gbm",  trControl = control, tuneGrid = grid)
gradient_presse # Opti : deph à 6, trees à 250
ggplot(gradient_presse)

regression_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "lm")              
regression_presse

grid <- expand.grid(nvmax = seq(5, 100, 5)) # Paramètres : nvmax (nombre de variables gardées)
regbackward_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "leapBackward", trControl = control, tuneGrid = grid)
regbackward_presse # Opti : 90 variables
plot(regbackward_presse)

grid <- expand.grid(nvmax = seq(5, 100, 5)) # Paramètres : nvmax (nombre de variables gardées)
regforward_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "leapForward", trControl = control, tuneGrid = grid)
regforward_presse # Opti : 95 variables
plot(regforward_presse)

grid <- expand.grid(alpha= c(0, 0.5, 1), lambda = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1)) # Paramètres : alpha (lasso / ridge, elastic), lambda (régularisation)
glmnet_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "glmnet", tuneGrid = grid, trControl = control, preProc = c("center","scale") )
glmnet_presse # Opti : alpha = 1 et lambda = 0.005
ggplot(glmnet_presse)

grid <- expand.grid(C = 10^(-3:2), sigma=10^(-2:2)) 
svm_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "svmRadialSigma", tuneGrid = grid, trControl = control, preProc = c("center","scale") )
svm_presse # Opti : sigma = 0.01 et C = 1
ggplot(svm_presse)

grid <- expand.grid(C = 10^(-3:2)) # Paramètres : fonction de cout uniquement
svmlineaire_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "svmLinear", tuneGrid = grid, trControl = control, preProc = c("center","scale") )
svmlineaire_presse # Opti : ?
ggplot(svmlineaire_presse)

grid <- expand.grid(nrounds = 50, lambda = 0.1, alpha = 0.01778279, eta = 0.3)  # Paramètres : (nb variables dans chaque échantillon)
xgboost_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "xgbLinear", tuneLength = 10, trControl = control)
xgboost_presse # Opti : The final values used for the model were nrounds = 50, lambda = 0.1, alpha = 0.01778279 and eta = 0.3.
ggplot(xgboost_presse)


grid <- expand.grid(C = 10^(-3:2)) # Paramètres : fonction de cout uniquement
extraTrees_presse <- train(note_presse_moyenne ~ ., data = train_presse, method = "extraTrees", tuneLength = 10, trControl = control)
extraTrees_presse # Opti : 
ggplot(extraTrees_presse)

install.packages("rJava")
install.packages("extraTrees")

library(rJava)
library(extraTrees)

# Sauvegarde modèles
save(rf_presse, file = "rf_presse")
save(rpart_presse, file = "rpart_presse")
save(gradient_presse, file = "gradient_presse")
save(regression_presse, file = "regression_presse")
save(regbackward_presse, file = "regbackward_presse")
save(regforward_presse, file = "regforward_presse")
save(glmnet_presse, file = "glmnet_presse")
save(svm_presse, file = "svm_presse")
save(svmlineaire_presse, file = "svmlineaire_presse")
save(xgboost_presse, file = "xgboost_presse")
save(xgboost_presse, file = "extraTrees_presse")


# Prédictions sur échantillon de validation
results_rf_presse <- predict(rf_presse, newdata = x_validation_presse )
results_rpart_presse <- predict(rpart_presse, newdata = x_validation_presse )
results_gradient_presse <- predict(gradient_presse, newdata = x_validation_presse )
results_regression_presse <- predict(regression_presse, newdata = x_validation_presse )
results_regforward_presse <- predict(regforward_presse, newdata = x_validation_presse )
results_regbackward_presse <- predict(regbackward_presse, newdata = x_validation_presse )
results_glmnet_presse <- predict(glmnet_presse, newdata = x_validation_presse )
results_svm_presse <- predict(svm_presse, newdata = x_validation_presse )
results_svmlineaire_presse <- predict(svmlineaire_presse, newdata = x_validation_presse )
results_xgboost_presse <- predict(xgboost_presse, newdata = x_validation_presse )
results_extraTrees_presse <- predict(extraTrees_presse, newdata = x_validation_presse )

results_aleatoire <- rnorm(length(results_glmnet_presse), mean = mean(train_presse$note_presse_moyenne), sd = sd(train_presse$note_presse_moyenne))
results_moyenne <- rep(mean(train_presse$note_presse_moyenne),length(results_glmnet_presse))
results_aleatoire2 <- sample(2:4, replace = TRUE, length(results_glmnet_presse))


# Importance des variables retenues
varImp(gradient_presse)

# Mesures de performance
performances_model_presse <- data.frame(model = c("Random Forest", "Arbre CART", "Gradient boosting", "Régression linéaire", "Régression linéaire backward", "Régression linéaire Forward", "Régression ridge-lasso", "SVM"))
performances_model_presse <- data.frame(performances_model_presse, rbind(
postResample(pred = results_rf_presse, obs = validation_presse$note_presse_moyenne),
postResample(pred = results_rpart_presse, obs = validation_presse$note_presse_moyenne),
postResample(pred = results_gradient_presse, obs = validation_presse$note_presse_moyenne),
postResample(pred = results_regression_presse, obs = validation_presse$note_presse_moyenne),
postResample(pred = results_regforward_presse, obs = validation_presse$note_presse_moyenne),
postResample(pred = results_regbackward_presse, obs = validation_presse$note_presse_moyenne),
postResample(pred = results_glmnet_presse, obs = validation_presse$note_presse_moyenne),
postResample(pred = results_svm_presse, obs = validation_presse$note_presse_moyenne)))
performances_model_presse$model <- factor(performances_model_presse$model)

# Ouf, les choix aléatoires sont VRAIMENT en dessous des modèles.... !
1 - sum((y_validation_presse - results_aleatoire)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)
1 - sum((y_validation_presse - results_aleatoire2)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)
1 - sum((y_validation_presse - results_moyenne)^2) / sum((y_validation_presse - mean(y_validation_presse))^2)




# Visualisation performances

levels(performances_model_presse$model) <- str_replace(levels(performances_model_presse$model), " ", "\n")

ggplot(performances_model_presse) + 
  geom_bar(aes(x = model, y = Rsquared, fill = Rsquared), stat="identity", width = 0.75 ) +
  labs(title = "Performances des différents modèles sur note presse", subtitle = "R2 sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.8) + 
  scale_fill_gradient(low="red", high="light green") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = Rsquared, label = round(Rsquared, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)



# Dataframe compilant les résultats des différents modèles sur l'échantillon de validation
final_results <- data.frame( valeur_reelle = y_validation_presse,
                             Random_Forest = results_rf_presse,
                             Rpart = results_rpart_presse,
                             Gradient = results_gradient_presse,
                             Regression = results_regression_presse,
                             Regression_backward = results_regbackward_presse, 
                             Regression_penalisee = results_glmnet_presse,
                             SVM = results_svm_presse
)

plot(y_validation_presse, results_gradient_presse)

sapply(final_results, max)

# Visualisation des distributions obtenues
 
data <- melt(final_results) 
ggplot(data) + geom_histogram(aes(x=value, fill = variable), bins = 30) + 
  facet_wrap(~variable, ncol = 2) +
  labs(title ="Distribution des notes presse selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")



# Visualisation des nuages de points Y réel / Y estimé

data2 <- cbind(data, y_reel = rep(y_validation_presse, 8))[data2$variable != "valeur_reelle",]
str(data2)
head(data2)
ggplot(data2) + geom_jitter(aes(x=value, y = y_reel,color = variable )) + 
  facet_wrap(~variable, ncol = 2) +
  labs(title ="Distribution des notes presse selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")


############################################
#            CARET spectateurs             #
############################################


# Train des modèles pour note moyenne spectateurs




grid <- expand.grid(mtry = c(sqrt(ncol(train_spectateurs))))  # Paramètres : mtry (nb variables dans chaque échantillon)
rf_spectateurs <- train(note_spectateurs_moyenne ~ ., data = train_spectateurs, method = "rf", trControl = (trainControl(method = "oob")), ntree = 500)
rf_spectateurs
ggplot(rf_spectateurs)

grid <- expand.grid(mtry = c(sqrt(ncol(train_spectateurs)), numRandomCuts = 3:6))  # Paramétrer aussi : numRandomCuts = 1:10
rfextreme_spectateurs <- train(note_spectateurs_moyenne ~ ., data = train_spectateurs, method = "extraTrees", trControl = control, ntree = 500)
rfextreme_spectateurs
ggplot(rfextreme_spectateurs)

grid <- expand.grid(cp = c(0.0005, 0.001, 0.005, 0.01, 0.05, 0.1)) # Paramètres : cp, élagage de l'arbre
rpart_spectateurs <- train(note_spectateurs_moyenne ~ ., data = train_spectateurs, method = "rpart",  tuneGrid = grid, trControl = control)
rpart_spectateurs
ggplot(rpart_spectateurs)

grid <- expand.grid(n.trees= c(100, 150, 200, 250, 300, 350, 400), interaction.depth = c(3, 4, 5, 6), shrinkage = 0.1 , n.minobsinnode = 5) # Paramètres : n.trees, interaction.depth, shrinkage, n.minobsinnode
gradient_spectateurs <- train(note_spectateurs_moyenne ~ ., data = train_spectateurs, method = "gbm",  trControl = control, tuneGrid = grid)
gradient_spectateurs # Opti : deph à 6, trees à 250
ggplot(gradient_spectateurs)

regression_spectateurs <- train(note_spectateurs_moyenne ~ ., data = train_spectateurs, method = "lm")
regression_spectateurs

grid <- expand.grid(nvmax = seq(5, 100, 5)) # Paramètres : nvmax (nombre de variables gardées)
regbackward_spectateurs <- train(note_spectateurs_moyenne ~ ., data = train_spectateurs, method = "leapBackward", trControl = control, tuneGrid = grid)
regbackward_spectateurs # Opti : 90 variables
plot(regbackward_spectateurs)

grid <- expand.grid(nvmax = seq(5, 100, 5)) # Paramètres : nvmax (nombre de variables gardées)
regforward_spectateurs <- train(note_spectateurs_moyenne ~ ., data = train_spectateurs, method = "leapForward", trControl = control, tuneGrid = grid)
regforward_spectateurs # Opti : 95 variables
plot(regforward_spectateurs)

grid <- expand.grid(alpha= c(0, 0.5, 1), lambda = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1)) # Paramètres : alpha (lasso / ridge, elastic), lambda (régularisation)
glmnet_spectateurs <- train(note_spectateurs_moyenne~ ., data = train_spectateurs, method = "glmnet", tuneGrid = grid, trControl = control, preProc = c("center","scale") )
glmnet_spectateurs # Opti : alpha = 1 et lambda = 0.005
ggplot(glmnet_spectateurs)

grid <- expand.grid(C = 10^(-3:1), sigma=10^(-3:1)) # Paramètres : C (fonction de cout) et Sigma (?). On pourrait rajouter gamma ? 
svm_spectateurs <- train(note_spectateurs_moyenne~ ., data = train_spectateurs, method = "svmRadialSigma", tuneGrid = grid, trControl = control, preProc = c("center","scale") )
svm_spectateurs # Opti : sigma = 0.001 et C = 1
ggplot(svm_spectateurs)





# Prédictions sur échantillon de validation
results_rf_spectateurs <- predict(rf_spectateurs, newdata = x_validation_spectateurs )
results_rfextreme_spectateurs <- predict(rfextreme_spectateurs, newdata = x_validation_spectateurs )
results_rpart_spectateurs <- predict(rpart_spectateurs, newdata = x_validation_spectateurs )
results_gradient_spectateurs <- predict(gradient_spectateurs, newdata = x_validation_spectateurs )
results_regression_spectateurs <- predict(regression_spectateurs, newdata = x_validation_spectateurs )
results_regforward_spectateurs <- predict(regforward_spectateurs, newdata = x_validation_spectateurs )
results_regbackward_spectateurs <- predict(regbackward_spectateurs, newdata = x_validation_spectateurs )
results_glmnet_spectateurs <- predict(glmnet_spectateurs, newdata = x_validation_spectateurs )
results_svm_spectateurs <- predict(svm_spectateurs, newdata = x_validation_spectateurs )


results_aleatoire <- rnorm(length(results_glmnet_spectateurs), mean = mean(train_spectateurs$note_spectateurs_moyenne), sd = sd(train_spectateurs$note_spectateurs_moyenne))
results_moyenne <- rep(mean(train_spectateurs$note_spectateurs_moyenne),length(results_glmnet_spectateurs))
results_aleatoire2 <- sample(2:4, replace = TRUE, length(results_glmnet_spectateurs))
?sample

varImp(gradient_spectateurs)

# Mesures de performance
performances_model_spectateurs <- data.frame(model = c("Random Forest", "Random Forest Extreme", "Arbre CART", "Gradient boosting", "Régression linéaire", "Régression linéaire backward", "Régression linéaire Forward", "Régression ridge-lasso", "SVM"))
performances_model_spectateurs <- data.frame(performances_model_spectateurs, rbind(
  postResample(pred = results_rf_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
  postResample(pred = results_rfextreme_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
  postResample(pred = results_rpart_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
  postResample(pred = results_gradient_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
  postResample(pred = results_regression_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
  postResample(pred = results_regforward_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
  postResample(pred = results_regbackward_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
  postResample(pred = results_glmnet_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
  postResample(pred = results_svm_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne)))




# Dataframe compilant les résultats des différents modèles sur l'échantillon de validation
final_results <- data.frame( valeur_reelle = y_validation_spectateurs, 
                             Random_Forest = results_rf_spectateurs,
                             Random_Forest_Extreme = results_rfextreme_spectateurs,
                             Rpart = results_rpart_spectateurs,
                             Gradient = results_gradient_spectateurs,
                             Regression = results_regression_spectateurs,
                             Regression_backward = results_regbackward_spectateurs, 
                             Regression_penalisee = results_glmnet_spectateurs,
                             SVM = results_svm_spectateurs
)


# Sauvegarde modèles
save(rf_spectateurs, file = "rf_spectateurs")
save(rpart_spectateurs, file = "rpart_spectateurs")
save(gradient_spectateurs, file = "gradient_spectateurs")
save(regression_spectateurs, file = "regression_spectateurs")
save(regbackward_spectateurs, file = "regbackward_spectateurs")
save(regforward_spectateurs, file = "regforward_spectateurs")
save(glmnet_spectateurs, file = "glmnet_spectateurs")
save(svm_spectateurs, file = "svm_spectateurs")


# Visualisation performances

levels(performances_model_spectateurs$model) <- str_replace(levels(performances_model_spectateurs$model), " ", "\n")

ggplot(performances_model_spectateurs) + 
  geom_bar(aes(x = model, y = Rsquared, fill = Rsquared), stat="identity", width = 0.9 ) +
  labs(title = "Performances des différents modèles sur note spectateurs", subtitle = "R2 sur échantillon de validation") + 
  theme_minimal()  +  ylim(0, 0.8) + 
  scale_fill_gradient(low="red", high="light green") +
  theme(plot.title = element_text(color = "dark blue", size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(face = "italic"), 
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_blank(),
        legend.position = "null") + 
  geom_text(aes(x = model, y = Rsquared, label = round(Rsquared, 3), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", color = "white", size = 4)



# Visualisation des distributions obtenues : histogrammes
data <- melt(final_results) 
ggplot(data) + geom_histogram(aes(x=value, fill = variable), bins = 30) + 
  facet_wrap(~variable, ncol = 2) +
  labs(title ="Distribution des notes spectateurs selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

# Visualisation des distributions obtenues : boxplot (à peu près la même chose mais présenté de façon différente)
ggplot(data) + geom_boxplot(aes(x=variable, y = value, color = variable)) +
  labs(title ="Distribution des notes presse selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

]
# Visualisation des distributions obtenues : boxplot (à peu près la même chose mais présenté de façon différente)
ggplot(data) + geom_boxplot(aes(x=variable, y = value, fill= variable)) +
  labs(title ="Distribution des notes spectateurs selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

ggplot(data) + geom_violin(aes(x=variable, y = value, fill = variable)) +
  labs(title ="Distribution des notes spectateurs selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")

# Visualisation des nuages de points Y réel / Y estimé

data2 <- cbind(data, y_reel = rep(y_validation_spectateurs, 8))[data2$variable != "valeur_reelle",]
str(data2)
head(data2)
ggplot(data2) + geom_jitter(aes(x=y_reel, y = value,color = variable )) + 
  facet_wrap(~variable, ncol = 2) +
  labs(title ="Nuage de points Y / Y réel spectateurs selon le modèle choisi", subtitle = "Echantillon de validation") +
  theme_minimal() + theme(legend.position = "null")




# Modele : 
#ggplot(ksdata, aes(main_category, usd_pledged, fill=main_category)) + geom_boxplot() + 
#  ggtitle("Amount Pledged vs. Project Category") + xlab("Project Category") + 
#  ylab("Amount Pledged (USD)") + 
####  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
###        axis.title=element_text(size=12, face="bold"), 
##        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
#  coord_cartesian(ylim=c(0,20000))



############################################
#                  TESTS                   #
############################################



# Test de différents types de validation croisée sur arbre cart : comparaisons résultats et temps de calcul
grid <- expand.grid(cp = c(0.0005, 0.001, 0.005, 0.01, 0.05, 0.1)) 

# Validation croisée 5 groupes : 9
system.time(rpart_presse_test1 <- train(note_presse_moyenne ~ ., data = train_presse, method = "rpart",  tuneGrid = grid, trControl = trainControl(method = "cv", number = 5)))
# Validation croisée one / out : 13
system.time(rpart_presse_test2 <- train(note_presse_moyenne ~ ., data = train_presse, method = "rpart",  tuneGrid = grid, trControl = trainControl(method = "LGOCV", number = 5)))
# Validation croisée 5 groupes répétée 5 fois : 60
system.time(rpart_presse_test3 <- train(note_presse_moyenne ~ ., data = train_presse, method = "rpart",  tuneGrid = grid, trControl = trainControl(method = "repeatedcv", repeats = 5, number = 5)))
# Validation croisée leave one out : 16276 !!!
system.time(rpart_presse_test4 <- train(note_presse_moyenne ~ ., data = train_presse, method = "rpart",  tuneGrid = grid, trControl = trainControl(method = "LOOCV")))





