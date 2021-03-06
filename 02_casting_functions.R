########## Librairies - options ##########
library(RCurl)
library(rvest)
library(dplyr)
library(stringr)
library(caret)
#library(httr)

########## Fonction infos_casting() ########## 
# Fonction infos_casting() permet de recuperer les infos d'une url personne d'allocine
# Les arguments de la fonction sont :
# url_fiche - url sur laquelle on va recuperer les infos  
# type_cast - definir s'il s'agit d'un/e acteur/actrice ou d'un/e realisateur/realisatrice
# indice - definir le numero de l'acteur/actrice ou du/de la realisateur/realisatrice
# data_dir_casting - repertoire pour enregistrer les sauvegardes des fichiers html et jpg
# sauvegarde - enregistrement des sauvegardes des fichiers html et jpg

infos_casting <- function(url_fiche, type_cast = "acteur", indice, data_dir_casting, sauvegarde = FALSE) {
  
  # initialisation des variables par defaut a NA
  id_personne <- url_casting <- pers_nom <- pers_metiers <- pers_metier1 <- pers_metier2 <- pers_metier3 <- pers_nom_naiss <- pers_pseudo <- pers_nat <- pers_nat1 <- pers_nat2 <- pers_naiss <- pers_naiss_date <- pers_naiss_lieu <- pers_age <- pers_deces <- pers_deces_date <- pers_deces_age <- pers_deces_lieu <- pers_an_carriere <- pers_nb_film <- pers_nb_prix <- pers_nb_nominat <- NA
  
  if(!is.na(url_fiche)) {  
    
    if (type_cast == "realisateur") { 
      type_casting <- "_03realisation"
    } else { type_casting <- "_04casting" }
   
    # chargement de la page
    casting <- read_html(url_fiche)
    url_casting <- url_fiche
    url_casting_brut <- getURL(url_casting)
    
    
    # pour tester si la page existe
    names_casting <- casting %>% html_nodes(".meta-body-item .light") %>% html_text()
    if(length(names_casting) != 0) {
      
      # Id personne
      id_personne<-unlist(str_extract_all(url_casting, "[0-9]+"))
      
      # Sauvegarde de la page et de l'image en fichier
      if (sauvegarde) {
      
        # pour sauvegarder la page html
        nom_save<-paste0(data_dir_casting,id_personne,type_casting,".txt")
        
        # on sauvgarde la page si elle n'est pas encore enregistrée
        if(!file.exists(nom_save)) {
          write.csv(url_casting_brut,nom_save)
        }
        
        # pour sauvegarder la photo
        photo_casting<- casting %>% html_nodes(".card-person-overview .thumbnail-img") %>% html_attr("src")
        nom_save<-paste0(data_dir_casting,id_personne,type_casting,"_photo.jpg")
        
        # on sauvgarde la photo si elle n'est pas encore enregistrée
        if(!file.exists(nom_save)) {
          GET(photo_casting, write_disk(nom_save))
        }
      }
      # nom
      pers_nom <- casting %>% html_nodes(".titlebar-title-lg") %>% html_text()
      
      # infos
      names_casting<-str_trim(names_casting)
      
      for(j in 1:length(names_casting)) {
        
        infos_casting<- casting %>% html_nodes(paste0(".meta-body-item:nth-child(",j,")")) %>% html_text()
        infos_casting<- gsub("\\n","",infos_casting)
        infos_casting<- unlist(str_split(infos_casting,"  "))
        infos_casting<- gsub(",","",infos_casting)
        infos_casting<- str_trim(infos_casting)
        infos_casting<- setdiff(infos_casting,"")
        
        switch(infos_casting[1],
               Métiers = pers_metiers <- setdiff(infos_casting,c(names_casting,"plus","")),
               Métier = pers_metiers <- setdiff(infos_casting,c(names_casting,"plus","")),
               "Nom de naissance" = pers_nom_naiss <- setdiff(infos_casting,c(names_casting,"plus",""," ")),
               Pseudos = pers_pseudo <- setdiff(infos_casting,c(names_casting,"plus","")),
               Pseudo = pers_pseudo <- setdiff(infos_casting,c(names_casting,"plus","")),
               Nationalité = pers_nat <- setdiff(infos_casting,c(names_casting,"plus","")),
               Naissance = pers_naiss <- setdiff(infos_casting,c(names_casting,"plus","")),
               Age = pers_age <- as.numeric(str_trim(gsub("ans","",setdiff(infos_casting,c(names_casting,"plus",""))))),
               Décès = pers_deces <- setdiff(infos_casting,c(names_casting,"plus","")),
               "NA")
      }
      
      # enregistrements des infos de metiers
      if (length(pers_metiers) != 0) {
        pers_metier1 <- pers_metiers[1]
        pers_metier2 <- pers_metiers[2]
        pers_metier3 <- pers_metiers[3]
      }
      
      # enregistrements des infos de nom de naissance
      if (!is.na(pers_nom_naiss[2])) {
        pers_nom_naiss <- paste0(pers_nom_naiss[1]," ",pers_nom_naiss[2])
      }
      
      # enregistrements des infos de pseudo
      if (!is.na(pers_pseudo[2])) {
        pers_pseudo <- paste0(pers_pseudo[1]," ",pers_pseudo[2])
      }
      
      # enregistrements des infos de nationalite
      if (length(pers_nat) != 0) {
        pers_nat1 <- pers_nat[1]
        pers_nat2 <- pers_nat[2]
      }
      
      # enregistrements des infos de naissance
      if (length(pers_naiss) != 0) {
        pers_naiss <- gsub("\\(","",gsub("\\)","",gsub(",","",pers_naiss)))
        pers_naiss <- str_trim(pers_naiss)
        pers_naiss <- setdiff(pers_naiss,c(names_casting,"plus",""))
        pers_naiss_date <- pers_naiss[1]
        pers_naiss_lieu <- pers_naiss[2]
      }
      
      # enregistrements des infos de deces
      if (length(pers_deces) != 0) {
        pers_deces_date <- pers_deces[1]
        pers_deces_age <- as.numeric(unlist(str_extract_all(pers_deces[2], "[0-9]+")))
        pers_deces_lieu <- gsub("\\(","",gsub("\\)","",gsub(",","",pers_deces[3])))
      }
      
      # stats du realisateur
      stat_num_casting <- casting %>% html_nodes(".stats-number") %>% html_text()
      stat_info_casting <- casting %>% html_nodes(".stats-info") %>% html_text()
      
      if(length(stat_info_casting) != 0) {
        
        for(k in 1:length(stat_info_casting)) {
          
          #print(stat_info_casting[k])
          
          switch(stat_info_casting[k],
                 "ans de carrière" = pers_an_carriere <- as.numeric(stat_num_casting[k]),
                 "films et séries tournés" = pers_nb_film <- as.numeric(stat_num_casting[k]),
                 "film ou série tourné" = pers_nb_film <- as.numeric(stat_num_casting[k]),
                 "prix" = pers_nb_prix <- as.numeric(stat_num_casting[k]),
                 "nominations" = pers_nb_nominat <- as.numeric(stat_num_casting[k]),
                 "nomination" = pers_nb_nominat <- as.numeric(stat_num_casting[k]),
                 "NA")
        }
        
      }
      
    } 
    
  }  
  # Construction de la liste a retourner
  infos <- c(id_personne,pers_nom,url_casting,pers_metier1,pers_metier2,pers_metier3,pers_nom_naiss,pers_pseudo[1],pers_nat1,pers_nat2,pers_naiss_date,pers_naiss_lieu,pers_age,pers_deces_date,pers_deces_age,pers_deces_lieu,pers_an_carriere,pers_nb_film,pers_nb_prix,pers_nb_nominat)
  # Construction des noms des elements de la liste a retourner
  names(infos) <- paste0(type_cast,indice,"_",c("id_personne","pers_nom","url_casting","pers_metier1","pers_metier2","pers_metier3","pers_nom_naiss","pers_pseudo","pers_nat1","pers_nat2","pers_naiss_date","pers_naiss_lieu","pers_age","pers_deces_date","pers_deces_age","pers_deces_lieu","pers_an_carriere","pers_nb_film","pers_nb_prix","pers_nb_nominat"))
  return(infos)
}

########## Fonction infos_film() ########## 
# Fonction infos_film() permet de recuperer les infos d'un film d'allocine
# Les arguments de la fonction sont :
# id - id du film avec mois-annee de sortie
# id_film - id du film selon allocine
# url_film - url sur laquelle on va recuperer les infos  
# traite_real - definir s'il s'agit du traitement des realisateurs/realisatrices ou des acteurs/actrices
# sauvegarde - enregistrement des sauvegardes des fichiers html et jpg

infos_film <- function(id, id_film, url_film, traite_real = TRUE, sauvegarde = FALSE) {

########## Page principale ##########
# Chargement de l'url du film
principal <- read_html(url_film)
# Initialisation de la variable
casting_ok <- FALSE

# Parcours des differents entetes de page
for (ii in 1:7) {
  test<- html_nodes(principal, paste0(".js-item-mq:nth-child(",ii,")")) %>% html_attrs() %>% unlist()
  if (!is.null(test)) { if (!is.na(test[2])) { if (test[2] =="Casting")  casting_ok <- TRUE }}
}

########## Casting ##########
# Si l'entete de page Casting existe
if (casting_ok) {
  
  # Construction de l'url de casting du film
  url_site <- "http://www.allocine.fr"
  url_casting <-paste0(url_film_core, id_film, "/casting")
  casting_html <- read_html(url_casting)
  
  ########## Realisateurs/Realisatrices ##########
  
  if (traite_real) {
    # Recuperation de l'info de l'url des realisateurs/realisatrices du film
    real_casting <- html_nodes(casting_html, ".casting-director .meta-title-link") %>% html_attr("href")
    
    # Construction de l'url des realisateurs/realisatrices du film
    real_casting[!is.na(real_casting)] <- paste0(url_site,real_casting[!is.na(real_casting)])
    
    # Recuperation des infos des 2 premiers realisateurs/realisatrices du film
    infos_real <- c(id, id_film, url_film,infos_casting(real_casting[1],"realisateur","1","data/casting/",sauvegarde),infos_casting(real_casting[2],"realisateur","2","data/casting/",sauvegarde))
    names(infos_real)[1:3] <- c("id", "id_film", "url_film")
  }
  
  ########## Acteurs/Actrices ##########
  
  if (!traite_real) {
    # Recuperation de l'info de l'url des acteurs/actrices
    acteur_casting <- html_nodes(casting_html, "#actors .meta-title-link") %>% html_attr("href")
    
    # Construction de l'url des acteurs/actrices
    acteur_casting[!is.na(acteur_casting)] <- paste0(url_site,acteur_casting[!is.na(acteur_casting)])
    
    # Recuperation des infos des 8 premiers acteurs/actrices du film
    infos_acteur <- c(id, id_film, url_film,infos_casting(acteur_casting[1],"acteur","1","data/casting/",sauvegarde))
    for(nb_act in 2:8) {
      infos_acteur <- c(infos_acteur,infos_casting(acteur_casting[nb_act],"acteur",nb_act,"data/casting/",sauvegarde))
    }
    names(infos_acteur)[1:3] <- c("id", "id_film", "url_film")
  }
  
} else {  infos_real <- c(id, id_film, url_film, rep(NA, 40))
infos_acteur <- c(id, id_film, url_film, rep(NA, 160))
}
if (traite_real) { return(infos_real) } else { return(infos_acteur) }

}

########## Fonction decompose_modalite() ########## 
# Fonction decompose_modalite() permet de decomposer les modalites d'une ou plusieurs variables d'un tableau.
# la fonction retourne un dataframe avec le meme nombre de lignes que le tableau en argument (table_a_traiter) et le nombre de colonnes des modalites les plus frequentes choisies en argument (nb)
# Les arguments de la fonction sont :
# col_a_traiter - le nom de la/ des colonnes a traiter
# nom_col - prefixe du nom de la/ des colonnes a creer
# nb - nombre de modalites a conserver. Les nb modalites les plus frequentes seront utilisees
# table_a_traiter - table a partir de laquelle on lit les donnees

decompose_modalite <- function(col_a_traiter,nom_col,nb,table_a_traiter) {
  # Construction de la liste des modalites en supprimant les valeurs vides et NA
  modalites <- unlist(lapply(col_a_traiter, function(x) { table_a_traiter[,x] }))
  modalites <- modalites[which(modalites != "")]
  modalites <- modalites[which(!is.na(modalites))]
  # Comptage de chaque modalité dans une table et tri par ordre decroissant
  tmp_frequence <- data.frame(sort(table(modalites),decreasing=TRUE))
  
  # Construction du tableau decompose par modalite
  # Pour chacune des nb premieres modalites on construit un vecteur de 0 de longueur egale a la hauteur du tableau. On met 1 a chaque indice du tableau ou la modalite apparait 
  tmp_table <- unlist(lapply(as.vector(tmp_frequence[1:nb,1]), function(x) 
    {   indice <- unlist(lapply(col_a_traiter, function(y) { which(table_a_traiter[,y] == as.character(x)) })) # Construction de la liste des indices du tableau ou la modalite apparait
  
        tmp_vector <- vector(mode = "numeric",nrow(table_a_traiter)) # Construction du vecteur de 0 de longueur egale a la hauteur du tableau.
        tmp_vector[indice] <- 1  # On met 1 a chaque indice du tableau ou la modalite apparait 
        return(tmp_vector)
    }))
  # Mise en forme du tableau construit
  tmp_table <- data.frame(matrix(tmp_table,nrow = nrow(table_a_traiter), ncol = nb))
  # Mise a jour des noms de colonnes avec le prefixe en argument (nom_col) et le nom de la modalite
  names(tmp_table) <- paste0(nom_col,"_",tmp_frequence[1:nb,1])
  return(tmp_table)
}

########## Fonction remplace_NA_moyenne() ##########
# Fonction remplace_NA_moyenne() permet de remplacer les NA par la moyenne sur un vecteur.
# Les arguments de la fonction sont :
# x - vecteur a traiter

remplace_NA_moyenne <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}


########## Fonction remplace_NA_0() ##########
# Fonction remplace_NA_0() permet de remplacer les NA par zero sur un vecteur.
# Les arguments de la fonction sont :
# x - vecteur a traiter

remplace_NA_0 <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}


########## Fonction traite_table_notes() ##########
# Fonction traite_table_notes() permet de traiter les donnees de la table des notes des films Allocine.
# Les arguments de la fonction sont :
# notes - la table des notes

traite_table_notes <- function(notes) {
  
  # table note : on ne garde que l'ID et les notes moyennes
  # str(notes)
  notes <- select(notes, c(id_film.x, note_spectateurs_moyenne, note_presse_moyenne))
  notes <- rename(notes,id_film=id_film.x)
}


########## Fonction traite_table_personnes() ##########
# Fonction traite_table_personnes() permet de traiter les donnees de la table des personnes du casting des films Allocine.
# Les arguments de la fonction sont :
# personnes - la table des personnes

traite_table_personnes <- function(personnes) {
  
  # table personnes : supprimer les variables avec trop de NA, et imputer les autres NA avec la moyenne
  # str(personnes)
  # sapply(personnes, function(x) sum(is.na(x))) # Nombre de NA pour chacune des variables
  
  # Suppression des realisateurs 2 et acteurs > 4
  var_to_drop <- c(names(personnes)[str_detect(names(personnes), "realisateur2")],
                   names(personnes)[str_detect(names(personnes), "5")],
                   names(personnes)[str_detect(names(personnes), "6")],
                   names(personnes)[str_detect(names(personnes), "7")],
                   names(personnes)[str_detect(names(personnes), "8")])
  personnes <- select(personnes, -one_of(var_to_drop))
  
  # Remplacer les NA par la moyenne sur les age / ans / nb_films
  var_NA_to_moyenne <- c(names(personnes)[str_detect(names(personnes), "age")],
                         names(personnes)[str_detect(names(personnes), "an_carriere")],
                         names(personnes)[str_detect(names(personnes), "films")])
  personnes[,var_NA_to_moyenne] <- data.frame(sapply(personnes[,var_NA_to_moyenne], remplace_NA_moyenne))
  
  # Remplacer les NA par zero sur les prix / nominations
  var_NA_to_0 <- c(names(personnes)[str_detect(names(personnes), "prix")],
                   names(personnes)[str_detect(names(personnes), "nomination")])
  personnes[,var_NA_to_0] <- data.frame(sapply(personnes[,var_NA_to_0], remplace_NA_0))
  return(personnes)
  # str(personnes[,var_NA_to_moyenne])
  # str(data.frame(sapply(personnes[,var_NA_to_moyenne], remplace_NA_moyenne)))
  
}

########## Fonction traite_table_final() ##########
# Fonction traite_table_final() permet de traiter les donnees de la table final des infos Allocine.
# Les arguments de la fonction sont :
# final - la table final

traite_table_final <- function(final) {
  
  # Variables a convertir en factor
  list_var_fact <- c(names(final) %>% str_subset("dist"),
                     names(final) %>% str_subset("genre"),
                     names(final) %>% str_subset("nationalite"))
  
  # Passage en factors des variables quali 
  final[,list_var_fact] <- data.frame(sapply(select(final, list_var_fact), factor))
  
  # Suppression de l'annee 2018
  final <- filter(final, annee_sortie != 2018)
  
  # Suppression des annees avant 1990
  final <- filter(final, annee_sortie >= 1990)
  
  # Suppression des dernieres lignes a NA
  final <- filter(final, !is.na(jour_semaine_sortie)) %>% filter(!is.na(saison_sortie)) %>% filter(!is.na(acteur1_age_sortie))
  
  # Dernieres Variables a supprimer juste pour la modelisation
  final <- select(final, -one_of(c("X.x", "X.y", "synopsis", "nb_prix", "nb_nominations", "date_sortie", "annee_sortie")))
  
  # Transformation des boolean en factor (necessaire pour gradient boosting)
  final$reprise <- factor(final$reprise)
  final$multilingues <- factor(final$multilingues)
  
  return(final)
}

########## Fonction construit_table_final() ##########
# Fonction construit_table_final() permet de construire la table consolidee des infos Allocine a partir des differentes tables.
# Les arguments de la fonction sont :
# data_dir - repertoire pour charger les fichiers csv

construit_table_final <- function(data_dir, traitement = TRUE) {
   
  ########## Chargement des tables ###
  principal <- read.csv2(paste0(data_dir,"table_principal2.csv"), encoding = "latin1") # Avec specification de l'encodage des caracteres, ici latin1 
  notes <- read.csv2(paste0(data_dir,"table_notes.csv"), encoding = "latin1")
  personnes <- read.csv2(paste0(data_dir,"variables_personnes.csv"), encoding = "latin1")
  
  ########## Traitement sur la table notes ###
  notes <- traite_table_notes(notes)
  ########## Traitement sur la table personnes ###
  personnes <- traite_table_personnes(personnes)
  
  ########## Consolidation en une seule table ###
  final <- left_join(principal, notes[!is.na(notes$id_film),], "id_film")
  final <- left_join(final, personnes, "id_film")
  
  ########## Traitement sur la table final ###
  if(traitement) { final <- traite_table_final(final)}

  }
  
########## Fonction charge_model_presse() ##########
# Fonction charge_model_presse() permet de charger les modeles entrainer pour predire les notes presse.

charge_model_presse <- function() {
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
  #load("extraTrees_presse")
}

########## Fonction charge_model_spectateur() ##########
# Fonction charge_model_spectateur() permet de charger les modeles entraines pour predire les notes spectateur.

charge_model_spectateur <- function() {
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
  #load("extraTrees_spectateurs")
}


########## Fonction eval_model_presse() ##########
# Fonction eval_model_presse() permet d'evaluer les modeles entraines pour predire les notes presse.
# Les arguments de la fonction sont :
# final - la table final

eval_model_presse <- function(final) {
  
# Chargement des modeles entraines pour predire les notes presse.
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
#load("extraTrees_presse")
  
# Jeux de donnees presse
final_presse <- filter(final, !is.na(note_presse_moyenne))  %>% select(-one_of(c("note_spectateurs_moyenne")))
# Suppression de la table final
rm(final)
  
##### Decoupage test / validation #####

# On fixe la graine
set.seed(19)

nb_films_presse <- nrow(final_presse)
id_train_presse <- sample(1:nb_films_presse, nb_films_presse*(4/5))

validation_presse <- final_presse[-id_train_presse,] %>% select(-one_of(c("titre", "url", "id_film")))
x_validation_presse <- select(validation_presse, -one_of(c("note_presse_moyenne")))
y_validation_presse <-  pull(validation_presse,note_presse_moyenne)

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
#results_extraTrees_presse <- predict(extraTrees_presse, newdata = x_validation_presse )

# Mesures de performance
performances_model_presse <- data.frame(model = c("Random Forest", "Arbre CART", "Gradient boosting", "Régression linéaire", "Régression lin. backw.", "Régression lin. forw.", "Régression ridge-lasso", "SVM", "SVM lineaire", "Extra G. boosting"))
performances_model_presse <- data.frame(performances_model_presse, rbind(
  postResample(pred = results_rf_presse, obs = validation_presse$note_presse_moyenne),
  postResample(pred = results_rpart_presse, obs = validation_presse$note_presse_moyenne),
  postResample(pred = results_gradient_presse, obs = validation_presse$note_presse_moyenne),
  postResample(pred = results_regression_presse, obs = validation_presse$note_presse_moyenne),
  postResample(pred = results_regforward_presse, obs = validation_presse$note_presse_moyenne),
  postResample(pred = results_regbackward_presse, obs = validation_presse$note_presse_moyenne),
  postResample(pred = results_glmnet_presse, obs = validation_presse$note_presse_moyenne),
  postResample(pred = results_svm_presse, obs = validation_presse$note_presse_moyenne),
  postResample(pred = results_svmlineaire_presse, obs = validation_presse$note_presse_moyenne),
  postResample(pred = results_xgboost_presse, obs = validation_presse$note_presse_moyenne)))
performances_model_presse$model <- factor(performances_model_presse$model)

# Dataframe compilant les résultats des différents modèles sur l'échantillon de validation
final_results <- data.frame( valeur_reelle = y_validation_presse,
                             Random_Forest = results_rf_presse,
                             Rpart = results_rpart_presse,
                             Gradient = results_gradient_presse,
                             Regression = results_regression_presse,
                             Regression_backward = results_regbackward_presse, 
                             Regression_penalisee = results_glmnet_presse,
                             SVM = results_svm_presse,
                             SVM_lineaire = results_svmlineaire_presse,
                             Extra_Gradient = results_xgboost_presse
)

return(c(final_results, performances_model_presse))

}

########## Fonction eval_model_spectateur() ##########
# Fonction eval_model_spectateur() permet d'evaluer les modeles entraines pour predire les notes spectateur.
# Les arguments de la fonction sont :
# final - la table final

eval_model_spectateurs <- function(final) {
  
  # Chargement des modeles entraines pour predire les notes spectateur.
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
  #load("extraTrees_spectateurs")
  
  # Jeux de donnees spectateur
  final_spectateurs <- filter(final, !is.na(note_spectateurs_moyenne))  %>% select(-one_of(c("note_presse_moyenne")))
  # Suppression de la table final
  rm(final)
  
  ##### Decoupage test / validation #####
  
  # On fixe la graine
  set.seed(19)
  
  nb_films_spectateurs <- nrow(final_spectateurs)
  id_train_spectateurs <- sample(1:nb_films_spectateurs, nb_films_spectateurs*(4/5))
  
  validation_spectateurs <- final_spectateurs[-id_train_spectateurs,] %>% select(-one_of(c("titre", "url", "id_film")))
  x_validation_spectateurs <- select(validation_spectateurs, -one_of(c("note_spectateurs_moyenne")))
  y_validation_spectateurs <-  pull(validation_spectateurs,note_spectateurs_moyenne)
  
  # Prédictions sur échantillon de validation
  results_rf_spectateurs <- predict(rf_spectateurs, newdata = x_validation_spectateurs )
  results_rpart_spectateurs <- predict(rpart_spectateurs, newdata = x_validation_spectateurs )
  results_gradient_spectateurs <- predict(gradient_spectateurs, newdata = x_validation_spectateurs )
  results_regression_spectateurs <- predict(regression_spectateurs, newdata = x_validation_spectateurs )
  results_regforward_spectateurs <- predict(regforward_spectateurs, newdata = x_validation_spectateurs )
  results_regbackward_spectateurs <- predict(regbackward_spectateurs, newdata = x_validation_spectateurs )
  results_glmnet_spectateurs <- predict(glmnet_spectateurs, newdata = x_validation_spectateurs )
  results_svm_spectateurs <- predict(svm_spectateurs, newdata = x_validation_spectateurs )
  results_svmlineaire_spectateurs <- predict(svmlineaire_spectateurs, newdata = x_validation_spectateurs )
  results_xgboost_spectateurs <- predict(xgboost_spectateurs, newdata = x_validation_spectateurs )
  #results_extraTrees_spectateurs <- predict(extraTrees_spectateurs, newdata = x_validation_spectateurs )
  
  # Mesures de performance
  performances_model_spectateurs <- data.frame(model = c("Random Forest", "Arbre CART", "Gradient boosting", "Régression linéaire", "Régression lin. backw.", "Régression lin. forw.", "Régression ridge-lasso", "SVM", "SVM lineaire", "Extra G. boosting"))
  performances_model_spectateurs <- data.frame(performances_model_spectateurs, rbind(
    postResample(pred = results_rf_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
    postResample(pred = results_rpart_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
    postResample(pred = results_gradient_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
    postResample(pred = results_regression_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
    postResample(pred = results_regforward_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
    postResample(pred = results_regbackward_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
    postResample(pred = results_glmnet_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
    postResample(pred = results_svm_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
    postResample(pred = results_svmlineaire_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne),
    postResample(pred = results_xgboost_spectateurs, obs = validation_spectateurs$note_spectateurs_moyenne)))
  performances_model_spectateurs$model <- factor(performances_model_spectateurs$model)
  
  # Dataframe compilant les résultats des différents modèles sur l'échantillon de validation
  final_results <- data.frame( valeur_reelle = y_validation_spectateurs,
                               Random_Forest = results_rf_spectateurs,
                               Rpart = results_rpart_spectateurs,
                               Gradient = results_gradient_spectateurs,
                               Regression = results_regression_spectateurs,
                               Regression_backward = results_regbackward_spectateurs, 
                               Regression_penalisee = results_glmnet_spectateurs,
                               SVM = results_svm_spectateurs,
                               SVM_lineaire = results_svmlineaire_spectateurs,
                               Extra_Gradient = results_xgboost_spectateurs
  )
  
  return(c(final_results, performances_model_spectateurs))
  
}
########## OLD Fonction construit_table_final <- function(data_dir) { ########## 
#   
#   ########## Chargement des tables ###
#   principal <- read.csv2(paste0(data_dir,"table_principal2.csv"), encoding = "latin1") # Avec specification de l'encodage des caracteres, ici latin1 
#   casting <- read.csv2(paste0(data_dir,"table_casting.csv"), encoding = "latin1")
#   real <- read.csv2(paste0(data_dir,"table_real_1966-2017_v02.csv"))
#   acteurs <- read.csv2(paste0(data_dir,"table_acteur_1966-2017_v02.csv"))
#   notes<- read.csv2(paste0(data_dir,"table_notes.csv"), encoding = "latin1")
#   
#   ########## Consolidation en une seule table ###
#   
#   notes<- rename(notes,id_film=id_film.x)
#   real$id_film <- as.integer(real$id_film)
#   acteurs$id_film<- as.integer(acteurs$id_film)
#   
#   final<- left_join(principal[!(principal$mois_sortie %in% c("201801","201802","201803")),], casting, "id_film")
#   final<- left_join(final, real, "id_film")
#   final<- left_join(final, acteurs, "id_film")
#   final<- left_join(final, notes[!is.na(notes$id_film),], "id_film")
# }
