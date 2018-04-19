########## Librairies - options ##########
library(RCurl)
library(rvest)

#library(dplyr)
#library(httr)
#library(stringr)

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
