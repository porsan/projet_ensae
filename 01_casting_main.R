########## Librairies - options ##########

#install.packages("rvest")
#install.packages("RCurl")
#vignette("selectorgadget")

library(rvest)
library(dplyr)
library(httr)
library(stringr)
library(RCurl)

options(stringsAsFactors=F) # Pour importer les donnees quali en charactere et pas facteurs

########## Source ##########
# Permet de charger les fonctions definies dans le fichier

source("02_voca_jap_functions.R")

########## Programme ##########

# Repertoire de sauvergarde des donnees
data_dir <- "data/"
data_dir_casting <- "data/casting/"

# Indicateur intitialisation des tables
creation_table = TRUE

# Indicateur pour sauvegarder les pages et images en fichier
sauvegarde = FALSE

# Definir les traitements a executer : 
# Extraction des infos realisateur/realisatrice
traite_real = TRUE
# Extraction des infos acteur/actrice
traite_acteur = TRUE

# Liste des annees a parcourir
liste_annees <- 1980:1981


# On parcourt la liste de toutes les annees
for (annee in liste_annees) {
  
  # Liste des mois a parcourir
  liste_mois <- c(paste0(annee,"-0",1:9), paste0(annee, "-", 10:12))

  # On parcourt la liste de tous les mois d'une annee
  for (id_mois in liste_mois) {
    
    # Mois en cours
    #id_mois<- "2017-01"
    
    #extraction de la liste de toutes les url d'un mois de sortie donne, avec le nom de chaque film
    url_mois<- paste0("http://www.allocine.fr/film/agenda/mois/mois-",id_mois,"/")
    
    # Recuperation des URL des films
    liens_mois <- html_session(url_mois)
    liste_url_mois<- paste0("http://www.allocine.fr",unlist(html_nodes(liens_mois,"h3 a") %>% html_attrs()))
    
    # Recuperation du nom des films
    textes_mois<- read_html(url_mois)
    noms_films<- textes_mois %>% html_nodes("#col_main strong") %>% html_text() %>% gsub(pattern="\\n",x=.,"")
    
    # On affecte a chaque url son nom
    names(liste_url_mois)<- noms_films
    
    
    # URL de base d'une page film (a completer ensuite par ID et sous repertoire)
    url_film_core<-"http://www.allocine.fr/film/fichefilm-"
    
    ########## Debut de la boucle des films ##########
    
    # On parcourt la liste de toutes les URL des films du mois
    for (ind_film in 1:length(liste_url_mois)) {
      
      #ind_film <-9
      
      print(paste0("progression en cours : ", ind_film, " sur ", length(liste_url_mois), " , mois ", id_mois, " , annee ", annee))
      
      ########## Infos generales ##########
      
      # URL du film  
      url_film<- liste_url_mois[ind_film]
      
      # ID du film
      id_film <- unlist(str_extract_all(url_film, "[0-9]+"))
      #if (length(id_film) == 0) id_film <- unlist(str_extract_all(url_film, "[0-9]")) # Cas particulier de l'id film ? 1 chiffre...
      
      # Cle ID - Mois
      id <-paste0(id_mois,"_",id_film)
      
      
      ########## Page principale ##########
      
      principal <- read_html(url_film)
      
      casting_ok <- FALSE
      
      for (ii in 1:7) { # Parcours des differents entetes de page
        test<- html_nodes(principal, paste0(".js-item-mq:nth-child(",ii,")")) %>% html_attrs() %>% unlist()
        if (!is.null(test)) { if (!is.na(test[2])) { if (test[2] =="Casting")  casting_ok <- TRUE }}
      }
      
      ########## Casting ##########
      
      if (casting_ok) { # Seulement s'il y a un casting sur le film
        
        url_site <- "http://www.allocine.fr"
        url_casting <-paste0(url_film_core, id_film, "/casting")
        casting_html <- read_html(url_casting)
        
        #casting_html <- read_html("http://www.allocine.fr/film/fichefilm-32000/casting/")
        #casting_html <- read_html("http://www.allocine.fr/film/fichefilm-91975/casting/")
        
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
        
        #acteur_casting <- html_nodes(casting_html, ".casting-voice .meta-title-link") %>% html_attrs()
        #acteur_casting <- html_nodes(casting_html, "#actors .meta-title-link") %>% html_attrs()
        #acteur_casting <- html_nodes(casting_html, ".card-person .meta-title-link") %>% html_attrs()
        #acteur_casting
        
        if (traite_acteur) {
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
        
        if (creation_table) {
          if (traite_real) { table_real <- infos_real }
          if (traite_acteur) { table_acteur <- infos_acteur }
          creation_table = FALSE
        } else {
          if (traite_real) { table_real <- rbind(table_real, infos_real) }
          if (traite_acteur) { table_acteur <- rbind(table_acteur, infos_acteur) }
        }
      
      }
      # Sauvegarder en csv les tables crees, nouvelle version chaque mois
      if (traite_acteur) { write.csv2(table_acteur, paste0(data_dir,"table_acteur_", id_mois, ".csv")) }
      if (traite_real) { write.csv2(table_real, paste0(data_dir,"table_real_", id_mois, ".csv")) }
    }
    # Sauvegarder en csv les tables crees, nouvelle version chaque annee
  if (traite_acteur) { write.csv2(table_acteur, paste0(data_dir,"table_acteur_", annee, ".csv")) }
    if (traite_real) { write.csv2(table_real, paste0(data_dir,"table_real_", annee, ".csv")) }
}
  # Verifier le nombre d'observation des tables
  dim(table_real)
  dim(table_acteur)
  
  # Visualiser les tables
  View(table_real)
  View(table_acteur)