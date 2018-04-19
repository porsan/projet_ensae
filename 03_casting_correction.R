########## Librairies - options ##########

library(stringr)
library(httr)
library(RCurl)

options(stringsAsFactors=F) # Pour importer les donnees quali en charactere et pas facteurs

# Repertoire de sauvergarde des donnees
data_dir <- "data/"
data_dir_casting <- "data/casting/"

########## Lecture donnees ##########

source("02_casting_functions.R")


########## Gestion des pseudos et nom de naissance mal geres pour la table realisateur ########## 

# Chargement de la table realisateur
tmp_table <- read.csv2(paste0(data_dir,"table_real_1966-2017_v01.csv")) #1966 - 2017

# Indice des lignes a corriger : lorsqu'il y a un decalage le nom du 2nd realisateur est remplace par son id
which(!is.na(as.numeric(tmp_table$realisateur2_pers_nom)))
# Boucle de reprise des pseudos et nom de naissance mal geres pour la table realisateur
for (cpt in which(!is.na(as.numeric(tmp_table$realisateur2_pers_nom)))) {
  print(cpt)
  print(tmp_table$realisateur2_pers_nom[cpt])
  tmp_table[cpt,] <- c(tmp_table[cpt,1],infos_film(tmp_table[cpt,2],tmp_table[cpt,3],tmp_table[cpt,4],TRUE))
}


## Correction lorsqu'un nom apparaissait dans la colonne age ##
champ_real <- tmp_table$realisateur1_pers_age

# Indice des lignes a corriger : lorsque le nom teste est present dans la colonne age
which(champ_real == "Sidi Bel Abbes - Algérie")
# Boucle de reprise des pseudos et nom de naissance mal geres pour la table realisateur sur les lignes a corriger
for (cpt in which(champ_real == "Sidi Bel Abbes - Algérie")) {
  print(cpt)
  print(champ_real[cpt])
  # On remplace toute la ligne de la table. On reprend l'info de la premiere colonne en l'etat et on reconstruit les autres avec la fonction infos_film()
  tmp_table[cpt,] <- c(tmp_table[cpt,1],infos_film(tmp_table[cpt,2],tmp_table[cpt,3],tmp_table[cpt,4],TRUE,FALSE))
}

## Pour reconstruire les infos d'une ligne en particulier. cpt est l'indice de la ligne ##
cpt <- 6976
# Affichage de la ligne
tmp_table[cpt,]
# On remplace toute la ligne de la table. On reprend l'info de la premiere colonne en l'etat et on reconstruit les autres avec la fonction infos_film()
tmp_table[cpt,] <- c(tmp_table[cpt,1],infos_film(tmp_table[cpt,2],tmp_table[cpt,3],tmp_table[cpt,4],TRUE))

# Sauvegarder en csv les tables crees
write.csv2(tmp_table, paste0(data_dir,"table_real_1966-2017_v02.csv"))

########## Gestion des pseudos et nom de naissance mal geres pour la table acteur ########## 

# Chargement des tables acteur 
tmp_table1 <- read.csv2(paste0(data_dir,"table_acteur_2017.csv")) #2005 - 2017
tmp_table2 <- read.csv2(paste0(data_dir,"table_acteur_2004.csv")) #2001 - 2004
tmp_table3 <- read.csv2(paste0(data_dir,"table_acteur_2000.csv")) #1966 - 2000

# Le traitement est fait sur les tables contenant les infos des acteurs de 2001 a 2017 (tmp_table1 et tmp_table2) 
tmp_table <- rbind(tmp_table2,tmp_table1)

## Verification du max de l'age des acteurs ##
# Affichage de l'indice du max pour la colonne choisie
which.max(tmp_table$acteur6_pers_age)
# Affichage de l'url de la page acteur du max pour la colonne choisie
tmp_table$acteur6_url_casting[which.max(tmp_table$acteur6_pers_age)]


# Selection de la colonne sur laquelle une anomalie est observee et repetee
champ_acteur <- tmp_table$acteur1_pers_age
# Indice des lignes a corriger : lorsqu'il y a un decalage le nom d'acteur est remplace par son id
which(champ_acteur != "NA")
# Boucle de reprise des pseudos et nom de naissance mal geres pour la table acteur
for (cpt in which(champ_acteur != "NA")) {
  print(cpt)
  print(champ_acteur[cpt])
  tmp_table[cpt,] <- c(tmp_table[cpt,1],infos_film(tmp_table[cpt,2],tmp_table[cpt,3],tmp_table[cpt,4],FALSE))
}

## Pour reconstruire les infos d'une ligne en particulier. cpt est l'indice de la ligne ##
cpt <- 85
# Affichage de la ligne
tmp_table[cpt,]
# On remplace toute la ligne de la table. On reprend l'info de la premiere colonne en l'etat et on reconstruit les autres avec la fonction infos_film()
tmp_table[cpt,] <- c(tmp_table[cpt,1],infos_film(tmp_table[cpt,2],tmp_table[cpt,3],tmp_table[cpt,4],FALSE))

# Sauvegarder en csv la table cree
write.csv2(tmp_table, paste0(data_dir,"table_acteur_2001-2017_v01.csv"))

########## Reprise des id pour la table acteur ##########

# On construit l'id acteur a partir de la page html de l'acteur.
# On va parcourir chaque colonne de la table contenant l'url de la page acteur. Nous avons 8 acteurs enregistrés par film donc 8 colonnes a traiter.
for(i in 1:8) {
  id_tmp[[i]] <- as.matrix(as.numeric(str_extract_all(tmp_table[[paste0("acteur",i,"_url_casting")]], "[0-9]+")))
  tmp_table[paste0("acteur",i,"_id_personne")] <- id_tmp[i]
}

# Sauvegarder en csv la table modifiee
write.csv2(tmp_table, paste0(data_dir,"table_acteur_1966-2017_v02.csv"))


########## Boucle de sauvegarde des pages html et des photos des acteurs ########## 

# Chargement des donnees a partir du fichier csv de table_acteur
tmp_table <- read.csv2(paste0(data_dir,"table_acteur_1966-2017_v02.csv")) #1966 - 2017

# On va parcourir chaque colonne de la table contenant l'url de la page acteur. Nous avons 8 acteurs enregistrés par film donc 8 colonnes a traiter.
for(i in 1:8) {
  # Nom de la colonne
  nom_col <- paste0("acteur",i,"_url_casting")
  # Recuperation de la ieme colonne des urls 
  url_realisation <- tmp_table[,nom_col]
  # 
  url_realisation <- url_realisation[which(!is.na(url_realisation))]
  url_realisation <- unique(url_realisation)
  
  # Nbre d'urls de la colonne traitee
  nb_lignes <- length(url_realisation)
  # Initialisation du compteur 
  ligne <- 0
  for(urls in url_realisation) {
    # Affichage des infos de suivi du traitement  
    print(paste0("Colonne traitee: ",i, " - ligne: ",ligne <- ligne+1," sur ",nb_lignes))
  
    # Lecture de l'url
    url_realisation_brut <- getURL(urls)
    realisation <- read_html(url_realisation_brut)  
  
    # Pour tester si la page existe
    names_realisation <- realisation %>% html_nodes(".meta-body-item .light") %>% html_text()
    if(length(names_realisation) != 0) {
    
      # Pour sauvegarder la page html
      id_realisation <- unlist(str_extract_all(urls, "[0-9]+"))
      nom_save <- paste0(data_dir_casting,id_realisation,"_04casting",".txt")
      print(urls)
      # On sauvgarde la page si elle n'est pas encore enregistrée
      if(!file.exists(nom_save)) {
        write.csv(url_realisation_brut,nom_save)
        print("Page html enregistree")
      }
    
      # Pour sauvegarder la photo de l'acteur
      photo_rea <- realisation %>% html_nodes(".card-person-overview .thumbnail-img") %>% html_attr("src")
      nom_save <- paste0(data_dir_casting,id_realisation,"_04casting_photo",".jpg")
      # On sauvgarde la photo si elle n'est pas encore enregistrée
      if(!file.exists(nom_save)) {
        GET(photo_rea, write_disk(nom_save))
        print("Photo enregistree")
      }
  }
  }
}
