
########## Librairies - options ##########


#install.packages("rvest")
#install.packages("RCurl")
#vignette("selectorgadget")

library(rvest)
library(dplyr)
library(httr)
library(stringr)
library(RCurl)

options(stringsAsFactors=F) # Pour importer les données quali en charactère et pas facteurs

setwd("H:/Formation CEPE/Projet")

########## Programme ##########

# Indicateur intitialisation des tables
creation_table = TRUE

# Liste des annees à parcourir
liste_annees <- 1979:1950


# On parcourt la liste de toutes les années
for (annee in liste_annees) {

# Liste des mois à parcourir
liste_mois <- c(paste0(annee,"-0",1:9), paste0(annee, "-", 10:12))
  
# On parcourt la liste de tous les mois d'une année
for (id_mois in liste_mois) {

# Mois en cours
#id_mois<- "2017-01"

#extraction de la liste de toutes les url d'un mois de sortie donnÃ©, avec le nom de chaque film
url_mois<- paste0("http://www.allocine.fr/film/agenda/mois/mois-",id_mois,"/")

# Récupération des URL des films
liens_mois <- html_session(url_mois)
liste_url_mois<- paste0("http://www.allocine.fr",unlist(html_nodes(liens_mois,"h3 a") %>% html_attrs()))

# Récupération du nom des films
textes_mois<- read_html(url_mois)
noms_films<- textes_mois %>% html_nodes("#col_main strong") %>% html_text() %>% gsub(pattern="\\n",x=.,"")

# On affecte à chaque url son nom
names(liste_url_mois)<- noms_films


# URL de base d'une page film (à compléter ensuite par ID et sous répertoire)
url_film_core<-"http://www.allocine.fr/film/fichefilm-"




########## Début de la boucle des films ##########

# On parcourt la liste de toutes les URL des films du mois
for (ind_film in 1:length(liste_url_mois)) {

#ind_film <-9
  
print(paste0("progression en cours : ", ind_film, " sur ", length(liste_url_mois), " , mois ", id_mois, " , annee ", annee))

  
########## Infos générales ##########
  
# URL du film  
url_film<- liste_url_mois[ind_film]

# ID du film
id_film <- unlist(str_extract_all(url_film, "[0-9](.*)[0-9]"))
if (length(id_film) == 0) id_film <- unlist(str_extract_all(url_film, "[0-9]")) # Cas particulier de l'id film à 1 chiffre...

# Clé ID - Mois
id <-paste0(id_mois,"_",id_film)


########## Page principale ##########

principal <- read_html(url_film)




# Booléens indiquant s'il y a ou non des critiques / notes spectateurs
#presse_ok <- ! length(critiques_presse_principal) == 0
#spectateurs_ok <- ! length(notes_spectateurs_principal) == 0

spectateurs_ok <- FALSE
presse_ok <- FALSE

for (ii in 1:7) { # Parcours des différentes entetes de page
test<- html_nodes(principal, paste0(".js-item-mq:nth-child(",ii,")")) %>% html_attrs() %>% unlist()
if (!is.null(test)) { if (!is.na(test[2])) { if (test[2] =="Critiques presse")  presse_ok <- TRUE }}
if (!is.null(test)) { if (!is.na(test[2])) { if (test[2] =="Critiques spectateurs")  spectateurs_ok <- TRUE }}
}

########## Critiques presse ##########

if (presse_ok) { # Seulement s'il y a des critiques sur le film

url_critiques_presse <-paste0(url_film_core, id_film, "/critiques/presse")
presse_html <- read_html(url_critiques_presse)


encart_notes_presse <- html_nodes(presse_html, ".reviews-press-intro")   %>% html_text() %>% str_replace_all("[:blank:]", "")

note_presse_moyenne <- html_nodes(presse_html, ".stareval-note")[1]  %>% html_text() %>% str_replace_all("[:blank:]","") %>% str_replace_all("\\n","")  %>% str_replace_all(",",".") %>% as.numeric()
temp_lim <- str_trim(encart_notes_presse) %>% str_locate("titres")
nb_critiques <- str_sub(encart_notes_presse, temp_lim[1]-1, temp_lim[1]) %>% str_replace("[:alpha:]","") %>% as.numeric() 
str_sub(encart_notes_presse, temp_lim[1]-1, temp_lim[1])
html_nodes(presse_html, ".reviews-press-intro")   %>% html_text() %>% str_replace_all("[:blank:]", "")


notes_presse <- html_nodes(presse_html, "span.stareval-link") %>% html_attr("title")
notes_presse <- notes_presse[!is.na(notes_presse)]

journaux_presse <-  html_nodes(presse_html, "span.stareval-link") %>% html_text()
journaux_presse <- journaux_presse[journaux_presse != ""]



# Recodification des notes textuelles en notes numériques

notes_presse <- ifelse(notes_presse=="Nul", 0.5,
                 ifelse(notes_presse=="Très mauvais", 1, 
                 ifelse(notes_presse=="Mauvais", 1.5, 
                 ifelse(notes_presse=="Pas terrible", 2, 
                 ifelse(notes_presse=="Moyen", 2.5, 
                 ifelse(notes_presse=="Pas mal", 3, 
                 ifelse(notes_presse=="Bien", 3.5, 
                 ifelse(notes_presse=="Très bien", 4, 
                 ifelse(notes_presse=="Excellent", 4.5, 
                 ifelse(notes_presse=="Chef-d'oeuvre", 5, 
                        0))))))))))

# Compléter les notes et journaux presses non renseignées par des NA
notes_presse <- c(notes_presse, rep(NA, 50-NROW(notes_presse)))
journaux_presse <- c(journaux_presse, rep(NA, 50-NROW(journaux_presse)))

# Matrices des notes presse et noms de journaux associés
notes_presse <- matrix(notes_presse, nrow = 1)
journaux_presse <- matrix(journaux_presse, nrow = 1)
dim(notes_presse)
dim(journaux_presse)

colnames(notes_presse) <- paste0(c(rep("presse", 50)),1:50,"_note")
colnames(journaux_presse) <- paste0(c(rep("presse", 50)),1:50)

# Concaténation des infos générales de presse et des matrices de notes / noms de journaux
infos_presse <- data.frame(id, id_film, url_film, note_presse_moyenne, nb_critiques) %>% cbind(notes_presse, journaux_presse)

} else { infos_presse <- c(id, id_film, url_film, NA, NA, rep(NA, 100))
names(infos_presse) <- names(table_presse)}




length(infos_presse)
########## Notes spectateurs ##########


if (spectateurs_ok) { # Seulement s'il y a des notes spectateurs sur le film
  

url_critiques_spectateurs <-paste0(url_film_core,id_film,"/critiques/spectateurs")

spectateurs_html <- read_html(url_critiques_spectateurs)

note_spectateurs_moyenne <-  html_nodes(spectateurs_html, ".note") %>% html_text() %>% str_replace(",",".") %>%  as.numeric()

encart_notes_spectateurs <- html_nodes(spectateurs_html, ".big-note .light") %>% html_text() 
temp_lim <- str_trim(encart_notes_spectateurs) %>% str_locate("notes")
nb_notes_spectateurs <- str_trim(encart_notes_spectateurs) %>% str_sub(1, temp_lim[1] - 2) %>% as.numeric()

notes_spectateurs_5 <- html_nodes(spectateurs_html,".item:nth-child(1) .percent") %>% html_text() %>% str_replace('%','') %>% as.numeric()
notes_spectateurs_4 <- html_nodes(spectateurs_html,".item:nth-child(2) .percent") %>% html_text() %>% str_replace('%','') %>% as.numeric()
notes_spectateurs_3 <- html_nodes(spectateurs_html,".item:nth-child(3) .percent") %>% html_text() %>% str_replace('%','') %>% as.numeric()
notes_spectateurs_2 <-  html_nodes(spectateurs_html, ".item:nth-child(4) .percent") %>% html_text() %>% str_replace('%','') %>% as.numeric()
notes_spectateurs_1 <-  html_nodes(spectateurs_html,".item:nth-child(5) .percent") %>% html_text() %>% str_replace('%','') %>% as.numeric()
notes_spectateurs_0 <-  html_nodes(spectateurs_html,".item:nth-child(6) .percent") %>% html_text() %>% str_replace('%','') %>% as.numeric()

infos_spectateurs<- data.frame(id, id_film, url_film, note_spectateurs_moyenne, nb_notes_spectateurs, notes_spectateurs_0, notes_spectateurs_1, notes_spectateurs_2, notes_spectateurs_3, notes_spectateurs_4, notes_spectateurs_5)


} else { infos_spectateurs <- c(id, id_film, url_film, NA, NA, NA, NA, NA, NA, NA, NA) 
names(infos_spectateurs) <- names(table_spectateurs) }

########## Agrégation des différentes informations  ##########

if (creation_table) {
  table_spectateurs <- infos_spectateurs
  table_presse <- infos_presse
  creation_table = FALSE
  } else {
    table_spectateurs <- rbind(table_spectateurs, infos_spectateurs)
    table_presse <- rbind(table_presse, infos_presse) 
  }
}
# Sauvegarder en csv les tables crées, nouvelle version chaque mois
write.csv2(table_presse, paste0("table_presse_", id_mois, ".csv"))
write.csv2(table_spectateurs, paste0("table_spectateurs_", id_mois, ".csv"))
}
  # Sauvegarder en csv les tables crées, nouvelle version chaque année
  write.csv2(table_presse, paste0("table_presse_", annee, ".csv"))
  write.csv2(table_spectateurs, paste0("table_spectateurs_", annee, ".csv"))
}
# Vérifier le nombre d'observation des tables
dim(table_spectateurs)

dim(table_presse)


# Visualiser les tables
View(table_spectateurs)
View(table_presse)


