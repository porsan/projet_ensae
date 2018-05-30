

##########################################
########## Librairies - options ##########
##########################################

#install.packages("car")
#install.packages("lubridate")

library(ggplot2)
library(stringr)
library(plotly)
library(dplyr)
library(reshape2)
library(GGally)
library(car)
library(lubridate)

############################################
############ Lecture donn?es ###############
############################################



options(stringsAsFactors=F) # Pour importer les donn?es quali en charact?re et pas facteurs

setwd("H:/Formation CEPE/Projet")
table_principal <- read.csv2("table_principal.csv")

str(table_principal)

dim(table_principal)



############################################
#            Fonctions utiles              #
############################################

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


########## Fonction Freq() ########## 
 # Fonction permettant de donner des fréquences, comme "un "table" mais en plus abouti : 
 # fréquences brutes, % et % cumulé
 # trié par ordre décroissant de fréquences
 # avec une "coupure" à choisir sur les modalités les plus nombreuses à afficher


freq <- function(x, nblines = 20, empty = TRUE) {
  if (!empty) { x <- x[x != ""] } # Prend on ou non en compte la modalité vide ? (oui par défaut)
  res <- data.frame(sort(table(x), decreasing = TRUE)) # Fréquences simples sous format dataframe
  vcum <- round(cumsum(sort(table(x), decreasing = TRUE)) / length(x) * 100,2) # Vecteur de fréquences cumultées
  res <- data.frame(var = res[,1], freq = res[,2], percent = round(res[,2] / length(x) * 100,2), cum = vcum, row.names = NULL) # concaténations variables
  head(res, nblines) # Limité à nblines premières lignes
}


############################################
######### Traitements variables ############
############################################





# MOIS DE SORTIE
table_principal$mois_sortie <- str_extract(table_principal$date_sortie, "[:alpha:]+") %>% 
    recode(" 'janv'='01'; 'févr'='02'; 'mars'='03' ;'avr'='04' ; 'mai'= '05' ; 'juin'= '06' ;'juil'= '07'; 'août'= '08'; 'sept'= '09'; 'oct'= '10' ;'nov'= '11' ;'déc'= '12'") %>% 
    as.integer()

    # V?rif : que des mois compris entre 1 et 12
    table(table_principal$mois_sortie, useNA = "always")

    
# JOUR DE SORTIE

table_principal$jour_sortie <- str_extract(table_principal$date_sortie, "[:digit:]+") %>% as.integer %>%  recode("32:hi=NA")
table_principal$jour_sortie[is.na(table_principal$jour_sortie)] <- sample(1:28,1) # POur ne pas perdre les dates sans jour, on donne un jour al?atoire

    # Verif : que des jours compris entre 1 et 30
    table(table_principal$jour_sortie, useNA = "always")
  
        
# ANNEE DE SORTIE
annee_sortie <- str_extract(table_principal$date_sortie, "[:digit:]+$") %>% as.numeric
table_principal$annee_sortie <- ifelse(annee_sortie < 19, annee_sortie + 2000, 
                ifelse(annee_sortie < 100, annee_sortie + 1900,
                                           annee_sortie))

    # Verif : que des ann?es entre 1900 et 2018
    table(table_principal$annee_sortie, useNA = "always")


# DATE DE SORTIE au format DATE

table_principal$date_sortie_old <- table_principal$date_sortie
table_principal$str_date <- paste0(table_principal$jour_sortie,"/", table_principal$mois_sortie, "/", table_principal$annee_sortie)
table_principal$date_sortie <- as.Date(table_principal$str_date, "%d/%m/%Y") 

table_principal[is.na(table_principal$date_sortie), c("jour_sortie", "str_date", "date_sortie","date_sortie_old")]


# JOUR DE LA SEMAINE
table_principal$jour_semaine_sortie <- weekdays(table_principal$date_sortie)

  # Verif : que des jours de semaine
  table(table_principal$jour_semaine_sortie, useNA = "always")

  
  
# SAISON DE SORTIE

# fonction reprise de : "https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to"
getSeason <- function(DATES) { 
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Hiver",
          ifelse (d >= SE & d < SS, "Printemps",
                  ifelse (d >= SS & d < FE, "Et?", "Automne")))
}

table_principal$saison_sortie <- getSeason(table_principal$date_sortie)

    # Verif : que des saisons
    table(table_principal$saison_sortie, useNA = "always")

    
    
# REPRISE : OUI ou NON
table_principal$reprise <- ifelse(table_principal$date_reprise=="",0,1) %>% as.logical()

    # Verif : que true or false    
    round(table(table_principal$reprise, useNA = "always") / length(table_principal$reprise),2)

    
    
# RECOMPENSES (isoler prix et nominations)
    
    nb_prix <- str_extract(table_principal$recompenses,"[:digit:]+ prix")  %>% str_extract("[:digit:]+") %>%  as.integer()
    nb_nominations <- str_extract(table_principal$recompenses,"[:digit:]+ nomination")   %>% str_extract("[:digit:]+") %>%  as.integer()

    table_principal$nb_prix <- ifelse(is.na(nb_prix), 0, nb_prix)
    table_principal$nb_nominations <- ifelse(is.na(nb_nominations), 0, nb_nominations)
    
      # Verifs : pas de NA et beaucoup de 0
      table(table_principal$nb_nominations, useNA = "always")    
      table(table_principal$nb_prix, useNA = "always")    
      table_principal[table_principal$nb_prix>50, "titre"] # Ex du film le plus nomin? : the artistes  
      
 
      
# TYPE FILM (garder que les long et moyen m?trages)  
      table_principal <- filter(table_principal, type_film %in% c("Long-métrage", "Moyen-métrage"))
      
      # Verif : il ne doit rester que des long / moyens m?trages
      table(table_principal$type_film)
      

# NB_ANECDOTES (nombre de secrets tournage en num?rique)
      
      table_principal$nb_anecdotes <- str_extract(table_principal$secrets_tournage, "[:digit:]+")  %>% as.integer() %>%  recode("NA=0")
      
      # Verif : que des valeurs num?riques avec bcp de 0 et pas de NA
      table(table_principal$nb_anecdotes, useNA = "always")
      table_principal[table_principal$nb_anecdotes>70, "titre"] # Ex d'un film avec beaucoup d'anecdotes : avenger)
      

      summary(table_principal$budget)
      
      
      
# Box-office et budget : trop peu de valeurs manquantes
sum(table_principal$boxoffice_france=="") 
sum(table_principal$budget=="")       
      

# LANGUES : recoder en multi-langues oui / non

table_principal$multilingues <- str_detect(table_principal$langues, ",")

  # Verif : que des oui / non et pas de na, bpc de non
  table(table_principal$multilingues, useNA="always")

  
  
# COULEUR : quasi-rien ? faire ? part traiter les quelques valeurs manquantes

table_principal$couleur <- recode(table_principal$couleur, "''='Couleur'") # On consid?re que les non renseign?s sont des "couleurs", le genre ultra dominant
  
    # Verif : 3 modalit?s uniquement et pas de vide / na, ?norme majorit? de couleurs  
    table(table_principal$couleur, useNA="always")
    
    
    
# DUREE : tout convertir en minutes, traiter les valeurs manquantes et enlever les films de plus de 4h
    
duree <- table_principal$duree 
    
nb_heures <- str_extract(duree, "[:digit:]+h") %>% str_extract("[:digit:]+") %>% as.integer()
nb_minutes <- str_extract(duree, "[:digit:]+m") %>% str_extract("[:digit:]+") %>% as.integer()
duree <- nb_heures * 90 + nb_minutes 
    
summary(duree)  # Dur?e m?diane d'un film : 130 minutes
table_principal$duree <- recode(duree, "NA=130") # 512 films se voient attribuer arbitrairement une longueur de 130 minutes
    
# V?rif : renseign? partout et dur?e moyenne de 100 minutes
summary(table_principal$duree)
    

    

# E36 films de plus de 4 heures : principalement des longs documentaires ou films en plusieurs parties
table_principal[table_principal$duree >= 360, "titre"]
# 11 films de moins de 20 minutes : trucs bizarres
table_principal[table_principal$duree < 20, c("titre","type_film")]

# Exclusion de ces cas ? la marge
table_principal <- filter(table_principal, duree > 20 & duree < 360)       


# DISTRIBUTEUR 

# Recherche visuelle de la meilleure "coupure"
freq_distributeurs <- freq(table_principal$distributeur, 50, TRUE)
ggplot(freq_distributeurs, aes(x = var, y = cum)) + geom_bar(stat="identity") + coord_flip()
  # Choix d'une coupure aux 35 premiers distributeurs : 50% des distributeurs avec une valeur renseignée auront une de ces variables à 1

# Décomposition en 35 variables à rajouter
var_distributeur <- decompose_modalite("distributeur", "dist", 35, table_principal)
table_principal <- cbind(table_principal, var_distributeur )




# GENRE


# Nombre de films par genre (en cumulant les 5 variables)
freq_genre <- freq(rbind(table_principal$genre1, table_principal$genre2, table_principal$genre3, table_principal$genre4, table_principal$genre5), 50, FALSE)
ggplot(freq_genre, aes(x = var, y = percent)) + geom_bar(stat="identity", fill = "blue") + coord_flip()

# Quelques regroupements

  table_principal$genre1[table_principal$genre1 == "Péplum"] <- "Historique"
  table_principal$genre2[table_principal$genre2 == "Péplum"] <- "Historique"
  table_principal$genre3[table_principal$genre3 == "Péplum"] <- "Historique"
  table_principal$genre4[table_principal$genre4 == "Péplum"] <- "Historique"
  table_principal$genre5[table_principal$genre5 == "Péplum"] <- "Historique"
  
  table_principal$genre1[table_principal$genre1 == "Dessin animé"] <- "Animation"
  table_principal$genre2[table_principal$genre2 == "Dessin animé"] <- "Animation"
  table_principal$genre3[table_principal$genre3 == "Dessin animé"] <- "Animation"
  table_principal$genre4[table_principal$genre4 == "Dessin animé"] <- "Animation"
  table_principal$genre5[table_principal$genre5 == "Dessin animé"] <- "Animation"
  
  table_principal$genre1[table_principal$genre1 == "Concert"] <- "Musical"
  table_principal$genre2[table_principal$genre2 == "Concert"] <- "Musical"
  table_principal$genre3[table_principal$genre3 == "Concert"] <- "Musical"
  table_principal$genre4[table_principal$genre4 == "Concert"] <- "Musical"
  table_principal$genre5[table_principal$genre5 == "Concert"] <- "Musical"

  table_principal$genre1[table_principal$genre1 == "Opera"] <- "Musical"
  table_principal$genre2[table_principal$genre2 == "Opera"] <- "Musical"
  table_principal$genre3[table_principal$genre3 == "Opera"] <- "Musical"
  table_principal$genre4[table_principal$genre4 == "Opera"] <- "Musical"
  table_principal$genre5[table_principal$genre5 == "Opera"] <- "Musical" 
  
  table_principal$genre1[table_principal$genre1 == "Judiciaire"] <- "Policier"
  table_principal$genre2[table_principal$genre2 == "Judiciaire"] <- "Policier"
  table_principal$genre3[table_principal$genre3 == "Judiciaire"] <- "Policier"
  table_principal$genre4[table_principal$genre4 == "Judiciaire"] <- "Policier"
  table_principal$genre5[table_principal$genre5 == "Judiciaire"] <- "Policier" 
  
  table_principal$genre1[table_principal$genre1 == "Espionnage"] <- "Aventure"
  table_principal$genre2[table_principal$genre2 == "Espionnage"] <- "Aventure"
  table_principal$genre3[table_principal$genre3 == "Espionnage"] <- "Aventure"
  table_principal$genre4[table_principal$genre4 == "Espionnage"] <- "Aventure"
  table_principal$genre5[table_principal$genre5 == "Espionnage"] <- "Aventure" 
  
  
  # Décomposition en 21 variables à rajouter (+ de 100 films par genre)
  var_genre <- decompose_modalite(c("genre1","genre2","genre3","genre4","genre5"), "genre", 21, table_principal)
  table_principal <- cbind(table_principal, var_genre )

  
# NATIONALITES
  
  freq_nationalite <- freq(rbind(table_principal$nationalite1, table_principal$nationalite2, table_principal$nationalite3), 100, FALSE)
  
  # Recoupages géographiques
  table_principal$nationalite1[table_principal$nationalite1 == "ouest-allemand"] <- "allemand"
  table_principal$nationalite2[table_principal$nationalite2 == "ouest-allemand"] <- "allemand"
  table_principal$nationalite3[table_principal$nationalite3 == "ouest-allemand"] <- "allemand"
  
  table_principal$nationalite1[table_principal$nationalite1 == "est-allemand"] <- "allemand"
  table_principal$nationalite2[table_principal$nationalite2 == "est-allemand"] <- "allemand"
  table_principal$nationalite3[table_principal$nationalite3 == "est-allemand"] <- "allemand"
  
  table_principal$nationalite1[table_principal$nationalite1 == "argentin"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "argentin"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "argentin"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "brésilien"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "brésilien"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "brésilien"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "chilien"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "chilien"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "chilien"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "mexicain"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "mexicain"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "mexicain"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "uruguayen"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "uruguayen"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "uruguayen"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "colombien"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "colombien"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "colombien"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "paraguayen"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "paraguayen"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "paraguayen"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "bolivien"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "bolivien"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "bolivien"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "cubain"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "cubain"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "cubain"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "péruvien"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "péruvien"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "péruvien"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "vénézuélien"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "vénézuélien"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "vénézuélien"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 == "équatorien"] <- "sud-americain"
  table_principal$nationalite2[table_principal$nationalite2 == "équatorien"] <- "sud-americain"
  table_principal$nationalite3[table_principal$nationalite3 == "équatorien"] <- "sud-americain"
  
  table_principal$nationalite1[table_principal$nationalite1 %in% c("soviétique", "géorgien", "kazakh", "arménien")] <- "russe"
  table_principal$nationalite2[table_principal$nationalite2 %in% c("soviétique", "géorgien", "kazakh", "arménien")] <- "russe"
  table_principal$nationalite3[table_principal$nationalite3 %in% c("soviétique", "géorgien", "kazakh", "arménien")] <- "russe"
  
  table_principal$nationalite1[table_principal$nationalite1 == "danois"] <- "nordique"
  table_principal$nationalite2[table_principal$nationalite2 == "danois"] <- "nordique"
  table_principal$nationalite3[table_principal$nationalite3 == "danois"] <- "nordique" 
  
  table_principal$nationalite1[table_principal$nationalite1 == "finlandais"] <- "nordique"
  table_principal$nationalite2[table_principal$nationalite2 == "finlandais"] <- "nordique"
  table_principal$nationalite3[table_principal$nationalite3 == "finlandais"] <- "nordique" 
  
  table_principal$nationalite1[table_principal$nationalite1 == "suédois"] <- "nordique"
  table_principal$nationalite2[table_principal$nationalite2 == "suédois"] <- "nordique"
  table_principal$nationalite3[table_principal$nationalite3 == "suédois"] <- "nordique"
  
  table_principal$nationalite1[table_principal$nationalite1 == "norvégien"] <- "nordique"
  table_principal$nationalite2[table_principal$nationalite2 == "norvégien"] <- "nordique"
  table_principal$nationalite3[table_principal$nationalite3 == "norvégien"] <- "nordique"
  
  table_principal$nationalite1[table_principal$nationalite1 == "islandais"] <- "nordique"
  table_principal$nationalite2[table_principal$nationalite2 == "islandais"] <- "nordique"
  table_principal$nationalite3[table_principal$nationalite3 == "islandais"] <- "nordique"
  
  table_principal$nationalite1[table_principal$nationalite1 == "néo-zélandais"] <- "australien"
  table_principal$nationalite2[table_principal$nationalite2 == "néo-zélandais"] <- "australien"
  table_principal$nationalite3[table_principal$nationalite3 == "néo-zélandais"] <- "australien"
  
  table_principal$nationalite1[table_principal$nationalite1 == "Québecois"] <- "Canadien"
  table_principal$nationalite2[table_principal$nationalite2 == "Québecois"] <- "Canadien"
  table_principal$nationalite3[table_principal$nationalite3 == "Québecois"] <- "Canadien"
  
  
  table_principal$nationalite1[table_principal$nationalite1 == "hong-kongais"] <- "autre asiatique"
  table_principal$nationalite2[table_principal$nationalite2 == "hong-kongais"] <- "autre asiatique"
  table_principal$nationalite3[table_principal$nationalite3 == "hong-kongais"] <- "autre asiatique"
  
  table_principal$nationalite1[table_principal$nationalite1 == "sud-coréen"] <- "autre asiatique"
  table_principal$nationalite2[table_principal$nationalite2 == "sud-coréen"] <- "autre asiatique"
  table_principal$nationalite3[table_principal$nationalite3 == "sud-coréen"] <- "autre asiatique"
  
  table_principal$nationalite1[table_principal$nationalite1 == "taïwanais"] <- "autre asiatique"
  table_principal$nationalite2[table_principal$nationalite2 == "taïwanais"] <- "autre asiatique"
  table_principal$nationalite3[table_principal$nationalite3 == "taïwanais"] <- "autre asiatique"
  
  table_principal$nationalite1[table_principal$nationalite1 == "thaïlandais"] <- "autre asiatique"
  table_principal$nationalite2[table_principal$nationalite2 == "thaïlandais"] <- "autre asiatique"
  table_principal$nationalite3[table_principal$nationalite3 == "thaïlandais"] <- "autre asiatique"
  
  table_principal$nationalite1[table_principal$nationalite1 == "coréen"] <- "autre asiatique"
  table_principal$nationalite2[table_principal$nationalite2 == "coréen"] <- "autre asiatique"
  table_principal$nationalite3[table_principal$nationalite3 == "coréen"] <- "autre asiatique"

  table_principal$nationalite1[table_principal$nationalite1 == "vietnamien"] <- "autre asiatique"
  table_principal$nationalite2[table_principal$nationalite2 == "vietnamien"] <- "autre asiatique"
  table_principal$nationalite3[table_principal$nationalite3 == "vietnamien"] <- "autre asiatique"  
  
  table_principal$nationalite1[table_principal$nationalite1 == "cambodgien"] <- "autre asiatique"
  table_principal$nationalite2[table_principal$nationalite2 == "cambodgien"] <- "autre asiatique"
  table_principal$nationalite3[table_principal$nationalite3 == "cambodgien"] <- "autre asiatique"
  
  table_principal$nationalite1[table_principal$nationalite1 == "philippin"] <- "autre asiatique"
  table_principal$nationalite2[table_principal$nationalite2 == "philippin"] <- "autre asiatique"
  table_principal$nationalite3[table_principal$nationalite3 == "philippin"] <- "autre asiatique"
  
  table_principal$nationalite1[table_principal$nationalite1 == "bengali"] <- "autre asiatique"
  table_principal$nationalite2[table_principal$nationalite2 == "bengali"] <- "autre asiatique"
  table_principal$nationalite3[table_principal$nationalite3 == "bengali"] <- "autre asiatique"
  
  
  table_principal$nationalite1[table_principal$nationalite1 %in% c("kirghiz","afghan","sri-lankais","pakistanais","Irakien","indonésien", "guinéen", "singapourien", "népalais", "mongol")] <- "autre asiatique"
  table_principal$nationalite2[table_principal$nationalite2 %in% c("kirghiz","afghan","sri-lankais","pakistanais","Irakien","indonésien", "guinéen", "singapourien", "népalais", "mongol" )] <- "autre asiatique"
  table_principal$nationalite3[table_principal$nationalite3 %in% c("kirghiz","afghan","sri-lankais","pakistanais","Irakien","indonésien", "guinéen", "singapourien", "népalais", "mongol" )] <- "autre asiatique"
  
  
  table_principal$nationalite1[table_principal$nationalite1 %in% c("polonais","roumain","hongrois","tchèque", "tchécoslovaque", "bulgare", "serbe", "croate", "yougoslave", "bosniaque", "letton", "lituanien", "slovaque", "ukrainien", "albanais", "estonien", "slovène", "macédonien", "monténégrin")] <- "est-europeen"
  table_principal$nationalite2[table_principal$nationalite2 %in% c("polonais","roumain","hongrois","tchèque", "tchécoslovaque", "bulgare", "serbe", "croate", "yougoslave", "bosniaque", "letton", "lituanien", "slovaque", "ukrainien", "albanais", "estonien", "slovène", "macédonien", "monténégrin")] <- "est-europeen"
  table_principal$nationalite3[table_principal$nationalite3 %in% c("polonais","roumain","hongrois","tchèque", "tchécoslovaque", "bulgare", "serbe", "croate", "yougoslave", "bosniaque", "letton", "lituanien", "slovaque", "ukrainien", "albanais", "estonien", "slovène", "macédonien", "monténégrin")] <- "est-europeen"
  
  table_principal$nationalite1[table_principal$nationalite1 %in% c("autrichien","portugais", "néerlandais", "irlandais", "grec","luxembourgeois","maltais","chypriote","liechtensteinois")] <- "autre ouest-europeen"
  table_principal$nationalite2[table_principal$nationalite2 %in% c("autrichien","portugais", "néerlandais", "irlandais", "grec","luxembourgeois","maltais","chypriote","liechtensteinois")] <- "autre ouest-europeen"
  table_principal$nationalite3[table_principal$nationalite3 %in% c("autrichien","portugais", "néerlandais", "irlandais", "grec","luxembourgeois","maltais","chypriote","liechtensteinois")] <- "autre ouest-europeen"
  
  table_principal$nationalite1[table_principal$nationalite1 %in% c("turc","iranien","libanais","palestinien", "égyptien", "qatarien", "émirati", "kowetien", "Irakien", "syrien", "jordanien", "saoudien")] <- "oriental"
  table_principal$nationalite2[table_principal$nationalite2 %in% c("turc","iranien","libanais","palestinien", "égyptien", "qatarien", "émirati", "kowetien", "Irakien", "syrien", "jordanien", "saoudien")] <- "oriental"
  table_principal$nationalite3[table_principal$nationalite3 %in% c("turc","iranien","libanais","palestinien", "égyptien", "qatarien", "émirati", "kowetien", "Irakien", "syrien", "jordanien", "saoudien")] <- "oriental"
  
  
  
  table_principal$nationalite1[table_principal$nationalite1 %in% c("marocain", "algérien", "tunisien" )] <- "magreb"
  table_principal$nationalite2[table_principal$nationalite2 %in% c("marocain", "algérien", "tunisien" )] <- "magreb"
  table_principal$nationalite3[table_principal$nationalite3 %in% c("marocain", "algérien", "tunisien" )] <- "magreb"
  
   
  table_principal$nationalite1[table_principal$nationalite1 %in% c("sud-africain","sénégalais","camerounais", "béninois", "burkinabé", "malien", "zaïrois", "zambien", "rwandais", " ghanéen", "tchadien", "malgache", "ivoirien", "gabonais", "congolais", "mauritanien", "angolais", "éthiopien","mozambiquais", "nigérien", "tanzanien","zimbabwéen", "nigérien", "centrafricain","bissau-guinéen", "centrafricain", "ghanéen", "nigérian" )] <- "africain"
  table_principal$nationalite2[table_principal$nationalite2 %in% c("sud-africain","sénégalais","camerounais", "béninois", "burkinabé", "malien", "zaïrois", "zambien", "rwandais", " ghanéen", "tchadien", "malgache", "ivoirien", "gabonais", "congolais", "mauritanien", "angolais", "éthiopien","mozambiquais", "nigérien", "tanzanien","zimbabwéen", "nigérien", "centrafricain","bissau-guinéen", "centrafricain", "ghanéen", "nigérian"  )] <- "africain"
  table_principal$nationalite3[table_principal$nationalite3 %in% c("sud-africain","sénégalais","camerounais", "béninois", "burkinabé", "malien", "zaïrois", "zambien", "rwandais", " ghanéen", "tchadien", "malgache", "ivoirien", "gabonais", "congolais", "mauritanien", "angolais", "éthiopien","mozambiquais", "nigérien", "tanzanien","zimbabwéen", "nigérien", "centrafricain","bissau-guinéen", "centrafricain", "ghanéen", "nigérian"  )] <- "africain"
  
  # Décomposition en 23 variables à rajouter (+ de 100 films par zone geographique, jusqu'à l'Afrique)
  var_nationalite <- decompose_modalite(c("nationalite1", "nationalite2", "nationalite3"), "nationalite", 23, table_principal)
  table_principal <- cbind(table_principal, var_nationalite )
  
  sum(table_principal$nationalite_français)
  
# Variables A SUPPRIMER
    
table_principal <-  select(table_principal, -one_of(c("mois_sortie", # on a deja l'info dans date_sortie
                                  "date_sortie_old", # Ancienne valeur de date de sortie 
                                 "str_date", # variable interm?diaire
                                  "date_reprise", # On l'a recod? en Oui / non
                                  "titre_original", # Inutile
                                  "recompenses", # Recod? en prix et nominations
                                  "annee_production", # On a d?j? la date de sortie
                                  "date_sortie_dvd", # Variables connues ? posteriori
                                  "date_sortie_bluray", # idem
                                  "date_sortie_vod", # idem
                                  "secrets_tournage",  # Recod? plus proprement dans nb_anecdotes
                                  "budget", # Pas assez souvent renseign? et p?nible ? convertir
                                  "boxoffice_france", # Pas assez renseign?
                                  "format_production", # On s'en fout
                                  "format_audio", # On s'en fout
                                  "format_projection", # On s'en fout
                                  "langues",
                                  "num_visa", # recod? en multilingues oui non
                                  "genre1", "genre2", "genre3", "genre4", "genre5", # recodé
                                  "nationalite1", "nationalite2", "nationalite3", # recodé
                                  "distributeur", # recodé
                                 "X"
                            )))
    
    
    
# Fusion des tables principal et notes
table_notes <- rename(table_notes, id_film = id_film.x) 
table_final <- left_join(table_principal, table_notes, "id_film")

str(table_final)

# Notes moyennes par genre
group_by(table_final, genre1) %>% summarise(
                        nb_films = n(),
                        note_spectateurs = round(mean(note_spectateurs_moyenne, na.rm = TRUE),1), 
                        note_presse = round(mean(note_presse_moyenne, na.rm=TRUE), 1)
                        ) %>% arrange(note_spectateurs) %>%  as.data.frame()

# Fusion des tables principal et casting
table_final <- left_join(table_final, table_casting, "id_film")

dim(table_final)
str(table_final)


table(table_final$nationalite1)
summary(table_final$nationalite2)
summary(table_final)

