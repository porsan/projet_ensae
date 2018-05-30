

#install.packages("GGally")
#install.packages("plotly")
########## Librairies - options ##########

library(ggplot2)
library(stringr)
library(plotly)
library(dplyr)
library(reshape2)
library(GGally)

setwd("H:/Formation CEPE/Projet/")



########## Lecture donn?es ##########

# Repertoire de sauvergarde des donnees
data_dir <- "data/"
data_dir_casting <- "data/casting/"

table_spectateurs <- read.csv2(paste0(data_dir,"table_spectateurs.csv"))
table_presse <- read.csv2(paste0(data_dir,"table_presse.csv"))

########## Traitements sur table spectateurs ##########

View(table_spectateurs)

# V?rification dimensions
dim(table_spectateurs)

# V?rification variables et types
str(table_spectateurs)


# Convertir en dataframe dplyr
table_spectateurs <- tbl_df(table_spectateurs)  %>%  

  # Convertir la note spectateurs moyenne en num?rique
  mutate(note_spectateurs_moyenne = as.numeric(note_spectateurs_moyenne)) %>% 

  # Ajouter l'ann?e
    mutate(annee = as.integer(str_sub(table_spectateurs$id, 1, 4))) %>% 

  # Supprimer les ann?es sup?rieures ? 2017
    filter(annee <= 2017) %>% 

    # Supprimer les lignes ? NA
    filter(!is.na(note_spectateurs_moyenne)) 


# Converir note moyenne en num?rique
table_spectateurs$note_spectateurs_moyenne <- as.numeric(as.character(table_spectateurs$note_spectateurs_moyenne))
  
# Nombre d'entr?es par ann?es
table(table_spectateurs$annee)


# Les 10 films avec les plus fortes moyennes spectateurs
arrange(table_spectateurs, desc(note_spectateurs_moyenne)) %>% slice(1:10) %>% select(X, note_spectateurs_moyenne, annee)

# Les 10 films avec le plus de notes spectateurs
head(arrange(table_spectateurs, desc(nb_notes_spectateurs)) %>% slice(1:10) %>% select(X, nb_notes_spectateurs, annee), 10)


# V?rificiation de l'absence de doublons sur l'ID
sum(duplicated(table_spectateurs$id)) # Retourne bien 0
length(unique(table_spectateurs$id)) # Retourne bien 18 880 lignes


# Evolution du nombre de films par ann?e
evo_nbfilms <- ggplot(table_spectateurs, aes(x = annee)) + 
  geom_bar(fill = "blue", color = "white", size = 1) +
  labs(title = "Evolution du nombre de films diffus?s", 
       subtitle = "Par ann?e de diffusion en salles", 
       caption = "source : table_spectateurs - Allocin?",
       x = "Ann?e de diffusion", y = "Nombre de films" ) +
  theme(plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(face = "italic"))


# Nuage de points entre note  spectateur moyenne et nombre de notes spectateurs
corr_note_nbnotes <- ggplot(table_spectateurs, aes(x = note_spectateurs_moyenne, y = nb_notes_spectateurs)) + 
  geom_jitter(color="dark green") +
  labs(title = "Relations entre notes mpyennes spectateurs et nombre de notes spectateurs", 
       subtitle = "", 
       caption = "source : table_presse - Allocin?",
       x = "Note spectateurs moyenne", y = "Nombre de notes spectateurs" )

cor(table_spectateurs$note_spectateurs_moyenne, table_spectateurs$nb_notes_spectateurs, use="complete.obs")

# Distribution des notes spectateurs moyenne
ggplot(table_spectateurs, aes(x = nb_notes_spectateurs)) +
  geom_histogram(fill = "orange", color = "white", bins = 10) +
  labs(title = "Distribution du nombre de notes spectateurs", caption ="source : table_presse - Allocin?", x = "Nombre de notes spectateurs", y = "Fr?quence") +
  theme_void()

# Distribution du nombre de notes spectateurs
ggplot(table_spectateurs, aes(x = note_spectateurs_moyenne)) +
  geom_histogram(fill = "purple", color = "white", bins = 20) +
  labs(title = "Distribution des notes spectateurs moyennes", caption ="source : table_presse - Allocin?", x = "Note spectateur moyenne", y = "Fr?quence") +
  theme_void()



########## Traitements sur table presse ##########


# V?rification dimensions
dim(table_presse)

# V?rification variables et types
str(table_presse)

# V?rification structure de la note presse moyenne
table(table_presse$note_presse_moyenne)


# Convertir en dataframe dplyr
table_presse <- tbl_df(table_presse)  %>%  
  
  # Convertir la note presse moyenne en num?rique
  mutate(note_presse_moyenne = as.numeric(as.character(note_presse_moyenne))) %>% 
  
  # Ajouter l'ann?e
  mutate(annee = as.integer(str_sub(table_presse$id, 1, 4))) %>% 
  
  # Supprimer les ann?es sup?rieures ? 2017
  filter(annee <= 2017) %>% 
  
  # Supprimer les lignes ? NA
  filter(!is.na(note_presse_moyenne)) 


# Les 10 films avec les plus fortes moyennes presse
arrange(table_presse, desc(note_presse_moyenne)) %>% slice(1:10) %>% select(X, note_presse_moyenne, annee)


# convertir toutes les variables "notes" en num?rique (solution compliqu?e...)

# Fonction convertissant un vecteur de type factor et avec "note" dans son nom de variable, en num?rique
# convert_note <- function(var) {
#   if (str_detect(as.character(deparse(substitute(var))),"note") & class(var)=="factor") {
#     return(as.numeric(as.character(var)))
#   } else return(var)
# }


# Liste des variables de la table ? convertir en num?rique
liste_var_to_num <- names(table_presse)[str_detect(names(table_presse),"note")]

var_converties <- as.data.frame(apply(table_presse[,liste_var_to_num], 2, function(x) {as.numeric(as.character(x))}))

table_presse <- cbind(select(table_presse, -one_of(liste_var_to_num)), var_converties)



# convertir toutes les variables "presse" en caract?res (toujours aussi compliqu?...)

# Liste des variables de la table ? convertir en character
liste_var_to_char <- names(table_presse)[!str_detect(names(table_presse),"note") & str_detect(names(table_presse),"presse")]

var_converties2 <- as.data.frame(apply(table_presse[,liste_var_to_char], 2, function(x) {as.character(x)}), stringsAsFactors = FALSE)

table_presse <- cbind(select(table_presse, -one_of(liste_var_to_char)), var_converties2)


##### Compter le nombre total de critiques presse par journal


# Liste des variables de la table ? convertir en num?rique ('presse' sans "note")
liste_var <- names(table_presse)[str_detect(names(table_presse),"presse") & !str_detect(names(table_presse), "note")]

# Stocker le nombre de critiques par journal pour chacune des variables presse
critiques_presse <- lapply(table_presse[,liste_var], table)

# Convertir en dataframe les r?sultats de chaque ?l?ment de la liste
critiques_presse <- lapply(critiques_presse, as.data.frame)

# Parcourir chaque ?l?ment de la liste et le stocker dans un dataframe initialement vide
critiques_total <- critiques_presse[[1]][critiques_presse[[1]]$Var1=="teererr",] 
for(i in 1:length(critiques_presse)) {
  print(critiques_presse[[i]])
  critiques_total <- rbind(critiques_total, critiques_presse[[i]])
}

# Compter le nombre d'occurence de chaque titre de presse
#nb_journaux <- unlist(tapply(critiques_total$Freq, critiques_total$Var1, sum)) # Cr?e une liste je sais pas pourquoi
nb_journaux <- aggregate(Freq~Var1, critiques_total, sum)

# Trier par ordre d?croissant et prendre les 10 premiers
nb_journaux <- nb_journaux[order(desc(nb_journaux$Freq))[1:20],]
str(nb_journaux)
nb_journaux$Var1 <- factor(nb_journaux$Var1, nb_journaux$Var1) # Pour que le graphique s'affiche dans l'ordre...

# Visualisation des 10 critiques presses les plus r?pandues
nb_critiques_partitre <- ggplot(nb_journaux, aes(x=Var1, y = Freq, fill = Freq)) + 
  theme_minimal() +
  theme( axis.text.x = element_text(size = 10),legend.position="null",
         plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
         plot.subtitle = element_text(size = 10),
         plot.caption = element_text(face = "italic")) + coord_flip() +
  geom_bar(stat="identity", color = "white") +
  geom_text(aes(label = Freq), hjust = 1, color = "white", size = 3) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  geom_vline(aes(xintercept = 8.5), linetype=2) +
  labs(x ="", y = "Nombre de critiques", title = "Titres de presse les plus r?pandus", subtitle = "... et nombre de critiques recens?es")



##### Ne conserver que les notes des 8 critiques presses les plus r?pandues

# Vecteur des titres de presse ? garder
titres_presse <- as.character(nb_journaux[1:8, "Var1"])

# Nouvelles colonnes de la table presse ? vide pour l'instant : notes par critique
new_var <- matrix(NA, NROW(table_presse), length(titres_presse))
colnames(new_var) = titres_presse
table_presse <- cbind(table_presse, new_var)


# Liste des variables de la table presse ? r?cup?rer dans les colonnes correspondantes ('presse' sans "note")
liste_var <- names(table_presse)[str_detect(names(table_presse),"presse") & !str_detect(names(table_presse), "note")]

# Algorithme pour affecter les notes presse dans la bonne case
for(i in 1:NROW(table_presse)) { # Parcourir les lignes de la table
#for(i in 1:1) { 
  for(j in liste_var) {# Parcourir les colonnes de la table
    for(k in 1:length(titres_presse)) {# Parcourir le vecteur des titres de presse
      if (!is.na(table_presse[i, j])) {
        if(table_presse[i, j] == titres_presse[k]) {
          table_presse[i,as.character(titres_presse[k])] <- as.character(table_presse[i, paste0(j,"_note")])
          #print(paste0(titres_presse[k], "_", table_presse[i,paste0(j,"_note")], "_",as.character(titres_presse[k])))
        }
      }
    }
  }
  print(i)
}

# Liste des variables presse initiales ? supprimer (variables "presse" except? la note moyenne)
liste_var_to_delete <- names(table_presse)[str_detect(names(table_presse),"presse") & !str_detect(names(table_presse),"moyenne")]

# Supprimer les variables correspondantes
table_presse2 <- select(table_presse, -one_of(liste_var_to_delete))

# Renommer les variables presse sp?cifique avec des noms sans cacart?res sp?ciaux
nouveaux_noms <- c("telerama", "lemonde", "premiere", "inrock", "lexpress", "positif", "liberation", "telecineobs")
names(table_presse2) <- c(names(select(table_presse2, -one_of(titres_presse))), nouveaux_noms)

# Transformation des notes presses obtenues en format num?rique
table_presse2 <- cbind(select(table_presse2, -one_of(nouveaux_noms)), as.data.frame(lapply(table_presse2[, nouveaux_noms], as.numeric)))

# V?rification variables table presse 2
str(table_presse2)



########## Fusion tables presse et spectateurs ##########

# On enl?ve les variables dupliqu?es dans les deux tables
table_presse2 <- select(table_presse2, -one_of("url_film", "X", "url_film", "annee"))

# Fusion des deux tables
table_notes <- full_join(table_spectateurs, table_presse2, by = "id")

str(table_notes)

# Corr?lation entre note presse et note spectateurs
cor(table_notes$note_presse_moyenne, table_notes$note_spectateurs_moyenne, use = "pairwise.complete.obs")

# Nuage de points entre note presse et note spectateurs
corr_presse_spectateur <- ggplot(table_notes, aes(x = note_spectateurs_moyenne, y = note_presse_moyenne)) + 
  geom_jitter(color = "dark red") +
  labs(title = "Relations entre note presse et note spectateurs", 
       subtitle = "", 
       caption = "source : table_notes - Allocin?",
       x = "Note spectateurs moyenne", y = "Note critiques moyenne") +
      theme(plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(face = "italic"))

# Sauvegarde de la table finale en csv
write.csv2(table_notes, "table_notes.csv")


# ... pour la recharger
table_notes <- read.csv2(paste0(data_dir,"table_notes.csv"))



# Visualisation de l'?volution de la note moyenne spectateurs par ann?e

data<- melt(table_notes, id = "annee", measure.vars = c("note_spectateurs_moyenne", "note_presse_moyenne"))

    evos_notes <- ggplot(data) + 
       geom_line(aes(x = annee, y = value, color = variable),  size = 1.5, stat = "summary", fun.y = "mean") +
       labs(title = "Evolution des notes moyennes presse et spectateurs", 
            subtitle = "Par ann?e de diffusion en salles", 
            caption = "source : table_notes - Allocin?",
            x = "Ann?e de diffusion", y = "Note spectateurs moyenne", color = "notes" ) + 
      coord_cartesian(ylim = c(0,5)) +
       theme_minimal() +
       theme(plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
            plot.subtitle = element_text(size = 10),
            plot.caption = element_text(face = "italic"))
as.data.frame(table_notes %>% group_by(annee) %>% summarise(avg = mean(note_spectateurs_moyenne)))


# Corr?lations des notes entre les diff?rents titres de presse
corr_titres_presse <- ggcorr(table_notes[, nouveaux_noms])  + 
labs(title = "correlations des notes observ?es entre les diff?rents journaux", 
      subtitle = "Entre les 8 titres de presse les plus r?pandus") +
  theme(plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(face = "italic"))


# Comparaison des notes moyennes pour chacun des 8 titres de presse choisis
moyennes_par_titre <- as.data.frame(lapply(table_notes[, nouveaux_noms], mean, na.rm = TRUE))


# Reshape et Recodage pour ?liminer les notes avec d?cimales (+ suppressionn des 0 trop rares)
data <- melt(table_notes, id = "id", measure.vars = nouveaux_noms) 
data <- data[!is.na(data$value),]
data$value <- ifelse(data$value == 0, 1,
        ifelse(data$value == 0.5, 1,
        ifelse(data$value ==2.5, 2,
        ifelse(data$value ==3.5, 3, data$value))))

# Grouper les valeurs par couple titre / note
data <- data %>% group_by(variable, value) %>% summarise(n = n()) %>% mutate(percent = n / sum(n) * 100) %>% as.data.frame()
data$value <- factor(data$value)
levels(data$value) <- order(levels(data$value), decreasing = TRUE)
head(data)

# Visualisation des distributions des notes presses choisis
  distrib_notes_presse <- ggplot(data, aes(x = variable, y = percent, fill = value)) + 
  geom_bar(stat="identity", position = position_stack(vjust=0.5), width = 0.8) +   
  geom_text(aes(label = paste0(round(percent, 0),"%"), vjust = 1), position = position_stack(vjust=0.5), fontface ="bold", size = 4) +
  labs(title = "Notations des principaux titres de presse",
       subtitle = "Distributions des notes sur 5 sur tous les films recens?s",
       caption = "source : table_notes, allocine", x="", y = "") +
    scale_fill_brewer(name="Notes",palette ="RdYlGn", direction = -1) +
  theme_minimal() +
     theme(plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
           plot.subtitle = element_text(size = 10),
           plot.caption = element_text(face = "italic"), 
           axis.text.x = element_text(size = 10, face = "bold")) 

  
  
  # Distribution des notes spectateurs vs notes presse
  # ON ne conserve que les donn?es sans NA ni ? presse ni ? spectateur
  data <- table_notes[!is.na(table_notes$note_presse_moyenne) & !is.na(table_notes$note_spectateurs_moyenne),]
  # reshape
  data <- melt(data, id = "id", measure.vars = c("note_spectateurs_moyenne", "note_presse_moyenne")) 

  
  # Visualisation des histogrames
  distrib_presse_vs_spectateurs<- ggplot(data, aes(x = value, fill = variable)) +
    geom_histogram( color = "white", bins = 20) +
    facet_grid(.~variable) +
    labs(title = "Distribution des notes Presse vs notes spectateurs", caption ="source : table_notes - Allocin?", x = "Note moyenne", y = "Fr?quence") +
    theme_minimal() +
  theme(plot.title = element_text(color = "dark blue", size = 15, face = "bold"), 
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(face = "italic"), 
        axis.text.x = element_text(size = 10, face = "bold")) 
  
  # Visualisations finales
  print(evo_nbfilms)
  print(nb_critiques_partitre)
  print(corr_presse_spectateur)
  print(evos_notes)
  print(distrib_notes_presse)
  print(distrib_presse_vs_spectateurs)
  print(corr_titres_presse)
  
  summary(table_notes)
   