##########################################
#          Librairies - options          #
##########################################



# Visualisation
library(ggplot2)
library(plotly)
library(ggthemes)

# Traitement de données
library(reshape2)
library(stringr)
library(dplyr)



str(final)


# Nombre de films et notes moyennes si / sans reprise
stats_reprise <- group_by(final,reprise) %>% summarise(nb_films = n(),
                                      note_presse = mean(note_presse_moyenne, na.rm = TRUE),
                                      note_spectateurs = mean(note_spectateurs_moyenne, na.rm = TRUE))

# Nombre de films et notes moyennes en fonction des saisons
stats_saison_sortie <- group_by(final,saison_sortie) %>% summarise(nb_films = n(),
                                                       note_presse = mean(note_presse_moyenne, na.rm = TRUE),
                                                       note_spectateurs = mean(note_spectateurs_moyenne, na.rm = TRUE))

# Nombre de films et notes moyennes en fonction des saisons
group_by(final, nationalite_français) %>% summarise(nb_films = n(),
                                                                   note_presse = mean(note_presse_moyenne, na.rm = TRUE),
                                                                   note_spectateurs = mean(note_spectateurs_moyenne, na.rm = TRUE))

# Nombre de films et notes moyennes en fonction du type de film
group_by(final, genre_Comédie) %>% summarise(nb_films = n(),
                                                    note_presse = mean(note_presse_moyenne, na.rm = TRUE),
                                                    note_spectateurs = mean(note_spectateurs_moyenne, na.rm = TRUE))




##########################################
#            Test cartographie           #
##########################################


setwd("H:/Formation CEPE/Projet")
table_principal <- read.csv2("table_principal.csv")


decompose_modalite <- function(col_a_traiter,nom_col,nb,table_a_traiter) {
  # Construction de la liste des modalites en supprimant les valeurs vides et NA
  modalites <- unlist(lapply(col_a_traiter, function(x) { table_a_traiter[,x] }))
  modalites <- modalites[which(modalites != "")]
  modalites <- modalites[which(!is.na(modalites))]
  # Comptage de chaque modalit? dans une table et tri par ordre decroissant
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

var_nationalite <- decompose_modalite(c("nationalite1", "nationalite2", "nationalite3"), "nationalite", 100, table_principal)

data <- melt(var_nationalite)
str(data)


# Renommer la nationalit?
data$variable <- str_replace(data$variable, "nationalite_","")

# Recoder toutes les nationalit?s en pays....
data$variable <- str_replace(data$variable, "fran?ais","France") %>% 
                 str_replace("am?ricain","USA") %>% 
                 str_replace("britannique","Royaume-Uni") %>% 
                 str_replace("italien","Italy") %>% 
                 str_replace("allemand","Allemagne") 


  
# Nb films par nationalit?
nb_films_pays <- data %>% group_by(variable) %>% summarize( n = sum(value)) %>% arrange(desc(n)) %>% slice(1:5)
nb_films_pays$region <- nb_films_pays$variable


# Carte du monde sans data

map <- map_data("world")
head(map)
table(map$region)

ggplot(map, aes(long, lat, group = group)) + geom_polygon(fill = "blue") +
  labs(title = "Nombre de films par nationalite")

# Avec les data
map2 <- merge(map, nb_films_pays, by = "region", all.x = TRUE)

ggplot(map2, aes(long, lat, group = group, fill = n)) + geom_polygon() +
  labs(title = "Nombre de films par nationalite")

head(map2)
str(map2)
map2[map2$region=="France",]
# geocodage (r?cup?ration latitude et longitude) avec API google
#geocodage <- geocode(nb_films_pays$variable)
#nb_films_pays$lon <- geocodage[,1]
#nb_films_pays$lat <- geocodage[,2]


# Exemple Bordeaux
qmap(location = "Bordeaux") 
table(data$variable)





# Test Heatmap

ggplot(df_heatmap, aes(patient, genes )) +
  geom_tile(aes(fill = expression_level), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylab("List of genes ") +
  xlab("List of patients") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Expression level")



# Nombre de films et notes moyennes en fonction du type de film et des années
#

cat.year <- ksdata %>%
  filter(!year(launched) %in% c("1970", "2018")) %>%
  group_by(main_category, year=year(launched)) %>%
  summarize(count=n())

cat.year2 <- t(matrix(cat.year$count, nrow=9))
colnames(cat.year2) <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
rownames(cat.year2) <- levels(ksdata$main_category)

heatmap.2(cat.year2, dendrogram="row", Colv=F, trace="none", margins=c(10,10))


