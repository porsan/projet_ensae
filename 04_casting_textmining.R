########## Librairies - options ##########

library(dplyr)
library(stringr)

options(stringsAsFactors=F) # Pour importer les donnees quali en charactere et pas facteurs

########## Source ##########
# Permet de charger les fonctions definies dans le fichier

source("02_casting_functions.R")

########## Programme ##########

# Repertoire de sauvergarde des donnees
data_dir <- "data/"
data_dir_casting <- "data/casting/"

# Chargement des donnees
data <- construit_table_final(data_dir)

# Affiche la premiere du tableau pour controler le chargement
data[1,]

# Compte les differentes occurences du champ genre1 
table(data$genre1)
# Calcule le pourcentage des differentes occurences du champ genre1 
table(data$genre1)/sum(table(data$genre1))
# Conversion en caractere des variables titre et synopsis
data$titre <- sapply(data$titre, as.character)
data$synopsis <- sapply(data$synopsis, as.character)
# Creation de la variable txt comme concatenation des variables titre et synopsis
data$txt <- sapply(1:nrow(data),function(i) paste(data[i,c('titre','synopsis')],collapse=' '))

library(tm)

#require(devtools) #decommenter pour installer wordcloud2
#install_github("lchiffon/wordcloud2")
library(wordcloud2)

# Preparation des données pour le nuage de mots
words <- paste(data[,'txt'],collapse=' ') # on concatene tout le texte 
wordsFreq <- data.frame(sort(table(strsplit(words,"\\s+")),decreasing=TRUE)) # on compte chaque mot, le motif entre guillemet veut dire qu'on coupe la chaine de caractère quelque soit le nombre d'espaces entre les mots
head(wordsFreq)

# Affichage du nuage de mots
wordcloud2(data = wordsFreq[1:500,],minSize = 5, size = 3)

# Traitement du texte
# Pour normaliser le texte on va utiliser le package tm qui automatise une bonne partie des traitements les plus fréquents, on va opérer les transformations classiques suivantes :
#   
# on retire les accents
# on retire la ponctuation
# on réduit la casse afin que les mots avec majuscule ne se distinguent pas de leurs homologues sans majuscule
# on retire les mots dits “mots outils” (ou stopwords), qui n’apportent rien car ils sont presque toujours présents dans tous les documents
# on racinise (éventuellement, cela permet de ramener des mots ayant la même racine à une forme commune, par exemple “économie”, “économiste” deviendrait “économ”)
# on retire les espaces en trop

# Pour cela on commence par transformer le texte en corpus.
#library(tm)
documents <- Corpus(VectorSource(data$txt))
documents[1]$content

getTransformations() #transformations disponibles

# On peut ensuite appliquer les différents traitements successivement.
# Important, sur corpus en français, en général on retire les accents qui pourraient poser des problèmes par la suite

library(stringi)
library(stringr)

accent <- function(x) stri_trans_general(x, "Latin-ASCII") # cela signifie qu'on remplace un caractère encodé en Latin1 par son équivalent le plus proche en ASCII, il n'y a par exemple pas de caractères accentués en ASCII
documents <- tm_map(documents, content_transformer(accent))
documents[1]$content

# On retire la ponctuation et les nombres, ce qui revient à ne garder que ce qui est une lettre, pour cela on fait une opération de remplacement en s’appuyant sur les expressions régulières qui sont une manière de décrire des “motifs”,
# ici [^a-z] signifie : tout ce qui n’est pas du texte (fonctionne ici car on a retiré les accents, si on veut conserver les accents on peut utiliser la fonction removePunctuation de tm mais elle n’est pas exhaustive, elle ne traite pas l’apostrophe par exemple).

documents <- tm_map(documents, content_transformer(gsub), pattern = "[^a-zA-Z]", replacement = " ")
documents[1]$content

# On peut réduire la casse pour avoir une casse harmonisée.(Rq : cette étape et la précédente peuvent être interverties, il faut simplement le prendre en considération dans l’expression régulière précédente)

documents <- tm_map(documents, content_transformer(tolower))
documents[1]$content

# A présent on va retirer les mots outils, la plupart des librairies possèdent des listes pré construites… Comme on a enlevé les accents avant on enlève les accents des stopword (ne pas le faire si on a décidé de conserver les accents).

stopwords_fr <- sapply(stopwords("french"),accent)
stopwords_fr

documents <- tm_map(documents, removeWords, stopwords_fr)
documents[1]$content

# Mais on peut vouloir personnaliser la liste, pour les articles de journaux par exemple, on va rajouter les indicateurs de temps qui n’apportent pas d’information dans notre cas d’usage.

# stopwords_fr = c(stopwords_fr,'a','h','lundi','mardi','mercredi','jeudi','vendredi','samedi','dimanche','etre','apres','selon','comme','alors','tout','tous','faire','depuis','encore')
# stopwords_fr = setdiff(stopwords_fr, c("pas")) # 'pas' est inclus dans les stopword, on trouve que c'est un peu dommage alors on le retire de la liste
# documents <- tm_map(documents, removeWords, stopwords_fr)
# documents[1]$content

# Enfin, on peut souhaiter raciniser, on utilise pour cela la librairie SnowballC qui implémente l’algorithme de Porter.

library('SnowballC')
documents_nonstem <- documents
documents <- tm_map(documents, stemDocument, "french")
documents[1]$content

#remarque le stemming fait perdre la lisibilite, on peut la retrouver avec stemCompletion 
stemCompletion('ambiti',dictionary=documents_nonstem)

# Reste à nettoyer tous les espaces rajoutés par les opérations précédentes (il n’y en a pas forcément mais ça permet de s’en assurer).
documents <- tm_map(documents, stripWhitespace) #n'enleve pas le tout premier espace
documents <- tm_map(documents, content_transformer(gsub), pattern = "^\\s+", replacement = "")
documents[1]$content

# On peut désormais vectoriser le texte, ie construire la matrice pour laquelle chaque ligne correspond à une article et chaque colonne à un mot de vocabulaire (le vocabulaire étant le nombre total de mots distincts utilisés au moins une fois dans le corpus). Les coordonnées i,j de la matrice valent 0 si le mot j n’est pas dans le document i ou son occurrence sinon. Cette objet est facilement manipulable par la suite

dtm <- DocumentTermMatrix(documents)
dim(dtm)

minfreq <- findFreqTerms(dtm, 30) # minfreq réduit le vocabulaire, au lieu de prendre les mots apparaissant au moins une fois, on prend le nombre de mots apparaissant au moins 30 fois afin de limiter un peu le nombre de colonnes avant meme de calculer la matrice. 30 est choisi arbitrairement, mais rappelez vous que le vocabulaire représente des dizaines de milliers de mots sur des centaines de milliers d'articles, 30 semble donc légitimement faible.
dtm <- DocumentTermMatrix(documents, control=list(dictionary = minfreq, weight=weightTfIdf)) #ici on choisit tfidf car les documents sont longs mais ce n'est pas sur ce critère qu'on réduit la dimension. On pourrait le faire mais pour cela il faudrait calculer la somme par colonne des pondérations tf idf dans dtm, puis retirer les colonnes pour lesquelles cette somme est inférieure à un certain seuil. On peut aussi réduire la dimension après avec la fonction removeSparseTerms de tm (un exemple est donné plus loin)
dim(dtm)

categories <- levels(as.factor(data$genre1))
categories <- categories[c(-7,-22)]
dtm_m <- as.matrix(dtm)
for (i in categories){
  m <- dtm_m[data$genre1==i,]
  wordsFreq <- sort(colSums(m),decreasing = TRUE)
  wordsFreq <- data.frame(word = names(wordsFreq), Freq = as.vector(wordsFreq)) # mise au format pour wordcloud2
  print(wordcloud2(data = wordsFreq[1:500,],minSize = 1, size = 3))
}

# On peut également faire toutes sortes de statistiques basiques comme regarder la distribution du nombre de mots dans les articles :

counts <- rowSums(dtm_m)
hist(counts,breaks=50)

summary(counts)

# Ou encore étudier les similarités entre mot (et donc le contexte dans lequel un mot est employé)

findAssocs(dtm, terms = "men", corlimit = 0.3)

# Finalement, il faut aussi noter, qu’en nettoyant le texte, quelque part on a retiré plein de mots inutiles et ainsi réduit la dimension, mais parfois au contraire on va l’augmenter en incorporant les combinaisons de mots adjacents.

library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) #pour garder les unigrammes on peut changer min en 1
tdm_b <- TermDocumentMatrix(VCorpus(VectorSource(documents$content)), control = list(tokenize = BigramTokenizer)) #attention en raison d'une incompatibilite de NgramTokenizer avec la nouvelle version de tm pour la fonction Corpus mais pas VCorpus on convertit l'un dans l'autre juste pour cette opération
dim(tdm_b)

m <- as.matrix(tdm_b)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(freq = v)
head(d)

# Orthographe
# Ici on a la chance d’avoir un corpus sans faute. Si ce n’était pas le cas, on pourrait tenter de corriger le corpus à l’aide d’un dictionnaire. La librairie hunspell fait le lien avec un dictionnaire installé sur l’ordinateur. Il faut également préciser un dictionnaire de langue française. ATTENTION au chemin.

library(hunspell)
words <- c("parfois", "ortographe")
hunspell_check(words) # par défaut c'est un dictionnaire anglais

bad_words <- hunspell_find("il est parfois interresant de verifier l'orthographe", dict = '/Volumes/Data2/Datasets/fr_FR.dic')
print(bad_words) #là on a préciser le chemin vers les fichiers du dictionnaire francais donc ils trouvent les mots mal orthographiés, rq il prend en compte les accents

# Classification supervisée (binaire)
# On rappelle qu’on cherche à distinguer les articles traitant d’économie de tous les autres, on construit donc une nouvelle variable cible binaire :
#   

data$genrebin <- 'autre'
data$genrebin[data$genre1 == 'Drame'] <- 'drame'
data$genrebin <- as.factor(data$genrebin)
table(data$genrebin)/sum(table(data$genrebin))

head(data$genrebin)
levels(data$genrebin)

data$genrebin <- relevel(data$genrebin, "drame")
head(data$genrebin)
levels(data$genrebin)

# Pour tout le protocole de machine learning on utilise la librairie caret qui fournit les fonctions pour tout le workflow : construire les échantillons, le prétraitement, les différentes options de validations croisées et un très grand nombre de modèles (https://topepo.github.io/caret/available-models.html). Sur cette page on peut aussi voir si les méthodes sont disponibles pour la régression et/ou la classification. Un gros effort de standardisation a été fait, avec un grand nombre de fonctions et options communes mais attention toutefois aux petites variantes.
library(caret)

# Train/test sets
# On commence donc par créer un échantillon d’apprentissage et de test stratifié (80% train, 20% test). On rappelle que cela sert à vérifier que l’algorithme a de bonnes propriétés de généralisation. S’il sait très bien prédire la catégorie des articles sur lesquels il est estimé, c’est peut-être qu’il est “spécialisé” de ces articles et n’arrivera pas forcément à prédire correctement un article trop différent. Ce qu’on veut c’est qu’il capte les grandes tendances différenciant dans le texte “économie” du reste pour pouvoir classer correctement une majorité d’article. En mettant de côté une partie des articles, on se réserve la possibilité de tester sa capacité à généraliser sur des articles qu’il n’a “jamais vus”
set.seed(1234)
splitIndex <- createDataPartition(data$genrebin, p = .80, list = FALSE, times = 1)
data.train <- data.frame(dtm_m[ splitIndex,])
data.test  <- data.frame(dtm_m[-splitIndex,])

# On vérifie les proportions de la cible dans les deux sous échantillons

ytrain <- data$genrebin[splitIndex]
ytest  <- data$genrebin[-splitIndex]
table(ytrain)/sum(table(ytrain))
table(ytest)/sum(table(ytest))

# Calcul parallélisé
# Pour permettre la sollicitation des différents coeurs lorsque c’est possible (au minimum pour les opérations de validation croisée) on fait d’abord les opérations suivantes. Les algorithmes de machine learning peuvent être assez consommateurs en capacité de calcul.

library(parallel)
library(doParallel)

# nombre de coeurs sur la machine
detectCores()

cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

# Apprentissage et tests des algorithmes
seeds <- list(1:4,1:4,1:4,1:4,1:4,5)
objControl <- trainControl(method='cv', number=5, returnResamp='none', classProbs = TRUE, summaryFunction=twoClassSummary, allowParallel = TRUE, seeds = seeds)

# CART
# Pour l’algorithme CART qui est un simple arbre de décision, on considère la méthode rpart. En fonction des modèles, ce ne seront pas les mêmes hyperparametres donc il faudra personnaliser la grille. Les paramètres disponibles pour le tuning (ou calibrage) sont disponibles ici http://topepo.github.io/caret/available-models.html
# Ici cp est un critère de complexite lié à l’élagage, cp est en quelque sorte le coût de réaliser une nouvelle division qu’il faut compenser en gain de performance.

gridsearch <- expand.grid(cp=seq(0, 0.1, 0.025)) #
tune <- train(data.train,ytrain,method = "rpart",tuneGrid=gridsearch, trControl =objControl,metric='ROC')
tune

plot(tune) # attention on cherche ici à maximiser
tune$bestTune

pred <- predict(object=tune$finalModel, data.test,type='class')
head(pred)

conf.mat <- confusionMatrix(pred, ytest)

# Manifestement l’élagage fait perdre en performance.
# On aime bien représenter graphiquement la matrice de confusion, mais il ne semble pas y avoir de fonction dédiée dans caret, on crée une fonction à cet effet qui semble un peu complexe mais qui a juste la vocation de restituer les matrices de confusion de manière plus esthétique, attention on choisit de restituer des pourcentages (si vous voulez le nombre d’observations, modifier en conséquence la ligne 4) :
library(reshape2)
library(ggplot2)
cm.plot <- function(table_cm){
  tablecm <- round(t(t(table_cm) / colSums(as.matrix(table_cm))*100)) # crée les pourcentages
  tablemelt <- melt(tablecm)
  ggplot(tablemelt, aes(Reference, Prediction)) +
    geom_point(aes(size = value, color=value), alpha=0.8, show.legend=FALSE) +
    geom_text(aes(label = value), color="white") +
    scale_size(range = c(5,25)) +
    scale_y_discrete(limits = rev(levels(tablemelt$Prediction)))+
    theme_bw()
}

# On bon modèle présentera des coefficients élevés sur la diagonale et faibles ailleurs. Ici on a normalisé pour faciliter la lecture, du coup les coefficients sont les proportions de la classe X prédites comme Y.
cm.plot(conf.mat$table)

# On peut explorer les résultats très détaillés à partir de cette matrice :
conf.mat$overall
conf.mat$byClass

# On peut également afficher les variables qui ont le plus contribué à la construction de l’arbre.
imp <- varImp(tune$finalModel)
impdf <- data.frame(names = row.names(imp), imp = imp[,1])
impdf <- impdf[order(impdf$imp, decreasing = TRUE),]
names(impdf)[2]<-colnames(imp)[1]
impdf[1:30,]

# On peut représenter l’arbre
library(rpart.plot) 
rpart.plot(tune$finalModel)
# On peut tracer la courbe ROC puisqu’on a produit les prédictions au format de probabilité et on peut faire varier le seuil d’acceptation d’un article comme étant lié à l’économie (les classes précédantes étaient obtenues pour un seuil fixé à 0.5 par défaut).
library(pROC)
pred.rpart <- predict(object=tune$finalModel, data.test,type='prob')
rocCurve.rpart   <- roc(response = ytest, predictor = pred.rpart[, "drame"], levels = rev(levels(ytest)))
plot(rocCurve.rpart, print.thres = "best")
# On peut calculer l’aire sous la courbe qui est une mesure de la performance du modèle hors échantillon. Par la suite on va comparer cette métrique entre les différents modèles testés.
rocCurve.rpart$auc

# Forêts aléatoires
# Pour les forêts aléatoires on peut jouer sur mtry si on le souhaite, le nombre de variables sélectionnées comme candidates potentielles pour chaque embranchement.
# La méthode rf testée ici est la version proche de CART.

gridsearch <- expand.grid(mtry = seq(30,200,50))
tune <- train(data.train,ytrain,method = "rf",tuneGrid=gridsearch, trControl =objControl,metric='ROC')
tune
plot(tune)
tune$bestTune

pred <- predict(object=tune$finalModel, data.test,type='class')
conf.mat <- confusionMatrix(pred, ytest)
cm.plot(conf.mat$table)

conf.mat$overall
conf.mat$byClass

imp <- varImp(tune$finalModel)
impdf <- data.frame(names = row.names(imp), imp = imp[,1])
impdf <- impdf[order(impdf$imp, decreasing = TRUE),]
names(impdf)[2]<-colnames(imp)[1]
impdf[1:30,]

pred.rf <- predict(object=tune$finalModel, data.test,type='prob')
rocCurve.rf <- roc(response = ytest, predictor = pred.rf[, "drame"])
rocCurve.rf$auc

# Régression logistique
# Difficile de ne pas tester une régression logistique comme benchmark ! En grande dimension, on la pénalise. La vignette pour rappel de l’utilisation de glmnet https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html, alpha = 1 caractériste le lasso (sinon c’est une pénalité elasticnet combinant lasso et ridge, ie une combinaison de la pénalité fondée sur la norme L1 et celle fondée sur la norme L2). Lambda mesure l’importance de la pénalité. On rappelle que l’idée est que plus on pénalise et plus on force des coefficients à s’annuler et donc plus on obtient un modèle parcimonieux et a priori plus robuste (moins sensible au surapprentissage).
# Attention pour glmnet, les données doivent être au format matriciel, et le predict prend directement en argument train

gridsearch <- expand.grid(alpha=c(0, .5, 1), lambda=c(.1, 1, 10))
tune <- train(as.matrix(data.train),ytrain,method = "glmnet",tuneGrid=gridsearch, family='binomial', trControl =objControl,metric='ROC')
tune

plot(tune)
tune$bestTune

pred <- predict(object=tune, as.matrix(data.test),type='raw')
conf.mat <- confusionMatrix(pred, ytest)
cm.plot(conf.mat$table)
conf.mat$overall
conf.mat$byClass

imp <- abs(coef(tune$finalModel,tune$bestTune$lambda))
impdf <- data.frame(names = row.names(imp), imp = imp[,1])
impdf <- impdf[order(impdf$imp, decreasing = TRUE),]
impdf[1:30,]

pred.elasticnet <- predict(object=tune, as.matrix(data.test),type='prob')
rocCurve.elasticnet   <- roc(response = ytest, predictor = pred.elasticnet[, "drame"])
rocCurve.elasticnet$auc

# Analyse des courbes ROC
# On rappelle que la courbe ROC est le tracé de la sensibilité (taux de positifs bien prédits) contre 1- spécificité (taux de faux positifs), la meilleure performance est obtenue pour le point en haut à gauche (0,1) puisqu’on a 100% de vrais positifs et 0% de faux positifs.

plot(rocCurve.rpart,col=1)
abline(v=1)
plot(rocCurve.rf,col=2,add=TRUE)
plot(rocCurve.elasticnet,col=3,add=TRUE)
legend(0, 0.7, c('rpart','forest','elasticnet'), 1:3)

# On a terminé
stopCluster(cluster)
registerDoSEQ()

# Classification non supervisée
# Au lieu de classer les articles en utilisant les catégories, regardons comment ils se regroupent si on ne fixe pas d’a priori.

# K-means
# Les k-means sont probablement l’algorithme le plus utilisé en classification non supervisée, on va utiliser ici une variante : les kmeans sphériques (car les kmeans reposent sur la norme euclidienne ce qui n’est pas pertinente si la matrice est très sparse, ce qui est en particulier une caractéristique des données textuelles).
# Ici on doit choisir si on veut étudier les regroupements de mots ou de documents.
# Commençons par étudier les regroupements de mots (ils sont regroupés s’ils sont utilisés dans les mêmes articles). On utilise alors la matrice terme document (transposée de la matrice document terme)
install.packages("skmeans")
library(skmeans)
tdm <-TermDocumentMatrix(documents) #rq on peut aussi changer la pondération pour toutes ces méthodes et regarder l'impact, par exemple pour la pondération tfidf, on rajoute l'argument weight=weightTfIdf
dim(tdm)
tdmss <- removeSparseTerms(tdm, 0.99) #on retire les mots les plus rares pour réduire un peu la dimension, ie ceux qui apparaissent dans moins d'1% des documents.   
dim(tdmss)
sk <- skmeans(x=tdmss, 20) # on a choisi de voir ce qui ressortait pour 6 classes (pour voir si on retrouve les 6 thèmes utilisés pour classer les articles)
## proportion par classe:
table(sk$cluster)/sum(table(sk$cluster))
for (i in 1:20){
  print(paste('cluster ',i,sep=' '))
  print(names(sk$cluster)[sk$cluster==i][1:50])
  print('---------------')
}

# On peut imprimer les mots par cluster, mais ce n’est pas forcément évident d’en tirer du sens si on a beaucoup de mots de vocabulaire comme c’est le cas ici. Il faut bien voir que quelle que soit la méthode retenue pour le clustering, un gros travail manuel d’interprétation sera nécessaire ensuite pour en tirer du sens.
# 
# Considérons alors les regroupements de documents (en fonction de s’ils utilisent les mêmes mots). Un peu comme on a cherché à le faire avec la classification supervisée mais sans a priori sur les catégories de rangement. Réappliquons les skmeans mais sur la matrice document terme cette fois, on classe donc bien les documents et non plus les mots, mais en revanche on peut ensuite produire les nuages de mots par groupe de documents (ce qui peut aider à comprendre les regroupements d’articles en visualisant les mots les plus fréquents).
# 
# (rq pour les wordcloud, ça peut valoir le coup d’utiliser stemCompletion (cf début td) pour avoir de plus jolis wordcloud, ici les mots sont racinisés)

dtm <-DocumentTermMatrix(documents)
dim(dtm)
dtmss <- removeSparseTerms(dtm, 0.99)   
dim(dtmss)

sum(dtmss$Terms == 0)

library(skmeans)
## On partitionne en 6 clusters.
sk <- skmeans(x=dtmss, 15)
## On regarde la répartition dans les clusters
table(sk$cluster)/sum(table(sk$cluster))

dtmss_m <- as.matrix(dtmss)
for (i in 1:6){
  m <- dtmss_m[sk$cluster==i,]
  wordsFreq <- sort(colSums(m),decreasing = TRUE)
  wordsFreq <- data.frame(word = names(wordsFreq), Freq = as.vector(wordsFreq)) # mise au format pour wordcloud2
  print(wordcloud2(data = wordsFreq[1:500,],minSize = 1, size = 3))}

#d <- data.frame(freq = sort(colSums(m),decreasing=TRUE)) #frequence d'apparition des mots
#wordcloud(words = rownames(d), freq = d$freq, min.freq = 1,max.words=500, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),scale = c(7, 0.1),vfont=c("serif","plain"))}

# Classification ascendante hiérarchique
# Testons la CAH sur les mêmes observations, c’est tout de même un peu long car on calcule une grosse matrice de distances (on choisit la similarité cosinus comme distance entre mot puisqu’on est sur du texte ici donc une matrice très sparse pour laquelle la norme euclidienne n’aurait pas grand sens)

library(cluster)  
library(proxy)

d <- dist(dtmss_m, method="cosine")   #attention quand dist vient de proxy, elle veut une matrice en argument
fit <- hclust(d=d, method="ward.D")   
fit 

plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=10, border="red")

table(groups)

for (i in 1:6){
  m <- dtmss_m[groups==i,]
  wordsFreq <- sort(colSums(m),decreasing = TRUE)
  wordsFreq <- data.frame(word = names(wordsFreq), Freq = as.vector(wordsFreq)) # mise au format pour wordcloud2
  print(wordcloud2(data = wordsFreq[1:500,],minSize = 1, size = 3))}

#d <- data.frame(freq = sort(colSums(m),decreasing=TRUE)) #frequence d'apparition des mots 
#wordcloud(words = rownames(d), freq = d$freq, min.freq = 1,max.words=500, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"),scale = c(7, 0.1),vfont=c("serif","plain"))}

# Topic Modelling : LDA
# On va regarder à présent les résultats d’une LDA, l’intérêt c’est que l’on regroupe les documents et les mot simultanément cette fois et que cela permet une exploration de résultats beaucoup plus aisée. La première chose à faire est de vérifier que l’on a pas de documents sans mot (notamment après avoir filtré sur la sparsité/parcimonie).
# 
# La LDA s’applique sur une matrice documents termes sans pondération (nombre d’occurrences).

rowTotals <- apply(dtmss , 1, sum) # calcule la somme des termes dans chaque document
dtmss   <- dtmss[rowTotals> 0, ] # retire les documents vides, il peut arriver après nettoyage de se retrouver avec des documents vides, par exemple si on travaille sur des documents très courts type tweet
dim(dtmss)

# La LDA est estimée par Gibbs sampling, il faut fixer un certain nombre de paramètres

library(topicmodels)
# Paramètres du Gibbs sampling
burnin <- 500
iter <- 500
thin <- 100
seed <- list(2003,5,63,100001,765) # arbitraire
nstart <- 5
best <- TRUE

# Nombre de thèmes
k <- 10 # arbitraire à ce stade, nécessiterait d'être optimisé

# Estimation de la LDA
ldaOut <-LDA(dtmss,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
# L’estimation peut s’avérer un peu longue On peut désormais explorer les résultats

ldaOut.topics <- as.matrix(topics(ldaOut))
table(ldaOut.topics)

# Les mots les plus discriminants pour les topics (ici on en prend 5 arbitrairement) :
  
ldaOut.terms <- as.matrix(terms(ldaOut,5))
  
# On peut vouloir regarder la distribution des topics dans les documents. Pour labéliser les topics, on concatène les 5 top words.
  
topicProbabilities <- as.data.frame(ldaOut@gamma)
names(topicProbabilities) <- apply(data.frame(ldaOut.terms),2,function(x) paste(x,collapse=' '))
topicProbabilities[1:10,]  #pour les dix premiers documents arbitrairement, on voit que la plupart du temps un topic est prépondérant.
ldaOut.terms

# Il faudrait bien sûr y passer un peu plus de temps pour en extraire une information intéressante, mais ça peut permettre de récupérer le ou les thèmes principaux de chaque document par exemple.

# Pour explorer les topics de façon plus globale et intéractive, un package très intéressant LDAvis La subtilité c’est qu’il a besoin du corpus en plus de la sortie de la LDA précédente, donc on doit filtrer le corpus pour ne garder que les documents qui ont effectivement servi.

#pour ldavis
indices <- rownames(as.matrix(dtmss))
documents_ <- Corpus(VectorSource(data$content[indices]))

# La fonction suivante met les données au format souhaité pour la visualisation. Si tout va bien votre navigateur devrait s’ouvrir… ATTENTION A DECOMMENTER LA DERNIERE LIGNE Des explications sur les différents paramètres sont données ici http://cpsievert.github.io/slides/LDA/0926/#/4
  
library(topicmodels)
library(dplyr)
library(stringi)
library(tm)
library(LDAvis)

topicmodels_json_ldavis <- function(ldaOut, corpus, doc_term){
  phi <- posterior(ldaOut)$terms %>% as.matrix
  theta <- posterior(ldaOut)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  freq_matrix <- data.frame(ST = colnames(doc_term), Freq = colSums(as.matrix(doc_term)))
  
  # Convertit en json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

json <-topicmodels_json_ldavis(ldaOut,documents_,dtmss)
#decommenter pour lancer l'app
#serVis(json, out.dir = 'vis', open.browser = TRUE)

# Analyse textuelle : matching de chaines de caracteres
# Le matching peut servir à merger des bases disposant chacune d’une colonne texte proche mais pas exactement comparable (exemple typique : des adresses).

# un exemple : similarité cosinus
# Dans ce premier exemple on cherche à coder les offres d’emploi récupérées sur le site du bon coin à l’aide d’une codification existante, par exemple la codification Rome des métiers. Pour cela on a un intitulé par offre, la colonne ‘X.Offre.’ mais il n’y a aucune raison pour qu’il matche exactement avec l’intitulé tel qu’il est donné dans la codification Rome officielle.
# 
# Dans ce cas là on harmonise les deux champs textes (intitulé du bon coin et intitulé Rome), et on va chercher ensuite par similarité cosinus l’intitulé Rome matchant le mieux l’intitulé du bon coin.
# 
# Certaines opératons prennent du temps, montrons le seulement sur un échantillon.

data <- read.csv("/Volumes/Data2/Datasets/data_formation_tm_jobs.csv", sep="|",quote="",header=TRUE, nrows=6, encoding = "UTF-8")
head(data)

# On renomme le champ d’intérêt “offre”. Jetons un oeil aux données

names(data)[names(data)=="X.Offre."] <- "offre"
data$offre[1:2]
dim(data)

# On charge également la codification Rome, le libéllé que l’on cherche à matcher est la colonne “libelle_rome”
rome <- read.table('/Volumes/Data2/Datasets/rome_fap_ameliore.csv',sep=',',header=TRUE, stringsAsFactors =FALSE)
rome$libelle_rome[1:2]
dim(rome)

# Pour vectoriser les 2 champs tout en utilisant les mêmes dictionnaires/vocabulaires, on les concatène simplement. En gros on fait un même corpus avec les corpus initiaux.

docs <- c(rome$libelle_rome,as.character(data$offre))

# La suite vous connaissez…
library(stringi)
library(stringr)
library('SnowballC')
documents <- Corpus(VectorSource(docs))
documents <- tm_map(documents, content_transformer(gsub), pattern = "<br>", replacement = " ")
accent <- function(x) stri_trans_general(x, "Latin-ASCII")
documents <- tm_map(documents, content_transformer(accent))
documents <- tm_map(documents, content_transformer(gsub), pattern = "\\([^\\)]+\\)", replacement = " ")
documents <- tm_map(documents, content_transformer(gsub), pattern = "[^a-zA-Z]", replacement = " ")
documents <- tm_map(documents, content_transformer(tolower))
stopwords_fr = c(stopwords("french"),'a','h','euros','lundi','mardi','mercredi','jeudi','vendredi','samedi','dimanche')
stopwords_fr = setdiff(stopwords_fr, c("pas"))
documents <- tm_map(documents, removeWords, stopwords_fr)
lapply(documents[1:2],as.character)

documents <- tm_map(documents, stripWhitespace) #n'enleve pas le tout premier espace
documents <- tm_map(documents, content_transformer(gsub), pattern = "^\\s+", replacement = "")
#documents <- tm_map(documents, PlainTextDocument)  # needs to come before stemming
documents <- tm_map(documents, stemDocument, "french")
lapply(documents[1:2],as.character)

tdm.mat <- as.matrix(TermDocumentMatrix(documents))

install.packages("lsa")
library(lsa)
match <- function(i){
  temp<-sapply(1:nrow(rome),function(j) cosine(tdm.mat[,j],tdm.mat[,nrow(rome)+i]))
  return(rome$libelle_rome[which.max(temp)])}
# Ca semble marcher un peu ! … mais pas toujours

data$libelle_rome <- sapply(1:nrow(data), function(i) match(i))
head(data[,c('offre','libelle_rome')])

# un deuxieme exemple : la distance de Darau Levenshtein
# Cet exemple est proche du précédent, il permet de montrer que ce genre de manipulation peut permettre de merger sur des noms de communes par exemple. Et comment on peut utiliser la distance de Levenshtein.

data <- read.csv('/Volumes/Data2/Datasets/data_formation_tm_jobs.csv', sep="|",quote="",header=TRUE, encoding = "UTF-8")
head(data)
names(data)[names(data)=="X.Ville."] <- "commune"
data$commune[1:2]
dim(data)
data<-data['commune']
head(data)

# Le nom de commune comprend le nom et le code postal, nous devons séparer les deux. On part du principe qu’il n’y a qu’un seul espace pas simplicité mais en réalité, cela vaudrait le coup d’être vérifier ! Il y a peut-être un petit travail supplémentaire pour les noms avec article, mais cela dépasse le cadre de cet exercice.

data$nom_commune <- sapply(data$commune, function(x) strsplit(as.character(x)," ")[[1]][1])
data$codes_postaux <- sapply(data$commune, function(x) strsplit(as.character(x)," ")[[1]][2])
library(stringr)
data$codes_postaux <- sapply(data$codes_postaux, function(x) str_replace_all(x, '"',''))
# On charge un jeu de données avec les géolocalisations des communes, issu de data.gouv.fr
communes <- read.csv('/Volumes/Data2/Datasets/commune_loc_datagouv.csv',encoding ='UTF-8',stringsAsFactors =FALSE)
communes <- unique(communes[,c('nom_commune','codes_postaux','latitude','longitude')])
# A ce stade on peut tenter de fusionner les deux bases…

# a ce stade on pourrait tester un merge, on sent bien que ça va pas trop marcher
data_ <- merge(data,communes,by=c('nom_commune','codes_postaux'))
dim(data)
dim(data_) # échec !

# On harmonise les deux champs communes
library(stringi)
clean_string <- function(string){
  # Lowercase
  temp <- tolower(string)
  temp <- stri_trans_general(temp, "Latin-ASCII")
  temp <- str_replace_all(temp,"[^a-zA-Z]", " ") #nombres et ponctu espaces
  temp <- str_replace_all(temp, "[\\s]+","")
  return(temp)
}

data$clean_commune <- sapply(data$nom_commune, function(string) clean_string(string))
data$clean_commune[1:2]
communes$clean_commune <- sapply(communes$nom_commune, function(string) clean_string(string))
communes$clean_commune[1:2]
# Cette fois-ci, on retente le merge
data_ <- merge(data,communes,by=c('clean_commune','codes_postaux'))
dim(data)
dim(data_) # déjà bien mieux !
# Repérons les cas où les communes du bon coin ne sont pas trouvées dans datagouv (on passe outre les problèmes de codes postaux ici, d’où la distinction entre missed et reallymissed)
data_ <- merge(data,communes,by=c('clean_commune','codes_postaux'),all.x=TRUE) #on force le merge
dim(data_)
missed <- unique(data_$clean_commune[is.na(data_$latitude)]) #les communes du bon coin qui n'ont pas trouvé d'équivalent dans le fichier data.gouv, n'ont pas de géoloc
length(missed)
reallymissed <- missed[sapply(missed, function(i) length(grep(i,communes$clean_commune))==0)] #mais pour certaines c'est un autre pb, la commune correspond bien mais pas le code postal, c'est pour ça que le merge n'a pas fonctionné, on les retire
reallymissed # les communes qui ne trouvent finalement pas d'equivalent dans data.gouv
# La fonction grep est intéressante à connaître, elle cherche notamment si une chaîne de caractère en contient une autre et le cas échéant retourne la position dans un tableau de chaînes.
communes$clean_commune[grep("paris",communes$clean_commune)]
# Pour les communes du bon coin qui ne matchent pas, on va regarder celles du fichier data.gouv qui sont les plus proches au sens de la distance de Damerau Levenshtein (nombre d’éditions pour passer de l’une chaine de caractère à l’autre, l’inversion comptant pour une édition), attention ça met un peu de temps à tourner !
# La distance en question est repérée par la méthode dl de la fonction stringdist de la librairie stringdist. On donne un exemple bateau sur “ca” et “abc”. La fonction paste sert juste à concaténer des chaines de caractères pour afficher la commune du bon coin, la commune data.gouv la plus proche, et la distance correspondante.
install.packages("stringdist")
library(stringdist)
stringdist("ca","abc",method="dl")
levenshtein_prop <- function(com){
  temp<-sapply(communes$clean_commune,function(i) stringdist(i,com,method='dl'))
  return(paste(c(com, communes$clean_commune[which.min(temp)],as.character(min(temp))),collapse="  "))}
lapply(reallymissed, levenshtein_prop)
# On voit que dans certains cas, le merge a raté à cause de l’existence d’un article “Saint Aubin De Baubigné” vs “Saint Aubin Baubigné” Bon cet exemple n’est pas le plus utile à cause des nouvelles, anciennes communes etc… mais ça permet d’illustrer l’usage de ce genre de fonctionnalités.

# Un peu d’analyse sémantique
# Petit exemple autour de l’analyse de sentiment et des dictionnaires annotés
# L’approche classique consiste à utiliser un dictionnaire annoté. On va regarder ce que ça donne sur un échantillon des articles scrapés sur le site du Monde. Ils sont déjà nettoyés cette fois-ci. L’idée est de produire un indicateur de sentiment (très frustre) et de regarder son évolution dans le temps.

lemonde <- read.table('/Volumes/Data2/Datasets/lemonde_formation_tm.csv',header=TRUE) 
head(lemonde)
# On va avoir besoin d’une variable année
lemonde$annee <- sapply(lemonde$date, function(x) substr(as.character(x),1,4))
# On charge aussi un dictionnaire annoté manuellement (il s’agit d’un extrait ici, c’est pourquoi il n’y a que de mots positifs en réalité)
dico <- read.table('/Volumes/Data2/Datasets/dico_formation_tm.csv',header =TRUE)
head(dico)
# On le scinde en 2 listes de mots respectivement positifs et négatifs
positive_words <- dico[dico$SO=='POSITIVE','keyword']
negative_words <- dico[dico$SO=='NEGATIVE','keyword']
# Il ne reste plus qu’à compter, pour chaque article le nombre de mots présents dans la liste des mots positifs, moins le nombre de mots présents dans la liste des mots négatifs et on normalise par le nombre de mots total.
count_word <- function(x,lexicon_pos,lexicon_neg){
  txt <- as.character(x)
  words <- strsplit(txt," ")[[1]]
  return((sum(words %in% lexicon_pos)-sum(words %in% lexicon_neg))/length(words))}

lemonde$so <- sapply(lemonde$content,function(txt) count_word(txt,positive_words,negative_words))
mean(lemonde$so)

# On moyenne par année pour avoir un indicateur lissé. “So” signifie sentiment orientation.
aggdata <-aggregate(lemonde$so, by=list(lemonde$annee),FUN=mean, na.rm=TRUE)
names(aggdata)<-c('date','so')
# On va comparer avec l’indice de climat des affaires pour la France, calculé par l’insee. Cet indice est mensuel, on le moyenne aussi à l’année (évidemment tout cela mériterait d’être fait de façon bien plus fine mais pour l’exercice on travaille sur un petit échantillon d’articles)
climat <- read.table('/Volumes/Data2/Datasets/business_climate.csv',header=TRUE,sep=';')
climat$annee <- sapply(climat$date,function(x) substr(as.character(x),1,4))
head(climat)

climat <-aggregate(climat$business_climate, by=list(climat$annee),FUN=mean, na.rm=TRUE)
names(climat)<-c('date','business_climate')
# On fusionne les deux bases et on trace le graphique
aggdata <- merge(aggdata,climat[,c('date','business_climate')],by='date')
label <- aggdata$date
aggdata$x <- 1:nrow(aggdata)

par(mar = c(5,5,2,4))
with(aggdata, plot(x, business_climate, type="l", col="red3", 
                   ylab='climat',
                   ylim=c(70,115)))
par(new = T)
with(aggdata, plot(x, so, type='l',col='black', axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(1,at=1:length(label),labels=label)
axis(side = 4)
mtext(side = 4, line = 3, 'Number genes selected')
legend("topleft",
       legend=c('climat', "so"),
       lty=c(1,1), col=c("red3", "black"))
axis(1,at=1:length(label),labels=label)


# Word2vec
# Certains chercheurs mettent à disposition des modèles word2vec qui peuvent être considérés comme des tables de correspondance entre des mots et un vecteur numérique dans un espace d’une certaine taille (en générale très inférieure à la taille du vocabulaire qui est, on le rappelle, grande). Cet espace est un espace “sémantique” en quelques sortes.
# 
# L’apprentissage de ces énormes réseaux neuronaux ont été faits sur de très gros corpus (comme Wikipédia par exemple), et leur réutilisation à d’autres fins s’appelle le transfer learning. C’est ce que nous faisons ici.
# 
# attention pour l’installation, le package n’est pas sur CRAN
install.packages("devtools")
library(devtools)
devtools::install_github("bmschmidt/wordVectors")

# On charge le modèle, c’est un peu long…
library(wordVectors)
library(magrittr)
model = read.vectors("frWac_non_lem_no_postag_no_phrase_200_cbow_cut100.bin")
# Dans cet espace numérique, il a été montré que deux mots proches (au sens d’une distance mathématique donc) avaient des sens proches. C’est un énorme gain par rapport à l’approche bag of words où deux synonymes peuvent avoir une similarité apparente très faible du fait qu’ils sont rarement utilisés ensemble dans les mêmes documents (lorsque la similarité est calculée via les colonnes de la matrice documents termes typiquement).
model %>% closest_to("economie") # montre les mots les plus proche de economie dans cet espace
#Les propriétés mathématiques et sémantiques de ces modèles ont en fait leur renommée :
model %>% closest_to(~ "homme" - "il" + "elle")
model %>% closest_to(~ "roi" - "il" + "elle")
# (ok, on a un petit souci d’accent..)
# On peut représenter les mots graphiquement :
terms = closest_to(model,model[[c("croissance","rÃ©cession","chomage","impot")]],50) #on peut récupérer les mots les plus similaires à plusieurs références
eco = model[[terms$word,average=F]] #average = FALSE, on prend la version numérique de chaque mot
plot(eco,method="pca")
