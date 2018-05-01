
rm(list = ls())
#vignette("selectorgadget")

#install.packages("rvest")
library(rvest)
library(dplyr)
library(httr)
#library(jpeg)
#install.packages("imager")
library(imager)
library(stringr)
library(RCurl)
#install.packages("doParallel")
#library(doParallel)

options(stringsAsFactors=F)
source("allocine_scraping_functions.R")


#démarrer en 1919
#extraction de la liste de toutes les url d'un mois de sortie donné, avec le nom de chaque film

data_dir<- "data_tables/"
url_dir<- "data_urls/"



date_deb<-date()
liste_annees <- 1966:1985
mois<- c(paste0("-0",1:9), paste0( "-", 10:12))

#liste_anneesmois<-c("1966-01","1966-02","1966-03","1966-04","1966-05","1966-06","1966-07","1966-08")
#liste_anneesmois<-character(0)
for (annee in liste_annees) {
    liste_anneesmois <- c(liste_anneesmois,paste0(annee,mois))
    }
liste_anneesmois<- sort(liste_anneesmois, decreasing =TRUE)


# table_principal_total<- table_principal_total[table_principal_total$mois_sortie!="198601",]
# table_casting_total<- table_casting_total[table_casting_total$mois_sortie!="198601",]

# On parcourt la liste de tous les anneesmois
for (idd_mois in liste_anneesmois) {
    

    url_mois<- paste0("http://www.allocine.fr/film/agenda/mois/mois-",idd_mois,"/")

    liens_mois <- html_session(url_mois)
    liste_url_mois<- paste0("http://www.allocine.fr",unlist(html_nodes(liens_mois,"h3 a") %>% html_attrs()))
    
    textes_mois<- read_html(url_mois)
    noms_films<- textes_mois %>% html_nodes("#col_main strong") %>% html_text() %>% gsub(pattern="\\n",x=.,"")
    if (length(noms_films)==0) {next}
    names(liste_url_mois)<- noms_films
    
    annee<-unlist(str_extract_all(idd_mois, "[0-9]+"))[1]
    
    #rm(table_principal_total,table_principal)
    for (ind_film in 1:length(liste_url_mois)) {

    print(paste0("progression en cours : ", ind_film, " sur ", length(liste_url_mois), " , mois ", idd_mois, " , annee ", annee))  
        
    #url_film<- "http://www.allocine.fr/film/fichefilm_gen_cfilm=61282.html"
    url_film<- liste_url_mois[ind_film]
    names(url_film)<- "url"
    
    id_film<-unlist(str_extract_all(url_film, "[0-9]+"))
    names(id_film)<- "id_film"

    id_mois<- gsub("-","",idd_mois)
    names(id_mois)<-"mois_sortie"
    id<-paste0(id_mois,"_",id_film)
    
    nom_film<- noms_films[ind_film]
    #nom_film<- "La forme de l'eau"
    names(nom_film)<- "titre"
    
    
    ###page principale
    
    url_principal_brut<-getURL(url_film)
    nom_save_principal<-paste0(url_dir,id,"_01principal",".txt")
    if(!file.exists(nom_save_principal)) {
    write.csv(url_principal_brut,nom_save_principal)
    }
    
    
    principal<- read_html(url_principal_brut)
    
    affiche<- principal %>% html_nodes(".col-md-4 .thumbnail-img") %>% html_attr("src")
    
    nom_save_pprincipal<-paste0(url_dir,id_film,"_01principal_photo",".jpg")
    if(!file.exists(nom_save_pprincipal)) {
    GET(affiche, write_disk(nom_save_pprincipal, overwrite=TRUE))
    }
    
    #Sys.sleep(0.5)
    
    meta_body<- principal %>% html_nodes(".meta-body-item") %>% html_text()
    
    date_duree<- meta_body[grepl("Date de sortie",meta_body)]
    if (length(date_duree)==0) {
        date_duree<-""
        duree<-""} else {
        date_duree<- unlist(str_split(date_duree,"\\n"))
        date_duree<- str_trim(date_duree)
        date_duree<- setdiff(date_duree[date_duree!=""],"Date de sortie")
        date_sortie<- date_duree[1]
    
        duree<- date_duree[grep("[(.*)]",date_duree)]
        duree<- gsub("\\(|\\)| ","",duree)
    }
    names(date_sortie)<-"date_sortie"

    
    date_reprise<- meta_body[grepl("Date de reprise",meta_body)]
    if (length(date_reprise)==0) {
        date_reprise<-"" 
        duree2<-""} else {
        date_reprise<- unlist(str_split(date_reprise,"\\n"))
        date_reprise<- str_trim(date_reprise)
        date_reprise<- setdiff(date_reprise[date_reprise!=""],"Date de reprise")
        date_reprise<- date_reprise[1]
    
        duree2<- date_reprise[grep("[(.*)]",date_reprise)]
        duree2<- gsub("\\(|\\)| ","",duree2)
    }
    names(date_reprise)<-"date_reprise"

    if ( length(duree)==0 ) { if ( length(duree2)!=0 ) {duree<- duree2} else {duree<- ""} }
    names(duree)<-"duree"
    
    
    
    genres<- meta_body[grepl("Genres|Genre",meta_body)]
    genres<- unlist(str_split(genres,"\\n"))
    genres<- str_trim(genres)
    genres<- setdiff(genres[genres!=""],c("Genres","Genre","plus"))
    genres<- gsub(",","",genres)
    
    genres_format<- format_classique(genres,"genre",5)
    
    
    nationalites<- str_trim(principal %>% html_nodes(".nationality") %>% html_text())
    nationalites<- setdiff(nationalites,"plus")
    nationalites<- gsub(",","",nationalites)
    
    nationalites_format<- format_classique(nationalites,"nationalite",3)
    
    
    synopsis<- principal %>% html_nodes(".content-txt") %>% html_text()
    if (length(synopsis)==0) {synopsis<-""} else {
        synopsis<- gsub("\\n","",synopsis)
        synopsis<- str_trim(gsub("\\r"," ",synopsis))
        }
    names(synopsis)<-"synopsis"
    
    
    
    #à obtenir comme différence avec le nom des rubriques standards
    titre_original<- principal %>% html_nodes("h2") %>% html_text()
    liste_rubriques <-c("Synopsis et détails","Séances","Regarder ce principal","Bandes-annonces","Interviews, making-of et extraits",
                        "Critiques Presse","Critiques spectateurs","Photos","Secrets de tournage","Dernières news",
                        "Si vous aimez ce principal, vous pourriez aimer ...","Commentaires","Acteurs et actrices")
    titre_original<-setdiff(titre_original,liste_rubriques)
    titre_original<-str_trim(titre_original)
    
    
    synopsis_details<- principal %>% html_nodes(".that") %>% html_text()
    synopsis_details<- gsub("\\n","",synopsis_details)
    synopsis_details<- as.vector(str_trim(synopsis_details))
    names_synopsis_details<- principal %>% html_nodes("#synopsis-details .light") %>% html_text()
    names_synopsis_details<-str_trim(names_synopsis_details)
    names(synopsis_details)<- if (sum(names_synopsis_details=="Récompense") !=0) {
        gsub("Récompense","Récompenses",names_synopsis_details)} else {names_synopsis_details}
    
    synopsis_details_format<-character(17)
    names(synopsis_details_format)<-c("Titre original","Distributeur","Récompenses","Année de production","Date de sortie DVD",
                                        "Date de sortie Blu-ray","Date de sortie VOD","Type de film","Secrets de tournage",
                                        "Box Office France","Budget","Langues","Format production","Couleur","Format audio",
                                        "Format de projection","N° de Visa")
    synopsis_details_format<- synopsis_details[names(synopsis_details_format)]
    synopsis_details_format[is.na(synopsis_details_format)]<-""
    synopsis_details_format[synopsis_details_format=="-"]<-""
    
    names(synopsis_details_format)<-c("titre_original","distributeur","recompenses","annee_production","date_sortie_dvd",
                                      "date_sortie_bluray","date_sortie_vod","type_film","secrets_tournage",
                                      "boxoffice_france","budget","langues","format_production","couleur","format_audio",
                                      "format_projection","num_visa")
    
    if (!exists("table_principal_total")) {
        table_principal_total<- as.data.frame(t(c(id_film,id_mois,url_film,nom_film,date_sortie,date_reprise,duree,genres_format,nationalites_format,synopsis,synopsis_details_format)))
    } else {
        table_principal<- as.data.frame(t(c(id_film,id_mois,url_film,nom_film,date_sortie,date_reprise,duree,genres_format,nationalites_format,synopsis,synopsis_details_format)))
        table_principal_total<- rbind(table_principal_total,table_principal)
    # } else {
    #     stop("film déjà présent dans la table")
    #
    }   
    
    
    write.csv2(table_principal_total[table_principal_total$mois_sortie==id_mois,], paste0(data_dir,"table_principal_", id_mois, ".csv"))
    write.csv2(table_principal_total[unlist(str_extract_all(table_principal_total$mois_sortie,"[0-9]..."))==annee,], paste0(data_dir,"table_principal_", annee, ".csv"))

    
    ##casting

    
    list_films<-list()
    list_acteurs<-list()
    list_fonctions<-list()


    casting_ok <- FALSE

    for (ii in 1:7) { # Parcours des differents entetes de page
    test<- html_nodes(principal, paste0(".js-item-mq:nth-child(",ii,")")) %>% html_attrs() %>% unlist()
    if (!is.null(test)) { if (!is.na(test[2])) { if (test[2] =="Casting")  casting_ok <- TRUE }}
    }

    
    if (casting_ok) {
    
        url_film<- liste_url_mois[ind_film]
        #url_film<- "http://www.allocine.fr/film/fichefilm_gen_cfilm=222692.html"
        names(url_film)<- "url"
        
        id_film<-unlist(str_extract_all(url_film, "[0-9]+"))
        names(id_film)<- "id_film"
        
        id_mois<- gsub("-","",idd_mois)
        names(id_mois)<-"mois_sortie"
        id<-paste0(id_mois,"_",id_film)
        
        nom_film<- noms_films[ind_film]
        #nom_film<- "Livre de la jungle"
        names(nom_film)<- "titre_film"
        
        
        url_film_core<- "http://www.allocine.fr/film/fichefilm-"
        url_casting<-paste0(url_film_core,id_film,"/casting/")
        names(url_casting)<- "url"
        
        url_casting_brut<-getURL(url_casting)
        nom_save_casting<-paste0(url_dir,id,"_02casting",".txt")
        if(!file.exists(nom_save_casting)) {
        write.csv(url_casting_brut,nom_save_casting)
        }
        
        #Sys.sleep(1)
        
        casting<-read_html(url_casting_brut)
        
        links<- casting %>% html_nodes(".meta-title-link") %>% html_attr("href")
        links_personnes<-  casting %>% html_nodes(".meta-title-link") %>% html_text()
        links_personnes<- unlist(str_split(links_personnes,"\\n"))
        links_personnes<- str_trim(links_personnes)
        links_personnes<- links_personnes[links_personnes!=""]
        names(links)<- links_personnes
        links<- links[!is.na(links)]
        
        
        realisation<- casting %>% html_nodes(".casting-director .meta-title-link") %>% html_text()
        realisation<- unlist(str_split(realisation,"\\n"))
        realisation<- str_trim(realisation)
        realisation<- realisation[realisation!=""]
        
        realisation_format<- format_classique(realisation,"realisation",2)
        id_realisation<- format_id(realisation, realisation_format, "id_realisation")
        
        
        
        acteurs1<- casting %>% html_nodes("#actors .meta-title-link") %>% html_text()
        acteurs1<- unlist(str_split(acteurs1,"\\n"))
        acteurs1<- str_trim(acteurs1)
        acteurs1<- acteurs1[acteurs1!=""]
        
        acteurs2<- casting %>% html_nodes("#actors .link") %>% html_text()
        acteurs2<- unlist(str_split(acteurs2,"\\n"))
        acteurs2<- str_trim(acteurs2)
        acteurs2<- acteurs2[acteurs2!=""]
        
        acteurs<-c(acteurs1,acteurs2)
        
        acteurs_format<- format_classique(acteurs,"acteur",20)
        acteursprincipaux<- format_classique(acteurs,"acteur",8)
        id_acteursprincipaux<- format_id(acteurs, acteursprincipaux, "id_acteur")
        
        
        
        roles1a<- casting %>% html_nodes("#actors .titlebar+ .row-col-padded-10") %>% html_text()
        roles1a<- unlist(str_extract_all(roles1a, "Rôle(.*)\\n"))
        roles1a<- gsub("\\n","",roles1a)
        roles1a<- gsub("Rôle : ","",roles1a)
        
        roles1b<- casting %>% html_nodes(".row-col-padded-10+ .row-col-padded-10 .row-col-padded-10") %>% html_text()
        roles1b<- unlist(str_extract_all(roles1b, "Rôle(.*)\\n"))
        roles1b<- gsub("\\n","",roles1b)
        roles1b<- gsub("Rôle : ","",roles1b)
        
        roles1<- c(roles1a,roles1b)
        
        roles2<- casting %>% html_nodes("#actors span") %>% html_text()
        roles2<- gsub("\\n","",roles2)
        roles2<- str_trim(roles2)
        roles2<- roles2[roles2!=""]
        if (length(roles2)!=0) {
            index_acteurs<- integer(length(acteurs))
            for (i in 1:length(acteurs)) {
                if (length( grep(acteurs[i],roles2) )!= 0) {
                    index_acteurs<- c(index_acteurs,grep(acteurs[i],roles2))
                } else {next}
            }
            index_acteurs<- setdiff(index_acteurs,0)
            roles2<- roles2[-index_acteurs]
        }
        
        if (length(acteurs)!=0) {
            roles<- c(roles1,roles2)[1:length(acteurs)]
        } else {roles<-""}
        
        roles_format<- format_classique(roles,"role",20)

                
        
        voix<- casting %>% html_nodes(".casting-voice") %>% html_text()
        
        voix_vo_total<- format_voix("Voix originales")
        voix_vo<- voix_vo_total[[1]]
        voix_vo_roles<- voix_vo_total[[2]]
        
        voix_vf_total<- format_voix("Voix locales")
        voix_vf<- voix_vf_total[[1]]
        voix_vf_roles<- voix_vf_total[[2]]
        
        voix_vo_format<- format_classique(voix_vo,"voix_vo",10)
        voix_vo_roles_format<- format_classique(voix_vo_roles,"voix_vo_role",10)
        voix_vf_format<- format_classique(voix_vf,"voix_vf",10)
        voix_vf_roles_format<- format_classique(voix_vf_roles,"voix_vf_role",10)
        
        voix_format<- c(voix_vo_format,voix_vo_roles_format,voix_vf_format,voix_vf_roles_format)
        
      
          
        autres<- casting %>% html_nodes(".casting-list .link") %>% html_text()
        
        autres_tout<- casting %>% html_nodes(".casting-list") %>% html_text()
        autres_tout<- gsub("\\n","",autres_tout)
        autres_tout<- str_trim(autres_tout)
        
        rubriques_autres_tout<- casting %>% html_nodes(".casting-list .titlebar-title-md") %>% html_text()
        
        autres_fonctions<-autres_tout
        autres_fonctions<- unlist(str_split(autres_fonctions,"  "))
        autres_fonctions<- autres_fonctions[autres_fonctions!=""]
        index_rubriques<- which(autres_fonctions %in% setdiff(rubriques_autres_tout,"Production"))
        autres_fonctions<- autres_fonctions[-index_rubriques]
        if (sum(autres_fonctions %in% c("Producteur","Productrice"))!=0) {
            index_prod<- min(which(autres_fonctions %in% c("Producteur","Productrice")))-1
            autres_fonctions<- autres_fonctions[-index_prod]
        }
        
        if (length(autres_fonctions)!=0) {
            names_autres_fonctions<-str_trim(autres_fonctions[seq(1,max(2,length(autres_fonctions))-1,2)])
            autres_fonctions<-autres_fonctions[seq(2,max(2,length(autres_fonctions)),2)]
            names(autres_fonctions)<-names_autres_fonctions
        } else {
            autres_fonctions<- character(0)
        }
        
        
        #formatage des fonctions voulues autres que réalisation et acting
        
        scenario<- format_postes("scenario", 3, c("Scénariste"))
        musique<- format_postes("musique", 2, c("Compositeur","Compositrice"))
        production<- format_postes("production", 5, c("Producteur", "Productrice"))
        productionexecutive<- format_postes("production_executive", 3, c("Producteur exécutif", "Productrice exécutive"))
        coproduction<- format_postes("coproduction", 3, c("Coproducteur", "Coproductrice"))     
        directionphoto<- format_postes("direction_photo", 2, c("Directeur de la photographie", "Directrice de la photographie"))          
        montage<- format_postes("montage", 2, c("Monteur", "Monteuse","Chef monteur","Chef monteuse"))
        assistrealisation<- format_postes("assist_realisation", 2, c("1er assistant réalisateur", "2ème assistant réalisateur","3ème assistant réalisateur","4ème assistant réalisateur")) 
        directionartistique<- format_postes("direction_artistique", 1, c("Directeur artistique","Directrice artistique")) 
        decoration<- format_postes("decoration", 1, c("Chef décorateur","Chef décoratrice","Décorateur","Décoratrice")) 
        costumes<- format_postes("costumes", 1, c("Chef costumier","Chef costumière","Costumier","Costumière")) 
        maquillage<- format_postes("maquillage", 1, c("Chef maquilleur","Chef maquilleuse","Maquilleur","Maquilleuse"))    
        directioncasting<- format_postes("direction_casting", 1, c("Directeur du casting","Directrice du casting")) 
        son<- format_postes("son", 1, c("Ingénieur son","Monteur son","Monteuse son")) 
        effetsvisuels<- format_postes("effets_visuels", 2, c("Superviseur des effets visuels","Effets visuels")) 
        effetsspeciaux<- format_postes("effets_speciaux", 2, c("Superviseur des effets spéciaux","Effets spéciaux"))    
        oeuvreorigine<- format_postes("oeuvre_origine", 1, c("D'après l'oeuvre de"))
        studioproduction<- format_postes("studio_production", 3, c("Production","Coproduction"))
        distribution<- format_postes("distribution", 1, c("Distributeur France (Sortie en salle)","Distributeur France (Reprise)"))
        
        autres_format<-c(scenario,musique,production,productionexecutive,coproduction,directionphoto,montage,assistrealisation,
                         directionartistique,decoration,costumes,maquillage,directioncasting,son,effetsvisuels,effetsspeciaux,
                         oeuvreorigine,studioproduction,distribution)
        
        if (!exists("table_casting_total")) {
            table_casting_total<- as.data.frame(t(c(id_film,id_mois,url_casting,nom_film,id_realisation,realisation_format,id_acteursprincipaux,acteurs_format,roles_format,voix_format,autres_format)))
        } else  {
            table_casting<- as.data.frame(t(c(id_film,id_mois,url_casting,nom_film,id_realisation,realisation_format,id_acteursprincipaux,acteurs_format,roles_format,voix_format,autres_format)))
            table_casting_total<- rbind(table_casting_total,table_casting)
        # } else {
        #     stop("film déjà présent dans la table")
        }
        
        # list_films[[ind_film]]<-  as.data.frame(t(c(id_film,id_mois,url_casting,nom_film,id_realisation,realisation_format,id_acteursprincipaux,acteurs_format,roles_format,voix_format,autres_format)))
        # list_acteurs[[ind_film]]<-length(acteurs)
        # list_fonctions[[ind_film]]<-names(autres_fonctions)
        # list_acteurs<-length(acteurs)
        # list_fonctions<-autres_fonctions
        
        write.csv2(table_casting_total[table_casting_total$mois_sortie==id_mois,], paste0(data_dir,"table_casting_", id_mois, ".csv"))
        write.csv2(table_casting_total[unlist(str_extract_all(table_casting_total$mois_sortie,"[0-9]..."))==annee,], paste0(data_dir,"table_casting_", annee, ".csv"))
        
    }#end if casting_ok


    }#end boucle sur les url d'un mois

}#end boucle sur les mois

date_fin<-date()
date_deb
date_fin




library(data.table)
liste_annees <- 1966:2018
table_principal<-data.frame()
table_casting<-data.frame()
for (annee in liste_annees) {
    table_principal<-rbind(table_principal,fread(paste0(data_dir,"table_principal_", annee, ".csv"))[,-c("V1")])
    table_casting<-rbind(table_casting,fread(paste0(data_dir,"table_casting_", annee, ".csv"))[,-c("V1")])
}
table_principal<-table_principal[order(mois_sortie, decreasing = T)]
table_casting<-table_casting[order(mois_sortie, decreasing = T)]

write.csv2(table_principal, paste0(data_dir,"table_principal",".csv"))
write.csv2(table_casting, paste0(data_dir,"table_casting",".csv"))


